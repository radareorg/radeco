//! Defines `Module` and `Function` that act as containers.

#![deprecated(since="0.2.0", note="Replace with `radeco_containers`")]

use frontend::bindings::{Binding, LocalInfo, RBind, RBindings, RadecoBindings};

use frontend::source::Source;
use frontend::ssaconstructor::SSAConstruct;
use middle::ir::MOpcode;
use middle::regfile::SubRegisterFile;
use middle::ssa::cfg_traits::CFG;
use middle::ssa::ssa_traits::{NodeType, SSA, SSAMod, SSAWalk};
use middle::ssa::ssastorage::{SSAStorage, Walker};

use petgraph::graph::NodeIndex;

use r2api::structs::{FunctionInfo, LOpInfo, LRegInfo, LVarInfo};
use std::{thread, fmt, sync, hash};
use std::collections::{BTreeSet, HashMap};

pub struct RadecoModule<'a, F: RFunction> {
    pub functions: HashMap<u64, F>,
    fname: HashMap<String, u64>,
    pub src: Option<&'a mut Source>,
    pub regfile: Option<SubRegisterFile>,
}

pub type DefaultFnTy = RadecoFunction<RadecoBindings<Binding<NodeIndex>>>;

impl<'a, F: RFunction + fmt::Debug> fmt::Debug for RadecoModule<'a, F> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "RadecoModule {{ functions: {:?}, fname: {:?} }}",
               self.functions,
               self.fname)
    }
}

impl<'a, F: RFunction> Default for RadecoModule<'a, F> {
    fn default() -> RadecoModule<'a, F> {
        RadecoModule {
            functions: HashMap::new(),
            fname: HashMap::new(),
            src: None,
            regfile: None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct RadecoFunction<B: RBindings> {
    // TODO: Should not be pub.
    // TODO: Too many members.
    pub ssa: SSAStorage,
    pub name: String,
    pub offset: u64,
    pub call_ctx: Vec<CallContext<B::Idx, NodeIndex>>,
    pub locals: Option<Vec<LVarInfo>>,
    pub datarefs: Option<Vec<u64>>,
    // callrefs: Vec<u64>,
    callxrefs: Vec<u64>,
    pub bindings: B,
}

#[derive(Clone, Debug)]
pub struct CallContext<Idx: Clone + fmt::Debug + Eq + Ord + hash::Hash + From<usize>,
                       S: fmt::Debug + Clone>
{
    /// Start offset of caller (uniquely identifies a function).
    pub caller: Option<u64>,
    /// Start offset of callee (uniquely identifies a function).
    pub callee: Option<u64>,
    /// Offset the call. Note that this will belong to the caller function.
    pub call_site: Option<u64>,
    pub ssa_ref: Option<S>,
    /// Translate a node in callees context into a node in caller's context.
    pub ctx_translate: HashMap<Idx, Idx>,
}

impl<Idx, S> CallContext<Idx, S>
    where Idx: Clone + fmt::Debug + Eq + Ord + hash::Hash + From<usize>,
          S: fmt::Debug + Clone
{
    pub fn new(caller: Option<u64>,
               callee: Option<u64>,
               call_site: Option<u64>,
               ctx: &[(Idx, Idx)])
               -> CallContext<Idx, S> {
        CallContext {
            caller: caller,
            callee: callee,
            call_site: call_site,
            ssa_ref: None,
            ctx_translate: ctx.into_iter().cloned().collect::<HashMap<_, _>>(),
        }
    }
}

// Implementations sepecific to `RadecoFunction`.
impl<B: RBindings> RadecoFunction<B> {
    fn new() -> RadecoFunction<B> {
        RadecoFunction {
            ssa: SSAStorage::new(),
            offset: 0,
            name: String::new(),
            call_ctx: Vec::new(),
            // callrefs: Vec::new(),
            callxrefs: Vec::new(),
            bindings: B::new(),
            locals: None,
            datarefs: None,
        }
    }

    pub fn construct(reg_profile: &LRegInfo, insts: Vec<LOpInfo>) -> RadecoFunction<B> {
        let mut rfn = RadecoFunction::new();
        {
            let mut constructor = SSAConstruct::new(&mut rfn.ssa, reg_profile);
            constructor.run(insts.as_slice());
        }
        rfn
    }
}

// Private function to construct SSA for a single function and fill in the basic
// information. Note: This function is threaded in module-ssa construction.
fn ssa_single_fn(f: &FunctionInfo,
                 reg_info: &LRegInfo,
                 instructions: Vec<LOpInfo>,
                 offset: u64)
                 -> DefaultFnTy {
    radeco_trace!("Construction SSA for: {:?}", f.name);
    let mut rfn = RadecoFunction::construct(reg_info, instructions);
    rfn.name = f.name.as_ref().unwrap().clone();
    if let Some(ref callrefs) = f.callrefs {
        rfn.call_ctx = callrefs.iter()
            .filter(|x| {
                match x.call_type {
                    Some(ref c) if c == "C" => true,
                    _ => false,
                }
            })
            .map(|x| {
                let call_site = x.source;
                let callee = x.target;
                let caller = Some(offset);
                CallContext::<usize, NodeIndex>::new(caller, callee, call_site, &[])
            })
            .collect();
    }

    if let Some(ref callxrefs) = f.codexrefs {
        rfn.callxrefs = callxrefs.iter()
            .filter(|x| {
                match x.call_type {
                    Some(ref c) if c == "C" => true,
                    _ => false,
                }
            })
            .map(|x| x.source.expect("Invalid address"))
            .collect::<BTreeSet<_>>()
            .into_iter()
            .collect();
    }
    rfn
}

// This function fixes up the call information.
//
// During SSA construction, call instructions are special cased, i.e. we do not use the
// corresponding esil for these instructions as inferring that these were call instructions from
// the string is additional effort. As a concequence, the call instructions have a comment string
// as an argument rather than an integer constant (address of the callee). This function iterates
// through all such call sites and fixes this information.
fn fix_call_info(rfn: &mut DefaultFnTy) {
    let mut call_info =
        rfn.call_ctx.iter().cloned().map(|x| (x.call_site.unwrap(), x)).collect::<HashMap<_, _>>();
    {
        let caller = rfn.offset;
        let ssa = rfn.ssa_mut();
        for node in ssa.inorder_walk() {
            if let Ok(NodeType::Op(MOpcode::OpCall)) = ssa.node_data(node).map(|x| x.nt) {
                // Fixup the call by converting a comment to a proper argument.
                let call_node = &node;
                let call_site =
                    ssa.address(*call_node).expect("No address information found").address;
                if let Some(info) = call_info.get_mut(&call_site) {
                    if let Some(arg_node) = ssa.operands_of(*call_node).get(0) {
                        let target_node = ssa.insert_const(info.callee.unwrap())
                            .expect("Cannot insert new constants");
                        ssa.op_unuse(*call_node, *arg_node);
                        ssa.op_use(*call_node, 0, target_node);
                        info.ssa_ref = Some(*call_node);
                    }
                }
            }
        }
    }
    rfn.call_ctx = call_info.into_iter().map(|x| x.1).collect();
    //radeco_trace!("{:?}", rfn.call_ctx);
}

// TODO: Make this a method of SSA Soon to pretty print expression trees.
fn hash_subtree(ssa: &SSAStorage, n: &NodeIndex) -> String {
    let nt = ssa.node_data(*n).map(|x| x.nt);
    if nt.is_err() {
        return String::new();
    }
    let nt = nt.unwrap();

    let mut result = format!("{}", nt);
    let mut args = ssa.operands_of(*n);
    if let NodeType::Op(MOpcode::OpCall) = nt {
        args.truncate(1);
    }

    let len = args.len();
    for (i, arg) in args.iter().enumerate() {
        if i == 0 {
            result.push_str(" ")
        }
        if i < len - 1 {
            result = format!("{}{}, ", result, hash_subtree(ssa, arg));
        } else {
            result = format!("{}{}", result, hash_subtree(ssa, arg));
        }
    }

    match nt {
        NodeType::Op(_) | NodeType::Phi => format!("({})", result),
        _ => result,
    }
}

// This function analyzes memory, i.e. all the memory store and load operations.
//
// It identifies these loads and stores and makes appropriate bindings for them.
fn analyze_memory(rfn: &mut DefaultFnTy) {
    let mut hashes = HashMap::<String, NodeIndex>::new();
    let mut seen_l = HashMap::<NodeIndex, Binding<NodeIndex>>::new();
    let mut wl;
    let mut id: u16 = 0;
    {
        let name = rfn.name.clone();
        let ssa = rfn.ssa_mut();
        let mem = {
            let start = ssa.entry_node().expect("Incomplete CFG graph");
            let rs = ssa.registers_in(start).expect("No register state node found");
            // mem is the first argument for the register state.
            // NOTE: If something changes in the future, the above assumption may no longer be
            // true. In that case, look here!
            ssa.operands_of(rs).pop()
        };

        if let Some(mem) = mem {
            wl = ssa.uses_of(mem);
            while let Some(node) = wl.pop() {
                let data = ssa.node_data(node).map(|x| x.nt);
                match data {
                    Ok(NodeType::Op(MOpcode::OpLoad)) |
                    Ok(NodeType::Op(MOpcode::OpStore)) => {
                        let args = ssa.operands_of(node);
                        // If operation is a store it will produce a new memory instance. Hence,
                        // push all uses of new memory to the worklist.
                        if let Ok(NodeType::Op(MOpcode::OpStore)) = data {
                            wl.extend(&ssa.uses_of(node));
                        }
                        let mem_loc = args.get(1)
                            .expect("Load/Store has to have source/destination");
                        let mut bind = if seen_l.contains_key(mem_loc) {
                            seen_l.get_mut(mem_loc).expect("This can never panic")
                        } else {
                            let h = hash_subtree(ssa, mem_loc);
                            if let Some(idx) = hashes.get(&h).cloned() {
                                // TODO: Maybe redirect the edges and remove the older nodes too as
                                // they are equivalent?
                                seen_l.get_mut(&idx).expect("")
                            } else {
                                if name.clone().contains("main") {
                                    //radeco_trace!("New hash inserted: {:?}", h);
                                }
                                hashes.insert(h, *mem_loc);
                                let mut b = Binding::default();
                                b.mark_memory();
                                b.set_name(format!("mem_var_{}", id));
                                id += 1;
                                seen_l.insert(*mem_loc, b);
                                seen_l.get_mut(mem_loc).expect("")
                            }
                        };
                        bind.add_refs(vec![node]);
                    }
                    _ => {}
                }
            }
        } else {
            radeco_warn!("No Memory Found!");
        }
    }

    for b in seen_l.values() {
        rfn.bindings.insert(b.clone());
    }

    // println!("Printing bindings information: {:?}", rfn.bindings);
}

fn load_datarefs(rfn: &mut DefaultFnTy, datarefs: Option<Vec<u64>>) {
    // println!("Datarefs loaded: {:?}", datarefs);
    if datarefs.is_none() {
        return;
    }
    rfn.datarefs = datarefs.clone();
    let datarefs = datarefs.expect("This cannot be 'None'");
    for dataref in datarefs {
        //radeco_trace!("{:?}", dataref);
    }
}

fn load_locals(rfn: &mut DefaultFnTy, locals: Option<Vec<LVarInfo>>) {
    // println!("Locals loaded: {:?}", locals);
    // TODO
    if locals.is_none() {
        return;
    }
    rfn.locals = locals.clone();
    let locals = locals.expect("This cannot be `None`");
    for l in locals {
        // println!("[z] Local loaded: {:?}", l);
        let reference = l.reference.unwrap();
        //radeco_trace!("{:?} {:?}", reference.base, reference.offset);
    }
}

// MAYBE TODO: Replace `RadecoModule` and `RadecoFunction` by generic `RModule`
// and `RFunction`.
// From trait to construct a module from `Source`.
// Note that this conversion is expensive as the source is used to construct
// the SSA for all the function that it holds and perform basic analysis.
impl<'a, T: 'a + Source> From<&'a mut T> for RadecoModule<'a, DefaultFnTy> {
    fn from(source: &'a mut T) -> RadecoModule<'a, DefaultFnTy> {
        let reg_info = source.register_profile();
        let mut rmod = RadecoModule::default();
        let mut handles = Vec::new();
        rmod.regfile = Some(SubRegisterFile::new(&reg_info));
        let (tx, rx) = sync::mpsc::channel();
        for f in source.functions() {
            if f.name.as_ref().unwrap().contains("sym.imp") {
                // Do not analyze/construct for imports.
                // TODO: Still keep track of these functions.
                continue;
            }
            //radeco_trace!("Locals of {:?}: {:?}", f.name, f.locals);
            let offset = f.offset.expect("Invalid offset");
            let instructions = source.instructions_at(offset);
            let tx = tx.clone();
            let reg_info = reg_info.clone();

            let handle = thread::Builder::new().name(f.name.as_ref().unwrap().to_owned()).spawn(move || {
                let mut rfn = ssa_single_fn(&f, &reg_info, instructions, offset);
                // Attach additional information as necessay from `FunctionInfo`.
                {
                    // Add all defined registers to bindings.
                    let regs = {
                        let ssa = rfn.ssa_mut();
                        let start = ssa.entry_node().expect("Incomplete CFG graph");
                        let rs = ssa.registers_in(start).expect("No registers state node found");
                        ssa.operands_of(rs)
                    };
                    for (i, reg) in regs.iter().enumerate() {
                        let mut bind = Binding::default();
                        bind.mark_register(rfn.ssa
                            .regnames
                            .get(i)
                            .cloned()
                            .unwrap_or_else(String::new));
                        bind.add_refs(vec![*reg]);
                        // Set the initial bind as register name, which may be
                        // changed after analyzed.
                        bind.set_name(rfn.ssa.regnames.get(i).cloned().unwrap_or_else(String::new));
                        rfn.bindings.insert(bind);
                    }
                }
                rfn.offset = offset;
                fix_call_info(&mut rfn);
                load_datarefs(&mut rfn, f.datarefs);
                load_locals(&mut rfn, f.locals);
                // Actually, analyze_memory should be done after VSA
                // analyze_memory(&mut rfn);
                radeco_trace!("Finished analysis of {:?}.", f.name);
                tx.send((offset, f.name.unwrap(), rfn)).unwrap();
            });
            handles.push(handle.expect(""));
        }

        let mut success = 0;
        for h in handles {
            match h.join() {
                Ok(_) => success += 1,
                Err(e) => radeco_warn!("{:?}", e),
            }
        }

        for _ in 0..success {
            let (offset, name, rfn) = rx.recv().unwrap();
            rmod.functions.insert(offset, rfn);
            rmod.fname.insert(name, offset);
        }

        rmod.src = Some(source);
        rmod
    }
}

// Implementations that are specific to `RadecoModule`.
impl<'a> RadecoModule<'a, DefaultFnTy> {
    fn construct<S: 'a + Source>(source: &'a mut S) -> RadecoModule<'a, DefaultFnTy> {
        RadecoModule::from(source)
    }
}

/// Trait that defines a `Function`.
pub trait RFunction {
    type I: Iterator<Item = <Self::SSA as SSA>::ValueRef>;
    type SSA: SSAWalk<Self::I> + SSAMod + SSA;
    type B: RBindings;

    fn fn_name(&self) -> String {
        "Unsupported".to_owned()
    }

    fn args(&self) -> Vec<(usize, &<Self::B as RBindings>::BTy)>;
    fn locals(&self) -> Vec<(usize, &<Self::B as RBindings>::BTy)>;
    fn returns(&self) -> Vec<(usize, &<Self::B as RBindings>::BTy)>;
    fn modifides(&self) -> Vec<(usize, &<Self::B as RBindings>::BTy)>;
    fn call_convention(&self) -> String;

    fn set_args(&mut self, &[<Self::B as RBindings>::Idx]);
    fn set_locals(&mut self, &[(<Self::B as RBindings>::Idx, LocalInfo)]);
    fn set_returns(&mut self, &[<Self::B as RBindings>::Idx]);
    fn set_modifides(&mut self, &[<Self::B as RBindings>::Idx]);
    fn set_preserved(&mut self, &[<Self::B as RBindings>::Idx]);

    fn call_sites
        (&self)
         -> Vec<CallContext<<Self::B as RBindings>::Idx, <Self::SSA as SSA>::ValueRef>>;
    fn callrefs(&self) -> Vec<u64>;
    fn callxrefs(&self) -> Vec<u64>;

    // Expose the internally contained ssa.
    fn ssa_ref(&self) -> &Self::SSA;
    fn ssa_mut(&mut self) -> &mut Self::SSA;
}

/// Trait that defines a `Module`. `RModule` is to be implemented on the struct that encompasses
/// all information loaded from the binary. It acts as a container for `Function`s.
pub trait RModule<'b> {
    type FnRef: Copy + Clone + fmt::Debug + hash::Hash + Eq + Into<u64> + From<u64>;
    type RFn: RFunction;

    fn callees_of(&self, &Self::FnRef) -> Vec<Self::FnRef>;
    fn callers_of(&self, &Self::FnRef) -> Vec<Self::FnRef>;
    fn functions(&self) -> Vec<Self::FnRef>;

    fn function_by_ref(&self, &Self::FnRef) -> Option<&Self::RFn>;
    fn function_by_ref_mut(&mut self, &Self::FnRef) -> Option<&mut Self::RFn>;

    // Expose raw `Source`
    fn source(&'b mut self) -> &Option<&'b mut Source>;
}

impl<'a, F: RFunction> RModule<'a> for RadecoModule<'a, F> {
    type FnRef = u64;
    type RFn = F;

    fn callees_of(&self, fref: &Self::FnRef) -> Vec<Self::FnRef> {
        let mut callees = Vec::<Self::FnRef>::new();
        if let Some(c) = self.functions.get(fref) {
            callees.extend(c.callrefs().iter().cloned().map(Self::FnRef::from));
        }
        callees
    }

    fn callers_of(&self, fref: &Self::FnRef) -> Vec<Self::FnRef> {
        let mut callers = Vec::<Self::FnRef>::new();
        if let Some(c) = self.functions.get(fref) {
            callers.extend(c.callxrefs().iter().cloned().map(Self::FnRef::from));
        }
        callers
    }

    fn functions(&self) -> Vec<Self::FnRef> {
        self.functions.keys().cloned().collect()
    }

    fn function_by_ref(&self, fref: &Self::FnRef) -> Option<&Self::RFn> {
        self.functions.get(fref)
    }

    fn function_by_ref_mut(&mut self, fref: &Self::FnRef) -> Option<&mut Self::RFn> {
        self.functions.get_mut(fref)
    }

    fn source(&'a mut self) -> &Option<&'a mut Source> {
        &self.src
    }
}

impl<B: RBindings> RFunction for RadecoFunction<B> {
    type I = Walker;
    type SSA = SSAStorage;
    type B = B;

    fn fn_name(&self) -> String {
        self.name.clone()
    }

    fn args(&self) -> Vec<(usize, &<Self::B as RBindings>::BTy)> {
        self.bindings
            .bindings()
            .enumerate()
            .filter({
                |x| x.1.is_argument()
            })
            .collect()
    }

    fn locals(&self) -> Vec<(usize, &<Self::B as RBindings>::BTy)> {
        self.bindings
            .bindings()
            .enumerate()
            .filter({
                |x| x.1.is_local()
            })
            .collect()
    }

    fn returns(&self) -> Vec<(usize, &<Self::B as RBindings>::BTy)> {
        self.bindings
            .bindings()
            .enumerate()
            .filter({
                |x| x.1.is_return()
            })
            .collect()
    }

    fn modifides(&self) -> Vec<(usize, &<Self::B as RBindings>::BTy)> {
        self.bindings
            .bindings()
            .enumerate()
            .filter({
                |x| x.1.is_modified()
            })
            .collect()
    }

    fn call_convention(&self) -> String {
        // TODO
        "cdecl".to_owned()
    }

    fn set_args(&mut self, args: &[<Self::B as RBindings>::Idx]) {
        for arg in args {
            if let Some(binding) = self.bindings.binding_mut(arg) {
                binding.mark_argument();
            }
        }
    }

    fn set_locals(&mut self, locals: &[(<Self::B as RBindings>::Idx, LocalInfo)]) {
        for &(ref arg, ref info) in locals {
            if let Some(binding) = self.bindings.binding_mut(arg) {
                binding.mark_fn_local(info.base, info.offset);
            }
        }
    }

    fn set_returns(&mut self, rets: &[<Self::B as RBindings>::Idx]) {
        for r in rets {
            if let Some(binding) = self.bindings.binding_mut(r) {
                binding.mark_return();
            }
        }
    }

    fn set_modifides(&mut self, modifides: &[<Self::B as RBindings>::Idx]) {
        for mod_ in modifides {
            if let Some(binding) = self.bindings.binding_mut(mod_) {
                binding.mark_modified();
            }
        }
    }

    fn set_preserved(&mut self, preserved: &[<Self::B as RBindings>::Idx]) {
        for p in preserved {
            if let Some(binding) = self.bindings.binding_mut(p) {
                binding.mark_preserved();
            }
        }
    }

    // Expose the internally contained ssa.
    fn ssa_ref(&self) -> &Self::SSA {
        &self.ssa
    }

    fn ssa_mut(&mut self) -> &mut Self::SSA {
        &mut self.ssa
    }

    fn call_sites(&self) -> Vec<CallContext<B::Idx, NodeIndex>> {
        self.call_ctx.clone()
    }

    fn callrefs(&self) -> Vec<u64> {
        self.call_ctx.iter().map(|x| x.callee).filter(|x| x.is_some()).map(|x| x.unwrap()).collect()
    }

    fn callxrefs(&self) -> Vec<u64> {
        self.callxrefs.clone()
    }
}


#[cfg(test)]
mod test {
    use frontend::source::FileSource;
    use middle::dce;
    use super::*;

    #[test]
    fn module_test() {
        // let mut r2 = R2::new(Some("./ct1_sccp_ex.o")).expect("Failed to open r2");
        // r2.init();
        // let mut fsource = FileSource::from(r2);
        let mut fsource = FileSource::open(Some("./test_files/ct1_sccp_ex/ct1_sccp_ex"));
        let mut rmod = RadecoModule::from(&mut fsource);
        for (ref addr, ref mut rfn) in rmod.functions.iter_mut() {
            {
                dce::collect(&mut rfn.ssa);
            }
            // println!("Local Variable info: {:?}", rfn.locals);
            // println!("Local Variable info: {:?}", rfn.bindings);
            // ir_write!(Some(rfn.name.clone()), &rfn.ssa, "/tmp/module_test.ir");
            // let mut writer: IRWriter = Default::default();
            // println!("{}", writer.emit_il(Some(rfn.name.clone()), &rfn.ssa));
        }
    }
}
