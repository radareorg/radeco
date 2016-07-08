//! Defines `Module` and `Function` that act as containers.

use std::fmt::Debug;
use std::hash::Hash;
use std::collections::{BTreeSet, HashMap};
use std::thread;
use std::sync::mpsc;
use std::fmt;

use r2pipe::structs::{FunctionInfo, LOpInfo, LRegInfo, LVarInfo};

use petgraph::graph::NodeIndex;

use frontend::source::Source;
use frontend::ssaconstructor::SSAConstruct;
use frontend::bindings::Binding;
use middle::ssa::ssastorage::{SSAStorage, Walker};
use middle::ssa::ssa_traits::{SSA, SSAWalk, SSAMod};

pub struct RadecoModule<'a> {
    pub functions: HashMap<u64, RadecoFunction>,
    fname: HashMap<String, u64>,
    pub src: Option<&'a mut Source>,
}

impl<'a> fmt::Debug for RadecoModule<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "RadecoModule {{ functions: {:?}, fname: {:?} }}", self.functions, self.fname)
    }
}

impl<'a> Default for RadecoModule<'a> {
    fn default() -> RadecoModule<'a> {
        RadecoModule {
            functions: HashMap::new(),
            fname: HashMap::new(),
            src: None,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct SSAInfo {
    locals: Vec<NodeIndex>,
    args: Vec<NodeIndex>,
    returns: Vec<NodeIndex>,
    modifides: Vec<NodeIndex>,
}

#[derive(Clone, Debug, Default)]
pub struct RadecoFunction {
    // TODO: Should not be pub.
    pub ssa: SSAStorage,
    pub name: String,
    call_ctx: Vec<CallContext>,
    callrefs: Vec<u64>,
    callxrefs: Vec<u64>,
    bindings: Vec<Binding<NodeIndex>>,
    pub locals: Vec<LVarInfo>,
    // Var and argument information about the function.
    ssa_info: SSAInfo,
}

#[derive(Clone, Debug)]
pub struct CallContext {
    /// Start offset of caller (uniquely identifies a function).
    caller: u64,
    /// Start offset of callee (uniquely identifies a function).
    callee: u64,
    /// Offset the call. Note that this will belong to the caller function.
    call_site: u64,
    /// Translate a node in callees context into a node in caller's context.
    ctx_translate: HashMap<NodeIndex, NodeIndex>,
}

// TODO: Re-enable if needed. Current members of RadecoFunction have default
// implementation.
// impl Default for RadecoFunction {
// fn default() -> RadecoFunction {
// RadecoFunction {
// ssa: SSAStorage::new(),
// call_ctx: Vec::new(),
// callrefs: BTreeSet::new(),
// callxrefs: BTreeSet::new(),
// name: String::new(),
// }
// }
// }

// Implementations sepecific to `RadecoFunction`.
impl RadecoFunction {
    fn new() -> RadecoFunction {
        RadecoFunction::default()
    }

    pub fn construct(reg_profile: &LRegInfo, insts: Vec<LOpInfo>) -> RadecoFunction {
        let mut rfn = RadecoFunction::default();
        {
            let mut constructor = SSAConstruct::new(&mut rfn.ssa, &reg_profile);
            constructor.run(insts);
        }
        rfn
    }
}

impl CallContext {
    pub fn new(caller: u64, callee: u64, call_site: u64) -> CallContext {
        CallContext {
            caller: caller,
            callee: callee,
            call_site: call_site,
            ctx_translate: HashMap::new(),
        }
    }
}

// Private function to construct SSA for a single function and fill in the basic
// information. Note: This function is threaded in module-ssa construction.
fn ssa_single_fn(f: &FunctionInfo,
                 reg_info: &LRegInfo,
                 instructions: Vec<LOpInfo>)
                 -> RadecoFunction {
    let mut rfn = RadecoFunction::construct(reg_info, instructions);
    rfn.name = f.name.as_ref().unwrap().clone();
    if let Some(ref callrefs) = f.callrefs {
        rfn.callrefs = callrefs.iter()
                               .filter(|x| {
                                   match x.call_type {
                                       Some(ref c) if c == "C" => true,
                                       _ => false,
                                   }
                               })
                               .map(|x| x.addr.expect("Invalid address"))
                               .collect::<BTreeSet<_>>()
                               .into_iter()
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
                                 .map(|x| x.addr.expect("Invalid address"))
                                 .collect::<BTreeSet<_>>()
                                 .into_iter()
                                 .collect();
    }
    rfn
}

// MAYBE TODO: Replace `RadecoModule` and `RadecoFunction` by generic `RModule`
// and `RFunction`.
// From trait to construct a module from `Source`.
// Note that this conversion is expensive as the source is used to construct
// the SSA for all the function that it holds and perform basic analysis.
impl<'a, T: 'a + Source> From<&'a mut T> for RadecoModule<'a> {
    fn from(source: &'a mut T) -> RadecoModule<'a> {
        let reg_info = source.register_profile();
        let mut rmod = RadecoModule::default();
        let mut handles = Vec::new();
        let (tx, rx) = mpsc::channel();
        for f in source.functions() {
            if f.name.as_ref().unwrap().contains("sym.imp") {
                continue;
            }
            let offset = f.offset.expect("Invalid offset");
            let instructions = source.instructions_at(offset);
            let tx = tx.clone();
            let reg_info = reg_info.clone();

            let handle = thread::spawn(move || {
                let mut rfn = ssa_single_fn(&f, &reg_info, instructions);
                // Attach additional information as necessay from `FunctionInfo`.
                rfn.locals = f.locals.unwrap_or_else(Vec::new);
                tx.send((offset, f.name.unwrap(), rfn)).unwrap();
            });
            handles.push(handle);
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
impl<'a> RadecoModule<'a> {
    fn construct<S: 'a + Source>(source: &'a mut S) -> RadecoModule<'a> {
        RadecoModule::from(source)
    }
}

/// Trait that defines a `Function`.
pub trait RFunction {
    type I: Iterator<Item=<Self::SSA as SSA>::ValueRef>;
    type SSA: SSAWalk<Self::I> + SSAMod;

    fn args(&self) -> IdxIter<<Self::SSA as SSA>::ValueRef>;
    fn locals(&self) -> IdxIter<<Self::SSA as SSA>::ValueRef>;
    fn returns(&self) -> IdxIter<<Self::SSA as SSA>::ValueRef>;
    fn modifides(&self) -> IdxIter<<Self::SSA as SSA>::ValueRef>;
    fn call_convention(&self) -> String;

    fn set_args(&mut self, &[<Self::SSA as SSA>::ValueRef]);
    fn set_locals(&mut self, &[<Self::SSA as SSA>::ValueRef]);
    fn set_returns(&mut self, &[<Self::SSA as SSA>::ValueRef]);
    fn set_modifides(&mut self, &[<Self::SSA as SSA>::ValueRef]);

    // Expose the internally contained ssa.
    fn ssa_ref(&self) -> &Self::SSA;
    fn ssa_mut(&mut self) -> &mut Self::SSA;
}

pub struct IdxIter<'a, N: 'a> {
    idxs: ::std::slice::Iter<'a, N>,
}

impl<'a, N> Iterator for IdxIter<'a, N> {
    type Item = &'a N;
    fn next(&mut self) -> Option<&'a N> {
        self.idxs.next()
    }
}

/// Trait that defines a `Module`. `RModule` is to be implemented on the struct that encompasses
/// all information loaded from the binary. It acts as a container for `Function`s.
pub trait RModule<'b> {
    type FnRef: Copy + Clone + Debug + Hash + Eq;
    type RFn: RFunction;

    fn callees_of(&self, &Self::FnRef) -> Vec<Self::FnRef>;
    fn callers_of(&self, &Self::FnRef) -> Vec<Self::FnRef>;
    fn functions(&self) -> Vec<Self::FnRef>;

    fn function_by_ref(&self, &Self::FnRef) -> Option<&Self::RFn>;
    fn function_by_ref_mut(&mut self, &Self::FnRef) -> Option<&mut Self::RFn>;

    // Expose raw `Source`
    fn source(&'b mut self) -> &Option<&'b mut Source>;
}

impl<'a> RModule<'a> for RadecoModule<'a> {
    type FnRef = u64;
    type RFn = RadecoFunction;

    fn callees_of(&self, fref: &Self::FnRef) -> Vec<Self::FnRef> {
        let mut callees = Vec::new();
        if let Some(ref c) = self.functions.get(fref) {
            callees.extend(c.callrefs.iter().cloned());
        }
        callees
    }

    fn callers_of(&self, fref: &Self::FnRef) -> Vec<Self::FnRef> {
        let mut callers = Vec::new();
        if let Some(ref c) = self.functions.get(fref) {
            callers.extend(c.callxrefs.iter().cloned());
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

impl RFunction for RadecoFunction {
    type I = Walker;
    type SSA = SSAStorage;

    fn args(&self) -> IdxIter<<Self::SSA as SSA>::ValueRef> {
        IdxIter { idxs: self.ssa_info.args.iter() }
    }

    fn locals(&self) -> IdxIter<<Self::SSA as SSA>::ValueRef> {
        IdxIter { idxs: self.ssa_info.locals.iter() }
    }

    fn returns(&self) -> IdxIter<<Self::SSA as SSA>::ValueRef> {
        IdxIter { idxs: self.ssa_info.returns.iter() }
    }

    fn modifides(&self) -> IdxIter<<Self::SSA as SSA>::ValueRef> {
        IdxIter { idxs: self.ssa_info.modifides.iter() }
    }

    fn call_convention(&self) -> String {
        // TODO
        "cdecl".to_owned()
    }

    fn set_args(&mut self, args: &[<Self::SSA as SSA>::ValueRef]) {
        self.ssa_info.args = args.iter().cloned().collect();
    }

    fn set_locals(&mut self, locals: &[<Self::SSA as SSA>::ValueRef]) {
        self.ssa_info.locals = locals.iter().cloned().collect();
    }

    fn set_returns(&mut self, rets: &[<Self::SSA as SSA>::ValueRef]) {
        self.ssa_info.returns = rets.iter().cloned().collect();
    }

    fn set_modifides(&mut self, modifides: &[<Self::SSA as SSA>::ValueRef]) {
        self.ssa_info.modifides = modifides.iter().cloned().collect();
    }

    // Expose the internally contained ssa.
    fn ssa_ref(&self) -> &Self::SSA {
        &self.ssa
    }

    fn ssa_mut(&mut self) -> &mut Self::SSA {
        &mut self.ssa
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use r2pipe::r2::R2;
    use frontend::source::{FileSource, Source};
    use middle::ir_writer::IRWriter;
    use std::io;
    use middle::dce;

    #[test]
    fn module_test() {
        //let mut r2 = R2::new(Some("./ct1_sccp_ex.o")).expect("Failed to open r2");
        //r2.init();
        //let mut fsource = FileSource::from(r2);
        let mut fsource = FileSource::open(Some("./test_files/ct1_sccp_ex/ct1_sccp_ex"));
        let mut rmod = RadecoModule::from(&mut fsource);
        for (ref addr, ref mut rfn) in rmod.functions.iter_mut() {
            {
                dce::collect(&mut rfn.ssa);
            }
            println!("Local Variable info: {:?}", rfn.locals);
            let mut writer: IRWriter = Default::default();
            writer.emit_il(Some(rfn.name.clone()), &rfn.ssa, &mut io::stdout());
        }
    }
}
