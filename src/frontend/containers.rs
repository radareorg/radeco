//! Defines `Module` and `Function` that act as containers.

use std::fmt::Debug;
use std::hash::Hash;
use std::collections::{BTreeSet, HashMap};
use std::thread;
use std::sync::mpsc;

use r2pipe::structs::{LOpInfo, LRegInfo};

use petgraph::graph::NodeIndex;

use frontend::source::Source;
use frontend::ssaconstructor::SSAConstruct;
use middle::ssa::ssastorage::SSAStorage;

#[derive(Clone, Debug, Default)]
pub struct RadecoModule {
    functions: HashMap<u64, RadecoFunction>,
    fname: HashMap<String, u64>,
}

#[derive(Clone, Debug)]
pub struct RadecoFunction {
    ssa: SSAStorage,
    name: String,
    call_ctx: Vec<CallContext>,
    callrefs: BTreeSet<u64>,
    callxrefs: BTreeSet<u64>,
}

impl Default for RadecoFunction {
    fn default() -> RadecoFunction {
        RadecoFunction {
            ssa: SSAStorage::new(),
            call_ctx: Vec::new(),
            callrefs: BTreeSet::new(),
            callxrefs: BTreeSet::new(),
            name: String::new(),
        }
    }
}

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

// TODO
fn ssa_single_fn() -> () {
}

// From trait to construct a module from `Source`.
// Note that this conversion is expensive as the source is used to construct
// the SSA for all the
// function that it holds and perform basic analysis.
impl<'a, T: 'a + Source> From<&'a mut T> for RadecoModule {
    fn from(source: &'a mut T) -> RadecoModule {
        let reg_info = source.register_profile();
        let mut rmod = RadecoModule::default();
        //let mut childs = Vec::new();
        let (tx, rx) = mpsc::channel();
        let mut i = 0;
        let mut handles = Vec::new();
        for f in source.functions() {
            if f.name.as_ref().unwrap().contains("sym.imp") {
                continue;
            }
            let offset = f.offset.expect("Invalid offset");
            let instructions = source.instructions_at(offset);
            let tx = tx.clone();
            let reg_info = reg_info.clone();

            let handle = thread::spawn(move || {
                let mut rfn = RadecoFunction::construct(&reg_info, instructions);
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
                                           .collect::<BTreeSet<_>>();
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
                                             .collect::<BTreeSet<_>>();
                }
                tx.send((offset, f.name.unwrap(), rfn)).unwrap();
            });
            handles.push(handle);
        }

        let mut success = 0;
        for h in handles {
            if let Ok(_) = h.join() {
                success += 1;
            }
        }

        for _ in 0..success {
            let (offset, name, rfn) = rx.recv().unwrap();
            rmod.functions.insert(offset, rfn);
            rmod.fname.insert(name, offset);
        }
        rmod
    }
}

impl RadecoModule {
    fn construct<S: Source>(mut source: S) -> RadecoModule {
        RadecoModule::from(&mut source)
    }
}

pub trait RFunction { }
pub trait RModule {
    type FnRef: Copy + Clone + Debug + Hash + Eq;

    fn callees_of(&self, &Self::FnRef) -> Vec<Self::FnRef>;
    fn callers_of(&self, &Self::FnRef) -> Vec<Self::FnRef>;
    fn functions(&self) -> Vec<Self::FnRef>;
    fn function_ref(&self, &Self::FnRef) -> &RFunction;
    fn function_ref_mut(&mut self, &Self::FnRef) -> &mut RFunction;
}

#[cfg(test)]
mod test {
    use super::*;
    use r2pipe::r2::R2;
    use frontend::source::Source;
    use middle::ir_writer::IRWriter;
    use std::io;
    use middle::dce;

    #[test]
    fn module_test() {
        let mut r2 = R2::open(Some("./crowell_example.o")).expect("Failed to open r2");
        r2.init();
        let mut rmod = RadecoModule::from(&mut r2);
        for (ref addr, ref mut rfn) in rmod.functions.iter_mut() {
            {
                dce::collect(&mut rfn.ssa);
            }
            let mut writer: IRWriter = Default::default();
            writer.emit_il(Some(rfn.name.clone()), &rfn.ssa, &mut io::stdout());
        }
    }
}
