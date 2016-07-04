//! Uses inter proc analysis to fill in call summary information.

use std::collections::HashSet;

use analysis::interproc::transfer::InterProcAnalysis;
use frontend::containers::{RModule, RFunction};
use middle::ssa::ssa_traits::{SSA, SSAMod, NodeType};
use middle::ssa::cfg_traits::{CFG, CFGMod};
use middle::ir::MOpcode;

#[derive(Clone, Debug)]
pub struct CallSummary { }

impl<'a, T: RModule<'a>> InterProcAnalysis<'a, T> for CallSummary {
    fn new() -> CallSummary {
        CallSummary { }
    }

    // Compute fn arguments, modifides and returns lists.
    fn transfer(&mut self, rmod: &mut T, fn_ref: &T::FnRef) {
        {
            // fn arguments.
            let mut args = HashSet::new();
            //let mut modifides = HashSet::new();
            let rfn = rmod.function_by_ref_mut(fn_ref);
            if rfn.is_none() {
                // Nothing to analyze. This is not a function defined inside the loaded binary. So
                // it must be an import. Use the calling convention information for analysis and
                // return.
                // XXX: For now, we hardcode some analysis information. This will be loaded from
                // r2/Source once the information is avaliable.
                return;
            }
            let rfn = rfn.unwrap();
            {
                let locals = rfn.locals().collect::<HashSet<_>>();
                let ssa = rfn.ssa_ref();
                let start_block = ssa.start_node();
                // Get register state at the start block.
                let rs = ssa.registers_at(&start_block);
                // For every register in the starting block.
                for reg in ssa.args_of(rs) {
                    // Get the uses of the register
                    let uses = ssa.uses_of(reg);
                    if !uses.is_empty() {
                        let insert = uses.iter().any(|x| {
                            if let Ok(ref data) = ssa.get_node_data(x) {
                                match data.nt {
                                    NodeType::Op(MOpcode::OpLoad) | NodeType::Op(MOpcode::OpStore) => {
                                        // If the store is to a local variable, then it is still an
                                        // argument. Otherwise, it is part of preservation code and
                                        // must not be considered an argument to the function.
                                        locals.contains(x)
                                    },
                                    _ => true,
                                }
                            } else {
                                false
                            }
                        });
                        if insert {
                            args.insert(reg);
                        }
                    }
                }
            }
            rfn.set_args(&args.into_iter().collect::<Vec<_>>());
        }
    }

    // Iterate through all the call-sites in a function(`fn_ref`) and pull in changes from the
    // callees. i.e. Replace args_list by args_list in the callee and replace modifides by the
    // values the callee actually modifies.
    fn propagate(&mut self, rmod: &mut T, fn_ref: &T::FnRef) {
        unimplemented!()
    }
}




