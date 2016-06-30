//! Uses inter proc analysis to fill in call summary information.

use analysis::interproc::transfer::InterProcAnalysis;
use frontend::containers::{RModule, RFunction};
use middle::ssa::ssa_traits::{SSA, SSAMod};
use middle::ssa::cfg_traits::{CFG, CFGMod};

#[derive(Clone, Debug)]
pub struct CallSummary { }

impl<T: RModule> InterProcAnalysis<T> for CallSummary {
    fn new() -> CallSummary {
        CallSummary { }
    }

    // Compute fn arguments, locals, modifides and returns lists.
    fn transfer(&mut self, rmod: &mut T, fn_ref: &T::FnRef) {
        {
            // fn arguments.
            let mut args = Vec::new();

            let rfn = rmod.function_by_ref_mut(fn_ref);
            {
                let ssa = rfn.ssa_ref();
                let start_block = ssa.start_node();
                let rs = ssa.registers_at(&start_block);
                for reg in ssa.args_of(rs) {
                    if !ssa.uses_of(reg).is_empty() {
                        args.push(reg);
                    }
                }
            }
            rfn.set_args(&args);
        }
    }

    // Iterate through all the call-sites in a function(`fn_ref`) and pull in changes from the
    // callees. i.e. Replace args_list by args_list in the callee and replace modifides by the
    // values the callee actually modifies.
    fn propagate(&mut self, rmod: &mut T, fn_ref: &T::FnRef) {
        unimplemented!()
    }
}




