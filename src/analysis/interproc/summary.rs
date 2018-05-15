//! Uses inter proc analysis to fill in call summary information.

use std::collections::HashSet;

use analysis::interproc::transfer::InterProcAnalysis;
use frontend::containers::{RModule, RFunction};
use middle::ssa::ssa_traits::{SSA, NodeType};
use middle::ssa::cfg_traits::CFG;
use middle::ir::MOpcode;

#[derive(Clone, Debug, Default)]
pub struct CallSummary { }

impl<'a, T: RModule<'a>> InterProcAnalysis<'a, T> for CallSummary {
    fn new() -> CallSummary {
        Default::default()
    }

    // Compute fn arguments, modifides and returns lists.
    // TODO: Add support for memory args and modifides.
    fn transfer(&mut self, rmod: &mut T, fn_ref: &T::FnRef) {
        {
            // fn arguments.
            let mut args = HashSet::new();
            let mut modifides = HashSet::new();
            let mut returns = HashSet::new();
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
                let locals = rfn.locals().iter().map(|x| x.0).collect::<HashSet<_>>();
                let ssa = rfn.ssa_ref();
                // Get register state at the start block.
                let rs = registers_in_err!(ssa, entry_node_err!(ssa),
                    ssa.invalid_value().unwrap());
                // For every register in the starting block.
                for (i, reg) in ssa.operands_of(rs).iter().enumerate() {
                    // Get the uses of the register
                    let uses = ssa.uses_of(*reg);
                    if !uses.is_empty() {
                        let insert = uses.iter().any(|x| {
                            if let Ok(ref data) = ssa.node_data(*x) {
                                match data.nt {
                                    NodeType::Op(MOpcode::OpLoad) |
                                    NodeType::Op(MOpcode::OpStore) => {
                                        // If the store is to a local variable, then it is still an
                                        // argument. Otherwise, it is part of preservation code and
                                        // must not be considered an argument to the function.
                                        locals.contains(&i)
                                    },
                                    _ => true,
                                }
                            } else {
                                false
                            }
                        });
                        if insert {
                            args.insert(i);
                        }
                    }
                }
                let rs = registers_in_err!(ssa, exit_node_err!(ssa),
                    ssa.invalid_value().unwrap());
                for (i, r) in ssa.operands_of(rs).iter().enumerate() {
                    let (insert_r, insert_m) = if let Ok(ref data) = ssa.node_data(*r) {
                        match data.nt {
                            NodeType::Comment(_) => (false, false),
                            NodeType::Op(MOpcode::OpLoad) => {
                                (locals.contains(&i), true)
                            }
                            _ => (true, true),
                        }
                    } else {
                        (false, false)
                    };

                    if insert_m {
                        modifides.insert(i);
                    }
                    if insert_r {
                        returns.insert(i);
                    }
                }
            }

            rfn.set_returns(&returns.into_iter().map(From::from).collect::<Vec<_>>());
            rfn.set_modifides(&modifides.into_iter().map(From::from).collect::<Vec<_>>());
            rfn.set_args(&args.into_iter().map(From::from).collect::<Vec<_>>());
        }
    }

    // Iterate through all the call-sites in a function(`fn_ref`) and pull in changes from the
    // callees. i.e. Replace args_list by args_list in the callee and replace modifides by the
    // values the callee actually modifies.
    fn propagate(&mut self, rmod: &mut T, fn_ref: &T::FnRef) {
        if let Some(rfn) = rmod.function_by_ref(fn_ref) {
            for csite in rfn.call_sites() {
                let callee = csite.callee.unwrap_or_else(|| {
                    radeco_err!("Call site cannot have callee as `None`");
                    0
                });
            }
        }
    }
}
