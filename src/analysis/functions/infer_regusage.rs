//! Infers how each function uses every register

use analysis::inst_combine;
use frontend::imports::ImportInfo;
use frontend::radeco_containers::{RadecoFunction, RadecoModule};
use middle::dce;
use middle::ir;
use middle::regfile::{RegisterId, RegisterUsage, SubRegisterFile};
use middle::ssa::cfg_traits::*;
use middle::ssa::ssa_traits::*;
use middle::ssa::ssastorage::SSAStorage;

use r2api::structs::LCCInfo;

use petgraph::prelude::*;
use petgraph::visit::{DfsPostOrder, VisitMap, Walker};

use std::collections::{hash_map, BTreeMap, HashMap, HashSet};
use std::iter::{self, Extend, FromIterator};

/// For every function, patch all of its call sites to ignore registers that the
/// callee doesn't read and to preserve register values that the callee
/// preserves. Then, record which registers it reads and which registers it
/// preserves.
///
/// After this, all functions should have mutable [`regusage`][RadecoFunction::regusage]s.
///
/// This analysis is super conservative; for example, if a function preserves a
/// register by pushing it onto the stack and popping it back right before
/// returning, it is considered to be read and not preserved because we can't
/// guarantee that that stack location is never subsequently read or modified.
/// See #147 for further discussion
pub fn run(rmod: &mut RadecoModule, reginfo: &SubRegisterFile) -> () {
    Inferer::new().run(rmod, reginfo);
}

struct Inferer {
    /// Addresses of the functions we've already analyzed
    analyzed: HashSet<u64>,
}

impl Inferer {
    fn new() -> Inferer {
        Inferer {
            analyzed: HashSet::new(),
        }
    }

    /// Calls `patch_fn`, `dce::collect`, and `analyze_fn` on every function,
    /// callees first
    fn run(&mut self, rmod: &mut RadecoModule, reginfo: &SubRegisterFile) -> () {
        // for imports, *ASSUME* that the callconv that r2 says is correct
        {
            let imp_ru_iter = rmod.imports.iter().filter_map(|(&imp_addr, imp_info)| {
                let imp_rfn = imp_info.rfn.borrow();
                let regusage = reginfo.r2callconv_to_register_usage(
                    imp_rfn.callconv.as_ref()?, // ignore imports without callconvs
                    &*imp_rfn.callconv_name,
                )?;
                Some((imp_addr, regusage))
            });
            for (imp_addr, imp_ru) in imp_ru_iter {
                rmod.functions.get_mut(&imp_addr).unwrap().regusage = imp_ru;
                self.analyzed.insert(imp_addr);
            }
        }

        let mut dfs_wi = DfsPostOrder::empty(&rmod.callgraph).iter(&rmod.callgraph);
        // pick a function ...
        for fn_ni in rmod.callgraph.node_indices() {
            // ... and start a dfs on it
            dfs_wi.inner_mut().move_to(fn_ni);
            while let Some(fn_to_anal) = dfs_wi.next() {
                let fn_addr = rmod.callgraph[fn_to_anal];

                // ignore functions already in `call_convs` (probably because its an import)
                if !self.analyzed.contains(&fn_addr) {
                    self.patch_fn(fn_addr, &mut rmod.functions);

                    let rfn = &mut rmod.functions.get_mut(&fn_addr).unwrap();
                    dce::collect(rfn.ssa_mut());
                    inst_combine::run(rfn.ssa_mut());

                    let ru = self.analyze_fn(rfn, reginfo).unwrap_or_else(|| {
                        radeco_err!("Failed to analyze fn: {:?} (@ {:#X})", rfn.name, fn_addr);
                        // if analysis failed, default to "reads and clobbers everything"
                        reginfo.new_register_usage()
                    });

                    rfn.regusage = ru;
                    self.analyzed.insert(fn_addr);
                }
            }
        }
    }

    /// Using the callconv info we've gathered so far, patch-up call sites to
    /// to remove arguments that the callee doesn't read and make values in
    /// callee-saved registers be preserved across the call.
    fn patch_fn(&self, fn_addr: u64, fn_map: &mut BTreeMap<u64, RadecoFunction>) -> () {
        radeco_trace!("patching calls in fn: {}", fn_map[&fn_addr].name);
        for node in fn_map[&fn_addr].ssa().inorder_walk() {
            if let Ok(NodeType::Op(ir::MOpcode::OpCall)) =
                fn_map[&fn_addr].ssa().node_data(node).map(|nd| nd.nt)
            {
                self.patch_call_node(node, fn_addr, fn_map)
                    .unwrap_or_else(|| {
                        radeco_warn!(
                            "failed to remove unused args for call at {:#X}",
                            fn_map[&fn_addr].ssa().address(node).unwrap()
                        );
                    });
            }
        }
    }

    fn patch_call_node(
        &self,
        call_node: <SSAStorage as SSA>::ValueRef,
        fn_addr: u64,
        fn_map: &mut BTreeMap<u64, RadecoFunction>,
    ) -> Option<()> {
        // bail on indirect or weird call
        let (call_tgt_addr, call_reg_map) = direct_call_info(fn_map[&fn_addr].ssa(), call_node)?;

        // remove unread args
        for (reg_idx, &op_node) in (0..).zip(&call_reg_map) {
            if fn_map[&call_tgt_addr]
                .regusage
                .is_ignored(RegisterId::from_usize(reg_idx))
            {
                fn_map
                    .get_mut(&fn_addr)
                    .unwrap()
                    .ssa_mut()
                    .op_unuse(call_node, op_node);
            }
        }

        // bridge preserved registers
        for use_node in fn_map[&fn_addr].ssa().uses_of(call_node) {
            let reg_idx = match fn_map[&fn_addr]
                .ssa()
                .sparse_operands_of(use_node)
                .as_slice()
            {
                &[(reg_idx, _)] => reg_idx,
                _ => panic!("invalid use of call as operand"),
            };
            if fn_map[&call_tgt_addr]
                .regusage
                .is_preserved(RegisterId::from_u8(reg_idx))
            {
                fn_map
                    .get_mut(&fn_addr)
                    .unwrap()
                    .ssa_mut()
                    .replace_value(use_node, call_reg_map[reg_idx as usize]);
            }
        }

        Some(())
    }

    fn analyze_fn(&self, rfn: &RadecoFunction, reginfo: &SubRegisterFile) -> Option<RegisterUsage> {
        radeco_trace!("analyzing fn: {}", rfn.name);
        let ssa = rfn.ssa();
        let entry_regstate_node = ssa.registers_in(ssa.entry_node()?)?;
        let exit_regstate_node = ssa.registers_in(ssa.exit_node()?)?;
        // some registers may not be present in the entry node;
        // this means that the function neither reads nor preserves that register
        let entry_regstate: Vec<Option<_>> = register_state_info(ssa, entry_regstate_node);
        // every register must be present in the exit node though
        let exit_regstate: Vec<_> = register_state_info(ssa, exit_regstate_node)
            .into_iter()
            .collect::<Option<_>>()?;

        let mut ret = reginfo.new_register_usage();
        ret.set_all_ignored();

        // ignore registers not in entry regstate
        let mut regstate_iter = (0..)
            .zip(exit_regstate)
            .zip(entry_regstate)
            .filter_map(|((i, x), on)| on.map(|n| (i, n, x)));

        for (i, reg_val_entry, reg_val_exit) in regstate_iter {
            if reg_val_exit == reg_val_entry {
                ret.set_preserved(RegisterId::from_u8(i));
            }

            // find all uses, ignoring entry/exit register state
            let mut uses_iter = ssa.uses_of(reg_val_entry)
                .into_iter()
                .filter(|&n| n != entry_regstate_node && n != exit_regstate_node);
            if uses_iter.next().is_some() {
                ret.set_read(RegisterId::from_u8(i));
            }
        }

        Some(ret)
    }
}

/// Extracts the value of all registers at a `RegisterState` SSA node.
/// The length of the returned `Vec` is exactly `ssa.regnames.len() + 1`.
fn register_state_info(
    ssa: &SSAStorage,
    regstate_node: <SSAStorage as SSA>::ValueRef,
) -> Vec<Option<<SSAStorage as SSA>::ValueRef>> {
    let mut reg_opt_map: Vec<Option<_>> = vec![None; ssa.regnames.len() + 1];
    for (op_idx, op_node) in ssa.sparse_operands_of(regstate_node) {
        if let Some(p) = reg_opt_map.get_mut(op_idx as usize) {
            *p = Some(op_node);
        }
    }
    reg_opt_map
}

/// Extracts the call target address and the value of all registers
/// The length of the returned `Vec` is exactly `ssa.regnames.len() + 1`.
/// Returns `None` if the call is indirect or if not all registers have a value.
fn direct_call_info(
    ssa: &SSAStorage,
    call_node: <SSAStorage as SSA>::ValueRef,
) -> Option<(u64, Vec<<SSAStorage as SSA>::ValueRef>)> {
    let mut tgt_opt: Option<u64> = None;
    let mut reg_opt_map: Vec<Option<_>> = vec![None; ssa.regnames.len() + 1];

    for (op_idx, op_node) in ssa.sparse_operands_of(call_node) {
        if op_idx == 0 {
            tgt_opt = tgt_opt.or_else(|| ssa.constant(op_node));
        } else {
            if let Some(p) = reg_opt_map.get_mut((op_idx - 1) as usize) {
                *p = Some(op_node);
            }
        }
    }

    let reg_map_opt: Option<Vec<_>> = reg_opt_map.into_iter().collect();
    Some((tgt_opt?, reg_map_opt?))
}
