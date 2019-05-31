//! Infers how each function uses every register
//! For every function, patch all of its call sites to ignore registers that the
//! callee doesn't read and to preserve register values that the callee
//! preserves. Then, record which registers it reads and which registers it
//! preserves.
//!
//! After this, all functions should have mutable [`regusage`][RadecoFunction::regusage]s.
//!
//! This analysis is super conservative; for example, if a function preserves a
//! register by pushing it onto the stack and popping it back right before
//! returning, it is considered to be read and not preserved because we can't
//! guarantee that that stack location is never subsequently read or modified.
//! See #147 for further discussion

use crate::analysis::analyzer::{
    all, Action, Analyzer, AnalyzerInfo, AnalyzerKind, AnalyzerResult, Change, FuncAnalyzer,
    ModuleAnalyzer,
};
use crate::analysis::dce::DCE;
use crate::analysis::inst_combine::Combiner;
use crate::frontend::radeco_containers::{RadecoFunction, RadecoModule};
use crate::middle::ir;
use crate::middle::regfile::*;
use crate::middle::ssa::cfg_traits::*;
use crate::middle::ssa::ssa_traits::*;
use crate::middle::ssa::ssastorage::SSAStorage;
use crate::middle::ssa::utils;

use petgraph::visit::{DfsPostOrder, Walker};

use std::any::Any;
use std::collections::{BTreeMap, HashSet};

const NAME: &str = "inferer";
const REQUIRES: &[AnalyzerKind] = &[];

pub const INFO: AnalyzerInfo = AnalyzerInfo {
    name: NAME,
    kind: AnalyzerKind::Inferer,
    requires: REQUIRES,
    uses_policy: false,
};

#[derive(Debug)]
pub struct Inferer {
    /// Register file of the current architecture.
    reginfo: SubRegisterFile,

    /// Addresses of the functions we've already analyzed
    analyzed: HashSet<u64>,
}

impl Analyzer for Inferer {
    fn info(&self) -> &'static AnalyzerInfo {
        &INFO
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl ModuleAnalyzer for Inferer {
    /// Calls `patch_fn`, `dce::collect`, and `analyze_fn` on every function,
    /// callees first
    fn analyze<T: FnMut(Box<dyn Change>) -> Action>(
        &mut self,
        rmod: &mut RadecoModule,
        _policy: Option<T>,
    ) -> Option<Box<dyn AnalyzerResult>> {
        // for imports, *ASSUME* that the callconv that r2 says is correct
        let mut new_analyzed = Vec::new();
        {
            let imp_ru_iter = rmod.imports.iter().filter_map(|(&imp_addr, imp_info)| {
                let imp_rfn = imp_info.rfn.borrow();
                let regusage = self.reginfo.r2callconv_to_register_usage(
                    imp_rfn.callconv.as_ref()?, // ignore imports without callconvs
                    &*imp_rfn.callconv_name,
                )?;
                Some((imp_addr, regusage))
            });
            for (imp_addr, imp_ru) in imp_ru_iter {
                rmod.functions.get_mut(&imp_addr).unwrap().regusage = imp_ru;
                new_analyzed.push(imp_addr);
            }
        }

        for func in new_analyzed {
            self.analyzed.insert(func);
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
                    let mut dce = DCE::new();
                    dce.analyze(rfn, Some(all));

                    let mut combiner = Combiner::new();
                    combiner.analyze(rfn, Some(all));

                    let ru = self.analyze_fn(rfn, &self.reginfo).unwrap_or_else(|| {
                        radeco_err!("Failed to analyze fn: {:?} (@ {:#X})", rfn.name, fn_addr);
                        // if analysis failed, default to "reads and clobbers everything"
                        self.reginfo.new_register_usage()
                    });

                    rfn.regusage = ru;
                    self.analyzed.insert(fn_addr);
                }
            }
        }

        None
    }
}

impl Inferer {
    pub fn new(reginfo: SubRegisterFile) -> Inferer {
        Inferer {
            reginfo: reginfo,
            analyzed: HashSet::new(),
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
        for (regid, &op_node) in &call_reg_map {
            if fn_map[&call_tgt_addr].regusage.is_ignored(regid) {
                fn_map
                    .get_mut(&fn_addr)
                    .unwrap()
                    .ssa_mut()
                    .op_unuse(call_node, op_node);
            }
        }

        // bridge preserved registers
        for (regid, (use_node, _)) in utils::call_rets(call_node, fn_map[&fn_addr].ssa()) {
            if fn_map[&call_tgt_addr].regusage.is_preserved(regid) {
                fn_map
                    .get_mut(&fn_addr)
                    .unwrap()
                    .ssa_mut()
                    .replace_value(use_node, call_reg_map[regid]);
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
        let entry_regstate = utils::register_state_info(entry_regstate_node, ssa);
        let exit_regstate = utils::register_state_info(exit_regstate_node, ssa);

        let mut ret = reginfo.new_register_usage();
        ret.set_all_ignored();

        for regid in ssa.regfile.iter_register_ids() {
            // ignore registers not in entry regstate
            if let Some(&(reg_val_entry, _)) = entry_regstate.get(regid) {
                // bail if a register isn't present in exit regstate
                let &(reg_val_exit, _) = exit_regstate.get(regid)?;

                if reg_val_exit == reg_val_entry {
                    ret.set_preserved(regid);
                }

                // find all uses, ignoring entry/exit register state
                let mut uses_iter = ssa
                    .uses_of(reg_val_entry)
                    .into_iter()
                    .filter(|&n| n != entry_regstate_node && n != exit_regstate_node);
                if uses_iter.next().is_some() {
                    ret.set_read(regid);
                }
            }
        }

        Some(ret)
    }
}

fn direct_call_info(
    ssa: &SSAStorage,
    call_node: <SSAStorage as SSA>::ValueRef,
) -> Option<(u64, RegisterMap<<SSAStorage as SSA>::ValueRef>)> {
    let callinfo = utils::call_info(call_node, ssa)?;
    Some((ssa.constant(callinfo.target)?, callinfo.register_args))
}
