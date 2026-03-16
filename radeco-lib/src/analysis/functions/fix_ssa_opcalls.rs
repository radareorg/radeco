//! Fixes the call target for all call sites in the SSA
//!
//! For every [`OpCall`] SSA node in every function, try to find that call
//! site's corresponding edge in [the callgraph] and replace the "target"
//! operand of the SSA node with a constant value for the address of the actual
//! call target.
//!
//! [`OpCall`]: ir::MOpcode::OpCall
//! [the callgraph]: RadecoModule::callgraph

use crate::analysis::analyzer::{
    Action, Analyzer, AnalyzerInfo, AnalyzerKind, AnalyzerResult, Change, ModuleAnalyzer,
};
use crate::frontend::radeco_containers::*;
use crate::middle::ir;
use crate::middle::ssa::ssa_traits::*;
use crate::middle::ssa::ssastorage::SSAStorage;

use std::any::Any;
use std::collections::HashMap;

pub const INFO: AnalyzerInfo = CallSiteFixer::INFO;

/// Represents the call site fixer to replace SSA "target" operands with a constant value.
///
/// See the top-level documentation for details.
#[derive(Debug)]
pub struct CallSiteFixer;

impl Default for CallSiteFixer {
    fn default() -> Self {
        Self::new()
    }
}

impl CallSiteFixer {
    /// Represents the [CallSiteFixer] analyzer name.
    pub const NAME: &str = "call_site_fixer";
    /// Represents the [CallSiteFixer] required analyzers.
    pub const REQUIRES: &[AnalyzerKind] = &[];

    /// Represents the [CallSiteFixer] analyzer information.
    pub const INFO: AnalyzerInfo = AnalyzerInfo {
        name: Self::NAME,
        kind: AnalyzerKind::CallSiteFixer,
        requires: Self::REQUIRES,
        uses_policy: false,
    };

    /// Creates a new [CallSiteFixer].
    #[inline]
    pub const fn new() -> Self {
        CallSiteFixer
    }

    /// Walks the call graph, fixing the call site operands for any call operations.
    pub(crate) fn go_fn(rfun: &mut RadecoFunction, callgraph: &CallGraph) {
        let fn_addr = rfun.offset;
        let call_site_addr_to_target_addr: HashMap<u64, u64> = callgraph
            .callees(rfun.cgid())
            .map(|(cs_a, tgt_idx)| (cs_a, callgraph[tgt_idx]))
            .collect();
        let ssa = rfun.ssa_mut();
        for node in ssa.inorder_walk() {
            if let Ok(NodeType::Op(ir::MOpcode::OpCall)) = ssa.node_data(node).map(|x| x.nt) {
                if Self::fix_call_site(ssa, node, &call_site_addr_to_target_addr).is_none() {
                    radeco_err!("failed to fix call site {node:?} in function at {fn_addr:#X}")
                }
            }
        }
    }

    /// Attempts to insert a constant value in the `call_node` "target" operand.
    ///
    /// See top-level documentation for details.
    pub(crate) fn fix_call_site(
        ssa: &mut SSAStorage,
        call_node: <SSAStorage as SSA>::ValueRef,
        fn_call_map: &HashMap<u64, u64>,
    ) -> Option<()> {
        let call_site_addr = ssa.address(call_node)?.address;

        if let Some(&call_target_addr) = fn_call_map.get(&call_site_addr) {
            let old_opcall_tgt_node = ssa
                .sparse_operands_of(call_node)
                .iter()
                .find(|(x, _)| *x == 0)
                .map(|(_, tgt)| *tgt)?;
            let new_opcall_tgt_node = ssa.insert_const(call_target_addr, None)?;
            ssa.op_unuse(call_node, old_opcall_tgt_node);
            ssa.op_use(call_node, 0, new_opcall_tgt_node);
        } else {
            radeco_trace!(
                "call site at {call_site_addr:#X} isn't in call graph; perhaps the call is \
                 indirect?"
            );
        }

        Some(())
    }
}

impl Analyzer for CallSiteFixer {
    fn info(&self) -> &'static AnalyzerInfo {
        &Self::INFO
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl ModuleAnalyzer for CallSiteFixer {
    fn analyze<T: FnMut(Box<dyn Change>) -> Action>(
        &mut self,
        rmod: &mut RadecoModule,
        _policy: Option<T>,
    ) -> Option<Box<dyn AnalyzerResult>> {
        for rfun in rmod.functions.values_mut() {
            Self::go_fn(rfun, &rmod.callgraph);
        }

        None
    }
}
