use crate::analysis::analyzer::{
    Action, Analyzer, AnalyzerInfo, AnalyzerKind, AnalyzerResult, Change, FuncAnalyzer,
    ReplaceValue,
};
use crate::frontend::radeco_containers::RadecoFunction;
use crate::middle::ir::MOpcode;
use crate::middle::ssa::cfg_traits::CFG;
use crate::middle::ssa::ssa_traits::*;
use crate::middle::ssa::ssastorage::SSAStorage;

use std::any::Any;
use std::collections::HashSet;

/// Represents copy-propagation operations for analysis.
///
/// Currently, gathers all [OpMov](MOpcode::OpMov) opcodes as [ReplaceValue]s.
///
/// Maintains a `skip`-list of opcodes to skip over during analysis.
#[derive(Debug)]
pub struct CopyPropagation {
    skip: Vec<ReplaceValue>,
}

impl Default for CopyPropagation {
    fn default() -> Self {
        Self::new()
    }
}

impl CopyPropagation {
    const NAME: &str = "copy_propagation";
    const REQUIRES: &[AnalyzerKind] = &[];

    /// Represents the [AnalyzerInfo] for [CopyPropagation].
    pub const INFO: AnalyzerInfo = AnalyzerInfo {
        name: Self::NAME,
        kind: AnalyzerKind::CopyPropagation,
        requires: Self::REQUIRES,
        uses_policy: true,
    };

    /// Creates a new [CopyPropagation].
    pub const fn new() -> Self {
        CopyPropagation { skip: Vec::new() }
    }

    fn gather_copies(ssa: &SSAStorage) -> Vec<ReplaceValue> {
        ssa.blocks()
            .into_iter()
            .flat_map(|b| ssa.exprs_in(b))
            .filter_map(|e| {
                ssa.opcode(e)
                    .filter(|op| matches!(op, MOpcode::OpMov))
                    .and_then(|_| ssa.operands_of(e).first().copied())
                    .map(|o| ReplaceValue(o, e))
                    .or_else(|| {
                        radeco_err!("No operand of `OpMov` found");
                        None
                    })
            })
            .collect::<Vec<_>>()
    }
}

pub const INFO: AnalyzerInfo = CopyPropagation::INFO;

impl Analyzer for CopyPropagation {
    fn info(&self) -> &'static AnalyzerInfo {
        &Self::INFO
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl FuncAnalyzer for CopyPropagation {
    fn analyze<T: FnMut(Box<dyn Change>) -> Action>(
        &mut self,
        func: &mut RadecoFunction,
        policy: Option<T>,
    ) -> Option<Box<dyn AnalyzerResult>> {
        let mut policy = policy.expect("A policy function must be provided");
        let ssa = func.ssa_mut();
        loop {
            let copies = CopyPropagation::gather_copies(ssa)
                .into_iter()
                .filter(|change| !self.skip.contains(change))
                .collect::<Vec<_>>();

            if copies.is_empty() {
                break;
            }

            let mut replaced = HashSet::new();
            for change in copies {
                let from = change.0;
                let to = change.1;

                if replaced.contains(&from) {
                    continue;
                }

                match policy(Box::new(change)) {
                    Action::Apply => {
                        replaced.insert(to);
                        ssa.replace_value(to, from);
                        self.skip.clear();
                    }
                    Action::Skip => {
                        self.skip.push(change);
                    }
                    Action::Abort => {
                        return None;
                    }
                }
            }
        }

        None
    }
}
