use analysis::analyzer::{Action, Analyzer, AnalyzerKind, AnalyzerResult, Change, FuncAnalyzer, ReplaceValue};
use frontend::radeco_containers::RadecoFunction;
use middle::ir::MOpcode;
use middle::ssa::cfg_traits::CFG;
use middle::ssa::ssa_traits::*;
use middle::ssa::ssastorage::SSAStorage;

use std::collections::HashSet;

#[derive(Debug)]
pub struct CopyPropagation {
    skip: Vec<ReplaceValue>,
}

impl CopyPropagation {
    pub fn new() -> Self {
        CopyPropagation {
            skip: Vec::new()
        }
    }

    fn gather_copies(ssa: &SSAStorage) -> Vec<ReplaceValue> {
        ssa.blocks()
            .into_iter()
            .flat_map(|b| ssa.exprs_in(b))
            .filter_map(|e| match ssa.opcode(e) {
                Some(MOpcode::OpMov) => {
                    if let Some(&o) = ssa.operands_of(e).iter().nth(0) {
                        Some(ReplaceValue(o, e))
                    } else {
                        radeco_err!("No operand of `OpMov` found");
                        None
                    }
                }
                _ => None,
            })
            .collect::<Vec<_>>()
    }
}

impl Analyzer for CopyPropagation {
    fn name(&self) -> String {
        return "copy_propagation".to_owned();
    }

    fn kind(&self) -> AnalyzerKind {
        AnalyzerKind::CopyPropagation
    }

    fn requires(&self) -> Vec<AnalyzerKind> {
        Vec::new()
    }

    fn uses_policy(&self) -> bool {
        true
    }
}

impl FuncAnalyzer for CopyPropagation {
    fn analyze<T: Fn(Box<Change>) -> Action>(
        &mut self,
        func: &mut RadecoFunction,
        policy: Option<T>,
    ) -> Option<Box<AnalyzerResult>> {
        let policy = policy.expect("A policy function must be provided");
        let ssa = func.ssa_mut();
        loop {
            let copies = CopyPropagation::gather_copies(&ssa)
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
                    },
                    Action::Skip => {
                        self.skip.push(change);
                    },
                    Action::Abort => {
                        return None;
                    }
                }

            }
        }

        None
    }
}
