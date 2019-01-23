use analysis::analyzer::{Analyzer, AnalyzerKind, AnalyzerResult, FuncAnalyzer};
use frontend::radeco_containers::RadecoFunction;
use middle::ir::MOpcode;
use middle::ssa::cfg_traits::CFG;
use middle::ssa::ssa_traits::*;
use middle::ssa::ssastorage::SSAStorage;

use petgraph::graph::NodeIndex;

use std::collections::HashSet;

#[derive(Debug)]
pub struct CopyPropagation;

impl CopyPropagation {
    pub fn new() -> Self {
        CopyPropagation
    }
}

impl Analyzer for CopyPropagation {
    fn name(&self) -> String {
        return "copy_propagation".to_owned()
    }

    fn kind(&self) -> AnalyzerKind {
        AnalyzerKind::CopyPropagation
    }

    fn requires(&self) -> Vec<AnalyzerKind> {
        Vec::new()
    }
}

impl FuncAnalyzer for CopyPropagation {
    fn analyze(&mut self, func: &mut RadecoFunction) -> Option<Box<AnalyzerResult>> {
        let ssa = func.ssa_mut();
        loop {
            let copies = CopyInfo::gather_copies(&ssa);
            if copies.is_empty() {
                break;
            }
            let mut replaced = HashSet::new();
            for CopyInfo(from, to) in copies {
                if replaced.contains(&from) {
                    continue;
                }
                replaced.insert(to);
                ssa.replace_value(to, from);
            }
        }

        None
    }
}

type From = NodeIndex;
type To = NodeIndex;
struct CopyInfo(From, To);

impl CopyInfo {
    fn gather_copies(ssa: &SSAStorage) -> Vec<Self> {
        ssa.blocks()
            .into_iter()
            .flat_map(|b| ssa.exprs_in(b))
            .filter_map(|e| match ssa.opcode(e) {
                Some(MOpcode::OpMov) => {
                    if let Some(&o) = ssa.operands_of(e).iter().nth(0) {
                        Some(CopyInfo(o, e))
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
