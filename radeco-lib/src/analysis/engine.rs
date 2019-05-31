//! This module implements the logic needed to apply the proper analysis passes to simplify the IR.

use petgraph::algo::tarjan_scc;
use petgraph::Graph;

use rayon::prelude::*;

use std::any::Any;
use std::collections::HashMap;
use std::fmt::Debug;

use crate::analysis::analyzer;
use crate::analysis::analyzer::{
    Action, AnalyzerInfo, AnalyzerKind, Change, FuncAnalyzer, ModuleAnalyzer,
};
use crate::analysis::arithmetic::{ArithChange, Arithmetic};
use crate::analysis::copy_propagation::CopyPropagation;
use crate::analysis::cse::cse::CSE;
use crate::analysis::cse::ssasort::Sorter;
use crate::analysis::dce::DCE;
use crate::analysis::functions::fix_ssa_opcalls::CallSiteFixer;
use crate::analysis::functions::infer_regusage::Inferer;
use crate::analysis::inst_combine::Combiner;
use crate::analysis::interproc::fixcall::CallFixer;
use crate::analysis::sccp::SCCP;
use crate::frontend::radeco_containers::{FunctionKind, RadecoFunction, RadecoModule};
use crate::middle::regfile::SubRegisterFile;

fn sort_by_requires(analyzers: &Vec<AnalyzerKind>) -> impl Iterator<Item = AnalyzerKind> {
    // Build the dependency graph.
    let mut graph = Graph::<AnalyzerKind, ()>::new();
    let mut kind2id = HashMap::new();

    for analyzer in analyzers {
        let n = kind2id
            .entry(*analyzer)
            .or_insert_with(|| graph.add_node(*analyzer))
            .clone();
        let info: &'static AnalyzerInfo = From::from(*analyzer);

        for dep in info.requires {
            let d = kind2id
                .entry(*dep)
                .or_insert_with(|| graph.add_node(*dep))
                .clone();
            graph.add_edge(d, n, ());
        }
    }

    // Compute the reverse topological sorting using the Tarjan's algorithm. If SCCs are present
    // we can safely run the analyzers within the same SCC in arbitrary order because the `requires`
    // property is not garanteed to be respected.
    let sccs = tarjan_scc(&graph);

    sccs.into_iter().flatten().map(move |id| graph[id]).rev()
}

pub trait EngineResult: Any + Debug {}

pub trait Engine: Any + Debug {
    fn run_module(
        &self,
        rmod: &mut RadecoModule,
        regfile: &SubRegisterFile,
    ) -> Option<Box<dyn EngineResult>>;
    fn run_func(&self, rfn: &mut RadecoFunction) -> Option<Box<dyn EngineResult>>;
}

/// Radeco's default engine.
#[derive(Debug)]
pub struct RadecoEngine {
    max_iteration: u32,
}

impl RadecoEngine {
    pub fn new(max_iteration: u32) -> Self {
        RadecoEngine {
            max_iteration: max_iteration,
        }
    }
}

impl Engine for RadecoEngine {
    fn run_module(
        &self,
        rmod: &mut RadecoModule,
        regfile: &SubRegisterFile,
    ) -> Option<Box<dyn EngineResult>> {
        radeco_trace!("run_module");

        // Analyze preserved for all functions.
        {
            let bp_name = regfile.get_name_by_alias(&"BP".to_string());
            let bp_name = bp_name.map(|s| s.to_owned());
            let sp_name = regfile.get_name_by_alias(&"SP".to_string());
            let sp_name = sp_name.map(|s| s.to_owned());
            let mut callfixer = CallFixer::new(rmod, bp_name, sp_name);
            callfixer.rounded_analysis();
        }

        // Fix call sites
        let mut call_site_fixer = CallSiteFixer::new();
        call_site_fixer.analyze(rmod, None::<fn(_) -> _>);

        // Infer calling conventions
        let mut inferer = Inferer::new((*regfile).clone());
        inferer.analyze(rmod, None::<fn(_) -> _>);

        rmod.functions.par_iter_mut().for_each(|(_, rfn)| {
            self.run_func(rfn);
        });

        None
    }

    fn run_func(&self, rfn: &mut RadecoFunction) -> Option<Box<dyn EngineResult>> {
        radeco_trace!("run_func: {}", rfn.name);

        // There is no code for imported/relocated functions.
        if rfn.kind != FunctionKind::Local {
            return None;
        }

        // Try to convert the condition codes to relational operators. This should be done before
        // all the other passes.
        let mut arithmetic = Arithmetic::new();
        arithmetic.analyze(
            rfn,
            Some(|change: Box<dyn Change>| {
                let change = change.as_any().downcast_ref::<ArithChange>().unwrap();
                if change.new_expr.contains("OpEq")
                    || change.new_expr.contains("OpGt")
                    || change.new_expr.contains("OpLt")
                {
                    Action::Apply
                } else {
                    Action::Skip
                }
            }),
        );

        {
            // Sort the IR.
            let mut sorter = Sorter::new(rfn.ssa_mut());
            sorter.run();
        }

        let mut analyzers = sort_by_requires(&analyzer::all_func_analyzers());

        // Run iteratively all the available analyzers until a stable point or the maximum
        // number of iterations is reached.
        for _ in 0..self.max_iteration {
            let mut stable = true;

            // Build and run the analyzers.
            while let Some(analyzer) = analyzers.next() {
                radeco_trace!("running analyzer: {:?}", analyzer);
                // If the policy is called then there is still something to change, thus this is
                // not a stable point.
                let policy = |_| {
                    stable = false;
                    Action::Apply
                };

                match analyzer {
                    AnalyzerKind::Arithmetic => {
                        let mut arithmetic = Arithmetic::new();
                        arithmetic.analyze(rfn, Some(policy));
                    }
                    AnalyzerKind::Combiner => {
                        let mut combiner = Combiner::new();
                        combiner.analyze(rfn, Some(policy));
                    }
                    AnalyzerKind::CopyPropagation => {
                        let mut copy_propagation = CopyPropagation::new();
                        copy_propagation.analyze(rfn, Some(policy));
                    }
                    AnalyzerKind::CSE => {
                        let mut cse = CSE::new();
                        cse.analyze(rfn, Some(policy));
                    }
                    AnalyzerKind::DCE => {
                        let mut dce = DCE::new();
                        dce.analyze(rfn, Some(policy));
                    }
                    AnalyzerKind::SCCP => {
                        let mut sccp = SCCP::new();
                        sccp.analyze(rfn, Some(policy));
                    }
                    _ => (),
                }
            }

            if stable {
                break;
            }
        }

        None
    }
}
