//! Implements inter-function reference propagation
//!
//! Design points:
//!   - Keep the generic case in mind. The inter-function propagation should, in theory, work
//!   with any generic intra-function analysis. This allows us reuse code whenever the general
//!   structure of the analysis is same.
//!

use frontend::radeco_containers::{RadecoFunction, RadecoModule, CallContextInfo};
use middle::ir::MAddress;
use middle::regfile::SubRegisterFile;
use petgraph::Direction;
use petgraph::visit::EdgeRef;
use r2api::structs::LSectionInfo;
use rayon::prelude::*;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::sync::Arc;

pub trait InterProcAnalysis: Transfer + Propagate {}

impl<T: Transfer + Propagate> InterProcAnalysis for T {}

pub trait Transfer {
    // Generalize to take anything (any information about the context/module)
    // This function is only called the first time the analysis is executed. This function then
    // returns `Self` that contains (partially-)analyzed information.
    fn transfer(&mut RadecoFunction, Arc<SubRegisterFile>, Arc<Vec<LSectionInfo>>) -> Self;
    // Called when transfer function needs to be executed iteratively.
    // This function returns a `bool` which indicates if the analysis made any progress on this
    // iteration of call.
    fn transfer_iterative(&mut Self, &mut RadecoFunction) -> bool;
}

pub trait Propagate {
    type Info: Default + Eval + Clone;
    // Used to `pull` information, `Info`, relevant in inter-proc analysis computed in
    // the transfer phase.
    fn pull(&mut Self, &RadecoFunction, &CallContextInfo) -> Option<Self::Info>;
    fn summary(&mut Self, &RadecoFunction) -> Option<Self::Info>;
    // Used to aggregate information obtained from various sources/callsites
    fn union(&mut Self, &[Self::Info]) -> Option<Self::Info>;
    // Used to `push` information to the analyzer based on computed InterProc information.
    // Returns true if something in the internal state of the analyzer changed. This could be used
    // as a hint to determine if the analyzer can make further progress.
    fn push(&mut Self, Option<&Self::Info>) -> bool;
}

// TODO: Maybe `Eval` can be a part of definition of a lattice later on.
pub trait Eval: Default {
    fn eval(&Self, &Self) -> Self;
}

// TODO: Think about implementing this on top of the trait directly rather than a dummy struct
pub struct InterProceduralAnalyzer<T: InterProcAnalysis> {
    _t: PhantomData<T>,
}

struct AnalyzerWrapper<T: InterProcAnalysis> {
    analyzer: T,
    offset: u64,
    should_run: bool,
    times_run: u64,
}

impl<T: InterProcAnalysis> AnalyzerWrapper<T> {
    pub fn new(offset: u64, analyzer: T) -> AnalyzerWrapper<T> {
        AnalyzerWrapper {
            analyzer: analyzer,
            offset: offset,
            should_run: true,
            times_run: 0,
        }
    }

    pub fn analyzer_mut(&mut self) -> &mut T {
        &mut self.analyzer
    }

    pub fn analyzer(&self) -> &T {
        &self.analyzer
    }

    pub fn should_run(&self) -> bool {
        self.should_run
    }

    pub fn increment_run_count(&mut self) {
        self.times_run += 1;
    }

    pub fn offset(&self) -> u64 {
        self.offset
    }

    pub fn set_run(&mut self, should_run: bool) {
        self.should_run = should_run;
    }
}

impl<T: InterProcAnalysis> InterProceduralAnalyzer<T> {
    pub fn analyze(rmod: &mut RadecoModule, regfile: &Arc<SubRegisterFile>, n_iters: Option<u64>) {
        let sections = Arc::clone(rmod.sections());
        let mut analyzers: Vec<AnalyzerWrapper<T>> = Vec::new();
        let mut fixpoint = false;

        // Transfer can be done in (TODO) parallel
        for wrapper in rmod.iter_mut() {
            let (current_offset, current_fn) = wrapper.function;
            let analyzer = T::transfer(current_fn, Arc::clone(&regfile), Arc::clone(&sections));
            analyzers.push(AnalyzerWrapper::new(*current_offset, analyzer));
        }

        let mut max_iterations = n_iters.unwrap_or(u64::max_value());

        while !fixpoint  && max_iterations > 0 {
            max_iterations -= 1;
            // Propagation should be done in serial
            let mut infos: HashMap<u64, Vec<T::Info>> = HashMap::new();
            for (i, wrapper) in rmod.iter().enumerate() {
                let (current_offset, current_fn) = wrapper.function;
                let current_analyzer = analyzers.get_mut(i).map(|a| a.analyzer_mut()).expect("");
                // Get info about current function
                if let Some(this_info) = T::summary(current_analyzer, current_fn) {
                    infos.entry(*current_offset).or_insert(Vec::new()).push(this_info);
                }
                // Get callsite information for every callee of current function
                let callgraph = rmod.callgraph();
                let current_fn_node = current_fn.cgid();
                for (callee, info) in callgraph.edges_directed(current_fn_node, Direction::Outgoing)
                    .map(|call_edge| {
                        let callee = *callgraph.node_weight(call_edge.target()).expect("");
                        let csite = &call_edge.weight();
                        (callee, T::pull(current_analyzer, current_fn, csite))
                    }) {
                    let ref mut e = infos.entry(callee).or_insert(Vec::new());
                    if let Some(inf) = info {
                        e.push(inf);
                    }
                }
            }

            // Union all infos collected for a function. TODO: Parallelize as there is no
            // dependency.
            for (_, infov) in infos.iter_mut() {
                // XXX: Avoid allocation
                *infov = vec![infov.iter()
                                  .fold(T::Info::default(), |acc, x| T::Info::eval(&acc, &x))];
            }

            // Push the information down to the analyzers.
            for analyzer_wrapper in analyzers.iter_mut() {
                let offset = analyzer_wrapper.offset();
                analyzer_wrapper.should_run = {
                    let info = infos.get(&offset);
                    let analyzer = analyzer_wrapper.analyzer_mut();
                    if let Some(ref inf) = info {
                        T::push(analyzer, Some(&inf[0]))
                    } else {
                        // Assume that the analyzer should be run.
                        true
                    }
                }
            }

            // TODO: Upward propagation of latest results in callgraph

            // Continue analysis. TODO: Parallelize
            for (wrapper, aw) in rmod.iter_mut().zip(analyzers.iter_mut()) {
                // Check if the analyzer should be run for the current function.
                if !aw.should_run() {
                    continue;
                }
                aw.increment_run_count();
                let analyzer = aw.analyzer_mut();
                let (_, current_fn) = wrapper.function;
                let fp = T::transfer_iterative(analyzer, current_fn);
                fixpoint = fixpoint || fp;
            }
        }
    }
}
