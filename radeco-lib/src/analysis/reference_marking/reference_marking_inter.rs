//! Implements inter-function reference propagation
//!
//! Design points:
//!   - Keep the generic case in mind. The inter-function propagation should, in theory, work
//!   with any generic intra-function analysis. This allows us reuse code whenever the general
//!   structure of the analysis is same.
//!

use crate::frontend::radeco_containers::{CallContextInfo, CallGraph, RadecoFunction, RadecoModule};
use crate::middle::regfile::SubRegisterFile;

use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;
use petgraph::Direction;
use r2api::structs::LSectionInfo;
use std::collections::HashMap;
// TODO: Remove this
use std::fmt;
use std::marker::PhantomData;
use std::sync::Arc;

pub trait InterProcAnalysis: Transfer + Propagate {}

impl<T: Transfer + Propagate> InterProcAnalysis for T {}

pub trait Transfer {
    // Generalize to take anything (any information about the context/module)
    // This function is only called the first time the analysis is executed. This function then
    // returns `Self` that contains (partially-)analyzed information.
    fn transfer(_: &mut RadecoFunction, _: Arc<SubRegisterFile>, _: Arc<Vec<LSectionInfo>>) -> Self;
    // Called when transfer function needs to be executed iteratively.
    // This function returns a `bool` which indicates if the analysis made any progress on this
    // iteration of call.
    fn transfer_iterative(_: &mut Self, _: &mut RadecoFunction) -> bool;
}

pub trait Propagate {
    type Info: Default + Eval + Clone + fmt::Debug;
    // Used to `pull` information, `Info`, relevant in inter-proc analysis computed in
    // the transfer phase.
    // For imports, the first argument, the analyzer, is passed as None.
    fn pull(_: &mut Option<&mut Self>, _: &RadecoFunction, _: &CallContextInfo) -> Option<Self::Info>;
    fn summary(_: &mut Self, _: &RadecoFunction) -> Option<Self::Info>;
    // Used to aggregate information obtained from various sources/callsites
    fn union(_: &mut Self, _: &[Self::Info]) -> Option<Self::Info>;
    // Used to `push` information to the analyzer based on computed InterProc information.
    // Returns true if something in the internal state of the analyzer changed. This could be used
    // as a hint to determine if the analyzer can make further progress.
    fn push(_: &mut Self, _: Option<&Self::Info>) -> bool;
}

// TODO: Maybe `Eval` can be a part of definition of a lattice later on.
pub trait Eval: Default {
    fn eval(_: &Self, _: &Self) -> Self;
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

    pub fn should_run(&self) -> bool {
        self.should_run
    }

    pub fn increment_run_count(&mut self) {
        self.times_run += 1;
    }

    pub fn offset(&self) -> u64 {
        self.offset
    }
}

impl<T: InterProcAnalysis> InterProceduralAnalyzer<T> {
    // Propagate argument information to the callsite of every caller of this function.
    // current_fn -> callers of current_fn
    fn propagate_up_callgraph(
        current_fn_node: &NodeIndex,
        current_fn: &RadecoFunction,
        mut current_analyzer: Option<&mut T>,
        callgraph: &CallGraph,
        infos: &mut HashMap<u64, Vec<T::Info>>,
    ) {
        for (caller, info) in callgraph
            .edges_directed(*current_fn_node, Direction::Incoming)
            .map(|call_edge| {
                let caller = *callgraph.node_weight(call_edge.source()).expect("");
                let csite = &call_edge.weight();
                //let caller_analyzer = analyzers.get_mut(caller).map(|a| a.analyzer_mut()).expect("");
                let rcsite = CallContextInfo {
                    map: csite.map.iter().map(|&(x, y)| (y, x)).collect(),
                    csite: 0,
                    csite_node: csite.csite_node,
                };
                (caller, T::pull(&mut current_analyzer, current_fn, &rcsite))
            })
        {
            let ref mut e = infos.entry(caller).or_insert(Vec::new());
            if let Some(inf) = info {
                e.push(inf);
            }
        }
    }

    // Propagate argument information to the callsite of every callee of this function.
    // current_fn -> callees of current_fn
    fn propagate_down_callgraph(
        current_fn_node: &NodeIndex,
        current_fn: &RadecoFunction,
        mut current_analyzer: Option<&mut T>,
        callgraph: &CallGraph,
        infos: &mut HashMap<u64, Vec<T::Info>>,
    ) {
        let mut callee_info_map = Vec::new();
        for call_edge in callgraph.edges_directed(*current_fn_node, Direction::Outgoing) {
            let callee = *callgraph.node_weight(call_edge.target()).expect("");
            let csite = &call_edge.weight();
            callee_info_map.push((callee, T::pull(&mut current_analyzer, current_fn, csite)));
        }
        for (callee, info) in callee_info_map.into_iter() {
            let ref mut e = infos.entry(callee).or_insert(Vec::new());
            if let Some(inf) = info {
                e.push(inf);
            }
        }
    }

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

        let mut infos: HashMap<u64, Vec<T::Info>> = HashMap::new();

        {
            // Push information from imports (if any). Since this analysis (currently) cannot modify
            // information of any function that is outside the current module, the analysis information
            // for imports cannot change and we cannot gain new information about them. So it is
            // sufficient to do this once (and not iteratively).
            //
            // TODO: Currently, there is nothing to set this information inside radeco-lib (as we do
            // not analyze dependencies yet). This phase is only useful if another analysis pass
            // already preloads this information.
            let callgraph = &rmod.callgraph;
            for imp in rmod.imports.values() {
                let current_fn = imp.rfn.borrow_mut();
                let current_fn_node = current_fn.cgid();
                Self::propagate_up_callgraph(
                    &current_fn_node,
                    &current_fn,
                    None,
                    &callgraph,
                    &mut infos,
                );
            }
        }

        let mut max_iterations = n_iters.unwrap_or(u64::max_value());

        while !fixpoint && max_iterations > 0 {
            max_iterations -= 1;
            // Propagation should be done in serial
            for (i, wrapper) in rmod.iter().enumerate() {
                let (current_offset, current_fn) = wrapper.function;
                let current_analyzer = analyzers.get_mut(i).map(|a| a.analyzer_mut()).expect("");
                // Get info about current function
                if let Some(this_info) = T::summary(current_analyzer, current_fn) {
                    infos
                        .entry(*current_offset)
                        .or_insert(Vec::new())
                        .push(this_info);
                }
                // Get callsite information for every callee of current function
                let callgraph = &rmod.callgraph;
                let current_fn_node = current_fn.cgid();

                Self::propagate_down_callgraph(
                    &current_fn_node,
                    current_fn,
                    Some(current_analyzer),
                    &callgraph,
                    &mut infos,
                );

                Self::propagate_up_callgraph(
                    &current_fn_node,
                    current_fn,
                    Some(current_analyzer),
                    &callgraph,
                    &mut infos,
                );
            }

            // Union all infos collected for a function. TODO: Parallelize as there is no
            // dependency.
            for infov in infos.values_mut() {
                // XXX: Avoid allocation
                *infov = vec![infov
                    .iter()
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

            infos.clear();
        }
    }
}
