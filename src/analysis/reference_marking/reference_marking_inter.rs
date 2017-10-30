//! Implements inter-function reference propagation
//!
//! Design points:
//!   - Keep the generic case in mind. The inter-function propagation should, in theory, work
//!   with any generic intra-function analysis. This allows us reuse code whenever the general
//!   structure of the analysis is same.
//!

use frontend::radeco_containers::{RadecoFunction, RadecoModule};
use middle::ir::MAddress;
use middle::regfile::SubRegisterFile;
use r2api::structs::LSectionInfo;
use rayon::prelude::*;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::sync::Arc;
use petgraph::visit::EdgeRef;
use petgraph::Direction;

pub trait InterProcAnalysis: Transfer + Propagate {}

impl<T: Transfer + Propagate> InterProcAnalysis for T {}

pub trait Transfer {
    // Generalize to take anything (any information about the context/module)
    // This function is only called the first time the analysis is executed. This function then
    // returns `Self` that contains (partially-)analyzed information.
    fn transfer(&mut RadecoFunction, &SubRegisterFile, &[LSectionInfo]) -> Self;
    // Called when transfer function needs to be executed iteratively.
    // This function returns a `bool` which indicates if the analysis made any progress on this
    // iteration of call.
    fn transfer_iterative(&mut Self, &mut RadecoFunction) -> bool;
}

pub trait Propagate {
    type Info: Default + Eval;
    // Used to `pull` information, `Info`, relevant in inter-proc analysis computed in
    // the transfer phase.
    fn pull(&mut Self, &RadecoFunction, u64) -> Option<Self::Info>;
    fn summary(&mut Self) -> Option<Self::Info>;
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

// TODO: Move to a more appropriate place, i.e. `radeco_containers.rs`, once the details are
// concrete.
pub struct CallSite<T> {
    site: MAddress,
    info: T,
}

// TODO: Think about implementing this on top of the trait directly rather than a dummy struct
struct InterProceduralAnalyzer<T: InterProcAnalysis> {
    _t: PhantomData<T>,
}

impl<T: InterProcAnalysis> InterProceduralAnalyzer<T> {
    pub fn analyze(rmod: &mut RadecoModule, regfile: &SubRegisterFile, n_iters: Option<u64>) {
        let sections = Arc::clone(rmod.sections());
        let mut analyzers: Vec<(u64, T)> = Vec::new();
        let mut fixpoint = false;

        // Transfer can be done in (TODO) parallel
        for wrapper in rmod.iter_mut() {
            let (current_offset, current_fn) = wrapper.function;
            let analyzer = T::transfer(current_fn, &regfile, &sections);
            analyzers.push((*current_offset, analyzer));
        }

        // TODO: Set fixpoint correctly
        while !fixpoint {
            // Propagation should be done in serial
            let mut infos: HashMap<u64, Vec<T::Info>> = HashMap::new();
            for (i, wrapper) in rmod.iter().enumerate() {
                let (current_offset, current_fn) = wrapper.function;
                let &mut (_, ref mut current_analyzer) = analyzers.get_mut(i).expect("");
                // Get info about current function
                if let Some(this_info) = T::summary(current_analyzer) {
                    infos.entry(*current_offset).or_insert(Vec::new()).push(this_info);
                }
                // Get callsite information for every callee of current function
                let callgraph = rmod.callgraph();
                let current_fn_node = current_fn.cgid();
                for (callee, info) in callgraph
                    .edges_directed(current_fn_node, Direction::Outgoing)
                    .map(|call_edge| {
                        let callee = *callgraph.node_weight(call_edge.target()).expect("");
                        let csite = *call_edge.weight();
                        (callee, T::pull(current_analyzer, current_fn, csite))
                    }) {
                    infos.entry(callee).or_insert(Vec::new()).push(info.unwrap_or_default());
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
            for &mut(offset, ref mut analyzer) in analyzers.iter_mut() {
                let info = infos.get(&offset);
                if let Some(ref inf) = info {
                    T::push(analyzer, Some(&inf[0]));
                }
            }

            // Continue analysis. TODO: Parallelize
            for (wrapper, aw) in rmod.iter_mut().zip(analyzers.iter_mut()) {
                let &mut(_, ref mut analyzer) = aw;
                let (_, current_fn) = wrapper.function;
                T::transfer_iterative(analyzer, current_fn);
            }
        }
    }
}
