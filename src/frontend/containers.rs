//! Defines `Module` and `Function` that act as containers.

use std::fmt::Debug;
use std::hash::Hash;
use std::collections::{HashMap, BTreeSet};
use petgraph::graph::NodeIndex;

use frontend::source::Source;
use middle::ssa::ssastorage::SSAStorage;

#[derive(Clone, Debug)]
pub struct RadecoModule {
    functions: HashMap<u64, RadecoFunction>,
}

#[derive(Clone, Debug)]
pub struct RadecoFunction {
    ssa: SSAStorage,
    call_ctx: Vec<CallContext>,
    callrefs: BTreeSet<u64>,
    callxrefs: BTreeSet<u64>,
}

impl Default for RadecoFunction {
    fn default() -> RadecoFunction {
        RadecoFunction {
            ssa: SSAStorage::new(),
            call_ctx: Vec::new(),
            callrefs: BTreeSet::new(),
            callxrefs: BTreeSet::new(),
        }
    }
}

impl RadecoFunction {
    fn new() -> RadecoFunction {
        RadecoFunction {
            ssa: SSAStorage::new(),
            call_ctx: Vec::new(),
            callrefs: BTreeSet::new(),
            callxrefs: BTreeSet::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct CallContext {
    /// Start offset of caller (uniquely identifies a function).
    caller: u64,
    /// Start offset of callee (uniquely identifies a function).
    callee: u64,
    /// Translate a node in callees context into a node in caller's context.
    ctx_translate: HashMap<NodeIndex, NodeIndex>,
}

// From trait to construct a module from `Source`.
// Note that this conversion is expensive as the source is used to construct the SSA for all the
// function that it holds and perform basic analysis.
impl<'a, T: 'a + Source> From<&'a mut T> for RadecoModule {
    fn from(source: &'a mut T) -> RadecoModule {
        unimplemented!()
    }
}

impl RadecoModule {
    fn construct<S: Source>(source: &mut S) -> RadecoModule {
        unimplemented!()
    }
}

pub trait RFunction { }
pub trait RModule {
    type FnRef: Copy + Clone + Debug + Hash + Eq;

    fn callees_of(&self, &Self::FnRef) -> Vec<Self::FnRef>;
    fn callers_of(&self, &Self::FnRef) -> Vec<Self::FnRef>;
    fn functions(&self) -> Vec<Self::FnRef>;
}
