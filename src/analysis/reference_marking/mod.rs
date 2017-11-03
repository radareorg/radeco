//! Implements reference marking to set correct `ValueType` to nodes
//!
//! This module is broken down into two phases:
//!     1. Intra-functional reference marking -- Identifies references within a single function
//!     2. Inter-function reference marking -- Propagates information across function boundaries
//!     at call-sites.
//!
//! The intra-function reference marking is called as a part of the inter-functional analysis.
//! The inter-function analyzer runs the analysis in parallel over all functions and then
//! sequentially merges this information.
//!
//! This process repeats until a cap of `n` iterations is hit, or we reach a fixpoint.

pub mod reference_marking_intra;
pub mod reference_marking_inter;

use petgraph::graph::NodeIndex;
use frontend::radeco_containers::{RadecoFunction, RadecoModule, CallContextInfo};
use middle::regfile::SubRegisterFile;
use middle::ssa::ssa_traits::ValueType;
use r2api::structs::LSectionInfo;
// XXX: This will move out sometime in the future
pub use self::reference_marking_inter::{Transfer, Propagate, Eval};
pub use self::reference_marking_intra::ReferenceMarker;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

// Implement interfunction reference marking for `reference_marking_intra::ReferenceMarker`
impl Transfer for ReferenceMarker {
    fn transfer(rfn: &mut RadecoFunction,
                regfile: Arc<SubRegisterFile>,
                sections: Arc<Vec<LSectionInfo>>)
                -> ReferenceMarker {
        ReferenceMarker::resolve_references(rfn, regfile, sections)
    }

    fn transfer_iterative(analyzer: &mut ReferenceMarker, rfn: &mut RadecoFunction) -> bool {
        analyzer.resolve_references_iterative(rfn)
    }
}

// XXX: Move to intra-
impl Default for ReferenceMarkerInfo {
    fn default() -> ReferenceMarkerInfo {
        ReferenceMarkerInfo(HashMap::new())
    }
}

// XXX: Move to intra-
/// Defines how to merge `ReferenceMarkerInfo` from two different callsites.
/// More precise information is used and propagated for each of the arguments and return values
impl Eval for ReferenceMarkerInfo {
    fn eval(op1: &ReferenceMarkerInfo, op2: &ReferenceMarkerInfo) -> ReferenceMarkerInfo {
        ReferenceMarkerInfo(op1.0
            .keys()
            .collect::<HashSet<_>>()
            .intersection(&op2.0.keys().collect())
            .map(|&key| {
                (*key,
                 match (op1.0[key], op2.0[key]) {
                     (ValueType::Invalid, _) |
                     (_, ValueType::Invalid) => ValueType::Invalid,
                     (ValueType::Unresolved, ref op_v) | 
                     (ref op_v, ValueType::Unresolved) => *op_v,
                     (ValueType::Scalar, ValueType::Scalar) => ValueType::Scalar,
                     (ValueType::Reference, ValueType::Reference) => ValueType::Reference,
                     (_, _) => ValueType::Invalid,
                 })
            })
            .collect())
    }
}

#[derive(Clone, Debug)]
pub struct ReferenceMarkerInfo(HashMap<NodeIndex, ValueType>);

impl Propagate for ReferenceMarker {
    type Info = ReferenceMarkerInfo;

    fn pull(analyzer: &mut ReferenceMarker,
            rfn: &RadecoFunction,
            callsite: &CallContextInfo)
            -> Option<ReferenceMarkerInfo> {
        Some(ReferenceMarkerInfo(callsite.map
            .iter()
            .map(|&(caller, callee)| (callee, analyzer.cs.bvalue(caller)))
            .collect()))
    }

    fn summary(analyzer: &mut ReferenceMarker,
               rfn: &RadecoFunction)
               -> Option<ReferenceMarkerInfo> {
        Some(ReferenceMarkerInfo(rfn.bindings().into_iter()
            .filter(|x| x.btype.is_argument() || x.btype.is_return())
            .map(|x| (x.idx, analyzer.cs.bvalue(x.idx)))
            .collect()))
    }

    fn union(analyzer: &mut ReferenceMarker, infov: &[ReferenceMarkerInfo]) -> Option<Self::Info> {
        unimplemented!()
    }

    fn push(analyzer: &mut ReferenceMarker, info: Option<&ReferenceMarkerInfo>)
            -> bool {
        if let Some(refinfo) = info {
            for (idx, vt) in &refinfo.0 {
                analyzer.cs.add_eq(*idx, *vt);
            }
        }
        true
    }
}
