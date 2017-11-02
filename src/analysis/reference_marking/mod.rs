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


use frontend::radeco_containers::{RadecoFunction, RadecoModule};
use middle::regfile::SubRegisterFile;
use middle::ssa::ssa_traits::ValueType;
use r2api::structs::LSectionInfo;
// XXX: This will move out sometime in the future
pub use self::reference_marking_inter::{Transfer, Propagate, Eval};
pub use self::reference_marking_intra::ReferenceMarker;
use std::collections::HashMap;
use std::sync::Arc;

// Implement interfunction reference marking for `reference_marking_intra::ReferenceMarker`
impl<'r> Transfer for ReferenceMarker<'r> {
    fn transfer(rfn: &mut RadecoFunction,
                regfile: &SubRegisterFile,
                sections: &[LSectionInfo])
                -> ReferenceMarker<'r> {
        unimplemented!()
    }

    fn transfer_iterative(analyzer: &mut ReferenceMarker<'r>, rfn: &mut RadecoFunction) -> bool {
        unimplemented!()
    }
}

// XXX: Move to intra-
impl Default for ReferenceMarkerInfo {
    fn default() -> ReferenceMarkerInfo {
        ReferenceMarkerInfo {
            args: Vec::new(),
            ret: ValueType::Unresolved,
        }
    }
}

// XXX: Move to intra-
impl Eval for ReferenceMarkerInfo {
    fn eval(op1: &ReferenceMarkerInfo, op2: &ReferenceMarkerInfo) -> ReferenceMarkerInfo {
        unimplemented!()
    }
}

#[derive(Clone, Debug)]
pub struct ReferenceMarkerInfo {
    args: Vec<ValueType>,
    ret: ValueType,
}

impl<'r> Propagate for ReferenceMarker<'r> {
    type Info = ReferenceMarkerInfo;

    fn pull<'a>(analyzer: &mut ReferenceMarker<'r>,
                rfn: &RadecoFunction,
                callsite: u64)
                -> Option<ReferenceMarkerInfo> {
        unimplemented!()
    }

    fn summary(analyzer: &mut ReferenceMarker<'r>,
               rfn: &RadecoFunction)
               -> Option<ReferenceMarkerInfo> {
        unimplemented!()
    }

    fn union(analyzer: &mut ReferenceMarker<'r>,
             infov: &[ReferenceMarkerInfo])
             -> Option<Self::Info> {
        unimplemented!()
    }

    fn push(analyzer: &mut ReferenceMarker<'r>, info: Option<&ReferenceMarkerInfo>) -> bool {
        unimplemented!()
    }
}
