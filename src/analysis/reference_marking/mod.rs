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

pub use self::reference_marking_intra::ReferenceMarker;
