// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Defines the traits to be implemented by the Control Flow Graph (CFG).
//!
//! These traits act as a way to abstract the actual storage
//! mechanism of the CFG and SSA.
//! Any struct that implement these traits can be used with other methods.
//!
//! # Design
//!
//!  * `Graph` - This acts as the base trait upon which the
//!  whole SSA form is build. The `Graph` trait provides functions to the
//!  Basic Graph Operation.
//!
//!  The above traits are generic over indexes that are used to refer to edges
//!  and nodes in a graph.
//!
//!  It is important to note that the trait `SSA` and `CFG` require `Graph` to
//!  be implemented.
//!
//!  Individual traits and their methods are explained in their respective
//!  docs.
//!
//!  Note: Reference in the docs refers to any type that is used to index
//!  nodes and edges in the graph and not necessarily __pointers__.

use std::fmt::Debug;
use std::hash::Hash;

use petgraph::EdgeDirection;

// TODO: Add invalid function for EdgeInfo and ConditionInfo

/// Edge info for avoiding ugly tuples
#[derive(Clone, Debug)]
pub struct EdgeInfo<T: Eq + Hash + Clone + Copy + Debug> {
    pub source: T,
    pub target: T,
}

impl<T: Eq + Hash + Clone + Copy + Debug> EdgeInfo<T> {
    pub fn new(source: T, target: T) -> EdgeInfo<T> {
        EdgeInfo {
            source: source,
            target: target,
        }
    }
}

/// Struct for conditional data, to avoid ugly tuples
#[derive(Clone, Debug)]
pub struct ConditionInfo<T: Eq + Hash + Clone + Copy + Debug> {
    pub true_side: T,
    pub false_side: T,
}

impl<T: Eq + Hash + Clone + Copy + Debug> ConditionInfo<T> {
    pub fn new(true_side: T, false_side: T) -> ConditionInfo<T> {
        ConditionInfo {
            true_side: true_side,
            false_side: false_side,
        }
    }
}

/// Trait provide basic graph operations.
pub trait Graph {
    type GraphNodeRef: Eq + Hash + Clone + Copy + Debug;
    type GraphEdgeRef: Eq + Hash + Clone + Copy + Debug;

    type NodeData: Clone + Debug;
    type EdgeData: Clone + Debug;

    /// Return all nodes in the graph.
    fn nodes(&self) -> Vec<Self::GraphNodeRef>;

    /// Return the count of nodes.
    fn nodes_count(&self) -> usize;

    /// Return the count of edges.
    fn edges_count(&self) -> usize;

    /// Return edge information.
    fn edge_info(&self, i: Self::GraphEdgeRef) -> Option<EdgeInfo<Self::GraphNodeRef>>;

    /// Insert a new node into graph.
    fn insert_node(&mut self, d: Self::NodeData) -> Option<Self::GraphNodeRef>;

    /// Remove a certain node in the graph.
    fn remove_node(&mut self, exit: Self::GraphNodeRef);

    /// Replace a node by another node.
    fn replace_node(&mut self, i: Self::GraphNodeRef, j: Self::GraphNodeRef);

    /// Insert a new edge into graph.
    fn insert_edge(
        &mut self,
        i: Self::GraphNodeRef,
        j: Self::GraphNodeRef,
        e: Self::EdgeData,
    ) -> Option<Self::GraphEdgeRef>;

    /// Update edge information.
    fn update_edge(
        &mut self,
        i: Self::GraphNodeRef,
        j: Self::GraphNodeRef,
        e: Self::EdgeData,
    ) -> Option<Self::GraphEdgeRef>;

    /// Reference to the edge that connects the source to the target.
    fn find_edges_between(
        &self,
        source: Self::GraphNodeRef,
        target: Self::GraphNodeRef,
    ) -> Vec<Self::GraphEdgeRef>;

    /// Remove edges beteween nodes.
    fn remove_edges_between(&mut self, i: Self::GraphNodeRef, j: Self::GraphNodeRef);

    /// Gather neighborhood nodes.
    fn gather_adjacences(
        &self,
        node: Self::GraphNodeRef,
        direction: EdgeDirection,
        data: bool,
    ) -> Vec<Self::GraphNodeRef>;
}
