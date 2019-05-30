// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Defines the traits to be implemented by the Control Flow Graph (CFG).
//!
//! These traits extend upon the ones provided in `graph_traits`
//!
//! # Design
//!
//!  * `CFG` - This acts as the base trait upon which the
//!  whole SSA form is build. The `CFG` trait provides __accessors__ to the
//!  Basic Blocks of the program as well as the Control Flow edges that
//!  connect these blocks.
//!
//!  * `CFGMod` - This trait provides methods to __manipulate__ the Control
//!  Flow Graph.
//!
//!  The above traits are generic over indexes that are used to refer to edges
//!  and nodes in a CFG. `CFG::ActionRef` represents the type that is used
//!  to reference the Basic Blocks in the Control Flow Graph.
//!  Similarly, `CFG::CFEdgeRef` represents the type that is used to reference
//!  edges in the graph.
//!
//!  It is important to note that the trait `SSA` requires `CFG` to be
//!  implemented. The underlying CFG trait continues to provide access to the
//!  control flow structure of the program even in the SSA form. In essence,
//!  the SSA graph can be thought of
//!  as a composition (or superposition) of the SSA graph and the CFG graph.
//!
//!  Individual traits and their methods are explained in their respective
//!  docs.
//!
//!  Note: Reference in the docs refers to any type that is used to index
//!  nodes and edges in the graph and not necessarily __pointers__.

use std::fmt::Debug;
use std::hash::Hash;

use super::graph_traits::{ConditionInfo, Graph};
use crate::middle::ir::MAddress;

/// Provides __accessors__ to the underlying storage
pub trait CFG: Graph {
    type ActionRef: Eq + Hash + Clone + Copy + Debug;
    type CFEdgeRef: Eq + Hash + Clone + Copy + Debug;

    /// Check whether the node is a basic block.
    fn is_block(&self, action: Self::ActionRef) -> bool;

    /// Check whether the node is an action.
    fn is_action(&self, action: Self::ActionRef) -> bool;

    /// Reference to all blocks in the CFG
    fn blocks(&self) -> Vec<Self::ActionRef>;

    /// Reference to entry block of the CFG
    fn entry_node(&self) -> Option<Self::ActionRef>;

    /// Reference to exit block of the CFG
    fn exit_node(&self) -> Option<Self::ActionRef>;

    /// Reference to immediate predecessors of block
    fn preds_of(&self, node: Self::ActionRef) -> Vec<Self::ActionRef>;

    /// Reference to immediate successors of block
    fn succs_of(&self, node: Self::ActionRef) -> Vec<Self::ActionRef>;

    /// Reference to the next block in the natural flow of the CFG
    fn unconditional_block(&self, i: Self::ActionRef) -> Option<Self::ActionRef>;

    /// Reference to the conditional blocks in the natural flow of the CFG
    fn conditional_blocks(&self, i: Self::ActionRef) -> Option<ConditionInfo<Self::ActionRef>>;

    /// Reference that represents and Invalid block
    fn invalid_action(&self) -> Option<Self::ActionRef>;

    ///////////////////////////////////////////////////////////////////////////
    //// Edge accessors and helpers
    ///////////////////////////////////////////////////////////////////////////

    /// Reference to the conditional edges that flows out of the block
    fn conditional_edges(&self, i: Self::ActionRef) -> Option<ConditionInfo<Self::CFEdgeRef>>;

    /// Reference to the unconditional edge that flows out of the block
    fn unconditional_edge(&self, i: Self::ActionRef) -> Option<Self::CFEdgeRef>;

    /// Reference to all the incoming edges to a block
    fn incoming_edges(&self, i: Self::ActionRef) -> Vec<(Self::CFEdgeRef, u8)>;

    /// Reference to all the outgoing edges from a block
    fn outgoing_edges(&self, i: Self::ActionRef) -> Vec<(Self::CFEdgeRef, u8)>;

    /// Starting address of a basic block or dynamic
    fn starting_address(&self, block: Self::ActionRef) -> Option<MAddress>;

    /// Size of this basic block in bytes
    fn block_size(&self, block: Self::ActionRef) -> Option<u64>;

    /// Reference that represents an Invalid control flow edge.
    fn invalid_edge(&self) -> Option<Self::CFEdgeRef>;
}

/// Provides __mutators__ to the underlying storage
pub trait CFGMod: CFG {
    type BBInfo;

    /// Mark the start node for the SSA graph
    fn set_entry_node(&mut self, start: Self::ActionRef);

    /// Mark the exit node for the SSA graph
    fn set_exit_node(&mut self, exit: Self::ActionRef);

    /// Insert a new basic block
    fn insert_block(&mut self, info: Self::BBInfo) -> Option<Self::ActionRef>;

    /// Insert a new exit
    fn insert_dynamic(&mut self) -> Option<Self::ActionRef>;

    /// Insert a control edge between to basic blocks
    fn insert_control_edge(
        &mut self,
        source: Self::ActionRef,
        target: Self::ActionRef,
        index: u8,
    ) -> Option<Self::CFEdgeRef>;

    /// Remove a block and all its associated data from the graph
    fn remove_block(&mut self, node: Self::ActionRef);

    /// Remove a control edge from the graph
    fn remove_control_edge(&mut self, source: Self::CFEdgeRef);

    fn set_block_size(&mut self, bb: Self::ActionRef, last: u64);
}
