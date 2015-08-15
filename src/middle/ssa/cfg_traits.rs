//! Defines the traits to be implemented by the Control Flow Graph (CFG).
//!
//! These traits act as a way to abstract the actual storage 
//! mechanism of the CFG (and SSA).
//! Any struct that implement these traits can be used with other methods.
//! 
//! # Design
//!
//!  * `CFG` - This acts as the base trait upon which the
//!  whole SSA form is build. The `CFG` trait provides __accessors__ to the Basic
//!  Blocks of the program as well as the Control Flow edges that 
//!  connect these blocks.
//!
//!  * `CFGMod` - This trait provides methods to __manipulate__ the Control Flow Graph.
//!
//!  The above traits are generic over indexes that are used to refer to edges
//!  and nodes in a CFG. `CFG::ActionRef` represents the type that is used
//!  to reference the Basic Blocks in the Control Flow Graph.
//!  Similarly, `CFG::CFEdgeRef` represents the type that is used to reference
//!  edges in the graph.
//!
//!  It is important to note that the trait `SSA` requires `CFG` to be implemented.
//!  The underlying CFG trait continues to provide access to the control flow structure
//!  of the program even in the SSA form. In essence, the SSA graph can be thought of
//!  as a composition (or superposition) of the SSA graph and the CFG graph.
//!
//!  Individual traits and their methods are explained in their respective docs.
//!
//!  Note: Reference in the docs refers to any type that is used to index nodes and edges
//!  in the graph and not necessarily __pointers__.

use std::fmt::Debug;
use std::hash::Hash;

/// Provides __accessors__ to the underlying storage
pub trait CFG {
	type ActionRef: Eq + Hash + Clone + Copy + Debug;
	type CFEdgeRef: Eq + Hash + Clone + Copy + Debug;

	/// Reference to all blocks in the CFG
	fn blocks(&self) -> Vec<Self::ActionRef>;

	/// Reference to entry block of the CFG
	fn start_node(&self) -> Self::ActionRef;

	/// Reference to exit block of the CFG
	fn exit_node(&self) -> Self::ActionRef;

	/// Reference to the next block in the natural flow of the CFG
	fn get_unconditional(&self, i: &Self::ActionRef) -> Self::ActionRef;

	/// Reference to immediate predecessors of block
	fn preds_of(&self, node: Self::ActionRef) -> Vec<Self::ActionRef>;

	/// Reference to immediate successors of block
	fn succs_of(&self, node: Self::ActionRef) -> Vec<Self::ActionRef>;

	/// Reference that represents and Invalid block
	fn invalid_action(&self) -> Self::ActionRef;

	///////////////////////////////////////////////////////////////////////////
	//// Edge accessors and helpers
	///////////////////////////////////////////////////////////////////////////

	/// Reference to all outgoing edges from a block
	fn edges_of(&self, i: &Self::ActionRef) -> Vec<Self::CFEdgeRef>;
	
	/// Reference to all the incoming edges to a block
	fn incoming_edges(&self, i: &Self::ActionRef) -> Vec<Self::CFEdgeRef>;

	/// Returns (source_block, target_block) for an edge
	fn info(&self, i: &Self::CFEdgeRef) -> (Self::ActionRef, Self::ActionRef);

	/// Reference to the source block for the edge
	fn source_of(&self, i: &Self::CFEdgeRef) -> Self::ActionRef { self.info(i).0 }
	
	/// Reference to the target block for the edge
	fn target_of(&self, i: &Self::CFEdgeRef) -> Self::ActionRef { self.info(i).1 }

	/// Reference to the edge that connects the source to the target.
	fn find_edge(&self, source: &Self::ActionRef, target: &Self::ActionRef) -> Self::CFEdgeRef;

	/// Reference to the true edge
	fn true_edge_of(&self, i: &Self::ActionRef) -> Self::CFEdgeRef;

	/// Reference to the false edge
	fn false_edge_of(&self, i: &Self::ActionRef) -> Self::CFEdgeRef;

	/// Reference to the unconditional edge that flows out of the block
	fn next_edge_of(&self, i: &Self::ActionRef) -> Self::CFEdgeRef;

	/// Reference that represents an Invalid control flow edge.
	fn invalid_edge(&self) -> Self::CFEdgeRef;

}

/// Provides __mutators__ to the underlying storage
pub trait CFGMod: CFG {
	type BBInfo;

	/// Mark the start node for the SSA graph
	fn mark_start_node(&mut self, start: &Self::ActionRef);
	
	/// Mark the exit node for the SSA graph
	fn mark_exit_node(&mut self, exit: &Self::ActionRef);

	/// Add a new basic block
	fn add_block(&mut self, info: Self::BBInfo) -> Self::ActionRef;

	/// Add a new exit
	fn add_dynamic(&mut self) -> Self::ActionRef;

	/// Add a control edge between to basic blocks
	fn add_control_edge(&mut self, source: Self::ActionRef, target: Self::ActionRef, index: u8);

	/// Will remove a block and all its associated data from the graph
	fn remove_block(&mut self, node: Self::ActionRef);
}
