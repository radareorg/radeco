use std::hash::Hash;
use std::fmt::Debug;

pub trait CFG {
	type ActionRef: Eq + Hash + Clone + Copy + Debug;
	type CFEdgeRef: Eq + Hash + Clone + Copy + Debug;

	/// Get NodeIndex of all BasicBlocks available in the SSA form.
	fn get_blocks(&self) -> Vec<Self::ActionRef>;

	/// Start node of the CFG.
	fn start_node(&self) -> Self::ActionRef;

	/// Exit node of the CFG.
	fn exit_node(&self) -> Self::ActionRef;

	/// Get false branch of a conditional jump.
	fn get_unconditional(&self, i: &Self::ActionRef) -> Self::ActionRef;

	/// Get the predecessors of a basic block.
	fn preds_of(&self, node: Self::ActionRef) -> Vec<Self::ActionRef>;

	/// Get the successors of a basic block.
	fn succs_of(&self, node: Self::ActionRef) -> Vec<Self::ActionRef>;

	fn invalid_action(&self) -> Self::ActionRef;

	///////////////////////////////////////////////////////////////////////////
	//// Edge accessors and helpers
	///////////////////////////////////////////////////////////////////////////

	/// Gets all the outgoing edges of a BasicBlock.
	fn edges_of(&self, i: &Self::ActionRef) -> Vec<Self::CFEdgeRef>;
	
	/// Gets all the incoming edges to a BasicBlock.
	fn incoming_edges(&self, i: &Self::ActionRef) -> Vec<Self::CFEdgeRef>;

	/// Get info i.e. (source_edge, target_edge)
	fn info(&self, i: &Self::CFEdgeRef) -> (Self::ActionRef, Self::ActionRef);

	/// Get the ActionRef of the source of the Edge.
	fn source_of(&self, i: &Self::CFEdgeRef) -> Self::ActionRef { self.info(i).0 }
	
	/// Get the ActionRef of the target of the Edge.
	fn target_of(&self, i: &Self::CFEdgeRef) -> Self::ActionRef { self.info(i).1 }

	/// Find the edge that connects the source to the target.
	fn find_edge(&self, source: &Self::ActionRef, target: &Self::ActionRef) -> Self::CFEdgeRef;

	/// Get the True Edge
	fn true_edge_of(&self, i: &Self::ActionRef) -> Self::CFEdgeRef;

	/// Get the False Edge
	fn false_edge_of(&self, i: &Self::ActionRef) -> Self::CFEdgeRef;

	/// Get the Unconditional Edge
	fn next_edge_of(&self, i: &Self::ActionRef) -> Self::CFEdgeRef;

	/// Invalid Action
	// TODO: Remove this and use Option<> instead.
	fn invalid_edge(&self) -> Self::CFEdgeRef;

}

pub trait CFGMod: CFG {
	type BBInfo;

	/// Mark the start node for the SSA graph.
	fn mark_start_node(&mut self, start: &Self::ActionRef);
	
	/// Mark the exit node for the SSA graph.
	fn mark_exit_node(&mut self, exit: &Self::ActionRef);

	/// Add a new basic block.
	fn add_block(&mut self, info: Self::BBInfo) -> Self::ActionRef;

	/// Add a new exit
	fn add_dynamic(&mut self) -> Self::ActionRef;

	/// Add a control edge between to basic blocks.
	fn add_control_edge(&mut self, source: Self::ActionRef, target: Self::ActionRef, index: u8);

	/// Will remove a node and all its associated data from the graph
	fn remove_block(&mut self, node: Self::ActionRef);
}
