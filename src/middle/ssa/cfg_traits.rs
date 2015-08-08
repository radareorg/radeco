use std::hash::Hash;
use std::fmt::Debug;

pub trait CFG {
	type ActionRef: Eq + Hash + Clone + Copy + Debug;

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
}

pub trait CFGMod: CFG {
	type BBInfo;

	/// Mark the start node for the SSA graph.
	fn mark_start_node(&mut self, start: &Self::ActionRef);
	
	/// Mark the exit node for the SSA graph.
	fn mark_exit_node(&mut self, exit: &Self::ActionRef);

	/// Add a new basic block.
	fn add_block(&mut self, info: Self::BBInfo) -> Self::ActionRef;

	/// Add a control edge between to basic blocks.
	fn add_control_edge(&mut self, source: Self::ActionRef, target: Self::ActionRef, index: u8);
}
