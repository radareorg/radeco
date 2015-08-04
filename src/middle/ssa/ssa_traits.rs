//! Module that holds the struct and trait implementations for the ssa form.

use std::hash::Hash;
use middle::ir;
use std::fmt::Debug;

#[derive(Clone, Copy, Debug)]
pub enum ValueType {
	Integer {width: ir::WidthSpec},
	//MachineState,
}

#[derive(Clone, Debug)]
pub struct BBInfo {
	pub addr: u64
}

// TODO: Hide implementation details (like 'Removed' and 'BasicBlock')
#[derive(Clone, Debug)]
pub enum NodeData {
	Op(ir::MOpcode, ValueType),
	Comment(String),
	Const(u64),
	Phi(String),
	Undefined,
	Removed,
	BasicBlock(BBInfo),
	RegisterState,
}

/// Trait for the SSA Form implementation.
// This trait ensures that any other ssa form will be compatible with our implementations provided
// the SSA form implements the following traits.
pub trait SSA {
	type ValueRef: Eq + Hash + Clone + Copy + Debug; // We could drop the Copy trait later and insert .clone()
	type ActionRef: Eq + Hash + Clone + Copy + Debug;

	/// Get NodeIndex of all BasicBlocks available in the SSA form.
	fn get_blocks(&self) -> Vec<Self::ActionRef>;

	/// Start node of the CFG.
	fn start_node(&self) -> Self::ActionRef;

	/// Get all the NodeIndex of all operations/expressions in the BasicBlock with index 'i'.
	fn get_exprs(&self, i: &Self::ActionRef) -> Vec<Self::ValueRef>;

	/// Get all phis in the BasicBlock with index 'i'.
	fn get_phis(&self, i: &Self::ActionRef) -> Vec<Self::ValueRef>;

	/// Get all the uses of the node with index 'i'.
	fn get_uses(&self, i: &Self::ValueRef) -> Vec<Self::ValueRef>;

	/// Get the NodeIndex of the BasicBlock to which node with index 'i' belongs to.
	fn get_block(&self, i: &Self::ValueRef) -> Self::ActionRef;

	/// Get the operands for the operation with NodeIndex 'i'.
	fn get_operands(&self, i: &Self::ValueRef) -> Vec<Self::ValueRef>;

	/// Get the operands for the operation with NodeIndex 'i' as tuples.
	fn get_sparse_operands(&self, i: &Self::ValueRef) -> Vec<(u8, Self::ValueRef)>;

	/// Get the lhs() of the Operation with NodeIndex 'i'.
	fn lhs(&self, i: &Self::ValueRef) -> Self::ValueRef {
		self.get_operands(i)[0].clone()
	}

	/// Get the rhs() of the Operation with NodeIndex 'i'.
	fn rhs(&self, i: &Self::ValueRef) -> Self::ValueRef {
		self.get_operands(i)[1].clone()
	}

	/// Get the actual NodeData.
	// TODO: Merge the below two functions. get_node_data should always return an Option and not
	// panic.
	fn get_node_data(&self, i: &Self::ValueRef) -> NodeData;
	fn safe_get_node_data(&self, i: &Self::ValueRef) -> Option<NodeData> {
		if *i != self.invalid_value() {
			Some(self.get_node_data(i))
		} else {
			None
		}
	}

	// NOTE:
	// These three functions will change their signatures
	// when we remove "jmp" from the list of ops

	/// Get Jump target of a call or an unconditional jump.
	fn get_target(&self, i: &Self::ValueRef) -> Self::ActionRef;

	/// Get true branch of a conditional jump.
	fn get_true_branch(&self, i: &Self::ValueRef) -> Self::ActionRef;

	/// Get false branch of a conditional jump.
	fn get_false_branch(&self, i: &Self::ValueRef) -> Self::ActionRef;

	/// Gets the data dependencies of a value node in any order.
	/// (See get_operands for ordered return value)
	fn args_of(&self, node: Self::ValueRef) -> Vec<Self::ValueRef>;

	/// Gets uses dependents of a value node.
	/// (Equivalent as `SSAMod::get_uses`)
	fn uses_of(&self, node: Self::ValueRef) -> Vec<Self::ValueRef>;

	/// Get the predecessors of a basic block.
	fn preds_of(&self, node: Self::ActionRef) -> Vec<Self::ActionRef>;

	/// Get the successors of a basic block.
	fn succs_of(&self, node: Self::ActionRef) -> Vec<Self::ActionRef>;

	/// Get the NodeIndex of the BasicBlock to which node with index 'i' belongs to.
	/// (Alias for get_block)
	fn block_of(&self, i: &Self::ValueRef) -> Self::ActionRef { self.get_block(i) }

	/// Get a node that has all register values at the beginning of the specified basic block as args
	fn registers_at(&self, Self::ActionRef) -> Self::ValueRef;

	/// Updates a node reference to the latest version in case of replacement
	// TODO: Hide this implementation detail
	fn refresh(&self, node: Self::ValueRef) -> Self::ValueRef;

	fn invalid_value(&self) -> Self::ValueRef;
	fn invalid_action(&self) -> Self::ActionRef;

	fn to_value(&self, Self::ActionRef) -> Self::ValueRef;
	fn to_action(&self, Self::ValueRef) -> Self::ActionRef;
}

/// Trait for modifying SSA data
pub trait SSAMod: SSA {

	type BBInfo;

	/// Mark the start node for the SSA graph.
	fn mark_start_node(&mut self, start: &Self::ActionRef);

	/// Add a new operation node.
	fn add_op(&mut self, block: Self::ActionRef, opc: ir::MOpcode, vt: ValueType) -> Self::ValueRef;

	/// Add a new constant node.
	fn add_const(&mut self, block: Self::ActionRef, value: u64) -> Self::ValueRef;

	/// Add a new phi node.
	fn add_phi(&mut self, block: Self::ActionRef) -> Self::ValueRef;

	/// Add a new undefined node
	fn add_undefined(&mut self, block: Self::ActionRef) -> Self::ValueRef;

	/// Add a new basic block.
	fn add_block(&mut self, info: Self::BBInfo) -> Self::ActionRef;

	/// Add a control edge between to basic blocks.
	fn add_control_edge(&mut self, source: Self::ActionRef, target: Self::ActionRef, index: u8);

	/// Mark the node as selector for the control edges away from the specified basic block
	fn mark_selector(&mut self, node: Self::ValueRef, block: Self::ActionRef);

	/// Add a data source to a phi node.
	fn phi_use(&mut self, phi: Self::ValueRef, node: Self::ValueRef);

	/// Remove a data source from a phi node.
	fn phi_unuse(&mut self, phi: Self::ValueRef, node: Self::ValueRef);

	/// Set the index-th argument of the node.
	fn op_use(&mut self, node: Self::ValueRef, index: u8, argument: Self::ValueRef);

	/// Replace one node by another within one basic block.
	fn replace(&mut self, node: Self::ValueRef, replacement: Self::ValueRef);

	/// Perform a cleanup. (Will invalidate indices)
	fn cleanup(&mut self);
}
