//! Module that holds the struct and trait implementations for the ssa form.

use std::fmt::Debug;

use petgraph::EdgeDirection;
use petgraph::graph::{Graph, NodeIndex, EdgeIndex};
use middle::ir;

use super::ssa_traits;
use super::ssa_traits::NodeData as TNodeData;
use super::ssa_traits::NodeType as TNodeType;
use super::ssa_traits::{SSA, SSAMod, ValueType};
use super::cfg_traits::{CFG, CFGMod};

/// Node type for the SSAStorage-internal petgraph.
/// Both actions and values are represented using this same enum.
///
/// Value nodes are `Op`, `Phi`, `Comment`, `Undefined` and `Removed`.
/// Action nodes are `Unreachable`, `BasicBlock`, `DynamicAction`
/// `RegisterState` is neither.
/// Value nodes have a `ValueType` that can be extracted with `SSA::get_node_data`
#[derive(Clone, Debug)]
pub enum NodeData {
	/// Represents on operation.
	Op(ir::MOpcode, ValueType),
	/// Represents a phi node.
	Phi(ValueType, String),
	/// Represents an undefined node with a comment.
	Comment(ValueType, String),
	/// Represents an undefined node without comment.
	Undefined(ValueType),
	/// Placeholder for value nodes.
	Removed,
	/// Placeholder for action nodes.
	Unreachable,
	/// Represents a basic block.
	BasicBlock(ssa_traits::BBInfo),
	/// Represents an action that doesn't contain any value nodes, described by its associated RegisterState (which includes the instruction pointer).
	DynamicAction,
	/// Represents the state of the register file at the moment of entry into the associated action node.
	RegisterState,
}

/// Edge type for the SSAStorage-internal petgraph.
#[derive(Clone, Copy, Debug)]
pub enum EdgeData {
	/// Edge from action to action. Represents control flow. The number is used to distinguish true branch, false branch, etc.
	Control(u8),
	/// Edge from value or RegisterState to value. Represents data flow. The number describes the howmanyeth argument of the edge source is encoded by this edge.
	Data(u8),
	/// Edge from value to BasicBlock.
	ContainedInBB,
	/// Edge from BasicBlock to value. Points from a basic block with multiple successors to a value that decides which branch will be taken.
	Selector,
	/// Edge from action to RegisterState. Represents the values of all registers at entry to the action.
	RegisterState,
	/// Edge from Removed to value or from Unreachable to action. Created when calling replace() in stable indices mode.
	ReplacedBy,
}

const CONTEDGE: EdgeData = EdgeData::ContainedInBB;

/// A petgraph based SSA storage.
#[derive(Debug, Clone)]
pub struct SSAStorage {
	pub g: Graph<NodeData, EdgeData>,
	pub start_node: NodeIndex,
	pub exit_node: NodeIndex,
	pub stable_indexing: bool,
	needs_cleaning: bool,
}

impl SSAStorage {
	pub fn new() -> SSAStorage {
		SSAStorage {
			g: Graph::new(),
			needs_cleaning: false,
			stable_indexing: true,
			start_node: NodeIndex::end(),
			exit_node: NodeIndex::end(),
		}
	}

	/*pub fn add_phi_comment(&mut self, block: NodeIndex, comment: &String) -> NodeIndex {
		let n = self.g.add_node(NodeData::Phi(comment.clone()));
		self.g.update_edge(n, block, CONTEDGE);
		n
	}*/

	pub fn read_const(&self, ni: NodeIndex) -> Option<u64> {
		if let &NodeData::Op(ir::MOpcode::OpConst(n), _) = &self.g[ni] {
			Some(n)
		} else {
			None
		}
	}

	fn gather_adjacent(&self, node: NodeIndex, direction: EdgeDirection, data: bool) -> Vec<NodeIndex> {
		let mut adjacent = Vec::new();
		let mut walk = self.g.walk_edges_directed(node, direction);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if data {
				if let EdgeData::Data(_) = self.g[edge] {
					adjacent.push(othernode);
				}
			} else {
				if let EdgeData::Control(_) = self.g[edge] {
					adjacent.push(othernode);
				}
			}
		}
		adjacent
	}

	pub fn args_of_ordered(&self, node: NodeIndex) -> Vec<NodeIndex> {
		let ordered = if let NodeData::Phi(_, _) = self.g[node] { false } else { true };
		let mut args = Vec::new();
		if ordered {
			args.push(NodeIndex::end());
			args.push(NodeIndex::end());
		}

		let mut walk = self.g.walk_edges_directed(node, EdgeDirection::Outgoing);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::Data(index) = self.g[edge] {
				if ordered {
					args[index as usize] = othernode;
				} else {
					args.push(othernode);
				}
			}
		}
		let mut i = args.len();
		while i > 0 && args[i-1] == NodeIndex::end() {
			i -= 1;
		}
		args.truncate(i);
		args
	}

	pub fn replaced_by(&self, node: NodeIndex) -> NodeIndex {
		let mut walk = self.g.walk_edges_directed(node, EdgeDirection::Outgoing);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::ReplacedBy = self.g[edge] {
				return othernode
			}
		}
		NodeIndex::end()
	}

	pub fn is_block(&self, node: NodeIndex) -> bool {
		if let NodeData::BasicBlock(_) = self.g[node] { true } else { false }
	}

	pub fn is_action(&self, action: NodeIndex) -> bool {
		match self.g[action] {
			NodeData::BasicBlock(_) | NodeData::DynamicAction => true,
			_ => false
		}
	}

	pub fn remove_with_spacer(&mut self, node: NodeIndex, spacer: NodeData) {
		if self.stable_indexing {
			self.needs_cleaning = true;
			self.g.add_node(spacer);
		}
		self.g.remove_node(node);
	}
}

impl CFG for SSAStorage {
	type ActionRef = NodeIndex;
	type CFEdgeRef = EdgeIndex;

	fn blocks(&self) -> Vec<NodeIndex> {
		let len = self.g.node_count();
		let mut blocks = Vec::<NodeIndex>::new();
		for i in (0..len).map(|x| NodeIndex::new(x)).collect::<Vec<NodeIndex>>().iter() {
			match self.g[*i] {
				NodeData::BasicBlock(_) => blocks.push(i.clone()),
				_ => continue,
			}
		}
		return blocks;
	}

	fn start_node(&self) -> NodeIndex {
		assert!(self.start_node != NodeIndex::end());
		self.start_node
	}

	fn exit_node(&self) -> NodeIndex {
		assert!(self.exit_node != NodeIndex::end());
		self.exit_node
	}

	fn get_unconditional(&self, i: &Self::ActionRef) -> Self::ActionRef {
		let mut walk = self.g.walk_edges_directed(*i, EdgeDirection::Outgoing);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::Control(2) = self.g[edge] {
				return othernode;
			}
		}
		return NodeIndex::end()
	}

	fn preds_of(&self, node: NodeIndex) -> Vec<NodeIndex> {
		self.gather_adjacent(node, EdgeDirection::Incoming, false)
	}

	fn succs_of(&self, node: NodeIndex) -> Vec<NodeIndex> {
		self.gather_adjacent(node, EdgeDirection::Outgoing, false)
	}

	fn invalid_action(&self) -> NodeIndex { NodeIndex::end() }

	///////////////////////////////////////////////////////////////////////////
	//// Edge accessors and helpers
	///////////////////////////////////////////////////////////////////////////
	fn edges_of(&self, i: &NodeIndex) -> Vec<EdgeIndex> {
		let mut edges = Vec::<EdgeIndex>::new();
		let mut walk = self.g.walk_edges_directed(*i, EdgeDirection::Outgoing);
		while let Some((edge, _)) = walk.next_neighbor(&self.g) {
			if let EdgeData::Control(_) = self.g[edge] {
				edges.push(edge);
			}
		}
		return edges;
	}

	fn info(&self, i: &EdgeIndex) -> (NodeIndex, NodeIndex) {
		let edge_count = self.g.edge_count();
		assert!(i.index() < edge_count);
		let edges = self.g.raw_edges();
		let edge_data = edges[i.index()].clone();
		return (edge_data.source(), edge_data.target());
	}

	fn find_edge(&self, source: &NodeIndex, target: &NodeIndex) -> EdgeIndex {
		self.g.find_edge(*source, *target).unwrap_or(EdgeIndex::end())
	}

	fn true_edge_of(&self, i: &NodeIndex) -> EdgeIndex {
		let edges = self.edges_of(i);
		for edge in edges.iter() {
			if let EdgeData::Control(1) = self.g[*edge] {
				return *edge;
			}
		}
		return EdgeIndex::end();
	}

	fn false_edge_of(&self, i: &NodeIndex) -> EdgeIndex {
		let edges = self.edges_of(i);
		for edge in edges.iter() {
			if let EdgeData::Control(0) = self.g[*edge] {
				return *edge;
			}
		}
		return EdgeIndex::end();
	}

	// TODO: Optimize and add asserts
	fn next_edge_of(&self, i: &NodeIndex) -> EdgeIndex {
		let edges = self.edges_of(i);
		for edge in edges.iter() {
			if let EdgeData::Control(2) = self.g[*edge] {
				return *edge;
			}
		}
		return EdgeIndex::end();
	}

	fn incoming_edges(&self, i: &NodeIndex) -> Vec<EdgeIndex> {
		let mut edges = Vec::<EdgeIndex>::new();
		let mut walk = self.g.walk_edges_directed(*i, EdgeDirection::Incoming);
		while let Some((edge, _)) = walk.next_neighbor(&self.g) {
			if let EdgeData::Control(_) = self.g[edge] {
				edges.push(edge);
			}
		}
		return edges;
	}

	fn invalid_edge(&self) -> EdgeIndex {
		EdgeIndex::end()
	}
}

impl CFGMod for SSAStorage {

	type BBInfo = ssa_traits::BBInfo;

	fn mark_start_node(&mut self, start: &Self::ActionRef) {
		self.start_node = *start;
	}

	fn mark_exit_node(&mut self, exit: &Self::ActionRef) {
		self.exit_node = *exit;
	}

	fn add_block(&mut self, info: Self::BBInfo) -> NodeIndex {
		let bb = self.g.add_node(NodeData::BasicBlock(info));
		let rs = self.g.add_node(NodeData::RegisterState);
		self.g.add_edge(bb, rs, EdgeData::RegisterState);
		self.g.add_edge(rs, bb, CONTEDGE);
		bb
	}

	fn add_dynamic(&mut self) -> NodeIndex {
		let a = self.g.add_node(NodeData::DynamicAction);
		let rs = self.g.add_node(NodeData::RegisterState);
		self.g.add_edge(a, rs, EdgeData::RegisterState);
		self.g.add_edge(rs, a, CONTEDGE);
		a
	}

	fn add_control_edge(&mut self, source: Self::ActionRef, target: Self::ActionRef, index: u8) {
		self.g.add_edge(source, target, EdgeData::Control(index));
	}

	fn remove_block(&mut self, node: Self::ActionRef) {
		assert!(self.is_block(node));
		assert!(self.stable_indexing);

		let regstate = self.registers_at(node);
		self.remove(regstate);

		let mut expressions = Vec::<NodeIndex>::new();
		let mut walk = self.g.walk_edges_directed(node, EdgeDirection::Incoming);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::ContainedInBB = self.g[edge] {
				expressions.push(othernode);
			}
		}

		for expr in expressions {
			self.remove(expr);
		}

		let preds = self.preds_of(node);
		self.remove_with_spacer(node, NodeData::Unreachable);

		// block removal can make predecessors lose selectors
		for pred in preds {
			if self.succs_of(pred).len() == 1 {
				let mut walk = self.g.walk_edges_directed(pred, EdgeDirection::Incoming);
				let mut seledge = None;
				while let Some((edge, _)) = walk.next_neighbor(&self.g) {
					if let EdgeData::Selector = self.g[edge] {
						seledge = Some(edge);
						break;
					}
				}
				if let Some(edge) = seledge {
					self.g.remove_edge(edge);
				}
			}
		}
	}

}

///////////////////////////////////////////////////////////////////////////////
//// Implementation of SSA for SSAStorage.
///////////////////////////////////////////////////////////////////////////////

impl SSA for SSAStorage {
	type ValueRef = NodeIndex;
	fn exprs_in(&self, i: &NodeIndex) -> Vec<NodeIndex> {
		let mut expressions = Vec::<NodeIndex>::new();
		let mut walk = self.g.walk_edges_directed(*i, EdgeDirection::Incoming);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::ContainedInBB = self.g[edge] {
				match self.g[othernode] {
					NodeData::Op(_, _) => expressions.push(othernode.clone()),
					_ => continue,
				}
			}
		}
		return expressions;
	}

	fn is_expr(&self, i: &NodeIndex) -> bool {
		match self.g[*i] {
			NodeData::Op(_, _) => true,
			_ => false,
		}
	}

	fn get_phis(&self, i: &NodeIndex) -> Vec<NodeIndex> {
		let mut phis = Vec::<NodeIndex>::new();
		let mut walk = self.g.walk_edges_directed(*i, EdgeDirection::Incoming);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::ContainedInBB = self.g[edge] {
				match self.g[othernode] {
					NodeData::Phi(_, _) => phis.push(othernode.clone()),
					_ => continue,
				}
			}
		}
		return phis;
	}

	fn get_uses(&self, i: &NodeIndex) -> Vec<NodeIndex> {
		let mut uses = Vec::new();
		let mut walk = self.g.walk_edges_directed(*i, EdgeDirection::Incoming);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::Data(_) = self.g[edge] {
				uses.push(othernode);
			}
		}
		return uses;
	}

	fn get_block(&self, i: &NodeIndex) -> NodeIndex {
		let ic = self.refresh(i.clone());
		let mut walk = self.g.walk_edges_directed(ic, EdgeDirection::Outgoing);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::ContainedInBB = self.g[edge] {
				return othernode
			}
		}
		return NodeIndex::end()
	}

	fn selects_for(&self, i: &NodeIndex) -> NodeIndex {
		// Make sure that the given node is actually a selector of some block.
		assert!(self.is_selector(i));
		let mut walk = self.g.walk_edges_directed(*i, EdgeDirection::Incoming);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::Selector = self.g[edge] {
				return othernode;
			}
		}
		return NodeIndex::end();
	}

	fn get_branches(&self, i: &NodeIndex) -> (NodeIndex, NodeIndex) {
		let selects_for = self.selects_for(i);

		// Make sure that we have a block for the selector.
		assert!(selects_for != NodeIndex::end());

		let mut true_branch = NodeIndex::end();
		let mut false_branch = NodeIndex::end();
		let mut walk = self.g.walk_edges_directed(selects_for, EdgeDirection::Outgoing);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::Control(0) = self.g[edge] {
				false_branch = othernode.clone();
			} else if let EdgeData::Control(1) = self.g[edge] {
				true_branch = othernode.clone();
			}
		}
		(false_branch, true_branch)
	}

	fn get_true_branch(&self, i: &NodeIndex) -> NodeIndex {
		self.get_branches(i).1
	}

	fn get_false_branch(&self, i: &NodeIndex) -> NodeIndex {
		self.get_branches(i).0
	}

	fn registers_at(&self, i: NodeIndex) -> NodeIndex {
		assert!(self.is_action(i));
		let mut walk = self.g.walk_edges_directed(i, EdgeDirection::Outgoing);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::RegisterState = self.g[edge] {
				return othernode
			}
		}
		NodeIndex::end()
	}

	fn get_operands(&self, i: &NodeIndex) -> Vec<NodeIndex> {
		let mut args = Vec::new();
		let mut walk = self.g.walk_edges_directed(*i, EdgeDirection::Outgoing);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::Data(index) = self.g[edge] {
				args.push((index, othernode));
			}
		}
		args.sort_by(|a, b| a.0.cmp(&b.0));
		args.iter().map(|a| a.1).collect()
	}

	fn get_sparse_operands(&self, i: &NodeIndex) -> Vec<(u8, NodeIndex)> {
		let mut args = Vec::new();

		let mut walk = self.g.walk_edges_directed(*i, EdgeDirection::Outgoing);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::Data(index) = self.g[edge] {
				args.push((index, othernode));
			}
		}
		return args;
	}

	fn get_node_data(&self, i: &NodeIndex) -> Result<TNodeData, Box<Debug>> {
		let ic = self.refresh(i.clone());
		if ic == NodeIndex::end() {
			return Err(Box::new(""))
		}
		match self.g[ic] {
			NodeData::Op(opc, vt)    => Ok(TNodeData {vt: vt, nt: TNodeType::Op(opc)}),
			NodeData::Phi(vt, _)     => Ok(TNodeData {vt: vt, nt: TNodeType::Phi}),
			NodeData::Comment(vt, _) => Ok(TNodeData {vt: vt, nt: TNodeType::Undefined}),
			NodeData::Undefined(vt)  => Ok(TNodeData {vt: vt, nt: TNodeType::Undefined}),
			NodeData::Removed        |
			NodeData::Unreachable    |
			NodeData::BasicBlock(_)  |
			NodeData::DynamicAction  |
			NodeData::RegisterState  => Err(Box::new(self.g[ic].clone())),
		}
	}

	fn is_selector(&self, i: &Self::ValueRef) -> bool {
		let mut walk = self.g.walk_edges_directed(*i, EdgeDirection::Incoming);
		while let Some((edge, _)) = walk.next_neighbor(&self.g) {
			if let EdgeData::Selector = self.g[edge] {
				return true;
			}
		}
		return false;
	}


	fn selector_of(&self, i: &Self::ActionRef) -> Option<Self::ValueRef> {
		let mut walk = self.g.walk_edges_directed(*i, EdgeDirection::Outgoing);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::Selector = self.g[edge] {
				return Some(othernode);
			}
		}
		// TODO: Something wrong here!
		return None;
	}

	fn get_target(&self, i: &NodeIndex) -> NodeIndex {
		let cur_block = self.get_block(i);
		let mut walk = self.g.walk_edges_directed(cur_block, EdgeDirection::Outgoing);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::Control(_) = self.g[edge] {
				return othernode;
			}
		}
		return NodeIndex::end()
	}

	fn args_of(&self, node: NodeIndex) -> Vec<NodeIndex> {
		self.gather_adjacent(node, EdgeDirection::Outgoing, true)
	}

	fn uses_of(&self, node: NodeIndex) -> Vec<NodeIndex> {
		self.gather_adjacent(node, EdgeDirection::Incoming, true)
	}

	fn refresh(&self, mut node: NodeIndex) -> NodeIndex {
		if node == NodeIndex::end() { return node }
		while let NodeData::Removed = self.g[node] {
			node = self.replaced_by(node);
			if node == NodeIndex::end() { break; }
		}
		node
	}

	fn invalid_value(&self) -> NodeIndex { NodeIndex::end() }
	fn to_value(&self, n: NodeIndex) -> NodeIndex { n }
	fn to_action(&self, n: NodeIndex) -> NodeIndex { n }

	fn node_count(&self) -> usize {
		self.g.node_count()
	}

	fn edge_count(&self) -> usize {
		self.g.edge_count()
	}
}

impl SSAMod for SSAStorage {

	fn add_op(&mut self, block: NodeIndex, opc: ir::MOpcode, vt: ValueType) -> NodeIndex {
		let n = self.g.add_node(NodeData::Op(opc, vt));
		self.g.update_edge(n, block, CONTEDGE);
		n
	}

	fn add_const(&mut self, block: NodeIndex, value: u64) -> NodeIndex {
		// TODO:
		//  - Set correct size for the data.
		//  - Constants need/should not belong to any block.
		let data = NodeData::Op(ir::MOpcode::OpConst(value),
		                        ValueType::Integer { width: 64 });
		let n = self.g.add_node(data);
		self.g.update_edge(n, block, CONTEDGE);
		n
	}

	fn add_phi(&mut self, block: NodeIndex, vt: ValueType) -> NodeIndex {
		let n = self.g.add_node(NodeData::Phi(vt, "".to_string()));
		self.g.update_edge(n, block, CONTEDGE);
		n
	}

	fn add_undefined(&mut self, block: NodeIndex, vt: ValueType) -> NodeIndex {
		let n = self.g.add_node(NodeData::Undefined(vt));
		self.g.update_edge(n, block, CONTEDGE);
		n
	}

	fn add_comment(&mut self, block: NodeIndex, vt: ValueType, msg: String) -> NodeIndex {
		let n = self.g.add_node(NodeData::Comment(vt, msg));
		self.g.update_edge(n, block, CONTEDGE);
		n
	}

	fn mark_selector(&mut self, node: Self::ValueRef, block: Self::ActionRef) {
		self.g.add_edge(block, node, EdgeData::Selector);
	}

	fn phi_use(&mut self, mut phi: NodeIndex, mut node: NodeIndex) {
		phi = self.refresh(phi);
		node = self.refresh(node);
		assert!(if let NodeData::Phi(_, _) = self.g[phi] { true } else { false });
		//assert!(if let NodeData::Removed = self.g[node] { false } else { true });
		self.g.update_edge(phi, node, EdgeData::Data(0));
	}

	fn phi_unuse(&mut self, mut phi: NodeIndex, mut node: NodeIndex) {
		phi = self.refresh(phi);
		node = self.refresh(node);
		assert!(if let NodeData::Phi(_, _) = self.g[phi] { true } else { false });
		if let Option::Some(edge) = self.g.find_edge(phi, node) {
			self.g.remove_edge(edge);
		}
	}

	fn op_use(&mut self, mut node: NodeIndex, index: u8, mut argument: NodeIndex) {
		node = self.refresh(node);
		argument = self.refresh(argument);
		if argument == NodeIndex::end() { return }
		// assert!(if let NodeData::Removed = self.g[argument] { false } else { true });
		// TODO: find existing edge
		self.g.add_edge(node, argument, EdgeData::Data(index));
	}

	fn replace(&mut self, mut node: NodeIndex, mut replacement: NodeIndex) {
		assert!(self.get_block(&node) != NodeIndex::end());
		assert!(self.get_block(&replacement) != NodeIndex::end());
		node = self.refresh(node);
		replacement = self.refresh(replacement);

		if node == replacement { return }

		let mut walk = self.g.walk_edges_directed(node, EdgeDirection::Incoming);
		let mut remove_us = Vec::<EdgeIndex>::new();
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			match self.g[edge] {
				EdgeData::Data(i) => {
					match self.g[othernode] {
						NodeData::Op(_, _) | NodeData::RegisterState => self.op_use(othernode, i, replacement),
						NodeData::Phi(_, _) => self.phi_use(othernode, replacement),
						_ => panic!()
					}
				},
				EdgeData::ReplacedBy => {
					self.g.add_edge(othernode, replacement, EdgeData::ReplacedBy);
				},
				_ => ()
			}
			remove_us.push(edge);
		}
		if self.start_node == node {
			self.start_node = replacement;
		}
		self.remove(node);
		if self.stable_indexing {
			self.g.add_edge(node, replacement, EdgeData::ReplacedBy);
		}
	}

	fn remove(&mut self, node: NodeIndex) {
		self.remove_with_spacer(node, NodeData::Removed);
	}

	fn cleanup(&mut self) {
		// TODO: Add api to toggle stable_indexing to SSA trait
		// assert!(!self.stable_indexing);
		let mut i = 0;
		let mut n = self.g.node_count();
		while i < n {
			if {
				let ni = NodeIndex::new(i);
				match self.g[ni] {
					NodeData::Removed => true,
					NodeData::Comment(_, _) if self.g.first_edge(ni, EdgeDirection::Incoming) == Option::None => true,
					_ => false
				}
			} {
				self.g.remove_node(NodeIndex::new(i));
				n -= 1;
			} else {
				i += 1;
			}
		}
		self.needs_cleaning = false;
	}

	fn remove_edge(&mut self, i: &Self::CFEdgeRef) {
		let edge_data = self.g[*i];
		let src_node = self.source_of(i);
		if let EdgeData::Control(2) = edge_data {
			self.g.remove_edge(*i);
			return;
		}
		// Removing a true/false edge.
		let selector = self.selector_of(&src_node);
		if selector.is_some() {
			self.remove(selector.unwrap());
		}

		let other_edge = match edge_data {
			EdgeData::Control(0) => self.true_edge_of(&src_node),
			EdgeData::Control(1) => self.false_edge_of(&src_node),
			_ => panic!("Found something other than a control flow edge!"),
		};

		{
			let wt = self.g.edge_weight_mut(other_edge).unwrap();
			*wt = EdgeData::Control(2);
		}

		self.g.remove_edge(*i);
	}
}
