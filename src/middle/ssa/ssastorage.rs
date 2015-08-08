use petgraph::EdgeDirection;
use petgraph::graph::{Graph, NodeIndex, EdgeIndex};
use middle::ir;

use super::ssa_traits;
use super::ssa_traits::NodeData as TNodeData;
use super::ssa_traits::{SSA, SSAMod, ValueType};
use super::cfg_traits::{CFG, CFGMod};

#[derive(Clone, Debug)]
pub enum NodeData {
	Op(ir::MOpcode, ValueType),
	Const(u64),
	Phi(String),
	Comment(String),
	Undefined,
	Removed,
	BasicBlock(ssa_traits::BBInfo),
	RegisterState,
}

#[derive(Clone, Copy)]
pub enum EdgeData {
	Control(u8),
	Data(u8),
	ContainedInBB,
	Selector,
	RegisterState,
	ReplacedBy,
}

const CONTEDGE: EdgeData = EdgeData::ContainedInBB;

#[derive(Clone)]
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

	pub fn add_phi_comment(&mut self, block: NodeIndex, comment: &String) -> NodeIndex {
		let n = self.g.add_node(NodeData::Phi(comment.clone()));
		self.g.update_edge(n, block, CONTEDGE);
		n
	}

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
		let ordered = if let NodeData::Phi(_) = self.g[node] { false } else { true };
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
		panic!();
		//return NodeIndex::end()
	}
}

impl CFG for SSAStorage {
	type ActionRef = NodeIndex;

	fn get_blocks(&self) -> Vec<NodeIndex> {
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
		let cur_block = self.get_block(i);
		let mut walk = self.g.walk_edges_directed(cur_block, EdgeDirection::Outgoing);
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

	fn add_control_edge(&mut self, source: Self::ActionRef, target: Self::ActionRef, index: u8) {
		self.g.add_edge(source, target, EdgeData::Control(index));
	}

	fn remove_block(&mut self, node: Self::ActionRef) {
		let outer_stable_indexing = self.stable_indexing;
		self.stable_indexing = true;
		{
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
		}
		self.stable_indexing = outer_stable_indexing;
	}
}

///////////////////////////////////////////////////////////////////////////////
//// Implementation of SSA for SSAStorage.
///////////////////////////////////////////////////////////////////////////////

impl SSA for SSAStorage {
	type ValueRef = NodeIndex;

	fn get_exprs(&self, i: &NodeIndex) -> Vec<NodeIndex> {
		let mut expressions = Vec::<NodeIndex>::new();
		let mut walk = self.g.walk_edges_directed(*i, EdgeDirection::Incoming);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::ContainedInBB = self.g[edge] {
				match self.g[othernode] {
					NodeData::Op(_, _)
						| NodeData::Const(_) => expressions.push(othernode.clone()),
						_ => continue,
				}
			}
		}
		return expressions;
	}

	fn get_phis(&self, i: &NodeIndex) -> Vec<NodeIndex> {
		let mut phis = Vec::<NodeIndex>::new();
		let mut walk = self.g.walk_edges_directed(*i, EdgeDirection::Incoming);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::ContainedInBB = self.g[edge] {
				match self.g[othernode] {
					NodeData::Phi(_) => phis.push(othernode.clone()),
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

	fn get_true_branch(&self, i: &NodeIndex) -> NodeIndex {
		let cur_block = self.get_block(i);
		let mut walk = self.g.walk_edges_directed(cur_block, EdgeDirection::Outgoing);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::Control(1) = self.g[edge] {
				return othernode;
			}
		}
		return NodeIndex::end()
	}

	fn get_false_branch(&self, i: &NodeIndex) -> NodeIndex {
		let cur_block = self.get_block(i);
		let mut walk = self.g.walk_edges_directed(cur_block, EdgeDirection::Outgoing);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::Control(0) = self.g[edge] {
				return othernode;
			}
		}
		return NodeIndex::end()
	}

	fn registers_at(&self, i: NodeIndex) -> NodeIndex {
		let mut walk = self.g.walk_edges_directed(i, EdgeDirection::Outgoing);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::RegisterState = self.g[edge] {
				return othernode
			}
		}
		return NodeIndex::end()
	}

	fn get_operands(&self, i: &NodeIndex) -> Vec<NodeIndex> {
		let ordered = if let NodeData::Phi(_) = self.g[*i] { false } else { true };
		let mut args = Vec::new();
		if ordered {
			args.push(NodeIndex::end());
			args.push(NodeIndex::end());
		}

		let mut walk = self.g.walk_edges_directed(*i, EdgeDirection::Outgoing);
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
		return args;
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

	fn get_node_data(&self, i: &NodeIndex) -> TNodeData {
		let ic = self.refresh(i.clone());
		match self.g[ic] {
			NodeData::Op(opc, vt)   => TNodeData::Op(opc, vt),
			NodeData::Const(num)    => TNodeData::Const(num),
			NodeData::Phi(_)        => TNodeData::Phi,
			NodeData::Comment(_)    => TNodeData::Undefined,
			NodeData::Undefined     => TNodeData::Undefined,
			NodeData::Removed       => TNodeData::Invalid,
			NodeData::BasicBlock(_) => TNodeData::Invalid,
			NodeData::RegisterState => TNodeData::Invalid,
		}
	}

	fn is_selector(&self, i:&Self::ValueRef) -> bool {
		let mut walk = self.g.walk_edges_directed(*i, EdgeDirection::Incoming);
		while let Some((edge, _)) = walk.next_neighbor(&self.g) {
			if let EdgeData::Selector = self.g[edge] {
				return true;
			}
		}
		return false;
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
		}
		node
	}

	fn invalid_value(&self) -> NodeIndex { NodeIndex::end() }
	fn to_value(&self, n: NodeIndex) -> NodeIndex { n }
	fn to_action(&self, n: NodeIndex) -> NodeIndex { n }

	fn node_count(&self) -> usize {
		self.g.node_count()
	}
}

impl SSAMod for SSAStorage {

	fn add_op(&mut self, block: NodeIndex, opc: ir::MOpcode, vt: ValueType) -> NodeIndex {
		let n = self.g.add_node(NodeData::Op(opc, vt));
		self.g.update_edge(n, block, CONTEDGE);
		n
	}

	fn add_const(&mut self, block: NodeIndex, value: u64) -> NodeIndex {
		let n = self.g.add_node(NodeData::Const(value));
		self.g.update_edge(n, block, CONTEDGE);
		n
	}

	fn add_phi(&mut self, block: NodeIndex) -> NodeIndex {
		let n = self.g.add_node(NodeData::Phi("".to_string()));
		self.g.update_edge(n, block, CONTEDGE);
		n
	}

	fn add_undefined(&mut self, block: NodeIndex) -> NodeIndex {
		let n = self.g.add_node(NodeData::Undefined);
		self.g.update_edge(n, block, CONTEDGE);
		n
	}

	fn add_comment(&mut self, block: NodeIndex, msg: String) -> NodeIndex {
		let n = self.g.add_node(NodeData::Comment(msg));
		self.g.update_edge(n, block, CONTEDGE);
		n
	}

	fn mark_selector(&mut self, node: Self::ValueRef, block: Self::ActionRef) {
		self.g.add_edge(block, node, EdgeData::Selector);
	}

	fn phi_use(&mut self, mut phi: NodeIndex, mut node: NodeIndex) {
		phi = self.refresh(phi);
		node = self.refresh(node);
		assert!(if let NodeData::Phi(_) = self.g[phi] { true } else { false });
		//assert!(if let NodeData::Removed = self.g[node] { false } else { true });
		self.g.update_edge(phi, node, EdgeData::Data(0));
	}

	fn phi_unuse(&mut self, mut phi: NodeIndex, mut node: NodeIndex) {
		phi = self.refresh(phi);
		node = self.refresh(node);
		assert!(if let NodeData::Phi(_) = self.g[phi] { true } else { false });
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
						NodeData::Phi(_) => self.phi_use(othernode, replacement),
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
		self.remove(node);
		if self.stable_indexing {
			self.g.add_edge(node, replacement, EdgeData::ReplacedBy);
		}
	}

	fn remove(&mut self, node: NodeIndex) {
		if self.stable_indexing {
			self.needs_cleaning = true;
			self.g.add_node(NodeData::Removed);
		}
		self.g.remove_node(node);
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
					NodeData::Comment(_) if self.g.first_edge(ni, EdgeDirection::Incoming) == Option::None => true,
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
}
