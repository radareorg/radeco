use petgraph::{EdgeDirection, Graph};
use petgraph::graph::{DefIndex, Edge, Node, NodeIndex, EdgeIndex};
use super::ir;
use std::mem;

pub struct SSA {
	pub g: Graph<NodeData, EdgeData>
}

pub enum NodeData {
	Op(ir::MOpcode),
	Const(u64),
	Phi
}

pub enum EdgeData {
	Control(u8),
	Data(u8)
}

impl SSA {
	pub fn replace(&mut self, pattern: NodeIndex, replacement: NodeIndex) {
		if pattern == replacement { return }
		unimplemented!();
	}

	pub fn args_of(&self, node: NodeIndex) -> Vec<NodeIndex> {
		unimplemented!();
		return Vec::new()
	}

	pub fn uses_of(&self, node: NodeIndex) -> Vec<NodeIndex> {
		unimplemented!();
		return Vec::new()
	}

	pub fn preds_of(&self, node: NodeIndex) -> Vec<NodeIndex> {
		unimplemented!();
		return Vec::new()
	}

	pub fn succs_of(&self, node: NodeIndex) -> Vec<NodeIndex> {
		unimplemented!();
		return Vec::new()
	}

	pub fn block_of(&self, node: NodeIndex) -> NodeIndex {
		unimplemented!();
		return node
	}

	pub fn phi_use(&mut self, phi: NodeIndex, node: NodeIndex) {
		assert!(if let NodeData::Phi = self.g[phi] { true } else { false });
		self.g.update_edge(phi, node, EdgeData::Data(0));
	}

	pub fn phi_unuse(&mut self, phi: NodeIndex, node: NodeIndex) {
		assert!(if let NodeData::Phi = self.g[phi] { true } else { false });
		if let Option::Some(edge) = self.g.find_edge(phi, node) {
			self.g.remove_edge(edge);
		}
	}
}
