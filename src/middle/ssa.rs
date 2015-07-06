use std::mem;
use petgraph::{EdgeDirection, Graph};
use petgraph::graph::{DefIndex, Edge, Node, NodeIndex, EdgeIndex};
use super::ir;
use super::dot::{GraphDot, EdgeInfo, Label};

pub struct SSA {
	pub g: Graph<NodeData, EdgeData>
}

#[derive(Clone, Copy, Debug)]
pub enum NodeData {
	Op(ir::MOpcode),
	Const(u64),
	Phi
}

#[derive(Clone, Copy)]
pub enum EdgeData {
	Control(u8),
	Data(u8)
}

impl SSA {
	pub fn new() -> SSA {
		SSA {g: Graph::new() }
	}
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

impl GraphDot for SSA {
    type NodeType = NodeData;
    type EdgeType = Edge<EdgeData>;

    fn configure(&self) -> String {
        format!("digraph cfg {{\nsplines=\"true\";\n")
    }

    fn nodes(&self) -> Vec<Self::NodeType> {
        let res = self.g.raw_nodes().iter().map(|e| e.weight.clone()).collect();
        res
    }

    fn edges(&self) -> Vec<Self::EdgeType> {
        let res = self.g.raw_edges().to_vec();
        res
    }

    fn get_node(&self, n: usize) -> Option<&Self::NodeType> {
        self.g.node_weight(NodeIndex::new(n))
    }
}

impl EdgeInfo for Edge<EdgeData> {
    fn source(&self) -> usize {
        self.source().index()
    }

    fn target(&self) -> usize {
        self.target().index()
    }
}

impl Label for Edge<EdgeData> {
    fn label(&self) -> String {
        ";\n".to_string()
    }

    fn name(&self) -> Option<String> {
        None
    }
}

impl Label for NodeData {
    fn label(&self) -> String {
        format!("{} [label={:?}];\n", self.name().unwrap(), self)
    }

    fn name(&self) -> Option<String> {
        Some(format!("n{:p}", self))
    }
}
