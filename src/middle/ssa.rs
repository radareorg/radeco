use petgraph::{EdgeDirection, Graph};
use petgraph::graph::{Edge, NodeIndex, EdgeIndex};
use super::ir;
use super::dot::{GraphDot, EdgeInfo, Label};

pub struct SSA {
	pub g: Graph<NodeData, EdgeData>
}

#[derive(Clone, Debug)]
pub enum NodeData {
	Op(ir::MOpcode),
	Comment(String),
	Const(u64),
	Phi(String),
	Undefined,
	Removed,
	BasicBlock
}

#[derive(Clone, Copy)]
pub enum EdgeData {
	Control(u8),
	Data(u8),
	ContainedInBB,
	ReplacedBy
}

impl SSA {
	pub fn new() -> SSA {
		SSA {g: Graph::new() }
	}

	pub fn add_op(&mut self, block: NodeIndex, opc: ir::MOpcode) -> NodeIndex {
		let n = self.g.add_node(NodeData::Op(opc));
		self.g.update_edge(n, block, EdgeData::ContainedInBB);
		n
	}

	pub fn add_comment(&mut self, block: NodeIndex, msg: &String) -> NodeIndex {
		let n = self.g.add_node(NodeData::Comment(msg.clone()));
		self.g.update_edge(n, block, EdgeData::ContainedInBB);
		n
	}

	pub fn add_const(&mut self, block: NodeIndex, value: u64) -> NodeIndex {
		let n = self.g.add_node(NodeData::Const(value));
		self.g.update_edge(n, block, EdgeData::ContainedInBB);
		n
	}

	pub fn add_phi(&mut self, block: NodeIndex) -> NodeIndex {
		let n = self.g.add_node(NodeData::Phi("".to_string()));
		self.g.update_edge(n, block, EdgeData::ContainedInBB);
		n
	}

	pub fn add_phi_comment(&mut self, block: NodeIndex, comment: &String) -> NodeIndex {
		let n = self.g.add_node(NodeData::Phi(comment.clone()));
		self.g.update_edge(n, block, EdgeData::ContainedInBB);
		n
	}

	pub fn add_block(&mut self) -> NodeIndex {
		self.g.add_node(NodeData::BasicBlock)
	}

	// TODO: Reuse code between args_of/uses_of/preds_of/succs_of

	pub fn args_of(&self, node: NodeIndex) -> Vec<NodeIndex> {
		let mut args = Vec::new();
		let mut walk = self.g.walk_edges_directed(node, EdgeDirection::Outgoing);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::Data(_) = self.g[edge] {
				args.push(othernode);
			}
		}
		args
	}

	pub fn uses_of(&self, node: NodeIndex) -> Vec<NodeIndex> {
		let mut uses = Vec::new();
		let mut walk = self.g.walk_edges_directed(node, EdgeDirection::Incoming);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::Data(_) = self.g[edge] {
				uses.push(othernode);
			}
		}
		uses
	}

	pub fn preds_of(&self, node: NodeIndex) -> Vec<NodeIndex> {
		let mut preds = Vec::new();
		let mut walk = self.g.walk_edges_directed(node, EdgeDirection::Incoming);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::Control(_) = self.g[edge] {
				preds.push(othernode);
			}
		}
		preds
	}

	pub fn succs_of(&self, node: NodeIndex) -> Vec<NodeIndex> {
		let mut succs = Vec::new();
		let mut walk = self.g.walk_edges_directed(node, EdgeDirection::Outgoing);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::Control(_) = self.g[edge] {
				succs.push(othernode);
			}
		}
		succs
	}

	pub fn block_of(&self, node: NodeIndex) -> NodeIndex {
		let mut walk = self.g.walk_edges_directed(node, EdgeDirection::Outgoing);
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::ContainedInBB = self.g[edge] {
				return othernode
			}
		}
		return NodeIndex::end()
	}

	pub fn phi_use(&mut self, phi: NodeIndex, node: NodeIndex) {
		assert!(if let NodeData::Phi(_) = self.g[phi] { true } else { false });
		assert!(if let NodeData::Removed = self.g[node] { false } else { true });
		self.g.update_edge(phi, node, EdgeData::Data(0));
	}

	pub fn phi_unuse(&mut self, phi: NodeIndex, node: NodeIndex) {
		assert!(if let NodeData::Phi(_) = self.g[phi] { true } else { false });
		if let Option::Some(edge) = self.g.find_edge(phi, node) {
			self.g.remove_edge(edge);
		}
	}

	pub fn op_use(&mut self, node: NodeIndex, index: u8, argument: NodeIndex) {
		if argument == NodeIndex::end() { return }
		assert!(if let NodeData::Removed = self.g[argument] { false } else { true });
		// TODO: find existing edge
		self.g.add_edge(node, argument, EdgeData::Data(index));
	}

	pub fn replace(&mut self, node: NodeIndex, replacement: NodeIndex) {
		if node == replacement { return }

		let mut walk = self.g.walk_edges_directed(node, EdgeDirection::Incoming);
		let mut remove_us = Vec::<EdgeIndex>::new();
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::Data(i) = self.g[edge] {
				match self.g[othernode] {
					NodeData::Op(_) => self.op_use(othernode, i, replacement),
					NodeData::Phi(_) => self.phi_use(othernode, replacement),
					_ => panic!()
				}
			}
			remove_us.push(edge);
		}
		self.g.add_node(NodeData::Removed);
		self.g.remove_node(node);
		self.g.add_edge(node, replacement, EdgeData::ReplacedBy);
	}

	pub fn cleanup(&mut self) {
		let mut i = 0;
		let mut n = self.g.node_count();
		while i < n {
			if let NodeData::Removed = self.g[NodeIndex::new(i)] {
				self.g.remove_node(NodeIndex::new(i));
				n -= 1;
			} else {
				i += 1;
			}
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
		match self.weight {
			EdgeData::Data(_) => self.target().index(),
			_                 => self.source().index()
		}
    }

    fn target(&self) -> usize {
		match self.weight {
			EdgeData::Data(_) => self.source().index(),
			_                 => self.target().index()
		}
    }

    fn skip(&self) -> bool {
    	if let EdgeData::ContainedInBB = self.weight {
 			true
		} else {
			false
    	}
    }
}

impl Label for Edge<EdgeData> {
    fn label(&self) -> String {
    	match self.weight {
    		EdgeData::Control(_)    => "[color=\"blue\"];\n",
    		EdgeData::Data(_)       => "[dir=\"back\"];\n",
    		EdgeData::ContainedInBB => "[color=\"gray\"];\n",
    		EdgeData::ReplacedBy    => "[color=\"red\"];\n"
    	}.to_string()
    }

    fn name(&self) -> Option<String> {
        None
    }
}

impl Label for NodeData {
    fn label(&self) -> String {
    	let l = format!("{:?}", self);
        format!(" [label=\"{}\"];\n", l.replace("\"", "\\\""))
    }

    fn name(&self) -> Option<String> {
        Some(format!("n{:p}", self))
    }
}
