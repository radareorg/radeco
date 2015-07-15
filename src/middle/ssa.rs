use std::hash::Hash;
use petgraph::{EdgeDirection, Graph};
use petgraph::graph::{Edge, NodeIndex, EdgeIndex};
use super::ir;
use super::dot::{GraphDot, DotAttrBlock};

pub struct SSAStorage {
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
	DynamicControl(u8),
	Control(u8),
	Data(u8),
	ContainedInBB,
	Selector,
	ReplacedBy
}

impl SSAStorage {
	pub fn new() -> SSAStorage {
		SSAStorage {g: Graph::new() }
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
	}
}

impl GraphDot for SSAStorage {
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

	fn edge_source(&self, edge: &Edge<EdgeData>) -> usize {
		match edge.weight {
			EdgeData::Data(_) => edge.target().index(),
			_                 => edge.source().index()
		}
	}

	fn edge_target(&self, edge: &Edge<EdgeData>) -> usize {
		match edge.weight {
			EdgeData::Data(_) => edge.source().index(),
			_                 => edge.target().index()
		}
	}

	fn edge_skip(&self, edge: &Edge<EdgeData>) -> bool {
		if let EdgeData::ContainedInBB = edge.weight {
			if let NodeData::Phi(_) = self.g[edge.source()] {
				false
			} else {
				true
			}
		} else {
			false
		}
	}

	fn edge_attrs(&self, edge: &Edge<EdgeData>) -> DotAttrBlock {
		DotAttrBlock::Attributes(match edge.weight {
			EdgeData::DynamicControl(_) => vec![("color".to_string(), "red".to_string())],
			EdgeData::Control(_)        => vec![("color".to_string(), "blue".to_string())],
			EdgeData::Data(i)           => vec![("dir".to_string(),   "back".to_string()),
			                                    ("label".to_string(), format!("{}", i))],
			EdgeData::ContainedInBB     => vec![("color".to_string(), "gray".to_string())],
			EdgeData::Selector          => vec![("color".to_string(), "purple".to_string())],
			EdgeData::ReplacedBy        => vec![("color".to_string(), "brown".to_string())]
		})
	}

	fn node_attrs(&self, node: &NodeData) -> DotAttrBlock {
		let l = format!("{:?}", node);
		DotAttrBlock::Raw(format!(" [label=\"{}\"]", l.replace("\"", "\\\"")))
	}
}

// pub struct SSAStorageValueRef<'a> {
// 	ni: NodeIndex,
// 	_: PhantomData<'a>
// }

/// Trait for the SSA Form implementation.
// This trait ensures that any other ssa form will be compatible with our implementations provided
// the SSA form implements the following traits.
pub trait SSA {
	type ValueRef: Eq + Hash + Clone;
	type ActionRef: Eq + Hash + Clone;

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

    /// Get the lhs() of the Operation with NodeIndex 'i'.
    fn lhs(&self, i: &Self::ValueRef) -> Self::ValueRef {
        self.get_operands(i)[0].clone()
    }

    /// Get the rhs() of the Operation with NodeIndex 'i'.
    fn rhs(&self, i: &Self::ValueRef) -> Self::ValueRef {
        self.get_operands(i)[1].clone()
    }

    /// Get the actual NodeData.
    fn get_node_data(&self, i: &Self::ValueRef) -> NodeData;

    // NOTE:
    // These three functions will change their signatures
    // when we remove "jmp" from the list of ops

    /// Get Jump target of a call or an unconditional jump.
    fn get_target(&self, i: &Self::ValueRef) -> Self::ActionRef;

    /// Get true branch of a conditional jump.
    fn get_true_branch(&self, i: &Self::ValueRef) -> Self::ActionRef;

    /// Get false branch of a conditional jump.
    fn get_false_branch(&self, i: &Self::ValueRef) -> Self::ActionRef;
}

///////////////////////////////////////////////////////////////////////////////
//// Implementation of SSA for SSA.
///////////////////////////////////////////////////////////////////////////////

impl SSA for SSAStorage {
	type ValueRef = NodeIndex;
	type ActionRef = NodeIndex;

    fn get_blocks(&self) -> Vec<NodeIndex> {
        let len = self.g.node_count();
        let mut blocks = Vec::<NodeIndex>::new();
        for i in (0..len).map(|x| NodeIndex::new(x)).collect::<Vec<NodeIndex>>().iter() {
            match self.g[*i] {
                NodeData::BasicBlock => blocks.push(i.clone()),
                _ => continue,
            }
        }
        return blocks;
    }

    fn start_node(&self) -> NodeIndex {
        // TODO: Return the actual start node when we add this information to ssa.
        NodeIndex::new(0)
    }

    fn get_exprs(&self, i: &NodeIndex) -> Vec<NodeIndex> {
        let mut expressions = Vec::<NodeIndex>::new();
        let mut walk = self.g.walk_edges_directed(*i, EdgeDirection::Incoming);
        while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
            if let EdgeData::ContainedInBB = self.g[edge] {
                match self.g[othernode] {
                      NodeData::Op(_)
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
        let mut walk = self.g.walk_edges_directed(*i, EdgeDirection::Outgoing);
        while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
            if let EdgeData::ContainedInBB = self.g[edge] {
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

    fn get_node_data(&self, i: &NodeIndex) -> NodeData {
        self.g[*i].clone()
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
}

