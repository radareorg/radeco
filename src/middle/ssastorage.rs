use petgraph::EdgeDirection;
use petgraph::graph::{Graph, NodeIndex, EdgeIndex};
use middle::ir;
use middle::ssa;
use middle::ssa::{SSA, SSAMod, NodeData, ValueType};

// pub struct SSAStorageValueRef<'a> {
// 	ni: NodeIndex,
// 	_: PhantomData<'a>
// }

#[derive(Clone, Copy)]
pub enum EdgeData {
	Control(u8),
	Data(u8),
	ContainedInBB,
	Selector,
	ReplacedBy
}

const CONTEDGE: EdgeData = EdgeData::ContainedInBB;

pub struct SSAStorage {
	pub g: Graph<NodeData, EdgeData>,
    pub start_node: NodeIndex,
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
		}
	}

	pub fn add_comment(&mut self, block: NodeIndex, msg: &String) -> NodeIndex {
		let n = self.g.add_node(NodeData::Comment(msg.clone()));
		self.g.update_edge(n, block, CONTEDGE);
		n
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

	pub fn cleanup(&mut self) {
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


///////////////////////////////////////////////////////////////////////////////
//// Implementation of SSA for SSAStorage.
///////////////////////////////////////////////////////////////////////////////

impl SSA for SSAStorage {
	type ValueRef = NodeIndex;
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

	fn args_of(&self, node: NodeIndex) -> Vec<NodeIndex> {
		self.gather_adjacent(node, EdgeDirection::Outgoing, true)
	}

	fn uses_of(&self, node: NodeIndex) -> Vec<NodeIndex> {
		self.gather_adjacent(node, EdgeDirection::Incoming, true)
	}

	fn preds_of(&self, node: NodeIndex) -> Vec<NodeIndex> {
		self.gather_adjacent(node, EdgeDirection::Incoming, false)
	}

	fn succs_of(&self, node: NodeIndex) -> Vec<NodeIndex> {
		self.gather_adjacent(node, EdgeDirection::Outgoing, false)
	}

	fn refresh(&self, mut node: NodeIndex) -> NodeIndex {
		if node == NodeIndex::end() { return node }
		while let NodeData::Removed = self.g[node] {
			node = self.replaced_by(node);
		}
		node
	}

	fn invalid_value(&self) -> NodeIndex { NodeIndex::end() }
	fn invalid_action(&self) -> NodeIndex { NodeIndex::end() }
	fn to_value(&self, n: NodeIndex) -> NodeIndex { n }
	fn to_action(&self, n: NodeIndex) -> NodeIndex { n }
}

impl SSAMod for SSAStorage {

	type BBInfo = ssa::BBInfo;

    fn mark_start_node(&mut self, start: &Self::ActionRef) {
        self.start_node = *start;
    }

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

	fn add_block(&mut self, info: Self::BBInfo) -> NodeIndex {
		self.g.add_node(NodeData::BasicBlock(info))
	}

	fn add_control_edge(&mut self, source: Self::ActionRef, target: Self::ActionRef, index: u8) {
		self.g.add_edge(source, target, EdgeData::Control(index));
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

	fn replace(&mut self, node: NodeIndex, replacement: NodeIndex) {
		if node == replacement { return }

		let mut walk = self.g.walk_edges_directed(node, EdgeDirection::Incoming);
		let mut remove_us = Vec::<EdgeIndex>::new();
		while let Some((edge, othernode)) = walk.next_neighbor(&self.g) {
			if let EdgeData::Data(i) = self.g[edge] {
				match self.g[othernode] {
					NodeData::Op(_, _) => self.op_use(othernode, i, replacement),
					NodeData::Phi(_) => self.phi_use(othernode, replacement),
					_ => panic!()
				}
			}
			remove_us.push(edge);
		}
		if self.stable_indexing {
			self.g.add_node(NodeData::Removed);
			self.g.remove_node(node);
			self.g.add_edge(node, replacement, EdgeData::ReplacedBy);
			self.needs_cleaning = true;
		} else {
			self.g.remove_node(node);
		}
	}
}

