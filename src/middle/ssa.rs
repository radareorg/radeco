use std::hash::Hash;
use petgraph::{EdgeDirection, Graph};
use petgraph::graph::{Edge, NodeIndex, EdgeIndex};
use super::ir;
use super::dot::{GraphDot, DotAttrBlock};

pub struct SSAStorage {
	pub g: Graph<NodeData, EdgeData>,
	pub stable_indexing: bool,
	needs_cleaning: bool,
}

#[derive(Clone, Debug)]
pub enum ValueType {
	Integer {width: u8}
}

#[derive(Clone, Debug)]
pub struct BBInfo {
	pub addr: u64
}

#[derive(Clone, Debug)]
pub enum NodeData {
	Op(ir::MOpcode, ValueType),
	Comment(String),
	Const(u64),
	Phi(String),
	Undefined,
	Removed,
	BasicBlock(BBInfo)
}

#[derive(Clone, Copy)]
pub enum EdgeData {
	Control(u8),
	Data(u8),
	ContainedInBB { is_selector: bool },
	ReplacedBy
}

const CONTEDGE: EdgeData = EdgeData::ContainedInBB {is_selector: false};

impl SSAStorage {
	pub fn new() -> SSAStorage {
		SSAStorage {
			g: Graph::new(),
			needs_cleaning: false,
			stable_indexing: true,
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
				if let EdgeData::Control(_) = self.g[edge] {
					adjacent.push(othernode);
				}
			} else {
				if let EdgeData::Data(_) = self.g[edge] {
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

	fn node_cluster(&self, n: usize) -> usize {
		let ni = NodeIndex::new(n);
		match self.g.node_weight(ni) {
			Some(&NodeData::BasicBlock(_)) => n,
			_ => self.get_block(&ni).index()
		}
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
		if let EdgeData::ContainedInBB{is_selector: s} = edge.weight {
			if let NodeData::Phi(_) = self.g[edge.source()] {
				false
			} else {
				!s
			}
		} else {
			false
		}
	}

	fn edge_attrs(&self, edge: &Edge<EdgeData>) -> DotAttrBlock {
		let target_is_bb = if let NodeData::BasicBlock(_) = self.g[edge.target()] { true } else { false };

		DotAttrBlock::Attributes(match edge.weight {
			EdgeData::Control(_) if !target_is_bb
				=> vec![("color".to_string(), "red".to_string())],

			EdgeData::Control(i)
				=> vec![("color".to_string(), "blue".to_string()),
				        ("label".to_string(), format!("{}", i))],

			EdgeData::Data(i)
				=> vec![("dir".to_string(),   "back".to_string()),
				        ("label".to_string(), format!("{}", i))],

			EdgeData::ContainedInBB {is_selector: false}
				=> vec![("color".to_string(), "gray".to_string())],

			EdgeData::ContainedInBB {is_selector: true}
				=> vec![("color".to_string(), "purple".to_string())],

			EdgeData::ReplacedBy
				=> vec![("color".to_string(), "brown".to_string())]
		})
	}

	fn node_attrs(&self, node: &NodeData) -> DotAttrBlock {
		let l = match node {
			&NodeData::Op(opc, ValueType::Integer{width: w}) => format!("[i{}] {:?}", w, opc),
			&NodeData::BasicBlock(BBInfo{addr}) => format!("@{:x}", addr),
			_ => format!("{:?}", node)
		};
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
	type ValueRef: Eq + Hash + Clone + Copy; // We could drop the Copy trait later and insert .clone()
	type ActionRef: Eq + Hash + Clone + Copy;

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

	/// Updates a node reference to the latest version in case of replacement
	// TODO: Hide his implementation detail
	fn refresh(&self, mut node: Self::ValueRef) -> Self::ValueRef;

	fn invalid_value(&self) -> Self::ValueRef;
	fn invalid_action(&self) -> Self::ActionRef;
}

/// Trait for modifying SSA data
pub trait SSAMod: SSA {

	type BBInfo;

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

	/// Add a data source to a phi node.
	fn phi_use(&mut self, phi: Self::ValueRef, node: Self::ValueRef);

	/// Remove a data source from a phi node.
	fn phi_unuse(&mut self, phi: Self::ValueRef, node: Self::ValueRef);

	/// Set the index-th argument of the node.
	fn op_use(&mut self, node: Self::ValueRef, index: u8, argument: Self::ValueRef);

	/// Replace one node by another within one basic block.
	fn replace(&mut self, node: Self::ValueRef, replacement: Self::ValueRef);

	/// Mark the node as selector for the control edges away from the specified basic block
	fn mark_selector(&mut self, block: Self::ActionRef, n: Self::ValueRef);
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
                NodeData::BasicBlock(_) => blocks.push(i.clone()),
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
            if let EdgeData::ContainedInBB{..} = self.g[edge] {
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
            if let EdgeData::ContainedInBB{..} = self.g[edge] {
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
            if let EdgeData::ContainedInBB{..} = self.g[edge] {
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
}

impl SSAMod for SSAStorage {

	type BBInfo = self::BBInfo;

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

	fn add_block(&mut self, info: BBInfo) -> NodeIndex {
		self.g.add_node(NodeData::BasicBlock(info))
	}

	fn add_control_edge(&mut self, source: Self::ActionRef, target: Self::ActionRef, index: u8) {
		self.g.add_edge(source, target, EdgeData::Control(index));
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

	fn mark_selector(&mut self, _: NodeIndex, n: NodeIndex) {
		// TODO: consider first argument and unmerge edgetypes contain/select
		let mut walk = self.g.walk_edges_directed(n, EdgeDirection::Outgoing);
		while let Some((edge, _)) = walk.next_neighbor(&self.g) {
			if let &mut EdgeData::ContainedInBB{is_selector: ref mut flag} = &mut self.g[edge] {
				*flag = true;
				return;
			}
		}
		panic!("Provided node doesn't belong to any basic block.");
	}
}
