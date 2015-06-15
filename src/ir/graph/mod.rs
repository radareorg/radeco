extern crate petgraph;

pub mod indextype;
pub mod inner;

use super::traits::{Accessible, InstructionType, LookupResult, NavigationInternal};

use self::indextype::IndexType;
use self::inner::{InnerGraph, InnerEdgeLight};
use self::petgraph::Incoming;
use self::petgraph::graph::{Edge, EdgeIndex, Graph, NodeIndex};

pub struct PhiIndex<Index>(Index);

pub struct BasicBlock<Index: IndexType, Instruction: InstructionType> {
	num_ext: Index,
	num_phi: Index,
	phis: Vec<Instruction::PhiType>,
	inner_graph: InnerGraph<Instruction, InnerEdgeLight<Index>>
}

impl<Index: IndexType, Instruction: InstructionType> BasicBlock<Index, Instruction> {
	pub fn new() -> Self {
		BasicBlock::<Index, Instruction> {
			num_phi: Index::zero(),
			num_ext: Index::zero(),
			phis: Vec::new(),
			inner_graph: InnerGraph::<Instruction, InnerEdgeLight<Index>>::new()
		}
	}
}

//impl<'a, Index: IndexType, Instruction: InstructionType> Accessible<Index, &'a Instruction, Index> for BasicBlock<Index, Instruction> {
//	pub fn lookup(&self, i: Index) -> LookupResult<&'a Instruction, Index> {

impl<Index: IndexType, Instruction: InstructionType> Accessible<Index, Instruction, Index> for BasicBlock<Index, Instruction> {
	fn lookup(&self, i: Index) -> LookupResult<Instruction, Index> {
		if i >= Index::zero() {
			// TODO
			// self.inner_graph.lookup(i)
			LookupResult::NotFound

		} else if i + self.num_phi >= Index::zero() {
			LookupResult::Found(Instruction::make_phi(self.phis[(i+self.num_phi).as_usize()]))

		} else if i + self.num_phi + self.num_ext >= Index::zero() {
			LookupResult::Redirect(i + self.num_phi + self.num_ext)

		} else {
			LookupResult::NotFound
		}
	}
}


type DefaultInnerIndex = i16;

//Instruction = Instruction<InternalEdge<DefaultInnerIndex>>

enum IRNode<Instruction: InstructionType> {
	// Represents an operation of a basic block that is used by non-phi nodes outside of that basic block
	Repr,
	// Represents a basic block
	BasicBlock(BasicBlock<DefaultInnerIndex, Instruction>)
}

enum IREdge {
	// Points from a `IRNode::Repr` to the `IR::BasicBlock` that it belongs to
	ReprToBlock(DefaultInnerIndex),
	// Points from a `IR::BasicBlock`s to `IRNode::Repr` whose target is used by operations in the basic block
	// Contains a id of the first user in the basic block, and the id the `IRNode::Repr` is known by in the basic block
	BlockToRepr(DefaultInnerIndex, DefaultInnerIndex),
	// Points from one `IR::BasicBlock` to another. Represents control flow.
	// It contains a vector of 'n' references to operations in the origin basic block
	// that are used by 'n' phi operations in the target basic block.
	Flow(Vec<DefaultInnerIndex>, u64)
}

type IRGraph<Instruction> = Graph<IRNode<Instruction>, IREdge>;

pub struct NodeRef<I>(NodeIndex, I);

fn lookup<NodeRef: InstructionType, Instruction: InstructionType>(graph: IRGraph<Instruction>, noderef: NodeRef) {

}


impl<Index: IndexType, Instruction: InstructionType> NavigationInternal<NodeRef<Index>> for IRGraph<Instruction> {
	fn add_uses_to(&self, noderef: NodeRef<Index>, r: &mut Vec<NodeRef<Index>>) {
		if let Option::Some(node) = self.node_weight(noderef.0) {
			// TODO
			panic!();
		} else {
			panic!();
		}
	}

	fn add_args_to(&self, noderef: NodeRef<Index>, r: &mut Vec<NodeRef<Index>>) {
		if let Option::Some(node) = self.node_weight(noderef.0) {
			let bb = match *node {
				IRNode::Repr => panic!(),
				IRNode::BasicBlock(ref bb) => {
					// TODO
					bb //bb.lookup()
				}
			};
			/* // TODO
			if (noderef.is_phi()) {
				let phiindex = !noderef.1;
				let mut edges = self.walk_edges_directed(noderef.0, Incoming);
				while let Some(edge) = edges.next(&self) {
					match self.edge_weight(edge) {
						None => {},
						Some(&IREdge::ReprToBlock(_)) => {},
						Some(&IREdge::BlockToRepr(_, _)) => {},
						Some(&IREdge::Flow(phisource, id)) => {
							// r.push(deref_noderef(self, noderef.0, i));
						}
					}
				}
			} else {
				let inner_r = Vec::<Index>::new();
				bb.inner_graph.add_args_to(noderef.1, &mut r);
				for i in inner_r {
					// TODO
					// r.push(deref_noderef(self, noderef.0, i));
				}
			}*/
		} else {
			panic!();
		}
	}
}

mod test {
	use super::super::traits::InstructionType;
	use super::petgraph::graph::Graph;
	use super::{IRNode, IREdge};

	#[derive(Debug)]
	enum TestInstr { Phi, NotPhi }

	impl InstructionType for TestInstr {
		type PhiType = ();
		fn make_phi(x: ()) -> TestInstr { TestInstr::Phi }
		fn is_phi(&self) -> bool { if let TestInstr::Phi = *self { true } else { false } }
	}

	#[test]
	fn construct() {
		let graph: Graph<IRNode<TestInstr>, IREdge>;
	}

	// more tests will follow
}
