extern crate petgraph;

pub mod indextype;
pub mod inner;

use super::traits::{InstructionType, NavigationInternal};

use self::indextype::IndexType;
use self::inner::{InnerGraph, InnerEdgeLight, InnerEdgeTrait, AuxQuery};
use self::petgraph::{Incoming, Outgoing};
use self::petgraph::graph::{Edge, EdgeIndex, Graph, NodeIndex};

type DefaultInnerEdge<Index/*: IndexType*/> = InnerEdgeLight<Index>;

trait BlockToReprPayload { type Type; }
trait UsedInnerEdgeType { type InnerEdgeType: InnerEdgeTrait; }

pub struct PhiIndex<Index>(Index);

pub struct BasicBlock<Index: IndexType, Instruction: InstructionType> {
	num_ext: Index,
	phis: Vec<(Instruction::PhiType, <DefaultInnerEdge<Index> as self::inner::InnerEdgeTrait>::NodeAux)>,
	inner_graph: InnerGraph<Instruction, DefaultInnerEdge<Index>>
}

enum LookupResult<Index: IndexType, Instruction: InstructionType> {
	FoundInBB(Instruction),
	Phi(Index),
	External(Index),
	NotFound(Index)
}

impl<Index: IndexType, Instruction: InstructionType> BlockToReprPayload for BasicBlock<Index, Instruction> {
	type Type = <InnerEdgeLight<Index> as InnerEdgeTrait>::NodeAux;
}

impl<Index: IndexType, Instruction: InstructionType> BasicBlock<Index, Instruction> {
	pub fn new() -> Self {
		BasicBlock::<Index, Instruction> {
			num_ext: Index::zero(),
			phis: Vec::new(),
			inner_graph: InnerGraph::<Instruction, InnerEdgeLight<Index>>::new()
		}
	}

	fn lookup(&self, i: Index) -> LookupResult<Index, Instruction> {
		if i >= Index::zero() {
			let instr = self.inner_graph.lookup(i);
			if instr.is_phi() {
				// TODO
			}
			LookupResult::NotFound(i)

		} else if i + self.num_ext >= Index::zero() {
			LookupResult::External(i)

		} else {
			LookupResult::NotFound(i)
		}
	}
}


type DefaultInnerIndex = i16;
type DefaultBasicBlock<Instruction> = BasicBlock<DefaultInnerIndex, Instruction>;

enum IRNode<Instruction: InstructionType> {
	// Represents an operation of a basic block that is used by non-phi nodes outside of that basic block
	Repr,
	// Represents a basic block
	BasicBlock(DefaultBasicBlock<Instruction>)
}

enum IREdge<Instruction: InstructionType> {
	// Points from a `IRNode::Repr` to the `IR::BasicBlock` that it belongs to
	ReprToBlock(DefaultInnerIndex),
	// Points from a `IR::BasicBlock`s to `IRNode::Repr` whose target BlockToReprPayloadis used by operations in the basic block
	// Contains a id of the first user in the basic block, and the id the `IRNode::Repr` is known by in the basic block
	BlockToRepr(DefaultInnerIndex, <DefaultBasicBlock<Instruction> as BlockToReprPayload>::Type),
	// Points from one `IR::BasicBlock` to another. Represents control flow.
	// It contains a vector of 'n' references to operations in the origin basic block
	// that are used by 'n' phi operations in the target basic block.
	Flow(Vec<DefaultInnerIndex>, u64)
}

type IRGraph<Instruction> = Graph<IRNode<Instruction>, IREdge<Instruction>>;

pub struct NodeRef<I>(NodeIndex, I);
struct AuxQueryImpl<'a, Instruction: InstructionType + 'a>(&'a mut IRGraph<Instruction>, NodeIndex);

// forward that type
impl<Index: IndexType, Instruction: InstructionType> UsedInnerEdgeType for BasicBlock<Index, Instruction> {
	type InnerEdgeType = DefaultInnerEdge<Index>;
}
impl<Instruction: InstructionType> UsedInnerEdgeType for IRGraph<Instruction> {
	type InnerEdgeType = <DefaultBasicBlock<Instruction> as UsedInnerEdgeType>::InnerEdgeType;
}
impl<'a, Instruction: InstructionType> UsedInnerEdgeType for AuxQueryImpl<'a, Instruction> {
	type InnerEdgeType = <IRGraph<Instruction> as UsedInnerEdgeType>::InnerEdgeType;
}

impl<'b, Instruction: InstructionType/*, InnerEdge: InnerEdgeTrait<Index=DefaultInnerIndex>*/>
	inner::AuxQuery<<AuxQueryImpl<'b, Instruction> as UsedInnerEdgeType>::InnerEdgeType>
	for AuxQueryImpl<'b, Instruction>
{
	//use AuxQueryImpl<'b, Instruction>::InnerEdgeType as InnerEdge;

	fn access_aux<'a>(&'a mut self, i: <<Self as UsedInnerEdgeType>::InnerEdgeType as InnerEdgeTrait>::Index)
		-> &'a mut <<Self as UsedInnerEdgeType>::InnerEdgeType as InnerEdgeTrait>::NodeAux
	{

		let mut fedge: Option<EdgeIndex> = Option::None;
		{
			let graph = &*self.0;
			let mut edges = graph.walk_edges_directed(self.1, Outgoing);
			while let Some(edge) = edges.next(graph) {
				match graph.edge_weight(edge) {
					Some(&IREdge::BlockToRepr(key, _)) => {
						if key == i {
							fedge = Option::Some(edge);
							break
						}
					},
					_ => {}
				}
			}
		}
		if let Some(edge) = fedge {
			if let Some(&mut IREdge::BlockToRepr(_, ref mut aux)) = self.0.edge_weight_mut(edge){
				return aux
			}
		}
		panic!();
	}
}

//fn deref<InstructionType>(graph: &IRGraph<Instruction>) {
//	//
//}

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
					bb
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
	use super::IRGraph;

	#[derive(Debug)]
	enum TestInstr { Phi, NotPhi }

	impl InstructionType for TestInstr {
		type PhiType = ();
		fn make_phi(x: ()) -> TestInstr { TestInstr::Phi }
		fn is_phi(&self) -> bool { if let TestInstr::Phi = *self { true } else { false } }
	}

	#[test]
	fn construct() {
		let graph: IRGraph<TestInstr>;
	}

	// more tests will follow
}
