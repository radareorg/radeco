extern crate petgraph;

pub mod dot;
pub mod indextype;
pub mod inner;

use std::cell::{Cell, RefCell, RefMut};

use super::traits::{InstructionType, Navigation, NavigationInternal};

use self::indextype::IndexType;
use self::inner::{InnerGraph, InnerEdgeLight, InnerEdgeTrait, InnerGraphWithMethods, AuxQuery};
use self::petgraph::{Incoming, Outgoing};
use self::petgraph::graph::{Edge, EdgeIndex, Graph, NodeIndex};

type DefaultInnerEdge<Index/*: IndexType*/> = InnerEdgeLight<Index>;

trait BlockToReprPayload { type Type; }
trait UsedInnerEdgeType { type InnerEdgeType: InnerEdgeTrait; }

pub struct PhiIndex<Index>(Index);

pub struct BasicBlock<Index: IndexType, Instruction: InstructionType> {
	num_ext: Index,
	//phis: Vec<(Instruction::PhiType, <DefaultInnerEdge<Index> as self::inner::InnerEdgeTrait>::NodeAux)>,
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
			//phis: Vec::new(),
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

enum IRNode<Index: IndexType, Instruction: InstructionType> {
	// Represents an operation of a basic block that is used by non-phi nodes outside of that basic block
	Repr,
	// Represents a basic block
	BasicBlock(RefCell<BasicBlock<Index, Instruction>>)
}

enum IREdge<Index: IndexType, Instruction: InstructionType> {
	// Points from a `IRNode::Repr` to the `IR::BasicBlock` that it belongs to
	ReprToBlock(Index),
	// Points from a `IR::BasicBlock`s to `IRNode::Repr` whose target is used by operations in the basic block
	// Contains a id of the first user in the basic block, and the id the `IRNode::Repr` is known by in the basic block
	BlockToRepr(Index, Cell<<BasicBlock<Index, Instruction> as BlockToReprPayload>::Type>),
	// Points from one `IR::BasicBlock` to another. Represents control flow.
	// It contains a vector of 'n' references to operations in the origin basic block
	// that are used by 'n' phi operations in the target basic block.
	Flow(Vec<Index>, u64)
}

pub type IRGraph<Index, Instruction> = Graph<
	IRNode<Index, Instruction>,
	IREdge<Index, Instruction>
>;

pub struct NodeRef<I>(NodeIndex, I);
struct AuxQueryImpl<'a, Index: IndexType + 'a, Instruction: InstructionType + 'a>(
	&'a IRGraph<Index, Instruction>,
	NodeIndex
);

// forward that type
impl<Index: IndexType, Instruction: InstructionType> UsedInnerEdgeType for IRGraph<Index, Instruction> {
	type InnerEdgeType = DefaultInnerEdge<Index>;
}
impl<'a, Index: IndexType, Instruction: InstructionType> UsedInnerEdgeType for AuxQueryImpl<'a, Index, Instruction> {
	type InnerEdgeType = <IRGraph<Index, Instruction> as UsedInnerEdgeType>::InnerEdgeType;
}

macro_rules! find_edge {
	( $graph:expr, $node:expr, $direction:expr, $( $pat:pat => $cond:expr ),* ) => {
		{
			let mut fedge: Option<EdgeIndex> = Option::None;
			let mut temp_edgewalk = $graph.walk_edges_directed($node, $direction);
			while let Some(edge) = temp_edgewalk.next($graph) {
				if match &$graph[edge] {
					$(
						$pat => $cond
					),*
					_ => { false }
				} {
					fedge = Option::Some(edge);
					break
				}
			}
			fedge
		}
	};
}

impl<'b, Index: IndexType, Instruction: InstructionType/*, InnerEdge: InnerEdgeTrait<Index=DefaultInnerIndex>*/>
	inner::AuxQuery<<AuxQueryImpl<'b, Index, Instruction> as UsedInnerEdgeType>::InnerEdgeType>
	for AuxQueryImpl<'b, Index, Instruction>
{
	//use AuxQueryImpl<'b, Instruction>::InnerEdgeType as InnerEdge;

	fn access_aux<'a>(&'a mut self, i: <<Self as UsedInnerEdgeType>::InnerEdgeType as InnerEdgeTrait>::Index)
		-> &'a Cell<<<Self as UsedInnerEdgeType>::InnerEdgeType as InnerEdgeTrait>::NodeAux>
	{
		let fedge: Option<EdgeIndex> = find_edge!(self.0, self.1, Outgoing,
			&IREdge::BlockToRepr(key, _) => { key == i }
		);
		if let Some(edge) = fedge {
			if let Some(&IREdge::BlockToRepr(_, ref aux)) = self.0.edge_weight(edge){
				return aux
			}
		}
		panic!();
	}
}

fn deref<Index: IndexType, Instruction: InstructionType>(
	graph: &IRGraph<Index, Instruction>,
	noderef: NodeRef<Index>
) -> NodeRef<Index> {
	let fedge: Option<EdgeIndex> = find_edge!(graph, noderef.0, Outgoing,
		&IREdge::BlockToRepr(key, _) => { key == noderef.1 }
	);
	if let Some(edge) = fedge {
		// TODO
	}
	panic!();
}

impl<Index: IndexType, Instruction: InstructionType> NavigationInternal<NodeRef<Index>> for IRGraph<Index, Instruction> {
	fn add_uses_to(&self, noderef: NodeRef<Index>, r: &mut Vec<NodeRef<Index>>) {
		let node = &self[noderef.0];
		let bb = if let &IRNode::BasicBlock(ref bb) = node { bb.borrow() } else { panic!() };
	}

	fn add_args_to(&self, noderef: NodeRef<Index>, r: &mut Vec<NodeRef<Index>>) {
		let node = &self[noderef.0];
		let bb = if let &IRNode::BasicBlock(ref bb) = node { bb.borrow_mut() } else { panic!() };
		let instruction = bb.inner_graph.lookup(noderef.1);

		if instruction.is_phi() {
			let mut edges = self.walk_edges_directed(noderef.0, Incoming);
			while let Some(edgeindex) = edges.next(&self) {
				let edge = &self.raw_edges()[edgeindex.index()];
				match &edge.weight {
					&IREdge::Flow(ref phisource, ref id) => {
						//r.push(deref(self, NodeRef::<Index>(edge.source(), phisource[])));
					}
					_ => {}
				}
			}
		} else {
			let inner_arg = bb.inner_graph.args_of(noderef.1);
			for i in inner_arg {
				r.push(deref(self, NodeRef::<Index>(noderef.0, i)));
			}
		}
	}
}

trait AddIR<Index: IndexType, Instruction: InstructionType> {
	fn add_bb(&mut self) -> NodeIndex;
	fn select_bb<'b, 'a: 'b>(&'a mut self, NodeIndex) -> Builder<'a, 'b, Index, Instruction>;
	fn add_and_select_bb<'b, 'a: 'b>(&'a mut self) -> Builder<'a, 'b, Index, Instruction> {
		let ni = self.add_bb();
		self.select_bb(ni)
	}
}

impl<Index: IndexType, Instruction: InstructionType> AddIR<Index, Instruction> for IRGraph<Index, Instruction> {
	fn add_bb(&mut self) -> NodeIndex {
		self.add_node(IRNode::BasicBlock(RefCell::new(BasicBlock::<Index, Instruction>::new())))
	}
	fn select_bb<'b, 'a: 'b>(&'a mut self, ni: NodeIndex) -> Builder<'a, 'b, Index, Instruction> {
		let node = &self[ni];
		let mut bb: RefMut<'b, BasicBlock<Index, Instruction>> = if let &IRNode::BasicBlock(ref bb) = node { bb.borrow_mut() } else { panic!() };
		let mut builder = Builder::<'a, 'b, Index, Instruction> {
			nodeindex: ni,
			bb: bb,
			external: AuxQueryImpl::<'a, Index, Instruction>(self, ni)
		};
		builder
	}
}

struct Builder<'a, 'b: 'a, Index: IndexType, Instruction: InstructionType + 'b> {
	pub nodeindex: NodeIndex,
	bb: RefMut<'b, BasicBlock<Index, Instruction>>,
	external: AuxQueryImpl<'a, Index, Instruction>
}

impl<'a, 'b, Index: IndexType, Instruction: InstructionType> Builder<'a, 'b, Index, Instruction>
	// where
	// InnerGraph           <Instruction, <AuxQueryImpl<'a, Index, Instruction> as UsedInnerEdgeType>::InnerEdgeType>:
	// InnerGraphWithMethods<Instruction, <AuxQueryImpl<'a, Index, Instruction> as UsedInnerEdgeType>::InnerEdgeType>
{
	fn add_instr(&mut self, instr: Instruction, args: &[NodeRef<Index>]) -> NodeRef<Index> {
		use self::inner::HasAdd;
		let inner_args = Vec::<Index>::with_capacity(args.len());
		let n = self.bb.inner_graph.add(&mut self.external, instr, &inner_args);
		NodeRef::<Index>(self.nodeindex, n)
	}
}

#[cfg(test)]
mod test {
	use std::fs::File;
	use std::cell::RefMut;

	use super::super::traits::InstructionType;
	use super::inner::HasAdd;
	use super::{AddIR, DefaultInnerIndex, IRGraph, IRNode, AuxQueryImpl};
	use super::dot;

	#[derive(Debug)]
	enum TestInstr { Phi, NotPhi }

	impl InstructionType for TestInstr {
		type PhiType = ();
		fn make_phi(_: ()) -> TestInstr { TestInstr::Phi }
		fn is_phi(&self) -> bool { if let TestInstr::Phi = *self { true } else { false } }
	}

	#[test]
	fn construct() {
		let mut graph = IRGraph::<DefaultInnerIndex, TestInstr>::new();

		let ni1 = {
			let mut bb1 = graph.add_and_select_bb();
			let i0 = bb1.add_instr(TestInstr::NotPhi, &[]);
			let i1 = bb1.add_instr(TestInstr::NotPhi, &[i0]);
			bb1.nodeindex
		};

		{
			let bb2 = graph.add_and_select_bb();
		}

		let mut dot_file = File::create("ssa.dot").ok().expect("Error. Cannot create file!\n");
		dot::dot(&mut dot_file, &graph);
	}
}
