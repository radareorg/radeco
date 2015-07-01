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

/*enum LookupResult<Index: IndexType, Instruction: InstructionType> {
	FoundInBB(Instruction),
	Phi(Index),
	ExternalReprPending(Index),
	ExternalReprCreated(Index, NodeIndex),
	NotFound(Index)
}*/

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

	/*fn lookup(&self, i: Index) -> LookupResult<Index, Instruction> {
		if i >= Index::zero() {
			let instr = self.inner_graph.lookup(i);
			if instr.is_phi() {
				unimplemented!();
			}
			LookupResult::NotFound(i)

		} else if i + self.num_ext >= Index::zero() {
			LookupResult::External(i)

		} else {
			LookupResult::NotFound(i)
		}
	}*/
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

pub type IRGraphRC<Index, Instruction> = RefCell<IRGraph<Index, Instruction>>;

#[derive(Copy, Clone)]
pub struct NodeRef<I>(NodeIndex, I);

// forward that type
impl<Index: IndexType, Instruction: InstructionType> UsedInnerEdgeType for IRGraph<Index, Instruction> {
	type InnerEdgeType = DefaultInnerEdge<Index>;
}
impl<'a, 'b, Index: IndexType, Instruction: InstructionType> UsedInnerEdgeType for Builder<'a, Index, Instruction> {
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
	inner::AuxQuery<<Builder<'b, Index, Instruction> as UsedInnerEdgeType>::InnerEdgeType>
	for Builder<'b, Index, Instruction>
{
	//use Builder<'b, Instruction>::InnerEdgeType as InnerEdge;

	fn access_aux<'a>(&'a mut self, i: <<Self as UsedInnerEdgeType>::InnerEdgeType as InnerEdgeTrait>::Index)
		-> &'a Cell<<<Self as UsedInnerEdgeType>::InnerEdgeType as InnerEdgeTrait>::NodeAux>
	{
		let graph = &*self.graph_rc.borrow();
		let fedge: Option<EdgeIndex> = find_edge!(graph, self.nodeindex, Outgoing,
			&IREdge::BlockToRepr(key, _) => { key == i }
		);
		if let Some(edge) = fedge {
			if let IREdge::BlockToRepr(_, ref aux) = graph[edge] {
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
		unimplemented!();
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
	fn ext_repr(&mut self, noderef: NodeRef<Index>) -> NodeIndex; 
	fn enumerated_bbs(&self) -> BBIter<Index, Instruction>;
}

trait AddIR_RC<Index: IndexType, Instruction: InstructionType> {
	fn add_bb(&self) -> NodeIndex;
	fn select_bb<'b, 'a: 'b>(&'a self, NodeIndex) -> Builder<'a, Index, Instruction>;
	fn add_and_select_bb<'b, 'a: 'b>(&'a self) -> Builder<'a, Index, Instruction> {
		let ni = self.add_bb();
		self.select_bb(ni)
	}
}

struct BBIter<'a, Index: IndexType, Instruction: InstructionType + 'a> {
	graph: &'a IRGraph<Index, Instruction>
}

/*impl Iterator for BBIter {
	type Output = (NodeIndex, BasicBlock)
	fn next() -> Option
}*/

impl<Index: IndexType, Instruction: InstructionType> AddIR<Index, Instruction> for IRGraph<Index, Instruction> {
	fn add_bb(&mut self) -> NodeIndex {
		self.add_node(IRNode::BasicBlock(RefCell::new(BasicBlock::<Index, Instruction>::new())))
	}
	fn ext_repr(&mut self, noderef: NodeRef<Index>) -> NodeIndex {
		let fedge: Option<EdgeIndex> = find_edge!(self, noderef.0, Outgoing,
			&IREdge::ReprToBlock(i) => { i == noderef.1 }
		);
		if let Some(edge) = fedge {
			let e = &self.raw_edges()[edge.index()];

		}
		panic!();
	}
	fn enumerated_bbs(&self) -> BBIter<Index, Instruction> {
		BBIter{graph: self}
	}
}

impl<Index: IndexType, Instruction: InstructionType> AddIR_RC<Index, Instruction> for IRGraphRC<Index, Instruction> {
	fn add_bb(&self) -> NodeIndex {
		(*self.borrow_mut()).add_bb()
	}
	fn select_bb<'b, 'a: 'b>(&'a self, ni: NodeIndex) -> Builder<'a, Index, Instruction> {
		// let node = &self[ni];
		// let mut bb: RefMut<'b, BasicBlock<Index, Instruction>> = if let &IRNode::BasicBlock(ref bb) = node { bb.borrow_mut() } else { panic!() };
		let mut builder = Builder::<'a, Index, Instruction> {
			graph_rc: self,
			nodeindex: ni
			// bb: bb
		};
		builder
	}
}

struct Builder<'a, Index: IndexType, Instruction: InstructionType + 'a> {
	pub nodeindex: NodeIndex,
	graph_rc: &'a RefCell<IRGraph<Index, Instruction>>
	// bb: RefMut<'b, BasicBlock<Index, Instruction>>,
}

impl<'a, Index: IndexType, Instruction: InstructionType> Builder<'a, Index, Instruction>
	// where
	// InnerGraph           <Instruction, <Builder<'a, Index, Instruction> as UsedInnerEdgeType>::InnerEdgeType>:
	// InnerGraphWithMethods<Instruction, <Builder<'a, Index, Instruction> as UsedInnerEdgeType>::InnerEdgeType>
{
	fn add_instr(&mut self, instr: Instruction, args: &[NodeRef<Index>]) -> NodeRef<Index> {
		use self::inner::HasAdd;
		let mut inner_args = Vec::<Index>::with_capacity(args.len());
		{
			let graph = &*self.graph_rc.borrow_mut();
			for arg in args {
				inner_args.push(
					if arg.0 == self.nodeindex { arg.1 } else { Index::zero() }
					//else { graph.ext_repr(arg) }
				)
			}
		}
		let n = {
			let graph = &*self.graph_rc.borrow();
			let node = &graph[self.nodeindex];
			let mut bb: RefMut<'a, BasicBlock<Index, Instruction>> = if let &IRNode::BasicBlock(ref bb) = node { bb.borrow_mut() } else { panic!() };
			bb.inner_graph.add(self, instr, &inner_args)
		};
		NodeRef::<Index>(self.nodeindex, n)
	}
}

#[cfg(test)]
mod test {
	use std::fs::File;
	use std::io::Write;

	use super::super::traits::InstructionType;
	use super::inner::HasAdd;
	use super::{AddIR, DefaultInnerIndex, IRGraph};
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

		let (_, i0, i1) = {
			let mut bb = graph.add_and_select_bb();
			let i0 = bb.add_instr(TestInstr::NotPhi, &[]);
			let i1 = bb.add_instr(TestInstr::NotPhi, &[i0, i0]);
			(bb.nodeindex, i0, i1)
		};

		{
			let mut bb = graph.add_and_select_bb();
			let i2 = bb.add_instr(TestInstr::NotPhi, &[i0, i1]);
		}

		let mut dot_file = File::create("ssa.dot").unwrap();
		dot_file.write_all(&dot::dot(&graph)).unwrap();
	}
}
