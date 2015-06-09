use super::basicblock::BasicBlock;
use super::instruction::Instruction;
use super::petgraph::graph::{Edge, EdgeIndex, Graph, NodeIndex};
use super::{InnerIndexType, KnowsIndexType};

type DefaultInnerIndex = i16;

enum IRNode {
	BasicBlock(BasicBlock<Instruction<InternalEdge<DefaultInnerIndex>>>),
	Ref(DefaultInnerIndex)
}

enum IREdge {
    RefToBlock,
    BlockToRef(DefaultInnerIndex, DefaultInnerIndex),
    Flow(Vec<DefaultInnerIndex>, u64)
}

type IRGraph = Graph<IRNode, IREdge>;

// rename this?
struct InternalEdge<I> {
	/// target of this edge
	target: I,
	/// the next item with an edge to the same target
	next: I,
}

impl<I: InnerIndexType> KnowsIndexType for InternalEdge<I> {
	type I = I;
}

pub struct NodeRef<I>(NodeIndex, I);
// replace NodeRef below with &[InternalEdge]?
pub struct UseSubIterator<I>(NodeRef<I>, u8);
pub struct ArgSubIterator<I>(NodeRef<I>, u8);

pub enum UseIterator<I> {
	UseInner(I, UseSubIterator<I>),
	UsePhi(I, EdgeIndex),
	UseExternal(I, EdgeIndex, EdgeIndex, UseSubIterator<I>)
}

pub enum ArgIterator<I> {
	ArgPhi(I, EdgeIndex),
	ArgOp(I, u8)
}

impl<I: InnerIndexType> Iterator for UseIterator<I> {
	type Item = NodeRef<I>;
	fn next(&mut self) -> Option<NodeRef<I>> {
		match *self {
			UseIterator::UseInner(i, ref si) => panic!(),
			UseIterator::UsePhi(i, ei) => panic!(),
			UseIterator::UseExternal(i, er, eu, ref j) => panic!()
		} // or get next
	}
}

impl<I: InnerIndexType> Iterator for ArgIterator<I> {
	type Item = NodeRef<I>;
	fn next(&mut self) -> Option<NodeRef<I>> {
		match *self {
			ArgIterator::ArgPhi(i, ei) => panic!(),
			ArgIterator::ArgOp(i, oi) => panic!()
		}
	}
}

/*
impl NodeRef<I> {
	fn uses() -> UseIterator<I> {

	}
	fn args() -> ArgIterator<I> {

	}
}
*/
