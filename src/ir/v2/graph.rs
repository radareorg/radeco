use std::ops::Deref;

use super::super::petgraph::graph::{Edge, EdgeIndex, Graph, NodeIndex};

use super::basicblock::BasicBlock;
use super::index::{InnerIndexType, KnowsIndexType};

type DefaultInnerIndex = i16;

//Instruction = Instruction<InternalEdge<DefaultInnerIndex>>

enum IRNode<Instruction> {
	// Represents an operation of a basic block that is used by non-phi nodes outside of that basic block
	Repr,
	// Represents a basic block
	BasicBlock(BasicBlock<Instruction>)
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

// rename this?
struct InternalEdge<I> {
	// target of this edge
	target: I,
	// the next item with an edge to the same target
	next: I,
}

impl<I: InnerIndexType> KnowsIndexType for InternalEdge<I> {
	type I = I;
}

/*impl<Opnd: KnowsIndexType> KnowsIndexType for Instruction<Opnd> {
	type I = Opnd::I;
}*/

struct NodeRef<I>(NodeIndex, I);

// replace NodeRef below with &[InternalEdge]?
// probably, yes along with a distinction betwen &Graph and &mut Graph throughout this code
struct UseSubIterator<I>(NodeRef<I>, u8);
struct ArgSubIterator<I>(NodeRef<I>, u8);

impl<I: InnerIndexType> Iterator for UseSubIterator<I> {
	type Item = NodeRef<I>;
	fn next(&mut self) -> Option<NodeRef<I>> {
		Option::None // TODO
	}	
}

impl<I: InnerIndexType> Iterator for ArgSubIterator<I> {
	type Item = NodeRef<I>;
	fn next(&mut self) -> Option<NodeRef<I>> {
		let total_args = 0;
		while self.1 < total_args {
			// TODO
		}
		Option::None // TODO
	}	
}

// Iterator over users of an operation (the cases of this enum are implementation details)
enum UseIterator<A, B, G: Deref<Target=Graph<A, B>>, I> {
	// Iterator is currently on a user in the same basic block
	UseInner(G, UseSubIterator<I>),
	// Iterator is currently on a non-phi user in another basic block
	UseExternal(G, UseSubIterator<I>, EdgeIndex, EdgeIndex),
	// Iterator is currently on a user that's a phi node
	UsePhi(G, EdgeIndex)
}

// Iterator over users of an operation
enum ArgIterator<I> {
	ArgPhi(I, EdgeIndex),
	ArgOp(I, u8)
}

impl<A, B, G, I: InnerIndexType> Iterator for UseIterator<A, B, G, I> {
	type Item = NodeRef<I>;
	fn next(&mut self) -> Option<NodeRef<I>> {
		let mut o: Option<NodeRef<I>> = Option::None;
		loop {
			match *self {
				UseIterator::UseInner (g, ref mut si) => {
					// (g, si, EdgeIndex::end()),
				}
				UseIterator::UseExternal (g, ref mut si, ei, ej) => {
					// (g, si, ei)
				}
				UseIterator::UsePhi (g, ei) => {
					// (g, _, ei)
				}
			};
			/*if let Option::Some(nr) = si.next() {
				return Option::Some(nr)
			}
			if e == EdgeIndex::end() { e = g.first_edge(si.0, Incoming) }
			match *edge {
				IREdge::ReprToBlock(i)    => self = UseIterator::UsePhi(),
				IREdge::BlockToRepr(i, j) => self = UseIterator::UsePhi(),
				IREdge::Flow(vi) =>
			}*/
		}
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
