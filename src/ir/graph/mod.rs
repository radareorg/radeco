extern crate petgraph;

pub mod basicblock;
pub mod inner;
pub mod traits;

use std::ops::Deref;

use self::basicblock::BasicBlock;
use self::inner::InnerEdgeTrait;
use self::petgraph::graph::{Edge, EdgeIndex, Graph, NodeIndex};
use self::traits::NavigationInternal;

type DefaultInnerIndex = i16;

//Instruction = Instruction<InternalEdge<DefaultInnerIndex>>

enum IRNode<Instruction> {
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

impl NavigationInternal<NodeRef> for IRGraph {
	fn add_uses_to(node: NodeRef) -> Vec<NodeRef> {
		let v: Vec<NodeRef>;
		if (node.is_phi()) {
			//v.re
		} else {

		}
		return v;
	}

	fn add_args_to(node: NodeRef) -> Vec<NodeRef> {
		let v: Vec<NodeRef>;
		return v;
	}
}
