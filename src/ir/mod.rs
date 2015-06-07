extern crate petgraph;
pub mod basicblock;

use self::petgraph::graph::Graph;
use self::basicblock::BasicBlock;

pub type IRIndex = u32;
#[allow(non_camel_case_types)]
pub type Vec_IRIndex = Vec<IRIndex>;
pub type BranchId = u64;

// the 'a here will disappear soon
pub enum IRNode<'a> {
	BasicBlock(BasicBlock<'a>),
	Ref(IRIndex)
}

pub enum IREdge {
    RefToBlock,
    BlockToRef(Vec_IRIndex),
    Flow(Vec_IRIndex, BranchId)
}

pub type IRGraph<'a> = Graph<IRNode<'a>, IREdge>;
