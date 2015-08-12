/// Data structures and algorithms for control flow restructuring.
// SCF = Structured control flow

use std::collections::BTreeMap;

pub type AST_ = Box<AST>;

pub type RUnit = u32;
pub type RCond = u32;
pub type RExpr = u32;

pub enum LoopType {
	While,
	DoWhile,
	ForEver,
}

pub enum AST {
	Unit(RUnit),

	Seq(Vec<AST_>),
	Cond(RCond, AST_, AST_),
	Loop(LoopType, RCond, AST_),
	Switch(RExpr, BTreeMap<u64, AST_>, AST_),
}
