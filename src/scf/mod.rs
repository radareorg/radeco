/* SCF = Structured control flow */

use std::collections::BTreeMap;
use std::marker::PhantomData;
use std::fmt::Debug;

pub trait SCFDomain {
	type Declaration: Debug;
	type Expression: Debug;
	type Statement: Debug;
	type Node;
}

pub struct MutRefDomain<'x> { _marker: PhantomData<&'x ()> }
pub struct BoxDomain;

impl<'x> SCFDomain for MutRefDomain<'x> {
	type Declaration = i32;
	type Expression = i32;
	type Statement = i32;
	type Node = &'x mut SCFNode<MutRefDomain<'x>>;
}

impl SCFDomain for BoxDomain {
	type Declaration = i32;
	type Expression = i32;
	type Statement = i32;
	type Node = Box<SCFNode<BoxDomain>>;
}

#[derive(Debug)]
pub enum ForInitClause<D: SCFDomain> {
	InitDeclaration(D::Declaration),
	InitExpression(D::Expression)
}

#[derive(Debug)]
pub enum SCFNode<D: SCFDomain> {
	Empty,
	Statement (D::Statement),
	Seq {
		body: Vec<SCFNode<D>>,
		noreturn: bool
	},
	Cond {
		cond: D::Expression,
		body: D::Node,
		alt:  D::Node
	},
	Switch {
		selector: D::Expression,
		cases:    BTreeMap<u64, D::Node>,
		default:  D::Node
	},
	While {
		cond: D::Expression,
		body: D::Node
	},
	DoWhile {
		cond: D::Expression,
		body: D::Node
	},
	For {
		init: ForInitClause<D>,
		cond: D::Expression,
		step: D::Expression,
		body: D::Node
	}
}
