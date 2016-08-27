//! Simple basic model of the C-AST
//!
//! This is the skeleton of the final decompiled output. However, further stages maybe added to
//! make the decompiled output easier to read and add more sugaring.

use petgraph::graph::{Graph, NodeIndex};
use std::default;

pub type Ty = String;

#[derive(Clone, Debug)]
enum CASTNode {
    FunctionHeader,
    Conditional,
    Declaration(Ty),
    Loop,
    Goto,
    Break,
    ExpressionNode(Expr),
}

#[derive(Clone, Debug)]
pub enum Expr {
    Eq,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Shr,
    Shl,
}

#[derive(Clone, Debug)]
enum ASTEdge {
    StatementOrd(u8),
    EdgeOrd(u8),
}

#[derive(Clone, Debug)]
pub struct CAST {
    ast: Graph<CASTNode, ASTEdge>,
}

impl default::Default for CAST {
    fn default() -> CAST {
        CAST {
            ast: Graph::new(),
        }
    }
}

impl CAST {
    pub fn expr(operator: Expr, lhs: NodeIndex, rhs: NodeIndex) -> NodeIndex {
        unimplemented!()
    }

    pub fn new_loop(loop_header: NodeIndex, loop_body: NodeIndex) -> NodeIndex {
        unimplemented!()
    }

    pub fn new_conditional(condition: NodeIndex, body: NodeIndex, else_condition: NodeIndex) -> NodeIndex {
        unimplemented!()
    }

    pub fn goto(label: String) -> NodeIndex {
        unimplemented!()
    }

    pub fn insert_break(after: NodeIndex) -> NodeIndex {
        unimplemented!()
    }

    pub fn print() -> String {
        unimplemented!()
    }
}

#[cfg(test)]
mod test {

    #[test]
    fn c_ast_basic_test() {
        unimplemented!()
    }
}
