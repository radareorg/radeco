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
    Goto(String),
    Label(String),
    Break,
    ExpressionNode(Expr),
    Var(String),
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
    StatementOrd(u64),
    OpOrd(u8),
}

#[derive(Clone, Debug)]
pub struct CAST {
    ast: Graph<CASTNode, ASTEdge>,
    eidx: u64,
}

impl default::Default for CAST {
    fn default() -> CAST {
        let mut ast = CAST {
            ast: Graph::new(),
            eidx: 0,
        };
        ast.ast.add_node(CASTNode::FunctionHeader);
        ast
    }
}

lazy_static! {
    static ref FN_HEAD: NodeIndex = NodeIndex::new(0);
}

impl CAST {
    fn next_edge_idx(&mut self) -> u64 {
        self.eidx += 1;
        self.eidx - 1
    }

    fn replace_incoming_edges(node: NodeIndex, by: NodeIndex) {
        unimplemented!()
    }

    pub fn expr(&mut self, operator: Expr, operands: &[NodeIndex]) -> NodeIndex {
        let operator = self.ast.add_node(CASTNode::ExpressionNode(operator));
        let idx = self.next_edge_idx();
        self.ast.add_edge(*FN_HEAD, operator, ASTEdge::StatementOrd(idx));
        for (i, n) in operands.iter().enumerate() {
            self.ast.add_edge(operator, *n, ASTEdge::OpOrd(i as u8));
        }
        operator
    }

    pub fn new_loop(&mut self, loop_header: NodeIndex, loop_body: NodeIndex) -> NodeIndex {
        let e1 = self.ast.find_edge(*FN_HEAD, loop_header).expect("This cannot be `None`");
        let e2 = self.ast.find_edge(*FN_HEAD, loop_body).expect("This cannot be `None`");
        let loop_h = self.ast.add_node(CASTNode::Loop);
        let idx = if let ASTEdge::StatementOrd(idx) = self.ast[e1] {
            idx
        } else {
            panic!()
        };
        self.ast.remove_edge(e2);
        self.ast.remove_edge(e1);
        self.ast.add_edge(*FN_HEAD, loop_h, ASTEdge::StatementOrd(idx));
        self.ast.add_edge(loop_h, loop_header, ASTEdge::OpOrd(0));
        self.ast.add_edge(loop_h, loop_body, ASTEdge::OpOrd(1));
        loop_h
    }

    pub fn new_conditional(&mut self, condition: NodeIndex, body: NodeIndex, else_condition: Option<NodeIndex>) -> NodeIndex {
        let e1 = self.ast.find_edge(*FN_HEAD, condition).expect("This cannot be `None`");
        let e2 = self.ast.find_edge(*FN_HEAD, body).expect("This cannot be `None`");
        let e3 = if let Some(else_c) = else_condition {
            self.ast.find_edge(*FN_HEAD, else_c)
        } else {
            None
        };
        let idx = if let ASTEdge::StatementOrd(idx) = self.ast[e1] {
            idx
        } else {
            panic!()
        };

        let conditional = self.ast.add_node(CASTNode::Conditional);
        self.ast.remove_edge(e1);
        self.ast.remove_edge(e2);
        if let Some(e3) = e3 {
            self.ast.remove_edge(e3);
        }

        self.ast.add_edge(*FN_HEAD, conditional, ASTEdge::StatementOrd(idx));
        self.ast.add_edge(conditional, condition, ASTEdge::OpOrd(0));
        self.ast.add_edge(conditional, body, ASTEdge::OpOrd(1));
        if let Some(else_condition) = else_condition {
            self.ast.add_edge(conditional, else_condition, ASTEdge::OpOrd(2));
        }
        conditional
    }

    pub fn goto(&mut self, label: String) {
        let goto_n = self.ast.add_node(CASTNode::Goto(label));
        let idx = self.next_edge_idx();
        self.ast.add_edge(*FN_HEAD, goto_n, ASTEdge::StatementOrd(idx));
    }

    pub fn label(label: String) {
        let label = self.ast.add_node(CASTNode::Label(label));
        let idx = self.next_edge_idx();
        self.ast.add_edge(*FN_HEAD, label, ASTEdge::StatementOrd(idx));
    }

    pub fn insert_break(after: NodeIndex) {
        let break_n = self.ast.add_node(CASTNode::Label(label));
        let idx = self.next_edge_idx();
        self.ast.add_edge(*FN_HEAD, break_n, ASTEdge::StatementOrd(idx));
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
