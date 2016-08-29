//! Simple basic model of the C-AST
//!
//! This is the skeleton of the final decompiled output. However, further
//! stages maybe added to
//! make the decompiled output easier to read and add more sugaring.

use petgraph::graph::{Graph, NodeIndex};
use petgraph::EdgeDirection;
use std::default;
use std::iter;

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

fn format_with_indent(string: &str, depth: usize) -> String {
    iter::repeat(' ').take(depth).collect::<String>() + string
}

impl CAST {
    fn next_edge_idx(&mut self) -> u64 {
        self.eidx += 1;
        self.eidx - 1
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

    pub fn new_conditional(&mut self,
                           condition: NodeIndex,
                           body: NodeIndex,
                           else_condition: Option<NodeIndex>)
                           -> NodeIndex {
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

    pub fn label(&mut self, label: String) {
        let label = self.ast.add_node(CASTNode::Label(label));
        let idx = self.next_edge_idx();
        self.ast.add_edge(*FN_HEAD, label, ASTEdge::StatementOrd(idx));
    }

    pub fn insert_break(&mut self, after: NodeIndex) {
        let break_n = self.ast.add_node(CASTNode::Break);
        let idx = self.next_edge_idx();
        self.ast.add_edge(*FN_HEAD, break_n, ASTEdge::StatementOrd(idx));
    }



    fn emit_c(&self, node: &NodeIndex, indent: usize) -> String {
        match self.ast[*node] {
            CASTNode::FunctionHeader => unimplemented!(),
            CASTNode::Conditional => {
                // Get the arguments -> condition, body, else branch.
                let arg1: NodeIndex = panic!();
                let arg2: NodeIndex = panic!();
                let arg3: Option<NodeIndex> = None;
                let condition = format!("{}\n{}",
                                        format_with_indent("if ( {} ) {{", indent),
                                        self.emit_c(&arg1, indent));
                let true_body = self.emit_c(&arg2, indent + 1);
                let false_body = if let Some(arg3) = arg3 {
                    let fbody = self.emit_c(&arg3, indent + 1);
                    if let CASTNode::Conditional = self.ast[arg3] {
                        // Else-If case
                        format_with_indent("}} else ", indent) + &fbody
                    } else {
                        // Plain Else case.
                        format_with_indent("}} else {{", indent) + &fbody
                    }
                } else {
                    "".to_owned()
                };

                format!("{}\n{}\n{}\n{}",
                        condition,
                        true_body,
                        false_body,
                        format_with_indent("}", indent))
            }
            CASTNode::Declaration(ref ty) => {
                unimplemented!()
            }
            CASTNode::Loop => {
                unimplemented!()
            }
            CASTNode::Goto(ref label) => {
                unimplemented!()
            }
            CASTNode::Label(ref label) => {
                unimplemented!()
            }
            CASTNode::Break => {
                unimplemented!()
            }
            CASTNode::ExpressionNode(ref expr) => {
                unimplemented!()
            }
            CASTNode::Var(ref ident) => {
                unimplemented!()
            }
        }
    }

    pub fn print(&self) -> String {
        // Get all the edges from the function header.
        // Take them in order
        // Traverse the subtree and print out accordingly.
        let mut result = String::new();
        let mut edges = self.ast
                            .edges_directed(*FN_HEAD, EdgeDirection::Outgoing)
                            .collect::<Vec<_>>();
        edges.sort_by(|a, b| {
            let idx1 = if let ASTEdge::StatementOrd(idx) = *a.1 {
                idx
            } else {
                u64::max_value()
            };

            let idx2 = if let ASTEdge::StatementOrd(idx) = *b.1 {
                idx
            } else {
                u64::max_value()
            };

            idx1.cmp(&idx2)
        });

        for &(ref node, _) in &edges {
            result.push_str(&self.emit_c(node, 0));
        }

        result
    }
}

#[cfg(test)]
mod test {

    #[test]
    fn c_ast_basic_test() {
        unimplemented!()
    }
}
