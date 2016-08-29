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
    FunctionHeader(String),
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
    Xor,
    Or,
    And,
    Not,
    Neg,
    Gt,
    GtEq,
    Lt,
    LtEq,
    Cmp,
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
        ast.ast.add_node(CASTNode::FunctionHeader("Unknown".to_owned()));
        ast
    }
}

lazy_static! {
    static ref FN_HEAD: NodeIndex = NodeIndex::new(0);
}

fn format_with_indent(string: &str, depth: usize) -> String {
    iter::repeat(' ').take(depth * 4).collect::<String>() + string
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
            if let Some(edx) = self.ast.find_edge(*FN_HEAD, *n) {
                self.ast.remove_edge(edx);
            }
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
        let e3 = if let Some(else_c) = else_condition {
            self.ast.find_edge(*FN_HEAD, else_c)
        } else {
            None
        };

        if let Some(e3) = e3 {
            self.ast.remove_edge(e3);
        }

        let conditional = self.ast.add_node(CASTNode::Conditional);
        let e1 = self.ast.find_edge(*FN_HEAD, condition).expect("This cannot be `None`");
        let idx = if let ASTEdge::StatementOrd(idx) = self.ast[e1] {
            idx
        } else {
            panic!()
        };
        self.ast.remove_edge(e1);

        let e2 = self.ast.find_edge(*FN_HEAD, body).expect("This cannot be `None`");
        self.ast.remove_edge(e2);

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

    pub fn declare_vars(&mut self, ty: String, vars: &[String]) -> Vec<NodeIndex> {
        let decl = self.ast.add_node(CASTNode::Declaration(ty));
        let mut var_decls = Vec::new();
        for (i, v) in vars.iter().enumerate() {
            let vn = self.ast.add_node(CASTNode::Var(v.clone()));
            self.ast.add_edge(decl, vn, ASTEdge::OpOrd(i as u8));
            var_decls.push(vn);
        }
        let idx = self.next_edge_idx();
        self.ast.add_edge(*FN_HEAD, decl, ASTEdge::StatementOrd(idx));
        var_decls
    }

    fn emit_c(&self, node: &NodeIndex, indent: usize) -> String {
        match self.ast[*node] {
            CASTNode::FunctionHeader(_) => unimplemented!(),
            CASTNode::Conditional => {
                // Get the arguments -> condition, body, else branch.
                let mut args = self.ast
                                   .edges_directed(*node, EdgeDirection::Outgoing)
                                   .collect::<Vec<_>>();
                args.sort_by(|a, b| {
                    let idx1 = if let ASTEdge::OpOrd(idx) = *a.1 {
                        idx
                    } else {
                        u8::max_value()
                    };

                    let idx2 = if let ASTEdge::OpOrd(idx) = *b.1 {
                        idx
                    } else {
                        u8::max_value()
                    };

                    idx1.cmp(&idx2)
                });
                let arg1 = args[0].0;
                let arg2 = args[1].0;
                let arg3 = args.get(2).map(|x| x.0);
                let condition = format!("{} {} {{\n",
                                        format_with_indent("if", indent),
                                        self.emit_c(&arg1, 0));
                let true_body = self.emit_c(&arg2, indent + 1);
                let false_body = if let Some(arg3) = arg3 {
                    let fbody = self.emit_c(&arg3, indent + 1);
                    if let CASTNode::Conditional = self.ast[arg3] {
                        // Else-If case
                        format_with_indent("\n}} else ", indent) + &fbody
                    } else {
                        // Plain Else case.
                        format_with_indent("\n}} else {{", indent) + &fbody
                    }
                } else {
                    "".to_owned()
                };

                format!("{}{}{}\n{}",
                        condition,
                        true_body,
                        false_body,
                        format_with_indent("}", indent))
            }
            CASTNode::Declaration(ref ty) => {
                let mut ty = format_with_indent(ty, indent);
                let mut vars = String::new();
                for (ref op, _) in self.ast.edges_directed(*node, EdgeDirection::Outgoing) {
                    if let CASTNode::Var(ref name) = self.ast[*op] {
                        if vars.is_empty() {
                            vars = vars + name;
                        } else {
                            vars = vars + ", " + name;
                        }
                    }
                }
                format!("{} {};", ty, vars)
            }
            CASTNode::Loop => {
                // Get the arguments -> loop header/check condition, loop body.
                let mut args = self.ast
                                   .edges_directed(*node, EdgeDirection::Outgoing)
                                   .collect::<Vec<_>>();
                args.sort_by(|a, b| {
                    let idx1 = if let ASTEdge::OpOrd(idx) = *a.1 {
                        idx
                    } else {
                        u8::max_value()
                    };
                    let idx2 = if let ASTEdge::OpOrd(idx) = *b.1 {
                        idx
                    } else {
                        u8::max_value()
                    };
                    idx1.cmp(&idx2)
                });
                let loop_header = self.emit_c(&args[0].0, 0);
                let loop_body = self.emit_c(&args[1].0, indent + 1);
                format!("{} ({}) {{\n{}\n{}",
                        format_with_indent("while", indent),
                        loop_header,
                        loop_body,
                        format_with_indent("}", indent))
            }
            CASTNode::Goto(ref label) => {
                format_with_indent(&format!("goto {}", label), indent)
            }
            CASTNode::Label(ref label) => {
                format_with_indent(&format!("label {}:", label), indent)
            }
            CASTNode::Break => {
                format_with_indent("break;", indent)
            }
            CASTNode::ExpressionNode(ref expr) => {
                let mut operands = self.ast
                                       .edges_directed(*node, EdgeDirection::Outgoing)
                                       .collect::<Vec<_>>();
                operands.sort_by(|a, b| {
                    let idx1 = if let ASTEdge::OpOrd(idx) = *a.1 {
                        idx
                    } else {
                        u8::max_value()
                    };
                    let idx2 = if let ASTEdge::OpOrd(idx) = *b.1 {
                        idx
                    } else {
                        u8::max_value()
                    };
                    idx1.cmp(&idx2)
                });

                let op_str = operands.iter().map(|x| self.emit_c(&x.0, 0)).collect::<Vec<_>>();
                match *expr {
                    Expr::Eq => format!("{} = {}",
                                        format_with_indent(&op_str[0], indent),
                                        &op_str[1]),
                    Expr::Add => format!("({} + {})",
                                         format_with_indent(&op_str[0], indent),
                                         &op_str[1]),
                    Expr::Sub => format!("({} - {})",
                                         format_with_indent(&op_str[0], indent),
                                         &op_str[1]),
                    Expr::Mul => format!("({} * {})",
                                         format_with_indent(&op_str[0], indent),
                                         &op_str[1]),
                    Expr::Div => format!("({} / {})",
                                         format_with_indent(&op_str[0], indent),
                                         &op_str[1]),
                    Expr::Mod => format!("({} % {})",
                                         format_with_indent(&op_str[0], indent),
                                         &op_str[1]),
                    Expr::Shr => format!("({} >> {})",
                                         format_with_indent(&op_str[0], indent),
                                         &op_str[1]),
                    Expr::Shl => format!("({} << {})",
                                         format_with_indent(&op_str[0], indent),
                                         &op_str[1]),
                    Expr::Xor => format!("({} ^ {})",
                                         format_with_indent(&op_str[0], indent),
                                         &op_str[1]),
                    Expr::Or => format!("({} | {})",
                                        format_with_indent(&op_str[0], indent),
                                        &op_str[1]),
                    Expr::And => format!("({} & {})",
                                         format_with_indent(&op_str[0], indent),
                                         &op_str[1]),
                    Expr::Not => format!("(!{})", format_with_indent(&op_str[0], indent)),
                    Expr::Neg => format!("(~{})", format_with_indent(&op_str[0], indent)),
                    Expr::Gt => format!("({} > {})",
                                        format_with_indent(&op_str[0], indent),
                                        &op_str[1]),
                    Expr::GtEq => format!("({} >= {})",
                                          format_with_indent(&op_str[0], indent),
                                          &op_str[1]),
                    Expr::Lt => format!("({} < {})",
                                        format_with_indent(&op_str[0], indent),
                                        &op_str[1]),
                    Expr::LtEq => format!("({} <= {})",
                                          format_with_indent(&op_str[0], indent),
                                          &op_str[1]),
                    Expr::Cmp => format!("({} == {})",
                                         format_with_indent(&op_str[0], indent),
                                         &op_str[1]),
                }
            }
            CASTNode::Var(ref ident) => {
                ident.clone()
            }
        }
    }

    pub fn print(&self) -> String {
        // Get all the edges from the function header.
        // Take them in order
        // Traverse the subtree and print out accordingly.
        let mut result = String::new();
        if let CASTNode::FunctionHeader(ref named) = self.ast[*FN_HEAD] {
            result.push_str(&format!("fn {} {{\n", named));
        }
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
            println!("In loop");
            result.push_str(&(self.emit_c(node, 1) + "\n"));
        }

        result.push_str("}");
        result
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn c_ast_basic_test() {
        let mut c_ast = CAST::default();
        let vars = c_ast.declare_vars("int".to_owned(), &["i".to_owned(), "j".to_owned()]);
        let cmp = c_ast.expr(Expr::Cmp, &vars);
        let increment = c_ast.expr(Expr::Add, &vars);
        let assignment = c_ast.expr(Expr::Eq, &[vars[0], increment]);
        c_ast.new_conditional(cmp, assignment, None);
        println!("{}", c_ast.print());
    }
}
