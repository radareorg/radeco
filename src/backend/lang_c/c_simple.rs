//! Simple basic model of the C-AST
//!
//! This is the skeleton of the final decompiled output. However, further
//! stages maybe added to
//! make the decompiled output easier to read and add more sugaring.

use std::{default, iter, fmt};

use petgraph::graph::{Graph, NodeIndex, EdgeIndex};
use petgraph::visit::{EdgeRef};
use petgraph::EdgeDirection;

//////////////////////////////////////////////////////////////////////////////
//// Declaration and implementation for basic C data types.
//////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug)]
/// Struct to describe a C-Type
pub struct Ty {
    base_type: BTy,
    signed: bool,
    long: u8,
}

impl Ty {
    pub fn new(base: BTy, signed: bool, long: u8) -> Ty {
        Ty {
            base_type: base,
            signed: signed,
            long: long,
        }
    }

    pub fn new_ptr(mut base: BTy, signed: bool, long: u8) -> Ty {
        base = BTy::Ptr(Box::new(base));
        Ty::new(base, signed, long)
    }

    pub fn new_array(mut base: BTy, signed: bool, long: u8, elements: u64) -> Ty {
        base = BTy::Array(Box::new(base), elements);
        Ty::new(base, signed, long)
    }
}

#[derive(Clone, Debug)]
/// Defines the basic valid C data types.
pub enum BTy {
    Int,
    Float,
    Char,
    Double,
    Void,
    /// Pointer to type `BTy`.
    Ptr(Box<BTy>),
    /// Array of type `BTy` and length.
    Array(Box<BTy>, u64),
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut result = format!("{}", self.base_type);

        if self.long > 0 {
            result = format!("long {}", result);
        } else if self.long > 1 {
            result = format!("long long {}", result);
        }

        if self.signed {
            result = format!("{}", result);
        } else {
            result = format!("unsigned {}", result);
        }

        write!(f, "{}", result)
    }
}

impl fmt::Display for BTy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let result = match *self {
            BTy::Int => "int".to_owned(),
            BTy::Float => "float".to_owned(),
            BTy::Char => "char".to_owned(),
            BTy::Double => "double".to_owned(),
            BTy::Void => "void".to_owned(),
            BTy::Ptr(ref bty) => format!("*{}", bty),
            BTy::Array(ref bty, count) => format!("{}[{}]", bty, count - 1),
        };
        write!(f, "{}", result)
    }
}

//////////////////////////////////////////////////////////////////////////////
//// Declaration and implementation for C AST (Nodes and Edge Types).
//////////////////////////////////////////////////////////////////////////////

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
    // Constant
    Constant(Ty, String),
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
enum CASTEdge {
    /// Used to ensure ordering of statements within a function. Lower index corresponds to a
    /// statement that should appear earlier in the output.
    StatementOrd(u64),
    /// Used to ensure ordering of operands in an expression. Operands are ordered from lower to
    /// higher index.
    OpOrd(u8),
}

const LHS: CASTEdge = CASTEdge::OpOrd(0);
const RHS: CASTEdge = CASTEdge::OpOrd(1);

#[derive(Clone, Debug)]
pub struct CAST {
    /// Internal graph that stores the AST.
    ast: Graph<CASTNode, CASTEdge>,
    /// Numbering of StatementOrd edges
    eidx: u64,
    /// Identifier for the function header.
    fn_head: NodeIndex,
}

impl default::Default for CAST {
    fn default() -> CAST {
        let mut ast = CAST {
            ast: Graph::new(),
            eidx: 0,
            fn_head: NodeIndex::end(),
        };
        ast.fn_head = ast.ast.add_node(CASTNode::FunctionHeader("unknown".to_owned()));
        ast
    }
}

// Function used to add indent levels to strings.
// NOTE: 1 level indent  = 4 spaces.
const INDENT_UNIT: char = ' ';
const INDENT_SHIFT: u8 = 4;
fn format_with_indent(string: &str, depth: usize) -> String {
    iter::repeat(INDENT_UNIT).take(depth * INDENT_SHIFT as usize).collect::<String>() + string
}

//////////////////////////////////////////////////////////////////////////////
//// Implementation to manipulate the C AST effectively.
//////////////////////////////////////////////////////////////////////////////
impl CAST {
    // TODO: Add return types and arguments to function header.
    pub fn new(fn_name: &str) -> CAST {
        let mut ast = CAST {
            ast: Graph::new(),
            eidx: 0,
            fn_head: NodeIndex::end(),
        };
        ast.fn_head = ast.ast.add_node(CASTNode::FunctionHeader(fn_name.to_owned()));
        ast
    }

    fn next_edge_idx(&mut self) -> u64 {
        self.eidx += 1;
        self.eidx - 1
    }

    fn get_statement_ord(&self, e: EdgeIndex) -> u64 {
        if let CASTEdge::StatementOrd(idx) = self.ast[e] {
            idx
        } else {
            u64::max_value()
        }
    }

    fn get_args_ordered(&self, node: &NodeIndex) -> Vec<NodeIndex> {
        let mut args = self.ast
            .edges_directed(*node, EdgeDirection::Outgoing)
            .filter(|x| if let CASTEdge::OpOrd(_) = *x.weight() {
                true
            } else {
                false
            })
            .collect::<Vec<_>>();
        args.sort_by(|a, b| {
            let idx1 = if let CASTEdge::OpOrd(idx) = *a.weight() {
                idx
            } else {
                u8::max_value()
            };

            let idx2 = if let CASTEdge::OpOrd(idx) = *b.weight() {
                idx
            } else {
                u8::max_value()
            };

            idx1.cmp(&idx2)
        });
        args.iter().map(|x| x.target()).collect()
    }

    pub fn expr(&mut self, operator: Expr, operands: &[NodeIndex]) -> NodeIndex {
        let operator = self.ast.add_node(CASTNode::ExpressionNode(operator));
        let idx = self.next_edge_idx();
        self.ast.add_edge(self.fn_head, operator, CASTEdge::StatementOrd(idx));
        for (i, n) in operands.iter().enumerate() {
            if let Some(edx) = self.ast.find_edge(self.fn_head, *n) {
                self.ast.remove_edge(edx);
            }
            self.ast.add_edge(operator, *n, CASTEdge::OpOrd(i as u8));
        }
        operator
    }

    pub fn new_loop(&mut self, loop_header: NodeIndex, loop_body: NodeIndex) -> NodeIndex {
        let e1 = self.ast.find_edge(self.fn_head, loop_header).expect("This cannot be `None`");
        let e2 = self.ast.find_edge(self.fn_head, loop_body).expect("This cannot be `None`");
        let loop_h = self.ast.add_node(CASTNode::Loop);
        let idx = self.get_statement_ord(e1);
        self.ast.remove_edge(e2);
        self.ast.remove_edge(e1);
        self.ast.add_edge(self.fn_head, loop_h, CASTEdge::StatementOrd(idx));
        self.ast.add_edge(loop_h, loop_header, CASTEdge::OpOrd(0));
        self.ast.add_edge(loop_h, loop_body, CASTEdge::OpOrd(1));
        loop_h
    }

    pub fn new_conditional(&mut self,
                           condition: NodeIndex,
                           body: NodeIndex,
                           else_condition: Option<NodeIndex>)
                           -> NodeIndex {
        let e3 = if let Some(else_c) = else_condition {
            self.ast.find_edge(self.fn_head, else_c)
        } else {
            None
        };

        if let Some(e3) = e3 {
            self.ast.remove_edge(e3);
        }

        let conditional = self.ast.add_node(CASTNode::Conditional);
        let e1 = self.ast.find_edge(self.fn_head, condition).expect("This cannot be `None`");
        let idx = self.get_statement_ord(e1);
        self.ast.remove_edge(e1);

        let e2 = self.ast.find_edge(self.fn_head, body).expect("This cannot be `None`");
        self.ast.remove_edge(e2);

        self.ast.add_edge(self.fn_head, conditional, CASTEdge::StatementOrd(idx));
        self.ast.add_edge(conditional, condition, CASTEdge::OpOrd(0));
        self.ast.add_edge(conditional, body, CASTEdge::OpOrd(1));
        if let Some(else_condition) = else_condition {
            self.ast.add_edge(conditional, else_condition, CASTEdge::OpOrd(2));
        }
        conditional
    }

    pub fn goto(&mut self, label: String) {
        let goto_n = self.ast.add_node(CASTNode::Goto(label));
        let idx = self.next_edge_idx();
        self.ast.add_edge(self.fn_head, goto_n, CASTEdge::StatementOrd(idx));
    }

    pub fn label(&mut self, label: String) {
        let label = self.ast.add_node(CASTNode::Label(label));
        let idx = self.next_edge_idx();
        self.ast.add_edge(self.fn_head, label, CASTEdge::StatementOrd(idx));
    }

    pub fn insert_break(&mut self, after: NodeIndex) {
        let break_n = self.ast.add_node(CASTNode::Break);
        let idx = self.next_edge_idx();
        self.ast.add_edge(self.fn_head, break_n, CASTEdge::StatementOrd(idx));
    }

    pub fn declare_vars(&mut self, ty: Ty, vars: &[String]) -> Vec<NodeIndex> {
        let decl = self.ast.add_node(CASTNode::Declaration(ty));
        let mut var_decls = Vec::new();
        for (i, v) in vars.iter().enumerate() {
            let vn = self.ast.add_node(CASTNode::Var((v.clone())));
            self.ast.add_edge(decl, vn, CASTEdge::OpOrd(i as u8));
            var_decls.push(vn);
        }
        let idx = self.next_edge_idx();
        self.ast.add_edge(self.fn_head, decl, CASTEdge::StatementOrd(idx));
        var_decls
    }

    pub fn function_args(&mut self, args: &[(Ty, String)]) -> Vec<NodeIndex> {
        let mut arg_nodes = Vec::new();
        for (i, &(ref t, ref named)) in args.iter().enumerate() {
            let decl = self.ast.add_node(CASTNode::Declaration(t.clone()));
            self.ast.add_edge(self.fn_head, decl, CASTEdge::OpOrd(i as u8));
            let arg_node = self.ast.add_node(CASTNode::Var(named.clone()));
            self.ast.add_edge(decl, arg_node, CASTEdge::OpOrd(0));
            arg_nodes.push(arg_node);
        }
        arg_nodes
    }

    fn emit_c(&self, node: &NodeIndex, indent: usize) -> String {
        match self.ast[*node] {
            CASTNode::FunctionHeader(_) => unimplemented!(),
            CASTNode::Conditional => {
                // Get the arguments -> condition, body, else branch.
                let args = self.get_args_ordered(node);
                let arg1 = args[0];
                let arg2 = args[1];
                let arg3 = args.get(2).map(|x| x);
                let condition = format!("{} {} {{\n",
                                        format_with_indent("if", indent),
                                        self.emit_c(&arg1, 0));
                let true_body = self.emit_c(&arg2, indent + 1);
                let false_body = if let Some(arg3) = arg3 {
                    let fbody = self.emit_c(&arg3, indent + 1);
                    if let CASTNode::Conditional = self.ast[*arg3] {
                        // Else-If case
                        format_with_indent("\n}} else ", indent) + &fbody
                    } else {
                        // Else case.
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
                let ty = format_with_indent(&ty.to_string(), indent);
                let mut vars = String::new();
                for op in self.ast.edges_directed(*node, EdgeDirection::Outgoing) {
                    if let CASTNode::Var(ref name) = self.ast[op.target()] {
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
                let args = self.get_args_ordered(node);
                let loop_header = self.emit_c(&args[0], 0);
                let loop_body = self.emit_c(&args[1], indent + 1);
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
                let operands = self.get_args_ordered(node);
                let op_str = operands.iter().map(|x| self.emit_c(&x, 0)).collect::<Vec<_>>();
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
            CASTNode::Constant(_, ref value) => {
                value.clone()
            }
        }
    }

    pub fn print(&self) -> String {
        // Get all the edges from the function header.
        // Take them in order
        // Traverse the subtree and print out accordingly.
        let mut result = String::new();
        if let CASTNode::FunctionHeader(ref named) = self.ast[self.fn_head] {
            let args = self.get_args_ordered(&self.fn_head);
            let mut args_string = String::new();
            for arg in args {
                let mut arg_s = String::new();
                if let CASTNode::Declaration(ref ty) = self.ast[arg] {
                    for op in self.ast.edges_directed(arg, EdgeDirection::Outgoing) {
                        if let CASTNode::Var(ref name) = self.ast[op.target()] {
                            arg_s = format!("{} {}", ty.to_string(), name);
                        }
                    }
                }
                if !args_string.is_empty() {
                    args_string.push_str(", ");
                }
                args_string = format!("{}{}", args_string, arg_s);
            }
            result.push_str(&format!("fn {} ({}) {{\n", named, args_string));
        }
        let mut edges = self.ast
                            .edges_directed(self.fn_head, EdgeDirection::Outgoing)
                            .filter(|x| if let CASTEdge::StatementOrd(_) = *x.weight() {
                                true
                            } else {
                                false
                            })
                            .collect::<Vec<_>>();
        edges.sort_by(|a, b| {
            let idx1 = if let CASTEdge::StatementOrd(idx) = *a.weight() {
                idx
            } else {
                u64::max_value()
            };

            let idx2 = if let CASTEdge::StatementOrd(idx) = *b.weight() {
                idx
            } else {
                u64::max_value()
            };

            idx1.cmp(&idx2)
        });

        for edge in &edges {
            result.push_str(&(self.emit_c(&edge.target(), 1) + "\n"));
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
        let mut c_ast = CAST::new("main");
        c_ast.function_args(&[(Ty::new(BTy::Int, false, 0), "x".to_owned())]);
        let vars = c_ast.declare_vars(Ty::new(BTy::Int, false, 0), &["i".to_owned(), "j".to_owned()]);
        let cmp = c_ast.expr(Expr::Cmp, &vars);
        let increment = c_ast.expr(Expr::Add, &vars);
        let assignment = c_ast.expr(Expr::Eq, &[vars[0], increment]);
        c_ast.new_conditional(cmp, assignment, None);
        println!("{}", c_ast.print());
    }
}
