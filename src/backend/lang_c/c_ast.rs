//! Simple basic model of the C-AST
//!
//! This is the skeleton of the final decompiled output. However, further
//! stages maybe added to
//! make the decompiled output easier to read and add more sugaring.

use std::{default, iter, fmt};
use std::collections::HashMap;

use petgraph::graph::{Graph, NodeIndex, EdgeIndex};
use petgraph::visit::EdgeRef;
use petgraph::{Direction, EdgeDirection};

use frontend::radeco_containers::RadecoFunction;
use super::c_cfg_builder;

//////////////////////////////////////////////////////////////////////////////
//// Declaration and implementation for basic C data types.
//////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
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

        if self.long == 1 {
            result = format!("long {}", result);
        } else if self.long > 1 {
            result = format!("long long {}", result);
        }

        if !self.signed {
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
pub enum CASTNode {
    FunctionHeader(String),
    If,
    Declaration(Ty),
    While,
    DoWhile,
    Goto(String),
    Label(String),
    Break,
    ExpressionNode(Expr),
    Var(String),
    // Constant
    Constant(Ty, String),
    Return(String),
    Call(String, Vec<String>),
    // Used in if-then, if-else body
    Block,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Assign,
    Add,
    //Add field `SignExt`, `ZeroExt`, `Narrow`
    Cast(usize),
    Sub,
    Mul,
    DeRef,
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
    Eq,
}

#[derive(Clone, Debug)]
enum CASTEdge {
    /// Used to ensure ordering of statements within a function. Lower index corresponds to a
    /// statement that should appear earlier in the output.
    StatementOrd(u64),
    /// Used to ensure ordering of operands in an expression. Operands are ordered from lower to
    /// higher index.
    OpOrd(u8),
    BlockOrd(u64),
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
    /// Debug information for given NodeIndex of statement,
    /// assignment, etc, but not of nested expressions.
    comments: HashMap<NodeIndex, String>,
}

impl default::Default for CAST {
    fn default() -> CAST {
        let mut ast = CAST {
            ast: Graph::new(),
            eidx: 0,
            fn_head: NodeIndex::end(),
            comments: HashMap::new(),
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
            comments: HashMap::new(),
        };
        ast.fn_head = ast.ast.add_node(CASTNode::FunctionHeader(fn_name.to_owned()));
        ast
    }

    pub fn construct(rfn: &RadecoFunction, fname_map: &HashMap<u64, String>,
                strings: &HashMap<u64, String>) -> CAST {
        let ast = c_cfg_builder::recover_simple_ast(&rfn, &fname_map, &strings);
        ast.to_c_ast()
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

    // Add no line in case is_implicit is true
    pub fn expr(&mut self, operator: Expr, operands: &[NodeIndex], is_implicit: bool) -> NodeIndex {
        let operator = self.ast.add_node(CASTNode::ExpressionNode(operator));
        let idx = self.next_edge_idx();
        if !is_implicit {
            self.ast.add_edge(self.fn_head, operator, CASTEdge::StatementOrd(idx));
        }
        for (i, n) in operands.iter().enumerate() {
            if let Some(edx) = self.ast.find_edge(self.fn_head, *n) {
                self.ast.remove_edge(edx);
            }
            self.ast.add_edge(operator, *n, CASTEdge::OpOrd(i as u8));
        }
        operator
    }

    pub fn new_while(&mut self, condition: NodeIndex, body: Vec<NodeIndex>) -> NodeIndex {
        let idx = if let Some(e1) = self.ast.find_edge(self.fn_head, condition) {
            let idx = self.get_statement_ord(e1);
            self.ast.remove_edge(e1);
            idx
        } else {
            self.next_edge_idx()
        };
        let while_h = self.ast.add_node(CASTNode::While);
        self.ast.add_edge(self.fn_head, while_h, CASTEdge::StatementOrd(idx));
        self.ast.add_edge(while_h, condition, CASTEdge::OpOrd(0));
        let node = self.ast.add_node(CASTNode::Block);
        self.ast.add_edge(while_h, node, CASTEdge::OpOrd(1));
        for (i, n) in body.iter().enumerate() {
            let e = self.ast.find_edge(self.fn_head, *n).expect("This cannot be `None`");
            self.ast.remove_edge(e);
            self.ast.add_edge(node, *n, CASTEdge::BlockOrd(i as u64));
        }
        while_h
    }

    pub fn new_do_while(&mut self, condition: NodeIndex, body: Vec<NodeIndex>) -> NodeIndex {
        let idx = if let Some(e1) = self.ast.find_edge(self.fn_head, condition) {
            let idx = self.get_statement_ord(e1);
            self.ast.remove_edge(e1);
            idx
        } else {
            self.next_edge_idx()
        };
        let while_h = self.ast.add_node(CASTNode::DoWhile);
        self.ast.add_edge(self.fn_head, while_h, CASTEdge::StatementOrd(idx));
        self.ast.add_edge(while_h, condition, CASTEdge::OpOrd(0));
        let node = self.ast.add_node(CASTNode::Block);
        self.ast.add_edge(while_h, node, CASTEdge::OpOrd(1));
        for (i, n) in body.iter().enumerate() {
            let e = self.ast.find_edge(self.fn_head, *n).expect("This cannot be `None`");
            self.ast.remove_edge(e);
            self.ast.add_edge(node, *n, CASTEdge::BlockOrd(i as u64));
        }
        while_h
    }

    pub fn new_if(&mut self,
                           condition: NodeIndex,
                           body: Vec<NodeIndex>,
                           else_condition: Option<Vec<NodeIndex>>)
                           -> NodeIndex {
        let idx = if let Some(e1) = self.ast.find_edge(self.fn_head, condition) {
            let idx = self.get_statement_ord(e1);
            self.ast.remove_edge(e1);
            idx
        } else {
            self.next_edge_idx()
        };
        let if_h = self.ast.add_node(CASTNode::If);
        self.ast.add_edge(self.fn_head, if_h, CASTEdge::StatementOrd(idx));
        self.ast.add_edge(if_h, condition, CASTEdge::OpOrd(0));

        let node = self.ast.add_node(CASTNode::Block);
        self.ast.add_edge(if_h, node, CASTEdge::OpOrd(1));
        for (i, n) in body.iter().enumerate() {
            let e = self.ast.find_edge(self.fn_head, *n).expect("This cannot be `None`");
            self.ast.remove_edge(e);
            self.ast.add_edge(node, *n, CASTEdge::BlockOrd(i as u64));
        }

        if let Some(elses) = else_condition {
            let else_node = self.ast.add_node(CASTNode::Block);
            self.ast.add_edge(if_h, else_node, CASTEdge::OpOrd(2));
            for (i, n) in elses.into_iter().enumerate() {
                let e = self.ast.find_edge(self.fn_head, n).expect("This cannot be `None`");
                self.ast.remove_edge(e);
                self.ast.add_edge(else_node, n, CASTEdge::BlockOrd(i as u64));
            }
        }
        if_h
    }

    pub fn call_func(&mut self, func_name: &str, args: Vec<Option<NodeIndex>>) -> NodeIndex {
        let args_str = args
            .into_iter()
            .map(|arg_opt| {
                if let Some(arg) = arg_opt {
                    match self.ast.node_weight(arg) {
                        Some(&CASTNode::Var(ref v)) => v.clone(),
                        Some(&CASTNode::Constant(_, ref c)) => c.clone(),
                        _ => "unknown".to_string(),
                    }
                } else {
                    "unknown".to_string()
                }
            }).collect::<Vec<_>>();
        let call_node = self.ast.add_node(CASTNode::Call(func_name.to_string(), args_str));
        let idx = self.next_edge_idx();
        self.ast.add_edge(self.fn_head, call_node, CASTEdge::StatementOrd(idx));
        call_node
    }

    pub fn ret(&mut self, value: Option<NodeIndex>) -> NodeIndex {
        let value_str = if let Some(_value) = value {
            match self.ast.node_weight(_value) {
                Some(&CASTNode::Var(ref v)) => v.clone(),
                Some(&CASTNode::Constant(_, ref c)) => c.clone(),
            _ => "unknown".to_string(),
            }
        } else {
            "".to_string()
        };
        let ret_node = self.ast.add_node(CASTNode::Return(value_str));
        let idx = self.next_edge_idx();
        self.ast.add_edge(self.fn_head, ret_node, CASTEdge::StatementOrd(idx));
        ret_node
    }

    pub fn goto(&mut self, label: &str) -> NodeIndex {
        let goto_n = self.ast.add_node(CASTNode::Goto(label.to_string()));
        let idx = self.next_edge_idx();
        self.ast.add_edge(self.fn_head, goto_n, CASTEdge::StatementOrd(idx));
        goto_n
    }

    pub fn label(&mut self, label: &str) -> NodeIndex {
        let label = self.ast.add_node(CASTNode::Label(label.to_string()));
        let idx = self.next_edge_idx();
        self.ast.add_edge(self.fn_head, label, CASTEdge::StatementOrd(idx));
        label
    }

    pub fn insert_break(&mut self, _ : NodeIndex) {
        let break_n = self.ast.add_node(CASTNode::Break);
        let idx = self.next_edge_idx();
        self.ast.add_edge(self.fn_head, break_n, CASTEdge::StatementOrd(idx));
    }

    // Declare variables/constants without declaration in case is_implicit is true
    pub fn declare_vars(&mut self, ty: Ty, vars: &[String], is_implicit: bool) -> Vec<NodeIndex> {
        let decl = self.ast.add_node(CASTNode::Declaration(ty));
        let mut var_decls = Vec::new();
        for (i, v) in vars.iter().enumerate() {
            let vn = self.ast.add_node(CASTNode::Var(v.clone()));
            self.ast.add_edge(decl, vn, CASTEdge::OpOrd(i as u8));
            var_decls.push(vn);
        }
        let idx = self.next_edge_idx();
        if !is_implicit {
            self.ast.add_edge(self.fn_head, decl, CASTEdge::StatementOrd(idx));
        }
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
        let comment = self.comments.get(&node).cloned();
        let result = match self.ast[*node] {
            CASTNode::FunctionHeader(_) => unimplemented!(),
            CASTNode::If => {
                // Get the arguments -> condition, body, else branch.
                let args = self.get_args_ordered(node);
                let arg1 = args[0];
                let arg2 = args[1];
                let arg3 = args.get(2).cloned();
                let condition = format!("{} {} {{\n",
                                        format_with_indent("if", indent),
                                        self.emit_c(&arg1, 0));
                let true_body = self.emit_c(&arg2, indent + 1);
                let false_body = if let Some(arg3) = arg3 {
                    let fbody = self.emit_c(&arg3, indent + 1);
                    if let CASTNode::If = self.ast[arg3] {
                        // Else-If case
                        format_with_indent("} else ", indent) + &fbody
                    } else {
                        // Else case.
                        format_with_indent("} else {\n", indent) + &fbody + "\n"
                    }
                } else {
                    "".to_owned()
                };

                format!("{}{}{}{}",
                        condition,
                        true_body,
                        format!("\n{}", false_body),
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
            CASTNode::While => {
                // Get the arguments -> while header/check condition, while body.
                let args = self.get_args_ordered(node);
                let condition = self.emit_c(&args[0], 0);
                let while_body = self.emit_c(&args[1], indent + 1);
                format!("{} ({}) {{\n{}\n{}",
                        format_with_indent("while", indent),
                        condition,
                        while_body,
                        format_with_indent("}", indent))
            }
            CASTNode::DoWhile => {
                // Get the arguments -> while header/check condition, while body.
                let args = self.get_args_ordered(node);
                let condition = self.emit_c(&args[0], 0);
                let while_body = self.emit_c(&args[1], indent + 1);
                format!("{} {{\n{}\n{}}} while ({});",
                        format_with_indent("do", indent),
                        while_body,
                        format_with_indent("", indent),
                        condition)
            }
            CASTNode::Goto(ref label) => {
                format_with_indent(&format!("goto {}", label), indent)
            }
            CASTNode::Label(ref label) => {
                format!("{}:", label)
            }
            CASTNode::Break => {
                format_with_indent("break;", indent)
            }
            CASTNode::ExpressionNode(ref expr) => {
                let operands = self.get_args_ordered(node);
                let op_str = operands.iter().map(|x| self.emit_c(x, 0)).collect::<Vec<_>>();
                match *expr {
                    Expr::Assign => format!("{} = {}",
                                        format_with_indent(&op_str[0], indent),
                                        &op_str[1]),
                    Expr::Add => format!("({} + {})",
                                         format_with_indent(&op_str[0], indent),
                                         &op_str[1]),
                    Expr::Cast(size) => format!("({} as {})",
                                         format_with_indent(&op_str[0], indent),
                                         &size.to_string()),
                    Expr::Sub => format!("({} - {})",
                                         format_with_indent(&op_str[0], indent),
                                         &op_str[1]),
                    Expr::Mul => format!("({} * {})",
                                         format_with_indent(&op_str[0], indent),
                                         &op_str[1]),
                    Expr::DeRef => format!("{}*({})", format_with_indent("", indent), &op_str[0]),
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
                    Expr::Eq => format!("({} == {})",
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
            CASTNode::Return(ref value) => {
                format!("{}return {}", format_with_indent("", indent), &value)
            }
            CASTNode::Call(ref func, ref args) => {
                format!("{}({})", format_with_indent(&func, indent), args.join(", "))
            }
            CASTNode::Block => {
                let mut ns = self.ast.edges_directed(*node, Direction::Outgoing)
                    .into_iter()
                    .filter_map(|x| {
                        match x.weight() {
                            CASTEdge::BlockOrd(i) => Some((i, x.target())),
                            _ => None
                        }
                    }).collect::<Vec<_>>();
                ns.sort_by_key(|k| k.0);
                ns.into_iter()
                    .map(|(_, n)| self.emit_c(&n, indent))
                    .collect::<Vec<_>>()
                    .join("\n")
            }
        };
        if comment.is_some() {
            format!("{}\t//{}", result, &comment.unwrap())
        } else {
            result
        }
    }

    pub fn comment_at(&mut self, node: NodeIndex, comment: &str) {
        self.comments.insert(node, comment.to_string());
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
        let vars = c_ast.declare_vars(Ty::new(BTy::Int, false, 0), &["i".to_owned(), "j".to_owned()], false);
        let eq = c_ast.expr(Expr::Eq, &vars, false);
        let increment = c_ast.expr(Expr::Add, &vars, false);
        let assignment = c_ast.expr(Expr::Assign, &[vars[0], increment], false);
        c_ast.new_if(eq, vec![assignment], None);
        let _ = c_ast.ret(None);
        println!("{}", c_ast.print());
    }

    #[test]
    fn c_ast_call_test() {
        let mut c_ast = CAST::new("main");
        let args = c_ast.function_args(&[(Ty::new(BTy::Int, false, 0), "x".to_owned())]);
        let vars = c_ast.declare_vars(Ty::new(BTy::Int, false, 0), &["i".to_owned(), "j".to_owned()], false);
        let test_args = args.iter().chain(vars.iter()).map(|n| Some(n.clone())).collect::<Vec<_>>();
        let _ = c_ast.call_func("test_func", test_args);
        let _ = c_ast.ret(None);
        println!("{}", c_ast.print());
    }

    #[test]
    fn c_ast_goto_test() {
        let mut c_ast = CAST::new("main");
        let _ = c_ast.declare_vars(Ty::new(BTy::Int, false, 0), &["i".to_owned(), "j".to_owned()], false);
        let lbl_str = "L1";
        let _ = c_ast.label(lbl_str);
        let _ = c_ast.goto(lbl_str);
        println!("{}", c_ast.print());
    }

    #[test]
    fn c_ast_while_test() {
        let mut c_ast = CAST::new("main");
        c_ast.function_args(&[(Ty::new(BTy::Int, false, 0), "x".to_owned())]);
        let vars = c_ast.declare_vars(Ty::new(BTy::Int, false, 0), &["i".to_owned(), "j".to_owned()], false);
        let eq = c_ast.expr(Expr::Eq, &vars, false);
        let increment = c_ast.expr(Expr::Add, &vars, false);
        let assignment = c_ast.expr(Expr::Assign, &[vars[0], increment], false);
        c_ast.new_while(eq, vec![assignment]);
        let _ = c_ast.ret(None);
        println!("{}", c_ast.print());
    }

    #[test]
    fn c_ast_do_while_test() {
        let mut c_ast = CAST::new("main");
        c_ast.function_args(&[(Ty::new(BTy::Int, false, 0), "x".to_owned())]);
        let vars = c_ast.declare_vars(Ty::new(BTy::Int, false, 0), &["i".to_owned(), "j".to_owned()], false);
        let eq = c_ast.expr(Expr::Eq, &vars, false);
        let increment = c_ast.expr(Expr::Add, &vars, false);
        let assignment = c_ast.expr(Expr::Assign, &[vars[0], increment], false);
        c_ast.new_do_while(eq, vec![assignment]);
        let _ = c_ast.ret(None);
        println!("{}", c_ast.print());
    }
}
