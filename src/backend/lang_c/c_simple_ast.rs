use std::{default, iter, fmt};
use std::collections::{HashMap, HashSet};

use super::c_simple;
use super::c_simple::{Ty, CAST, CASTNode};
use frontend::radeco_containers::RadecoFunction;
use petgraph::graph::{Graph, NodeIndex, EdgeIndex, Edges, EdgeReference};
use petgraph::visit::EdgeRef;
use petgraph::{EdgeDirection, Direction, Directed};

#[derive(Debug, Clone, PartialEq)]
enum SimpleCASTNode {
    /// Entry node of target function
    Entry,
    Action(ActionNode),
    Value(ValueNode),
}

#[derive(Debug, Clone, PartialEq)]
enum ActionNode {
    Assignment,
    Call(String),
    Return,
    If,
    Goto,
}

// TODO Use another type instead of String
// TODO Add type information
#[derive(Debug, Clone, PartialEq)]
enum ValueNode {
    /// The string is the name of variable
    Variable(Option<Ty>, String),
    /// Constant or immidiate value
    Constant(Option<Ty>, String),
    Expression(c_simple::Expr),
}

#[derive(Debug, Clone, PartialEq)]
enum SimpleCASTEdge {
    Action(ActionEdge),
    Value(ValueEdge),
}

#[derive(Debug, Clone, PartialEq)]
enum ActionEdge {
    IfThen,
    IfElse,
    Normal,
}

#[derive(Debug, Clone, PartialEq)]
enum ValueEdge {
    /// Source Node of assignment action
    AssignSrc,
    /// Destination Node of assignment action
    AssignDst,
    /// Arguments of function call
    /// the number represents it is a nth argument
    Arg(u8),
    /// Return value of target function
    RetVal,
    /// Return value of function call
    FuncVal,
    /// Operand of expression, the number
    /// represents this is nth operand.
    Operand(u8),
    /// It points to condition of If action
    Conditional,
    /// Destination of Goto statement
    GotoDst,
}

struct SimpleCAST {
    /// Name of function of this AST
    fname: String,
    /// Entry node of this function
    pub entry: NodeIndex,
    ast: Graph<SimpleCASTNode, SimpleCASTEdge>,
    /// Variables declared in this function
    vars: HashSet<NodeIndex>,
    /// Constants declared in this function
    consts: HashSet<NodeIndex>,
    /// Expressions declared in this function
    exprs: HashSet<NodeIndex>,
    /// Hashmap from label node to string it represents
    label_map: HashMap<NodeIndex, String>,
}


impl SimpleCASTNode {
    fn is_exit(&self) -> bool {
        match self {
            &SimpleCASTNode::Action(ActionNode::Return) => true,
            _ => false,
        }
    }
}

/// Returns nodes which is connected with given type of edge
fn neighbors_by_edge(edges: &Vec<EdgeReference<SimpleCASTEdge>>, ty: &SimpleCASTEdge) -> Vec<NodeIndex> {
    edges.iter().filter(|e| (*e).weight() == ty)
        .map(|e| e.target())
        .collect()
}

impl SimpleCAST {
    pub fn new(fn_name: &str) -> SimpleCAST {
        let mut ast = Graph::new();
        let entry = ast.add_node(SimpleCASTNode::Entry);
        SimpleCAST {
            fname: fn_name.to_string(),
            entry: entry,
            ast: ast,
            vars: HashSet::new(),
            consts: HashSet::new(),
            exprs: HashSet::new(),
            label_map: HashMap::new(),
        }
    }

    /// Add ValueNode of variable
    pub fn var(&mut self, name: &str, ty: Option<Ty>) -> NodeIndex {
        let node = self.ast.add_node(SimpleCASTNode::Value(ValueNode::Variable(ty, name.to_string())));
        self.vars.insert(node);
        node
    }

    /// Add ValueNode of constant value
    pub fn constant(&mut self, name: &str, ty: Option<Ty>) -> NodeIndex {
        let node = self.ast.add_node(SimpleCASTNode::Value(ValueNode::Constant(ty, name.to_string())));
        self.consts.insert(node);
        node
    }

    /// Add ValueNode of expression
    pub fn expr(&mut self, operands: &[NodeIndex], op: c_simple::Expr) -> NodeIndex {
        let node = self.ast.add_node(SimpleCASTNode::Value(ValueNode::Expression(op)));
        for (i, operand) in operands.iter().enumerate() {
            let _ = self.ast.add_edge(node, *operand, SimpleCASTEdge::Value(ValueEdge::Operand(i as u8)));
        }
        self.exprs.insert(node);
        node
    }

    /// Add ActionNode of assignment
    pub fn assign(&mut self, dst: NodeIndex, src: NodeIndex, prev_action: NodeIndex) -> NodeIndex {
        let node = self.ast.add_node(SimpleCASTNode::Action(ActionNode::Assignment));
        let _ = self.ast.add_edge(node, dst, SimpleCASTEdge::Value(ValueEdge::AssignDst));
        let _ = self.ast.add_edge(node, src, SimpleCASTEdge::Value(ValueEdge::AssignSrc));
        let _ = self.ast.add_edge(prev_action, node, SimpleCASTEdge::Action(ActionEdge::Normal));
        node
    }

    /// Add ActionNode of function call
    pub fn call_func(&mut self, fname: &str, args: &[NodeIndex], prev_action: NodeIndex,
                 ret_val: Option<NodeIndex>) -> NodeIndex {
        let call_node = self.ast.add_node(SimpleCASTNode::Action(ActionNode::Call(fname.to_string())));
        for (i, arg) in args.iter().enumerate() {
            self.ast.add_edge(call_node, *arg, SimpleCASTEdge::Value(ValueEdge::Arg(i as u8)));
        }
        self.ast.add_edge(prev_action, call_node, SimpleCASTEdge::Action(ActionEdge::Normal));
        if ret_val.is_some() {
            self.ast.add_edge(call_node, ret_val.unwrap(), SimpleCASTEdge::Value(ValueEdge::FuncVal));
        }
        call_node
    }

    fn insert_node(&mut self, prev_action: NodeIndex, next_action: NodeIndex, node: NodeIndex) {
        let e = self.ast.find_edge(prev_action, next_action)
            .map(|x| (x, self.ast.edge_weight(x).map(|a| a.clone())));
        match e {
            Some((idx, Some(SimpleCASTEdge::Action(_)))) => {
                self.ast.remove_edge(idx);
                self.ast.add_edge(prev_action, node, SimpleCASTEdge::Action(ActionEdge::Normal));
                self.ast.add_edge(node, next_action, SimpleCASTEdge::Action(ActionEdge::Normal));
                eprintln!("add edge {:?} to {:?}", prev_action, node);
                eprintln!("add edge {:?} to {:?}", node, next_action);
            },
            _ => {
                radeco_warn!("Invalid nodes {}, {}", prev_action, next_action);
            },
        }
    }

    fn remove_incoming_actions(&mut self, node: NodeIndex) {
        let ns = self.ast.edges_directed(node, Direction::Incoming)
            .into_iter()
            .filter_map(|e| {
                match e.weight() {
                    SimpleCASTEdge::Action(_) => Some(e.id()),
                    _ => None,
                }
            }).collect::<Vec<_>>();
        for e in ns {
            self.ast.remove_edge(e);
        }
    }

    /// Add ActionNode of if statement
    pub fn conditional(&mut self, condition: NodeIndex, if_then: NodeIndex,
                   if_else: Option<NodeIndex>, prev_action: NodeIndex) -> NodeIndex {
        let node = self.ast.add_node(SimpleCASTNode::Action(ActionNode::If));
        self.remove_incoming_actions(if_then);
        if if_else.is_some() {
            self.remove_incoming_actions(if_else.unwrap());
            self.ast.add_edge(node, if_else.unwrap(), SimpleCASTEdge::Action(ActionEdge::IfElse));
        }
        self.ast.add_edge(node, if_then, SimpleCASTEdge::Action(ActionEdge::IfThen));
        self.ast.add_edge(node, condition, SimpleCASTEdge::Value(ValueEdge::Conditional));
        self.ast.add_edge(prev_action, node, SimpleCASTEdge::Action(ActionEdge::Normal));
        node
    }

    /// Add ActionNode of return statement
    pub fn add_return(&mut self, ret_val: Option<NodeIndex>, prev_action: NodeIndex) -> NodeIndex {
        let node = self.ast.add_node(SimpleCASTNode::Action(ActionNode::Return));
        if ret_val.is_some() {
            self.ast.add_edge(node, ret_val.unwrap(), SimpleCASTEdge::Value(ValueEdge::RetVal));
        }
        self.ast.add_edge(prev_action, node, SimpleCASTEdge::Action(ActionEdge::Normal));
        node
    }

    pub fn add_goto(&mut self, dst: NodeIndex, label_str: &str, prev_action: NodeIndex) -> NodeIndex {
        let node = self.ast.add_node(SimpleCASTNode::Action(ActionNode::Goto));
        self.ast.add_edge(node, dst, SimpleCASTEdge::Value(ValueEdge::GotoDst));
        self.ast.add_edge(prev_action, node, SimpleCASTEdge::Action(ActionEdge::Normal));
        self.label_map.insert(dst, label_str.to_string());
        node
    }

    pub fn insert_goto(&mut self, prev_action: NodeIndex, next_action: NodeIndex,
                   dst: NodeIndex, label_str: &str) -> NodeIndex {
        let node = self.ast.add_node(SimpleCASTNode::Action(ActionNode::Goto));
        let _ = self.ast.add_edge(node, dst, SimpleCASTEdge::Value(ValueEdge::GotoDst));
        self.insert_node(prev_action, next_action, node);
        self.label_map.insert(dst, label_str.to_string());
        node
    }

    fn next_actions(&self, idx: NodeIndex) -> Vec<NodeIndex> {
        self.ast.edges_directed(idx, Direction::Outgoing)
            .into_iter()
            .filter_map(|e| {
                match e.weight() {
                    &SimpleCASTEdge::Action(_) => Some(e.target()),
                    _ => None,
                }
            }).collect()
    }

    // Returns a pair (IfThen, IfElse)
    fn branch(&self, idx: NodeIndex) -> Option<(Vec<NodeIndex>, Option<Vec<NodeIndex>>)> {
        if self.ast.node_weight(idx) != Some(&SimpleCASTNode::Action(ActionNode::If)) {
            return None;
        }
        let gather_actions = |index, action_type| {
            let next_node = |node, edge| {
                let ns = self.ast
                    .edges_directed(node, Direction::Outgoing)
                    .into_iter()
                    .collect::<Vec<_>>();
                neighbors_by_edge(&ns, edge)
                    .first().map(|e| e.clone())
            };
            let next_normal = move |node| {
                next_node(node, &SimpleCASTEdge::Action(ActionEdge::Normal))
            };
            move || {
                let mut first = next_node(index, action_type);
                let mut ret = Vec::new();
                while let Some(n) = first {
                    ret.push(n);
                    first = next_normal(n);
                }
                eprintln!("ret.len(): {:?}", ret.len());
                if ret.len() > 0 {
                    Some(ret)
                } else {
                    None
                }
            }
        };
        let if_then = gather_actions(idx, &SimpleCASTEdge::Action(ActionEdge::IfThen))();
        let if_else = gather_actions(idx, &SimpleCASTEdge::Action(ActionEdge::IfElse))();
        if if_then.is_some() {
            Some((if_then.unwrap(), if_else))
        } else {
            None
        }
    }

    // Returns value node which represents condition used by If statement
    fn branch_condition(&self, idx: NodeIndex) -> Option<NodeIndex> {
        if self.ast.node_weight(idx) != Some(&SimpleCASTNode::Action(ActionNode::If)) {
            return None;
        }
        let ns = self.ast.edges_directed(idx, Direction::Outgoing).into_iter().collect();
        let expr = {
            let tmp = neighbors_by_edge(&ns, &SimpleCASTEdge::Value(ValueEdge::Conditional));
            if tmp.len() > 1 {
                radeco_warn!("More than one expressions found: If");
            }
            tmp.first().map(|e| e.clone())
        };
        expr
    }

    // Returns destination of goto statement
    fn goto(&self, idx: NodeIndex) -> Option<NodeIndex> {
        if self.ast.node_weight(idx) != Some(&SimpleCASTNode::Action(ActionNode::Goto))  {
            return None;
        };
        let goto_dsts = self.ast.edges_directed(idx, Direction::Outgoing)
            .into_iter()
            .filter_map(|e| {
                match e.weight() {
                    &SimpleCASTEdge::Value(ValueEdge::GotoDst) => Some(e.target()),
                    _ => None,
                }
            }).collect::<Vec<_>>();
        if goto_dsts.len() > 1 {
            radeco_warn!("More than 1 labels found as Goto destination");
            return None;
        };
        goto_dsts.first().map(|n| n.clone())
    }

    // Returns a pair (Dst, Src) which represents Dst = Src
    fn assignment(&self, idx: NodeIndex) -> Option<(NodeIndex, NodeIndex)> {
        if self.ast.node_weight(idx) != Some(&SimpleCASTNode::Action(ActionNode::Assignment)) {
            return None;
        }
        let ns = self.ast.edges_directed(idx, Direction::Outgoing).into_iter().collect();
        let src = neighbors_by_edge(&ns, &SimpleCASTEdge::Value(ValueEdge::AssignSrc))
            .first().map(|e| e.clone());
        let dst = neighbors_by_edge(&ns, &SimpleCASTEdge::Value(ValueEdge::AssignDst))
            .first().map(|e| e.clone());
        if src.is_some() && dst.is_some() {
            return Some((dst.unwrap(), src.unwrap()))
        };
        None
    }

    // Returns arguments of function call
    fn args_call(&self, idx: NodeIndex) -> Option<Vec<NodeIndex>> {
        match self.ast.node_weight(idx) {
            Some(&SimpleCASTNode::Action(ActionNode::Call(_))) => {},
            _ => {return None;},
        };
        let mut args = self.ast.edges_directed(idx, Direction::Outgoing)
            .into_iter()
            .filter_map(|e| {
                match e.weight() {
                    &SimpleCASTEdge::Value(ValueEdge::Arg(o)) => Some((o, e.target())),
                    _ => None,
                }
            }).collect::<Vec<_>>();
        args.sort_by_key(|k| k.0);
        let ret = args.into_iter()
            .map(|(_, b)| b)
            .collect();
        Some(ret)
    }

    // Returns value node which is assigned by a given function call
    fn func_val(&self, idx: NodeIndex) -> Option<NodeIndex> {
        match self.ast.node_weight(idx) {
            Some(&SimpleCASTNode::Action(ActionNode::Call(_))) => {},
            _ => {return None;},
        };
        let ret = self.ast.edges_directed(idx, Direction::Outgoing)
            .into_iter()
            .filter_map(|e| {
                match e.weight() {
                    &SimpleCASTEdge::Value(ValueEdge::FuncVal) => Some(e.target()),
                    _ => None,
                }
            }).collect::<Vec<_>>();
        if ret.len() > 1 {
            radeco_warn!("More than one variable found for FuncVal");
        }
        ret.first().map(|x| *x)
    }

    // Returns value node of the return value of a return statement
    fn ret_val(&self, idx: NodeIndex) -> Option<NodeIndex> {
        match self.ast.node_weight(idx) {
            Some(&SimpleCASTNode::Action(ActionNode::Return)) => {},
            _ => {return None;}
        };
        let ret_val = self.ast.edges_directed(idx, Direction::Outgoing)
            .into_iter()
            .filter_map(|e| {
                match e.weight() {
                    &SimpleCASTEdge::Value(ValueEdge::RetVal) => Some(e.target()),
                    _ => None,
                }
            }).collect::<Vec<_>>();
        if ret_val.len() > 1 {
            radeco_warn!("More than one return values found");
        };
        ret_val.into_iter().next()
    }

    // Returns operands used by a given expression
    fn operands_from_expr(&self, expr: NodeIndex) -> Vec<NodeIndex> {
        let mut operands = self.ast.edges_directed(expr, Direction::Outgoing)
            .into_iter()
            .filter_map(|e| {
                match e.weight() {
                    &SimpleCASTEdge::Value(ValueEdge::Operand(i)) => Some((i, e.target())),
                    _ => None,
                }
            }).collect::<Vec<_>>();
        operands.sort_by_key(|k| k.0);
        operands.into_iter().map(|(_, n)| n).collect::<Vec<_>>()
    }

    pub fn to_c_ast(&self) -> CAST {
        let mut converter = CASTConverter::new(&self);
        converter.to_c_ast()
    }
}

struct CASTConverter<'a> {
    ast: &'a SimpleCAST,
    /// HashMap from SimpleCAST's node to CAST's node
    node_map: HashMap<NodeIndex, NodeIndex>,
    /// HashMap from ActionNode to label node
    label_node_map: HashMap<NodeIndex, NodeIndex>,
    visited: HashSet<NodeIndex>,
}

impl<'a> CASTConverter<'a> {
    fn new(ast: &SimpleCAST) -> CASTConverter {
        CASTConverter {
            ast: ast,
            node_map: HashMap::new(),
            label_node_map: HashMap::new(),
            visited: HashSet::new(),
        }
    }

    /// Entry point of Simple-C-AST to C-AST conversion.
    pub fn to_c_ast(&mut self) -> CAST {
        let mut c_ast = CAST::new(&self.ast.fname);
        for var in self.ast.vars.iter() {
            if let Some(&SimpleCASTNode::Value(ValueNode::Variable(ref ty_opt, ref var_name))) = self.ast.ast.node_weight(*var) {
                let ty = ty_opt.clone().unwrap_or(Ty::new(c_simple::BTy::Int, false, 0));
                let n = c_ast.declare_vars(ty, &[var_name.to_string()]);
                self.node_map.insert(*var, n[0]);
            }
        }
        for expr in self.ast.exprs.iter() {
            if let Some(&SimpleCASTNode::Value(ValueNode::Expression(ref op))) = self.ast.ast.node_weight(*expr) {
                let operands = self.ast.operands_from_expr(*expr)
                    .into_iter()
                    .map(|_n| {
                        if let Some(&n) = self.node_map.get(&_n) {
                            n
                        } else {
                            radeco_warn!("NodeIndex {:?} not found", _n);
                            NodeIndex::end()
                        }
                    }).collect::<Vec<_>>();
                let n = c_ast.expr(op.clone(), &operands);
                self.node_map.insert(*expr, n);
            }
        }
        let entry = self.ast.entry;
        self.to_c_ast_body(&mut c_ast, entry);
        c_ast
    }

    fn to_c_ast_body(&mut self, c_ast: &mut CAST, current_node: NodeIndex) {
        if self.visited.contains(&current_node) {
            return;
        };
        self.visited.insert(current_node);
        eprintln!("current: {:?}", current_node);
        if let Some(ref l) = self.ast.label_map.get(&current_node) {
            eprintln!("Label at {:?}", current_node);
            let node = c_ast.label(l);
            self.label_node_map.insert(current_node, node);
        };
        let idx = self.ast.ast.node_weight(current_node).map(|x| x.clone());
        match idx {
            Some(SimpleCASTNode::Action(ActionNode::Assignment)) => {
                let tmp = self.ast.assignment(current_node)
                    .and_then(|(d, s)| {
                        match (self.node_map.get(&d), self.node_map.get(&s)) {
                            (Some(&x), Some(&y)) => Some((x.clone(), y.clone())),
                            _ => None
                        }
                    });
                if let Some((dst, src)) = tmp {
                    let node = c_ast.expr(c_simple::Expr::Assign, &[dst, src]);
                    self.node_map.insert(current_node, node);
                } else {
                    radeco_err!("Something wrong");
                }
            },
            Some(SimpleCASTNode::Action(ActionNode::Call(ref name))) => {
                let args = self.ast.args_call(current_node)
                    .unwrap_or(Vec::new())
                    .into_iter()
                    .map(|arg| {
                        let ret = self.node_map.get(&arg).map(|a| *a);
                        if ret.is_none() {
                            radeco_warn!("Error args_call");
                        }
                        ret
                    }).collect();
                let ret_node_opt = self.ast.func_val(current_node)
                    .and_then(|x| self.node_map.get(&x).map(|a| *a));
                let node = c_ast.call_func(name, args);
                if let Some(ret_node) = ret_node_opt {
                    c_ast.expr(c_simple::Expr::Assign, &[ret_node, node]);
                }
                self.node_map.insert(current_node, node);
            },
            Some(SimpleCASTNode::Action(ActionNode::Return)) => {
                let opt = self.ast.ret_val(current_node)
                    .and_then(|n| self.node_map.get(&n))
                    .map(|n| *n);
                let node = c_ast.ret(opt);
                self.node_map.insert(current_node, node);
            },
            Some(SimpleCASTNode::Action(ActionNode::If)) => {
                let cond = self.ast.branch_condition(current_node);
                let branches = self.ast.branch(current_node).map(|x| x.clone());
                if let (Some((if_then, if_else)), Some(cond)) = (branches, cond) {
                    for n in if_then.iter() {
                        self.to_c_ast_body(c_ast, *n);
                    }
                    if if_else.is_some() {
                        for n in if_else.as_ref().unwrap().iter() {
                            self.to_c_ast_body(c_ast, *n);
                        }
                    }
                    // TODO avoid unwrap
                    let c = *self.node_map.get(&cond).unwrap();
                    let t = if_then.into_iter().map(|x| *self.node_map.get(&x).unwrap()).collect::<Vec<_>>();
                    let e = if_else.map(|x| x.iter().map(|y| *self.node_map.get(y).unwrap()).collect::<Vec<_>>());
                    let node = c_ast.new_conditional(c, t, e);
                    self.node_map.insert(current_node, node);
                }
            },
            Some(SimpleCASTNode::Action(ActionNode::Goto)) => {
                let reserved_edge = if self.ast.goto(current_node)
                    .and_then(|d| self.label_node_map.get(&d)).is_some() {
                        None
                    } else {
                        let ret = c_ast.reserve_edge();
                        for n in self.ast.next_actions(current_node) {
                            self.to_c_ast_body(c_ast, n);
                        }
                        Some(ret)
                    };
                let dst_opt = self.ast.goto(current_node)
                    .and_then(|d| self.label_node_map.get(&d))
                    .map(|d| d.clone());
                if let Some(dst) = dst_opt {
                    // TODO avoid unwrap
                    let node = c_ast.goto(dst, reserved_edge.is_none()).unwrap();
                    if reserved_edge.is_some() {
                        c_ast.replace_node(reserved_edge.unwrap(), node);
                    }
                    self.node_map.insert(current_node, node);
                } else {
                    radeco_warn!("Error Goto");
                }
            },
            Some(SimpleCASTNode::Entry) => {},
            _ => unreachable!(),
        };
        for n in self.ast.next_actions(current_node) {
            self.to_c_ast_body(c_ast, n);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    // fn main () {
    //     unsigned int x;
    //     unsigned int y;
    //     unsigned int z;
    //     unsigned int w;
    //     x = y
    //     func(z, w)
    // }
    #[test]
    fn simple_c_ast_basic_test() {
        let mut ast = SimpleCAST::new("main");
        let x = ast.var("x", None);
        let y = ast.var("y", None);
        let z = ast.var("z", None);
        let w = ast.var("w", None);
        let entry = ast.entry;
        let assn = ast.assign(x, y, entry);
        let _ = ast.call_func("func", &[z, w], assn, None);
        let output = ast.to_c_ast().print();
        println!("{}", output);
    }

    // fn main () {
    //     unsigned int x;
    //     unsigned int y;
    //     unsigned int z;
    //     x = (x + y)
    // }
    #[test]
    fn simple_c_ast_expr_test() {
        let mut ast = SimpleCAST::new("main");
        let x = ast.var("x", None);
        let y = ast.var("y", None);
        let z = ast.var("z", None);
        let entry = ast.entry;
        let expr = ast.expr(&[x, y], c_simple::Expr::Add);
        let assn = ast.assign(x, expr, entry);
        let output = ast.to_c_ast().print();
        println!("{}", output);
    }

    // fn main () {
    //     unsigned int y;
    //     unsigned int x;
    //     y = func(x)
    // }
    #[test]
    fn simple_c_ast_func_test() {
        let mut ast = SimpleCAST::new("main");
        let x = ast.var("x", None);
        let y = ast.var("y", None);
        let entry = ast.entry;
        let call_f = ast.call_func("func", &[x], entry, Some(y));
        let output = ast.to_c_ast().print();
        println!("{}", output);
    }

    // fn main() {
    //     unsigned int z;
    //     unsigned int x;
    //     unsigned int y;
    //     unsigned int w;
    //     if x {
    //         x = y
    //         test1()
    //     } else {
    //         func(z, w)
    //         test2()
    //     }
    // }
    #[test]
    fn simple_c_ast_conditional_test() {
        let mut ast = SimpleCAST::new("main");
        let x = ast.var("x", None);
        let y = ast.var("y", None);
        let z = ast.var("z", None);
        let w = ast.var("w", None);
        let entry = ast.entry;
        // XXX
        let assn = ast.assign(x, y, entry);
        let call_test1 = ast.call_func("test1", &[], assn, None);
        let call_f = ast.call_func("func", &[z, w], assn, None);
        let call_test2 = ast.call_func("test2", &[], call_f, None);
        let _ = ast.conditional(x, assn, Some(call_f), entry);
        let output = ast.to_c_ast().print();
        println!("{}", output);
    }

    // fn main () {
    //     unsigned int y;
    //     unsigned int x;
    //     goto L1
    //     x = y
    // L1:
    //     return x
    // }
    #[test]
    fn simple_c_ast_goto_test() {
        let mut ast = SimpleCAST::new("main");
        let entry = ast.entry;
        let x = ast.var("x", None);
        let y = ast.var("y", None);
        let assn = ast.assign(x, y, entry);
        let _ = ast.add_goto(entry, "L1", assn);
        let output = ast.to_c_ast().print();
        println!("{}", output);
    }

    // fn main () {
    //     unsigned int x;
    //     return x
    // }
    #[test]
    fn simple_c_ast_return_test() {
        let mut ast = SimpleCAST::new("main");
        let entry = ast.entry;
        let x = ast.var("x", None);
        let _ = ast.add_return(Some(x), entry);
        let output = ast.to_c_ast().print();
        println!("{}", output);
    }

    // fn main () {
    //     unsigned int y;
    //     unsigned int x;
    //     goto L1
    //     x = y
    // L1:
    //     return x
    // }
    #[test]
    fn simple_c_ast_insert_goto_test() {
        let mut ast = SimpleCAST::new("main");
        let entry = ast.entry;
        let x = ast.var("x", None);
        let y = ast.var("y", None);
        let assn = ast.assign(x, y, entry);
        let ret = ast.add_return(Some(x), assn);
        let _ = ast.insert_goto(entry, assn, ret, "L1");
        let output = ast.to_c_ast().print();
        println!("{}", output);
    }

    // fn main () {
    //     unsigned int x;
    //     unsigned int y;
    //     unsigned int z;
    //     unsigned int w;
    //     unsigned int v;
    //     unsigned int cond;
    //     x = y
    // L1:
    //     x = (x + w)
    //     cond = func(x)
    //     if cond {
    //         goto L1
    //     }
    //     return
    // }
    #[test]
    fn simple_c_ast_complex_test() {
        let mut ast = SimpleCAST::new("main");
        let entry = ast.entry;
        let x = ast.var("x", None);
        let y = ast.var("y", None);
        let w = ast.var("w", None);
        let z = ast.var("z", None);
        let v = ast.var("v", None);
        let cond = ast.var("cond", None);
        let assn1 = ast.assign(x, y, entry);
        let add = ast.expr(&[x, w], c_simple::Expr::Add);
        let assn2 = ast.assign(x, add, assn1);
        let f_call = ast.call_func("func", &[x], assn2, Some(cond));
        let break_goto = ast.add_goto(assn2, "L1", f_call);
        let if_node = ast.conditional(cond, break_goto, None, f_call);
        let _ = ast.add_return(None, if_node);
        let output = ast.to_c_ast().print();
        println!("{}", output);
    }

    // fn main() {
    //     unsigned int x;
    //     unsigned int v;
    //     unsigned int w;
    //     unsigned int cond;
    //     unsigned int y;
    //     unsigned int z;
    //     x = y
    // L1:
    //     x = (x + w)
    //     cond = func(x)
    //     if(cond) {
    //         go(x)
    //         goto L1
    //     }
    // }
    #[test]
    fn simple_c_ast_complex1_test() {
        let mut ast = SimpleCAST::new("main");
        let entry = ast.entry;
        let x = ast.var("x", None);
        let y = ast.var("y", None);
        let w = ast.var("w", None);
        let z = ast.var("z", None);
        let v = ast.var("v", None);
        let cond = ast.var("cond", None);
        let assn1 = ast.assign(x, y, entry);
        let add = ast.expr(&[x, w], c_simple::Expr::Add);
        let assn2 = ast.assign(x, add, assn1);
        let f_call = ast.call_func("func", &[x], assn2, Some(cond));
        let f_call1 = ast.call_func("go", &[x], f_call, None);
        let break_goto = ast.add_goto(assn2, "L1", f_call1);
        let if_node = ast.conditional(cond, f_call1, None, f_call);
        let _ = ast.add_return(None, if_node);
        let output = ast.to_c_ast().print();
        println!("{}", output);
    }
}
