//! This module is for coverting `CAST` from `CCFG`.
//!
//! `CCFG` is CFG-like Graph so that we can do control flow structuring easily.

use std::collections::{HashMap, HashSet};
use super::c_ast;
use super::c_ast::{Ty, CAST};
use petgraph::graph::{Graph, NodeIndex, EdgeIndex, EdgeReference};
use petgraph::visit::EdgeRef;
use petgraph::Direction;

#[derive(Debug, Clone, PartialEq)]
pub enum CCFGNode {
    /// Entry node of target function
    Entry,
    Action(ActionNode),
    Value(ValueNode),
    Unknown,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ActionNode {
    Assignment,
    Call(String),
    Return,
    If,
    Goto,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueNode {
    /// The string is the name of variable
    Variable(Option<Ty>, String),
    /// Constant or immidiate value
    Constant(Option<Ty>, String),
    Expression(c_ast::Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum CCFGEdge {
    Action(ActionEdge),
    Value(ValueEdge),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ActionEdge {
    IfThen,
    IfElse,
    Normal,
    /// Destination of Goto statement
    GotoDst,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueEdge {
    DeRef,
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
}

type CCFGRef = NodeIndex;
type CASTRef = NodeIndex;

pub struct CCFG {
    /// Name of function of this AST
    fname: String,
    /// Entry node of this function
    pub entry: CCFGRef,
    /// Unknown node
    pub unknown: CCFGRef,
    g: Graph<CCFGNode, CCFGEdge>,
    /// Variables declared in this function, bool value is `is_implicit` flag
    vars: HashSet<(bool, CCFGRef)>,
    /// Constants declared in this function, bool value is `is_implicit` flag
    consts: HashSet<(bool, CCFGRef)>,
    /// Expressions declared in this function, bool value is `is_implicit` flag
    exprs: Vec<(bool, CCFGRef)>,
    /// Hashmap from label node to string it represents
    label_map: HashMap<CCFGRef, String>,
    /// Debug information retrieved from SSA
    debug_info: HashMap<CCFGRef, String>,
}


impl CCFGNode {
    fn is_exit(&self) -> bool {
        match self {
            &CCFGNode::Action(ActionNode::Return) => true,
            _ => false,
        }
    }
}

/// Returns nodes which is connected with given type of edge
fn neighbors_by_edge(edges: &Vec<EdgeReference<CCFGEdge>>, ty: &CCFGEdge) -> Vec<CCFGRef> {
    edges.iter().filter(|e| (*e).weight() == ty)
        .map(|e| e.target())
        .collect()
}

impl CCFG {
    pub fn new(fn_name: &str) -> CCFG {
        let mut g = Graph::new();
        let entry = g.add_node(CCFGNode::Entry);
        let unknown = g.add_node(CCFGNode::Unknown);
        CCFG {
            fname: fn_name.to_string(),
            entry: entry,
            unknown: unknown,
            g: g,
            vars: HashSet::new(),
            consts: HashSet::new(),
            exprs: Vec::new(),
            label_map: HashMap::new(),
            debug_info: HashMap::new(),
        }
    }

    /// Append a string for given node
    pub fn debug_info_at(&mut self, node: CCFGRef, comment: String) {
        let s = if let Some(c) = self.debug_info.get(&node).cloned() {
            c
        } else {
            "".to_string()
        };
        self.debug_info.insert(node, format!("{} {}", s, comment));
    }

    pub fn add_edge(&mut self, source: CCFGRef, target: CCFGRef, edge: CCFGEdge) -> EdgeIndex {
        self.g.add_edge(source, target, edge)
    }

    /// Add ValueNode of variable
    pub fn var(&mut self, name: &str, ty: Option<Ty>) -> CCFGRef {
        let node = self.g.add_node(CCFGNode::Value(ValueNode::Variable(ty, name.to_string())));
        self.vars.insert((false, node));
        node
    }

    /// Add ValueNode of constant value
    pub fn constant(&mut self, name: &str, ty: Option<Ty>) -> CCFGRef {
        let node = self.g.add_node(CCFGNode::Value(ValueNode::Constant(ty, name.to_string())));
        self.consts.insert((true, node));
        node
    }

    /// Add ValueNode of expression
    pub fn expr(&mut self, operands: &[CCFGRef], op: c_ast::Expr) -> CCFGRef {
        let node = self.g.add_node(CCFGNode::Value(ValueNode::Expression(op)));
        for (i, operand) in operands.iter().enumerate() {
            let _ = self.g.add_edge(node, *operand, CCFGEdge::Value(ValueEdge::Operand(i as u8)));
        }
        self.exprs.push((true, node));
        node
    }

    pub fn derefed_node(&self, node: CCFGRef) -> Option<CCFGRef> {
        // TODO check whether there are more than two derefed nodes
        self.g.edges_directed(node, Direction::Incoming)
            .filter(|e| *e.weight() == CCFGEdge::Value(ValueEdge::DeRef))
            .next()
            .map(|e| e.source())
    }

    pub fn deref(&mut self, operand: CCFGRef) -> CCFGRef {
        let node = self.g.add_node(CCFGNode::Value(ValueNode::Expression(c_ast::Expr::DeRef)));
        let _ = self.g.add_edge(node, operand, CCFGEdge::Value(ValueEdge::DeRef));
        // Operand edge is needed so that CAST can evaluate a derefed node from this.
        let _ = self.g.add_edge(node, operand, CCFGEdge::Value(ValueEdge::Operand(0)));
        self.exprs.push((true, node));
        node
    }

    /// Add ActionNode of assignment
    pub fn assign(&mut self, dst: CCFGRef, src: CCFGRef, prev_action: CCFGRef) -> CCFGRef {
        let node = self.g.add_node(CCFGNode::Action(ActionNode::Assignment));
        let _ = self.g.add_edge(node, dst, CCFGEdge::Value(ValueEdge::AssignDst));
        let _ = self.g.add_edge(node, src, CCFGEdge::Value(ValueEdge::AssignSrc));
        let _ = self.g.add_edge(prev_action, node, CCFGEdge::Action(ActionEdge::Normal));
        node
    }

    /// Add ActionNode of function call
    pub fn call_func(&mut self, fname: &str, args: &[CCFGRef], prev_action: CCFGRef,
                 ret_val: Option<CCFGRef>) -> CCFGRef {
        let call_node = self.g.add_node(CCFGNode::Action(ActionNode::Call(fname.to_string())));
        for (i, arg) in args.iter().enumerate() {
            self.g.add_edge(call_node, *arg, CCFGEdge::Value(ValueEdge::Arg(i as u8)));
        }
        self.g.add_edge(prev_action, call_node, CCFGEdge::Action(ActionEdge::Normal));
        if ret_val.is_some() {
            self.g.add_edge(call_node, ret_val.unwrap(), CCFGEdge::Value(ValueEdge::FuncVal));
        }
        call_node
    }

    fn insert_node(&mut self, prev_action: CCFGRef, next_action: CCFGRef, node: CCFGRef) {
        let e = self.g.find_edge(prev_action, next_action)
            .map(|x| (x, self.g.edge_weight(x).map(|a| a.clone())));
        match e {
            Some((idx, Some(CCFGEdge::Action(_)))) => {
                self.g.remove_edge(idx);
                self.g.add_edge(prev_action, node, CCFGEdge::Action(ActionEdge::Normal));
                self.g.add_edge(node, next_action, CCFGEdge::Action(ActionEdge::Normal));
            },
            _ => {
                radeco_warn!("Invalid nodes {:?}, {:?}", prev_action, next_action);
            },
        }
    }

    fn remove_incoming_actions(&mut self, node: CCFGRef) {
        let ns = self.g.edges_directed(node, Direction::Incoming)
            .into_iter()
            .filter_map(|e| {
                match e.weight() {
                    CCFGEdge::Action(_) => Some(e.id()),
                    _ => None,
                }
            }).collect::<Vec<_>>();
        for e in ns {
            self.g.remove_edge(e);
        }
    }

    /// Add ActionNode of if statement,
    /// `if_then`, `if_else` is the CCFGRef of first node of ActionNode
    pub fn conditional(&mut self, condition: CCFGRef, if_then: CCFGRef,
                   if_else: Option<CCFGRef>, prev_action: CCFGRef) -> CCFGRef {
        let node = self.g.add_node(CCFGNode::Action(ActionNode::If));
        self.remove_incoming_actions(if_then);
        if if_else.is_some() {
            self.remove_incoming_actions(if_else.unwrap());
            self.g.add_edge(node, if_else.unwrap(), CCFGEdge::Action(ActionEdge::IfElse));
        }
        self.g.add_edge(node, if_then, CCFGEdge::Action(ActionEdge::IfThen));
        self.g.add_edge(node, condition, CCFGEdge::Value(ValueEdge::Conditional));
        self.g.add_edge(prev_action, node, CCFGEdge::Action(ActionEdge::Normal));
        node
    }

    pub fn conditional_insert(&mut self, condition: CCFGRef, if_then: CCFGRef,
                   if_else: Option<CCFGRef>, prev: CCFGRef) -> CCFGRef {
        let es = self.g.edges_directed(prev, Direction::Outgoing)
            .into_iter()
            .filter_map(|e| {
                match e.weight() {
                    CCFGEdge::Action(ActionEdge::Normal) => Some((e.target(), e.id())),
                    _ => None,
                }
            }).collect::<Vec<_>>();
        if es.len() > 1 {
            radeco_warn!("More than one Normal Edges found");
        }
        let if_node = self.conditional(condition, if_then, if_else, prev);
        self.add_edge(prev, if_node, CCFGEdge::Action(ActionEdge::Normal));
        if let Some(&(next, idx)) = es.first() {
            self.g.remove_edge(idx);
            self.add_edge(if_node, next, CCFGEdge::Action(ActionEdge::Normal));
        };
        if_node
    }

    /// Add ActionNode of return statement
    pub fn add_return(&mut self, ret_val: Option<CCFGRef>, prev_action: CCFGRef) -> CCFGRef {
        let node = self.g.add_node(CCFGNode::Action(ActionNode::Return));
        if ret_val.is_some() {
            self.g.add_edge(node, ret_val.unwrap(), CCFGEdge::Value(ValueEdge::RetVal));
        }
        self.g.add_edge(prev_action, node, CCFGEdge::Action(ActionEdge::Normal));
        node
    }

    pub fn insert_goto_before(&mut self, next: CCFGRef, dst: CCFGRef, label_str: &str) -> CCFGRef {
        let es = self.g.edges_directed(next, Direction::Incoming)
            .into_iter()
            .filter_map(|e| {
                match e.weight() {
                    CCFGEdge::Action(ActionEdge::Normal) => Some((e.source(), e.id())),
                    _ => None,
                }
            }).collect::<Vec<_>>();
        if es.len() > 1 {
            radeco_warn!("More than one Normal Edges found");
        }
        let goto_node = if let Some(&(prev, idx)) = es.first() {
            self.g.remove_edge(idx);
            let goto_node = self.add_goto(dst, label_str, prev);
            self.add_edge(prev, goto_node, CCFGEdge::Action(ActionEdge::Normal));
            goto_node
        } else {
            let entry = self.entry;
            self.add_goto(dst, label_str, entry)
        };
        self.add_edge(goto_node, next, CCFGEdge::Action(ActionEdge::Normal));
        goto_node
    }

    pub fn add_goto(&mut self, dst: CCFGRef, label_str: &str, prev_action: CCFGRef) -> CCFGRef {
        let node = self.g.add_node(CCFGNode::Action(ActionNode::Goto));
        self.g.add_edge(node, dst, CCFGEdge::Action(ActionEdge::GotoDst));
        self.g.add_edge(prev_action, node, CCFGEdge::Action(ActionEdge::Normal));
        self.label_map.insert(dst, label_str.to_string());
        node
    }

    pub fn insert_goto(&mut self, prev_action: CCFGRef, next_action: CCFGRef,
                   dst: CCFGRef, label_str: &str) -> CCFGRef {
        let node = self.g.add_node(CCFGNode::Action(ActionNode::Goto));
        let _ = self.g.add_edge(node, dst, CCFGEdge::Action(ActionEdge::GotoDst));
        self.insert_node(prev_action, next_action, node);
        self.label_map.insert(dst, label_str.to_string());
        node
    }

    fn next_action(&self, idx: CCFGRef) -> Option<CCFGRef> {
        self.g.edges_directed(idx, Direction::Outgoing)
            .into_iter()
            .filter_map(|e| {
                match e.weight() {
                    &CCFGEdge::Action(ActionEdge::Normal) => Some(e.target()),
                    _ => None,
                }
            }).next()
    }

    fn gather_actions(&self, idx: CCFGRef, action_type: &CCFGEdge) -> Option<Vec<CCFGRef>> {
        let next_node = |node, edge| {
            let ns = self.g
                .edges_directed(node, Direction::Outgoing)
                .into_iter()
                .collect::<Vec<_>>();
            neighbors_by_edge(&ns, edge)
                .first().map(|e| e.clone())
        };
        let next_normal = move |node| {
            next_node(node, &CCFGEdge::Action(ActionEdge::Normal))
        };
        let mut first = next_node(idx, action_type);
        let mut ret = Vec::new();
        while let Some(n) = first {
            ret.push(n);
            first = next_normal(n);
        }
        if ret.len() > 0 {
            Some(ret)
        } else {
            None
        }
    }


    // Returns a pair (IfThen, IfElse)
    fn branch(&self, idx: CCFGRef) -> Option<(Vec<CCFGRef>, Option<Vec<CCFGRef>>)> {
        if self.g.node_weight(idx) != Some(&CCFGNode::Action(ActionNode::If)) {
            return None;
        }
        let if_then = self.gather_actions(idx, &CCFGEdge::Action(ActionEdge::IfThen));
        let if_else = self.gather_actions(idx, &CCFGEdge::Action(ActionEdge::IfElse));
        if if_then.is_some() {
            Some((if_then.unwrap(), if_else))
        } else {
            None
        }
    }

    // Returns value node which represents condition used by If statement
    fn branch_condition(&self, idx: CCFGRef) -> Option<CCFGRef> {
        match self.g.node_weight(idx) {
            Some(&CCFGNode::Action(ActionNode::If)) => {},
            _ => return None,
        }
        let ns = self.g.edges_directed(idx, Direction::Outgoing).into_iter().collect();
        let expr = {
            let tmp = neighbors_by_edge(&ns, &CCFGEdge::Value(ValueEdge::Conditional));
            if tmp.len() > 1 {
                radeco_warn!("More than one expressions found: If");
            }
            tmp.first().map(|e| e.clone())
        };
        expr
    }

    // Returns destination of goto statement
    fn goto(&self, idx: CCFGRef) -> Option<CCFGRef> {
        if self.g.node_weight(idx) != Some(&CCFGNode::Action(ActionNode::Goto))  {
            return None;
        };
        let goto_dsts = self.g.edges_directed(idx, Direction::Outgoing)
            .into_iter()
            .filter_map(|e| {
                match e.weight() {
                    &CCFGEdge::Action(ActionEdge::GotoDst) => Some(e.target()),
                    _ => None,
                }
            }).collect::<Vec<_>>();
        if goto_dsts.len() > 1 {
            radeco_warn!("More than 1 labels found as Goto destination");
            return None;
        };
        goto_dsts.first().cloned()
    }

    // Returns a pair (Dst, Src) which represents Dst = Src
    fn assignment(&self, idx: CCFGRef) -> Option<(CCFGRef, CCFGRef)> {
        if self.g.node_weight(idx) != Some(&CCFGNode::Action(ActionNode::Assignment)) {
            return None;
        }
        let ns = self.g.edges_directed(idx, Direction::Outgoing).into_iter().collect();
        let src = neighbors_by_edge(&ns, &CCFGEdge::Value(ValueEdge::AssignSrc))
            .first().map(|e| e.clone());
        let dst = neighbors_by_edge(&ns, &CCFGEdge::Value(ValueEdge::AssignDst))
            .first().map(|e| e.clone());
        if src.is_some() && dst.is_some() {
            return Some((dst.unwrap(), src.unwrap()))
        };
        None
    }

    // Returns arguments of function call
    fn args_call(&self, idx: CCFGRef) -> Option<Vec<CCFGRef>> {
        match self.g.node_weight(idx) {
            Some(&CCFGNode::Action(ActionNode::Call(_))) => {},
            _ => {return None;},
        };
        let mut args = self.g.edges_directed(idx, Direction::Outgoing)
            .into_iter()
            .filter_map(|e| {
                match e.weight() {
                    &CCFGEdge::Value(ValueEdge::Arg(o)) => Some((o, e.target())),
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
    fn func_val(&self, idx: CCFGRef) -> Option<CCFGRef> {
        match self.g.node_weight(idx) {
            Some(&CCFGNode::Action(ActionNode::Call(_))) => {},
            _ => {return None;},
        };
        let ret = self.g.edges_directed(idx, Direction::Outgoing)
            .into_iter()
            .filter_map(|e| {
                match e.weight() {
                    &CCFGEdge::Value(ValueEdge::FuncVal) => Some(e.target()),
                    _ => None,
                }
            }).collect::<Vec<_>>();
        if ret.len() > 1 {
            radeco_warn!("More than one variable found for FuncVal");
        }
        ret.first().map(|x| *x)
    }

    // Returns value node of the return value of a return statement
    fn ret_val(&self, idx: CCFGRef) -> Option<CCFGRef> {
        match self.g.node_weight(idx) {
            Some(&CCFGNode::Action(ActionNode::Return)) => {},
            _ => {return None;}
        };
        let ret_val = self.g.edges_directed(idx, Direction::Outgoing)
            .into_iter()
            .filter_map(|e| {
                match e.weight() {
                    &CCFGEdge::Value(ValueEdge::RetVal) => Some(e.target()),
                    _ => None,
                }
            }).collect::<Vec<_>>();
        if ret_val.len() > 1 {
            radeco_warn!("More than one return values found");
        };
        ret_val.into_iter().next()
    }

    // Returns operands used by a given expression
    fn operands_from_expr(&self, expr: CCFGRef) -> Vec<CCFGRef> {
        let mut operands = self.g.edges_directed(expr, Direction::Outgoing)
            .into_iter()
            .filter_map(|e| {
                match e.weight() {
                    &CCFGEdge::Value(ValueEdge::Operand(i)) => Some((i, e.target())),
                    _ => None,
                }
            }).collect::<Vec<_>>();
        operands.sort_by_key(|k| k.0);
        operands.into_iter().map(|(_, n)| n).collect::<Vec<_>>()
    }

    pub fn to_c_ast(&self) -> CAST {
        let mut converter = CASTConverter::new(&self);
        converter.to_c_ast();
        converter.ast
    }

    /// Returns constant value which is either register or immidiate value
    pub fn constant_of(&self, node: CCFGRef) -> Option<String> {
        match self.g.node_weight(node) {
            Some(&CCFGNode::Value(ValueNode::Constant(_, ref s))) => Some(s.clone()),
            _ => None,
        }
    }

    pub fn is_assign_node(&self, node: CCFGRef) -> bool {
        match self.g.node_weight(node) {
            Some(&CCFGNode::Action(ActionNode::Assignment)) => true,
            _ => false
        }
    }

    pub fn is_call_node(&self, node: CCFGRef) -> bool {
        match self.g.node_weight(node) {
            Some(&CCFGNode::Action(ActionNode::Call(_))) => true,
            _ => false
        }
    }
}

/// CCFG should meet following conditions.
/// 1. There are at most 1 ActionEdge::Normal from each node.
/// 2. There are ActionEdge::IfThen, ValueEdge::Conditional from ActionNode::If
/// 3. The targets of value edges are ValueNode, The target of action edges are ActionNode.
/// 4. The destination node of GotoDst edge is ActionNode.
pub struct CCFGVerifier {
}

type Verifier = Fn(CCFGRef, &CCFG) -> Result<(), String>;
impl CCFGVerifier {
    const DELIM: &'static str = "; ";
    pub fn verify(cast: &CCFG) -> Result<(), String> {
        Self::verify_each_node(cast, &Self::verify_normal_action, "Normal action")?;
        Self::verify_each_node(cast, &Self::verify_if, "If")?;
        Self::verify_each_node(cast, &Self::verify_edge_action, "Edge-Action")?;
        Self::verify_each_node(cast, &Self::verify_goto, "Goto")?;
        Ok(())
    }

    fn verify_each_node(cast: &CCFG, verifier: &Verifier, name: &str) -> Result<(), String> {
        let mut errors = Vec::new();
        let nodes = cast.g.node_indices();
        for node in nodes {
            if let Err(msg) = verifier(node, cast) {
                errors.push(msg);
            }
        }
        if errors.len() > 0 {
            Err(format!("{} @ {}", errors.join(Self::DELIM), name.to_string()))
        } else {
            Ok(())
        }
    }

    // 1. There are at most 1 ActionEdge::Normal from each node.
    fn verify_normal_action(node: CCFGRef, cast: &CCFG) -> Result<(), String> {
        match cast.g.node_weight(node) {
            Some(&CCFGNode::Action(_)) => {},
            _ => return Ok(()),
        };
        let normal_actions = cast.g.edges_directed(node, Direction::Outgoing)
            .filter(|e| {
                match e.weight() {
                    &CCFGEdge::Action(ActionEdge::Normal) => true,
                    _ => false,
                }
            });
        let length = normal_actions.count();
        if length <= 1 {
            Ok(())
        } else {
            Err(format!("There are {} next normal actions @ {:?}", length, node))
        }
    }

    // 2. There are ActionEdge::IfThen, ValueEdge::Conditional from ActionNode::If
    fn verify_if(node: CCFGRef, cast: &CCFG) -> Result<(), String> {
        match cast.g.node_weight(node) {
            Some(&CCFGNode::Action(ActionNode::If)) => {},
            _ => return Ok(()),
        };
        let mut errors = Vec::new();
        let cond = cast.branch_condition(node);
        if cond.is_none() {
            errors.push("No condition node is found.");
        }
        let branches = cast.branch(node);
        if branches.is_none() {
            errors.push("No branch is found.");
        }
        if cond.is_none() || branches.is_none() {
            Err(errors.join(Self::DELIM))
        } else {
            Ok(())
        }
    }

    // 3. The targets of value edges are ValueNode, The target of action edges are ActionNode.
    fn verify_edge_action(node: CCFGRef, cast: &CCFG) -> Result<(), String> {
        let mut errors = Vec::new();
        let edges = cast.g.edges_directed(node, Direction::Outgoing);
        for edge in edges {
            match (edge.weight(), cast.g.node_weight(edge.target())) {
                (CCFGEdge::Action(_), Some(&CCFGNode::Action(_)))
                | (CCFGEdge::Value(_), Some(&CCFGNode::Value(_)))
                | (CCFGEdge::Action(ActionEdge::GotoDst), Some(&CCFGNode::Entry)) => {},
                _ => {
                    let error = format!("{:?} {:?}", edge.weight(),
                            cast.g.node_weight(edge.target()));
                    errors.push(error);
                }
            }
        }
        if errors.len() > 0 {
            Err(errors.join(Self::DELIM))
        } else {
            Ok(())
        }
    }

    // 4. The destination node of GotoDst edge is ActionNode.
    fn verify_goto(node: CCFGRef, cast: &CCFG) -> Result<(), String> {
        match cast.g.node_weight(node) {
            Some(&CCFGNode::Action(ActionNode::Goto)) => {},
            _ => return Ok(()),
        };
        let mut errors = Vec::new();
        let gotos = cast.g
            .edges_directed(node, Direction::Outgoing)
            .filter_map(|e| match e.weight() {
                CCFGEdge::Action(ActionEdge::GotoDst) => Some(e.target()),
                _ => None,
            }).collect::<Vec<_>>();
        if gotos.len() != 1 {
            errors.push("No or more than one ActionEdge::GotoDst found".to_string());
        }
        for goto in gotos {
            match cast.g.node_weight(goto) {
                Some(&CCFGNode::Action(_))
                | Some(&CCFGNode::Entry) => {},
                n => {
                    errors.push(format!("Invalid node {:?} @ {:?}", n, node));
                },
            }
        }
        if errors.len() > 0 {
            Err(errors.join(Self::DELIM))
        } else {
            Ok(())
        }
    }
}

/// This is used for translating CCFG to CAST
struct CASTConverter<'a> {
    cfg: &'a CCFG,
    ast: CAST,
    /// HashMap from CCFG's node to CAST's node
    node_map: HashMap<CCFGRef, CASTRef>,
    visited: HashSet<CCFGRef>,
}

impl<'a> CASTConverter<'a> {
    fn new(cfg: &CCFG) -> CASTConverter {
        CASTConverter {
            ast: CAST::new(&cfg.fname),
            cfg: cfg,
            node_map: HashMap::new(),
            visited: HashSet::new(),
        }
    }

    /// Entry point of Simple-C-AST to C-AST conversion.
    pub fn to_c_ast(&mut self) {
        let unknown_node = self.ast.declare_vars(Ty::new(c_ast::BTy::Int, false, 0), &["unknown".to_string()], true)
            .first().cloned().expect("This can not be None");
        self.node_map.insert(self.cfg.unknown, unknown_node);
        for &(is_implicit, con) in self.cfg.consts.iter() {
            if let Some(&CCFGNode::Value(ValueNode::Constant(ref ty_opt, ref value_name))) = self.cfg.g.node_weight(con) {
                let ty = ty_opt.clone().unwrap_or(Ty::new(c_ast::BTy::Int, false, 0));
                let n = self.ast.declare_vars(ty, &[value_name.to_string()], is_implicit);
                self.node_map.insert(con, n[0]);
            }
        }
        // XXX It should report error if there are defferent types for same name variables.
        let mut declared_vars = HashSet::new();
        for &(is_implicit, var) in self.cfg.vars.iter() {
            if let Some(&CCFGNode::Value(ValueNode::Variable(ref ty_opt, ref var_name))) = self.cfg.g.node_weight(var) {
                let ty = ty_opt.clone().unwrap_or(Ty::new(c_ast::BTy::Int, false, 0));
                let is_declared = declared_vars.contains(var_name);
                let n = self.ast.declare_vars(ty, &[var_name.to_string()], is_implicit || is_declared);
                if !is_declared {
                    declared_vars.insert(var_name.to_string());
                }
                self.node_map.insert(var, n[0]);
            }
        }
        for &(is_implicit, expr) in self.cfg.exprs.iter() {
            if let Some(&CCFGNode::Value(ValueNode::Expression(ref op))) = self.cfg.g.node_weight(expr) {
                let operands = self.cfg.operands_from_expr(expr)
                    .into_iter()
                    .map(|_n| {
                        if let Some(&n) = self.node_map.get(&_n) {
                            n
                        } else {
                            radeco_warn!("{:?} not found", _n);
                            unknown_node
                        }
                    }).collect::<Vec<_>>();
                let n = self.ast.expr(op.clone(), &operands, is_implicit);
                self.node_map.insert(expr, n);
            }
        }
        let entry = self.cfg.entry;
        self.to_c_ast_body(entry);
    }

    fn to_c_ast_body(&mut self, current_node: CCFGRef) {
        if self.visited.contains(&current_node) {
            return;
        };
        self.visited.insert(current_node);
        if let Some(ref l) = self.cfg.label_map.get(&current_node) {
            self.ast.label(l);
        };
        let idx = self.cfg.g.node_weight(current_node).cloned();
        let ast_node = match idx {
            Some(CCFGNode::Action(ActionNode::Assignment)) => {
                self.to_c_ast_assignment(current_node)
            },
            Some(CCFGNode::Action(ActionNode::Call(ref name))) => {
                self.to_c_ast_call(current_node, name)
            },
            Some(CCFGNode::Action(ActionNode::Return)) => {
                self.to_c_ast_return(current_node)
            },
            Some(CCFGNode::Action(ActionNode::If)) => {
                self.to_c_ast_if(current_node)
            },
            Some(CCFGNode::Action(ActionNode::Goto)) => {
                self.to_c_ast_goto(current_node)
            },
            Some(CCFGNode::Entry) => {
                // fallthrough
                Err("TODO")
            },
            _ => {
                radeco_err!("Unreachable node {:?}", idx);
                unreachable!()
            },
        };
        if let Ok(n) = ast_node {
            self.node_map.insert(current_node, ast_node.unwrap());
        }
        if let Err(err) = ast_node {
            radeco_err!(err);
        }

        if let Some(ref comment) = self.cfg.debug_info.get(&current_node) {
            if let Some(&node) = self.node_map.get(&current_node) {
                self.ast.comment_at(node, comment);
            }
        }

        if let Some(n) = self.cfg.next_action(current_node) {
            self.to_c_ast_body(n);
        };
    }

    fn to_c_ast_assignment(&mut self, node: CCFGRef) -> Result<CASTRef, &'static str> {
        let tmp = self.cfg.assignment(node)
            .and_then(|(d, s)| {
                match (self.node_map.get(&d), self.node_map.get(&s)) {
                    (Some(&x), Some(&y)) => Some((x.clone(), y.clone())),
                    _ => None
                }
            });
        if let Some((dst, src)) = tmp {
            let ret = self.ast.expr(c_ast::Expr::Assign, &[dst, src], false);
            Ok(ret)
        } else {
            Err("CASTConverter::to_c_ast_assignment")
        }
    }

    fn to_c_ast_call(&mut self, node: CCFGRef, name: &str) -> Result<CASTRef, &'static str> {
        let args = self.cfg.args_call(node)
            .unwrap_or(Vec::new())
            .into_iter()
            .map(|arg| {
                let ret = self.node_map.get(&arg).map(|a| *a);
                if ret.is_none() {
                    radeco_warn!("Error args_call");
                }
                ret
            }).collect();
        let ret_node_opt = self.cfg.func_val(node)
            .and_then(|x| self.node_map.get(&x).map(|a| *a));
        let node = self.ast.call_func(name, args);
        if let Some(ret_node) = ret_node_opt {
            self.ast.expr(c_ast::Expr::Assign, &[ret_node, node], false);
        }
        Ok(node)
    }

    fn to_c_ast_return(&mut self, node: CCFGRef) -> Result<CASTRef, &'static str> {
        let opt = self.cfg.ret_val(node)
            .and_then(|n| self.node_map.get(&n))
            .map(|n| *n);
        Ok(self.ast.ret(opt))
    }

    fn to_c_ast_if(&mut self, node: CCFGRef) -> Result<CASTRef, &'static str> {
        let cond = self.cfg.branch_condition(node)
            .unwrap_or(self.cfg.unknown);
        let branches = self.cfg.branch(node).map(|x| x.clone());
        if let Some((if_then, if_else)) = branches {
            for n in if_then.iter() {
                self.to_c_ast_body(*n);
            }
            if if_else.is_some() {
                for n in if_else.as_ref().unwrap().iter() {
                    self.to_c_ast_body(*n);
                }
            }
            // TODO avoid unwrap
            let c = *self.node_map.get(&cond).unwrap();
            let t = if_then.into_iter()
                .map(|x| {
                    self.node_map.get(&x).cloned().unwrap_or(self.cfg.unknown)
                }).collect::<Vec<_>>();
            let e = if_else.map(|x| x.iter().map(|y| {
                self.node_map.get(y).cloned().unwrap_or(self.cfg.unknown)
            }).collect::<Vec<_>>());
            Ok(self.ast.new_if(c, t, e))
        } else {
            Err("CCFG::branch failed")
        }
    }

    fn to_c_ast_goto(&mut self, node: CCFGRef) -> Result<CASTRef, &'static str> {
        let dst_opt = self.cfg.goto(node)
            .and_then(|d| self.cfg.label_map.get(&d))
            .map(|d| d.clone());
        if dst_opt.is_none() {
            radeco_warn!("Error Goto");
        };
        let dst = dst_opt.unwrap_or("unknown_label".to_string());
        Ok(self.ast.goto(&dst))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use self::c_ast::{BTy, Ty};

    // fn main () {
    //     unsigned int x;
    //     unsigned int y;
    //     unsigned int z;
    //     unsigned int w;
    //     x = y
    //     func(z, w)
    // }
    #[test]
    fn c_cfg_basic_test() {
        let mut cfg = CCFG::new("main");
        let x = cfg.var("x", None);
        let y = cfg.var("y", None);
        let z = cfg.var("z", None);
        let w = cfg.var("w", None);
        let entry = cfg.entry;
        let assn = cfg.assign(x, y, entry);
        let _ = cfg.call_func("func", &[z, w], assn, None);
        CCFGVerifier::verify(&cfg).expect("CCFG verification failed");
        let output = cfg.to_c_ast().print();
        println!("{}", output);
    }

    // fn main () {
    //     unsigned int x;
    //     unsigned int y;
    //     unsigned int z;
    //     x = (x + y)
    // }
    #[test]
    fn c_cfg_expr_test() {
        let mut cfg = CCFG::new("main");
        let x = cfg.var("x", None);
        let y = cfg.var("y", None);
        let z = cfg.var("z", None);
        let entry = cfg.entry;
        let expr = cfg.expr(&[x, y], c_ast::Expr::Add);
        let assn = cfg.assign(x, expr, entry);
        CCFGVerifier::verify(&cfg).expect("CCFG verification failed");
        let output = cfg.to_c_ast().print();
        println!("{}", output);
    }

    // fn main () {
    //     unsigned int y;
    //     unsigned int x;
    //     y = func(x)
    // }
    #[test]
    fn c_cfg_func_test() {
        let mut cfg = CCFG::new("main");
        let x = cfg.var("x", None);
        let y = cfg.var("y", None);
        let entry = cfg.entry;
        let call_f = cfg.call_func("func", &[x], entry, Some(y));
        CCFGVerifier::verify(&cfg).expect("CCFG verification failed");
        let output = cfg.to_c_ast().print();
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
    fn c_cfg_conditional_test() {
        let mut cfg = CCFG::new("main");
        let x = cfg.var("x", None);
        let y = cfg.var("y", None);
        let z = cfg.var("z", None);
        let w = cfg.var("w", None);
        let entry = cfg.entry;
        let assn = cfg.assign(x, y, entry);
        let call_test1 = cfg.call_func("test1", &[], assn, None);
        let call_f = cfg.call_func("func", &[z, w], assn, None);
        let call_test2 = cfg.call_func("test2", &[], call_f, None);
        let _ = cfg.conditional(x, assn, Some(call_f), entry);
        CCFGVerifier::verify(&cfg).expect("CCFG verification failed");
        let output = cfg.to_c_ast().print();
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
    fn c_cfg_goto_test() {
        let mut cfg = CCFG::new("main");
        let entry = cfg.entry;
        let x = cfg.var("x", None);
        let y = cfg.var("y", None);
        let assn = cfg.assign(x, y, entry);
        let _ = cfg.add_goto(entry, "L1", assn);
        let output = cfg.to_c_ast().print();
        CCFGVerifier::verify(&cfg).expect("CCFG verification failed");
        println!("{}", output);
    }

    // fn main () {
    //     unsigned int x;
    //     return x
    // }
    #[test]
    fn c_cfg_return_test() {
        let mut cfg = CCFG::new("main");
        let entry = cfg.entry;
        let x = cfg.var("x", None);
        let _ = cfg.add_return(Some(x), entry);
        CCFGVerifier::verify(&cfg).expect("CCFG verification failed");
        let output = cfg.to_c_ast().print();
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
    fn c_cfg_insert_goto_test() {
        let mut cfg = CCFG::new("main");
        let entry = cfg.entry;
        let x = cfg.var("x", None);
        let y = cfg.var("y", None);
        let assn = cfg.assign(x, y, entry);
        let ret = cfg.add_return(Some(x), assn);
        let _ = cfg.insert_goto(entry, assn, ret, "L1");
        CCFGVerifier::verify(&cfg).expect("CCFG verification failed");
        let output = cfg.to_c_ast().print();
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
    fn c_cfg_complex_test() {
        let mut cfg = CCFG::new("main");
        let entry = cfg.entry;
        let x = cfg.var("x", None);
        let y = cfg.var("y", None);
        let w = cfg.var("w", None);
        let z = cfg.var("z", None);
        let v = cfg.var("v", None);
        let cond = cfg.var("cond", None);
        let assn1 = cfg.assign(x, y, entry);
        let add = cfg.expr(&[x, w], c_ast::Expr::Add);
        let assn2 = cfg.assign(x, add, assn1);
        let f_call = cfg.call_func("func", &[x], assn2, Some(cond));
        let break_goto = cfg.add_goto(assn2, "L1", f_call);
        let if_node = cfg.conditional(cond, break_goto, None, f_call);
        let _ = cfg.add_return(None, if_node);
        CCFGVerifier::verify(&cfg).expect("CCFG verification failed");
        let output = cfg.to_c_ast().print();
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
    fn c_cfg_complex1_test() {
        let mut cfg = CCFG::new("main");
        let entry = cfg.entry;
        let x = cfg.var("x", None);
        let y = cfg.var("y", None);
        let w = cfg.var("w", None);
        let z = cfg.var("z", None);
        let v = cfg.var("v", None);
        let cond = cfg.var("cond", None);
        let assn1 = cfg.assign(x, y, entry);
        let add = cfg.expr(&[x, w], c_ast::Expr::Add);
        let assn2 = cfg.assign(x, add, assn1);
        let f_call = cfg.call_func("func", &[x], assn2, Some(cond));
        let f_call1 = cfg.call_func("go", &[x], f_call, None);
        let break_goto = cfg.add_goto(assn2, "L1", f_call1);
        let if_node = cfg.conditional(cond, f_call1, None, f_call);
        let _ = cfg.add_return(None, if_node);
        CCFGVerifier::verify(&cfg).expect("CCFG verification failed");
        let output = cfg.to_c_ast().print();
        println!("{}", output);
    }


    // fn main () {
    //     int i;
    //     unsigned long int u;
    //     double d;
    //     *float f;
    //     char c;
    //     void v;
    // }
    #[test]
    fn c_cfg_type_test() {
        let mut cfg = CCFG::new("main");
        let entry = cfg.entry;
        let i = cfg.var("i", Some(Ty::new(BTy::Int, true, 0)));
        let u = cfg.var("u", Some(Ty::new(BTy::Int, false, 1)));
        let d = cfg.var("d", Some(Ty::new(BTy::Double, true, 0)));
        let c = cfg.var("c", Some(Ty::new(BTy::Char, true, 0)));
        let v = cfg.var("v", Some(Ty::new(BTy::Void, true, 0)));
        let f = cfg.var("f", Some(Ty::new(BTy::Ptr(Box::new(BTy::Float)), true, 0)));
        CCFGVerifier::verify(&cfg).expect("CCFG verification failed");
        let output = cfg.to_c_ast().print();
        println!("{}", output);
    }
}
