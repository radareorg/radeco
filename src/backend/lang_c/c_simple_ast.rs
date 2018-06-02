use std::{default, iter, fmt};
use std::collections::{HashMap, HashSet};

use super::c_simple;
use super::c_simple::{Ty, CAST, CASTNode};
use middle::ir;
use middle::ir::MOpcode;
use middle::ssa::utils;
use middle::ssa::ssastorage::{NodeData, SSAStorage};
use middle::ssa::cfg_traits::CFG;
use middle::ssa::ssa_traits::{SSA, SSAExtra, SSAMod, SSAWalk, ValueInfo};
use frontend::radeco_containers::RadecoFunction;
use petgraph::graph::{Graph, NodeIndex, EdgeIndex, Edges, EdgeReference};
use petgraph::visit::EdgeRef;
use petgraph::{EdgeDirection, Direction, Directed};

macro_rules! add_jump_to_cfg {
    ($self: ident, $source: expr, $target: expr, $edge: expr) => {
        let src_node = $self.action_map.get(&$source).unwrap();
        let dst_node = $self.action_map.get(&$target).unwrap();
        $self.ast.ast.add_edge(*src_node, *dst_node, $edge);
    };
    ($self: ident, $source: expr, $target: expr) => {
        add_jump_to_cfg!($self, $source, $target, SimpleCASTEdge::Value(ValueEdge::GotoDst));
    }
}

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
    Dummy(String),
    DummyGoto,
}

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

    pub fn dummy_goto(&mut self, prev_action: NodeIndex) -> NodeIndex {
        let node = self.ast.add_node(SimpleCASTNode::Action(ActionNode::DummyGoto));
        let _ = self.ast.add_edge(prev_action, node, SimpleCASTEdge::Action(ActionEdge::Normal));
        node
    }

    pub fn dummy(&mut self, prev_action: NodeIndex, s: String) -> NodeIndex {
        let node = self.ast.add_node(SimpleCASTNode::Action(ActionNode::Dummy(s)));
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
            },
            _ => {
                radeco_warn!("Invalid nodes {:?}, {:?}", prev_action, next_action);
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

/// CASTBuilder is a constructor from RadecoFunction
struct CASTBuilder<'a> {
    ast: SimpleCAST,
    //NodeIndex of SimpleCAST
    last_action: NodeIndex,
    rfn: &'a RadecoFunction,
    // SSA of RadecoFunction
    ssa: &'a SSAStorage,
    data_graph: Graph<ValueNode, (u8, c_simple::Expr)>,
    // Hashmap from node of SSAStorage to one of self.data_graph
    node_map: HashMap<NodeIndex, NodeIndex>,
    action_map: HashMap<NodeIndex, NodeIndex>,
    reg_map: HashMap<String, NodeIndex>,
    seen: HashSet<NodeIndex>,
}

impl<'a> CASTBuilder<'a> {
    pub fn new(f: &'a RadecoFunction) -> CASTBuilder {
        let ast = SimpleCAST::new(f.name.as_ref());
        CASTBuilder {
            last_action: ast.entry,
            ast: ast,
            rfn: f,
            ssa: f.ssa(),
            data_graph: Graph::new(),
            node_map: HashMap::new(),
            action_map: HashMap::new(),
            reg_map: HashMap::new(),
            seen: HashSet::new(),
            // XXX
        }
    }

    // TODO rename
    fn declare_vars_from_rfn(&mut self) {
        unimplemented!()
    }

    //TODO Move to other trait, struct
    fn recover_data_flow(&mut self) {
        unimplemented!()
    }

    // For debugging
    fn dummy_goto(&mut self) -> NodeIndex {
        self.last_action = self.ast.dummy_goto(self.last_action);
        self.last_action
    }

    // For debugging
    fn dummy_action(&mut self, s: String) -> NodeIndex {
        self.last_action = self.ast.dummy(self.last_action, s);
        self.last_action
    }

    fn call_action(&mut self) -> NodeIndex {
        self.last_action = self.ast.call_func("", &[], self.last_action, None);
        self.last_action
    }

    fn recover(&mut self, node: NodeIndex) -> NodeIndex {
        assert!(self.is_recover_node(node));
        let op = self.ssa.opcode(node).unwrap_or(MOpcode::OpInvalid);
        radeco_trace!("CASTBuilder::recover {:?} @ {:?}", op, node);
        match op {
            MOpcode::OpCall => {
                self.call_action()
            },
            MOpcode::OpStore => {
                self.dummy_action(format!("{:?}", op))
            },
            MOpcode::OpLoad => {
                self.dummy_action(format!("{:?}", op))
            },
            _ => unreachable!(),
        }
    }

    fn is_recover_node(&self, node: NodeIndex) -> bool {
        let op = self.ssa.opcode(node).unwrap_or(MOpcode::OpInvalid);
        match op {
            MOpcode::OpCall
                | MOpcode::OpStore
                | MOpcode::OpLoad => true,
            _ => false,
        }
    }

    fn handle_binop(&mut self, ret_node: NodeIndex, ops: Vec<NodeIndex>,
                    expr: c_simple::Expr) {
        for (i, op) in ops.into_iter().enumerate() {
            self.data_graph.add_edge(ret_node, op, (i as u8, expr.clone()));
        }
    }

    fn handle_phi(&mut self, node: NodeIndex) {
        assert!(self.ssa.is_phi(node));
        radeco_trace!("CASTBuilder::handle_phi {:?}", node);
        if self.seen.contains(&node) {
            return;
        }
        let ops = self.ssa.operands_of(node);
        // TODO
        let ret_node = {
            let v = ValueNode::Variable(None, "TODO@phi".to_string());
            self.data_graph.add_node(v)
        };
        self.seen.insert(node);
        self.node_map.insert(node, ret_node);
    }

    fn update_values(&mut self, node: NodeIndex) {
        assert!(self.ssa.is_expr(node));
        radeco_trace!("CASTBuilder::update_values {:?}", node);
        if self.seen.contains(&node) {
            return;
        }
        self.seen.insert(node);
        let ops = self.ssa.operands_of(node);
        let ret_node = {
            let v = self.new_value(node);
            self.data_graph.add_node(v)
        };
        self.node_map.insert(node, ret_node);

        let mut ns = Vec::new();
        for op in ops {
            if let Some(&n) = self.node_map.get(&op) {
                ns.push(n);
            } else {
                radeco_warn!("Invalid operand");
            }
        }
        radeco_trace!("CASTBuilder::update_values opcode: {:?}", self.ssa.opcode(node));
        // TODO update data_graph
        match self.ssa.opcode(node).unwrap_or(MOpcode::OpInvalid) {
            MOpcode::OpStore => {
                // TODO add reference to source nodes
                for (i, op) in ns.into_iter().skip(1).enumerate() {
                    self.data_graph
                        .add_edge(ret_node,
                                  op,
                                  (i as u8, c_simple::Expr::Add));
                }
            }
            MOpcode::OpLoad => {
                self.data_graph
                    .add_edge(ret_node,
                              ns[1],
                              (0, c_simple::Expr::Assign));
            },
            MOpcode::OpAdd => self.handle_binop(ret_node, ns, c_simple::Expr::Add),
            MOpcode::OpAnd => self.handle_binop(ret_node, ns, c_simple::Expr::And),
            MOpcode::OpDiv => self.handle_binop(ret_node, ns, c_simple::Expr::Div),
            MOpcode::OpEq => self.handle_binop(ret_node, ns, c_simple::Expr::Eq),
            MOpcode::OpGt => self.handle_binop(ret_node, ns, c_simple::Expr::Gt),
            MOpcode::OpLsl => {
                //unimplemented!()
            },
            MOpcode::OpLsr => {
                //unimplemented!()
            },
            MOpcode::OpLt => self.handle_binop(ret_node, ns, c_simple::Expr::Lt),
            MOpcode::OpMod => self.handle_binop(ret_node, ns, c_simple::Expr::Mod),
            MOpcode::OpMul => self.handle_binop(ret_node, ns, c_simple::Expr::Mul),
            MOpcode::OpNarrow(size) => {
                self.data_graph
                    .add_edge(ret_node,
                              ns[0],
                              (0, c_simple::Expr::Cast(size as usize)));
            },
            MOpcode::OpNot => self.handle_binop(ret_node, ns, c_simple::Expr::Not),
            MOpcode::OpOr => self.handle_binop(ret_node, ns, c_simple::Expr::Or),
            MOpcode::OpRol => unimplemented!(),
            MOpcode::OpRor => unimplemented!(),
            MOpcode::OpSignExt(size) => {
                self.data_graph.add_edge(ret_node, ns[0], (0, c_simple::Expr::Cast(size as usize)));
            },
            MOpcode::OpSub => self.handle_binop(ret_node, ns, c_simple::Expr::Sub),
            MOpcode::OpXor => self.handle_binop(ret_node, ns, c_simple::Expr::Xor),
            MOpcode::OpZeroExt(size) => {
                self.data_graph.add_edge(ret_node, ns[0], (0, c_simple::Expr::Cast(size as usize)));
            },
            MOpcode::OpCall => {
                self.update_regs_by_call(node);
            },
            _ => {},
        }
    }

    fn prepare_consts(&mut self) {
        for (&val, &node) in self.ssa.constants.iter() {
            if let Ok(n) = self.ssa.node_data(node) {
                // TODO
                let v = ValueNode::Constant(None, val.to_string());
                let const_node = self.data_graph.add_node(v);
                self.node_map.insert(node, const_node);
            } else {
                radeco_warn!("Invalid constant");
            }
        }
    }

    fn update_regs_by_call(&mut self, call_node: NodeIndex) {
        radeco_trace!("CASTBuilder::update_regs_by_call {:?}", call_node);
        let reg_map = utils::call_rets(call_node, self.ssa);
        for (idx, (node, vt)) in reg_map.into_iter() {
            let name = self.ssa.regfile.get_name(idx).unwrap_or("mem").to_string();
            // TODO
            let v = ValueNode::Variable(None, "return value".to_string());
            let _node = self.data_graph.add_node(v);
            self.node_map.insert(node, _node);
            if let Some(&reg_node) = self.reg_map.get(&name) {
                self.data_graph.add_edge(_node, reg_node, (0, c_simple::Expr::Assign));
            }
        }
    }

    fn prepare_regs(&mut self) {
        let entry_node = self.ssa.entry_node();
        if entry_node.is_none() {
            radeco_warn!("Entry node not found");
            return;
        }
        // TODO avoid unwrap
        let reg_state = self.ssa.registers_in(self.ssa.entry_node().unwrap());
        if reg_state.is_none() {
            radeco_warn!("RegisterState not found");
            return;
        }
        let reg_map = utils::register_state_info(reg_state.unwrap(), self.ssa);
        for (idx, (node, vt)) in reg_map.into_iter() {
            let name = self.ssa.regfile.get_name(idx).unwrap_or("mem").to_string();
            // TODO
            let v = ValueNode::Variable(None, name.clone());
            let _node = self.data_graph.add_node(v);
            self.reg_map.insert(name, _node);
            self.node_map.insert(node, _node);
        }
    }

    fn new_value(&mut self, ssa_node: NodeIndex) -> ValueNode {
        // TODO avoid unwrap
        let vt = self.ssa.node_data(ssa_node).unwrap().vt;
        // TODO use vt
        ValueNode::Variable(None, "TODO@new_value".to_string())
    }

    fn replace_tmp_with_goto(&mut self) {
        let mut last = None;
        for node in self.ssa.inorder_walk() {
            if last.is_some() && self.ssa.is_block(node) {
                let s = self.action_map.get(&node).expect("The node should be added to action_map");
                if let Some(succ) = self.ssa.unconditional_block(node) {
                    if let Some(selector) = self.ssa.selector_in(node) {
                        // TODO
                        radeco_trace!("CASTBuilder::replace_tmp_with_goto INDIRET JMP");
                    } else {
                        // TODO
                        radeco_trace!("CASTBuilder::replace_tmp_with_goto JMP");
                        add_jump_to_cfg!(self, node, succ);
                    }
                } else if let Some(blk_cond_info) = self.ssa.conditional_blocks(node) {
                    // TODO
                    radeco_trace!("CASTBuilder::replace_tmp_with_goto IF");
                    add_jump_to_cfg!(self, node, blk_cond_info.true_side,
                                     SimpleCASTEdge::Action(ActionEdge::IfThen));
                    add_jump_to_cfg!(self, node, blk_cond_info.false_side,
                                     SimpleCASTEdge::Action(ActionEdge::IfElse));
                } else {
                    unreachable!();
                }
            } else if self.ssa.is_block(node) {
                last = Some(node);
            }
        }
    }

    pub fn from_ssa(&mut self) {
        self.prepare();
        self.data_flow_from_ssa();
        self.cfg_from_ssa();
    }

    fn prepare(&mut self) {
        self.prepare_consts();
        self.prepare_regs();
    }

    fn cfg_from_ssa(&mut self) {
        self.cfg_from_blocks(self.ssa.entry_node().unwrap(), &mut HashSet::new());
        self.replace_tmp_with_goto();
    }

    fn data_flow_from_ssa(&mut self) {
        for node in self.ssa.inorder_walk() {
            if self.ssa.is_phi(node) {
                self.handle_phi(node);
            } else if self.ssa.is_expr(node) {
                self.update_values(node);
            }
        }
    }

    fn cfg_from_nodes(&mut self, block: NodeIndex) {
        let nodes = self.ssa.nodes_in(block);
        for node in nodes {
            if self.is_recover_node(node) {
                let n = self.recover(node);
                self.action_map.insert(node, n);
            }
        }
    }

    fn cfg_from_blocks(&mut self, block: NodeIndex, visited: &mut HashSet<NodeIndex>) {
        if visited.contains(&block) {
            return;
        }
        visited.insert(block);
        let next_blocks = self.ssa.next_blocks(block);
        for blk in next_blocks {
            let n = self.dummy_goto();
            self.action_map.insert(blk, n);
            self.cfg_from_nodes(blk);
            self.cfg_from_blocks(blk, visited);
        }
    }
}

impl From<RadecoFunction> for SimpleCAST {
    fn from(rfn: RadecoFunction) -> Self {
        // TODO
        let ast = SimpleCAST::new(&rfn.name);
        // Gather variables, constants
        unimplemented!();
        // Recover expression, statement
        unimplemented!();
        ast
    }
}

/// This is used for translating SimpleCAST to CAST
struct CASTConverter<'a> {
    ast: &'a SimpleCAST,
    /// HashMap from SimpleCAST's node to CAST's node
    node_map: HashMap<NodeIndex, NodeIndex>,
    visited: HashSet<NodeIndex>,
}

impl<'a> CASTConverter<'a> {
    fn new(ast: &SimpleCAST) -> CASTConverter {
        CASTConverter {
            ast: ast,
            node_map: HashMap::new(),
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
        if let Some(ref l) = self.ast.label_map.get(&current_node) {
            c_ast.label(l);
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
                let dst_opt = self.ast.goto(current_node)
                    .and_then(|d| self.ast.label_map.get(&d))
                    .map(|d| d.clone());
                if let Some(dst) = dst_opt {
                    let node = c_ast.goto(&dst);
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
    use self::c_simple::{BTy, Ty};

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


    // fn main () {
    //     int i;
    //     unsigned long int u;
    //     double d;
    //     *float f;
    //     char c;
    //     void v;
    // }
    #[test]
    fn simple_c_ast_type_test() {
        let mut ast = SimpleCAST::new("main");
        let entry = ast.entry;
        let i = ast.var("i", Some(Ty::new(BTy::Int, true, 0)));
        let u = ast.var("u", Some(Ty::new(BTy::Int, false, 1)));
        let d = ast.var("d", Some(Ty::new(BTy::Double, true, 0)));
        let c = ast.var("c", Some(Ty::new(BTy::Char, true, 0)));
        let v = ast.var("v", Some(Ty::new(BTy::Void, true, 0)));
        let f = ast.var("f", Some(Ty::new(BTy::Ptr(Box::new(BTy::Float)), true, 0)));
        let output = ast.to_c_ast().print();
        println!("{}", output);
    }


    // Filters the functions that were in Radeco output AND requested by user
    fn filter_with<'a>(all_funcs: &Vec<(u64, &'a str)>,
                       requested: &Vec<&'a str>)
                       -> Vec<(u64, &'a str)> {

        all_funcs.iter()
            .filter(|&&(_, name)| requested.iter().any(|user_req| &name == user_req))
            .map(|&(addr, name)| (addr, name))
            .collect()
    }

#[cfg(feature="trace_log")] extern crate env_logger;
    // XXX For debbuging
    use frontend::radeco_containers::*;
    use analysis;
    use analysis::sccp;
    use analysis::cse::cse::CSE;
    use analysis::interproc::fixcall::CallFixer;
    use middle::dce;
    use middle::ssa::verifier;
    use std::path::PathBuf;
    use std::fs::{self, File};
    use std::process;
    use petgraph::dot::Dot;
    #[test]
    fn simple_c_ast_po() {
        #[cfg(feature="trace_log")] env_logger::init();
        // let requested_functions = vec!["sym.main"];
        let requested_functions = vec!["main"];
        // let proj_name = "./a.out".to_string();
        // let proj_name = "./po".to_string();
        let proj_name = "./fact".to_string();
        let mut rproj = {
            ProjectLoader::new().path(&proj_name).load()
        };
        let regfile = rproj.regfile().clone();
        for mut xy in rproj.iter_mut() {
            let rmod = &mut xy.module;
            {
                println!("[*] Fixing Callee Information");
                let bp_name = regfile.get_name_by_alias(&"BP".to_string());
                let bp_name = bp_name.map(|s| s.to_owned());
                let sp_name = regfile.get_name_by_alias(&"SP".to_string());
                let sp_name = sp_name.map(|s| s.to_owned());
                let mut callfixer = CallFixer::new(rmod, bp_name, sp_name);
                callfixer.rounded_analysis();
            }

            // Fix call sites
            analysis::functions::fix_ssa_opcalls::go(rmod);

            // Infer calling conventions
            analysis::functions::infer_regusage::run(rmod, &*regfile);

            // Filter the data if the user provided some args to be matched upon
            let matched_func_addrs = if requested_functions.len() != 0 {
                let mut matched_func_vec: Vec<(u64, &str)> =
                    rmod.iter().map(|_f| {
                        let f = _f.function.1;
                        (f.offset.clone(), &*f.name)
                    }).collect();
                let all_func_names: Vec<(&str)> =
                    matched_func_vec.iter().map(|&(_, name)| name).collect();
                matched_func_vec = filter_with(&matched_func_vec,
                                               &requested_functions.iter().map(|s| &s[..]).collect::<Vec<_>>());
                matched_func_vec.into_iter().map(|(addr, _)| addr).collect::<Vec<_>>()
            } else {
                Vec::new()
            };

            for addr in matched_func_addrs {

                let ref mut rfn = rmod.functions.get_mut(&addr).unwrap();

                println!("[+] Analyzing: {} @ {:#x}", rfn.name, addr);
                {
                    println!("  [*] Eliminating Dead Code");
                    dce::collect(rfn.ssa_mut());
                }
                // let mut ssa = {
                //     // Constant Propagation (sccp)
                //     println!("  [*] Propagating Constants");
                //     let mut analyzer = sccp::Analyzer::new(rfn.ssa_mut());
                //     analyzer.analyze();
                //     analyzer.emit_ssa()
                // };
                // {
                //     println!("  [*] Eliminating More DeadCode");
                //     dce::collect(&mut ssa);
                // }
                // *rfn.ssa_mut() = ssa;
                {
                    // Common SubExpression Elimination (cse)
                    println!("  [*] Eliminating Common SubExpressions");
                    let mut cse = CSE::new(rfn.ssa_mut());
                    cse.run();
                }
                {
                    // Verify SSA
                    println!("  [*] Verifying SSA's Validity");
                    match verifier::verify(rfn.ssa()) {
                        Err(e) => {
                            println!("  [*] Found Error: {}", e);
                            process::exit(255);
                        }
                        Ok(_) => {  }
                    }
                }
                let mut b = CASTBuilder::new(&rfn);
                b.data_flow_from_ssa();
                println!("{:?}", Dot::new(&b.data_graph));
            }
        }
    }
}
