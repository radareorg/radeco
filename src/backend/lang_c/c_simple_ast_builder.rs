use std::collections::{HashMap, HashSet};
use frontend::radeco_containers::RadecoFunction;
use middle::ir::MOpcode;
use middle::ssa::utils;
use middle::ssa::ssastorage::{NodeData, SSAStorage};
use middle::ssa::ssa_traits::{SSA, SSAExtra, SSAMod, SSAWalk, ValueInfo};
use middle::ssa::cfg_traits::CFG;
use super::c_simple_ast::{ValueNode, SimpleCAST, SimpleCASTEdge, ValueEdge, ActionEdge};
use super::c_simple;
use petgraph::visit::EdgeRef;
use petgraph::graph::{Graph, NodeIndex, EdgeIndex, Edges, EdgeReference};
use petgraph::{EdgeDirection, Direction, Directed};

macro_rules! add_jump_to_cfg {
    ($self: ident, $source: expr, $target: expr, $edge: expr) => {
        let src_node = $self.action_map.get(&$source).unwrap();
        let dst_node = $self.action_map.get(&$target).unwrap();
        $self.ast.add_edge(*src_node, *dst_node, $edge);
    };
    ($self: ident, $source: expr, $target: expr) => {
        add_jump_to_cfg!($self, $source, $target, SimpleCASTEdge::Value(ValueEdge::GotoDst));
    }
}

/// CASTBuilder constructs SimpleCAST from RadecoFunction
pub struct CASTBuilder<'a> {
    ast: SimpleCAST,
    //NodeIndex of SimpleCAST
    last_action: NodeIndex,
    rfn: &'a RadecoFunction,
    // SSA of RadecoFunction
    ssa: &'a SSAStorage,
    action_map: HashMap<NodeIndex, NodeIndex>,
    datamap: CASTDataMap<'a>,
}

impl<'a> CASTBuilder<'a> {
    fn new(f: &'a RadecoFunction) -> CASTBuilder {
        let ast = SimpleCAST::new(f.name.as_ref());
        CASTBuilder {
            last_action: ast.entry,
            ast: ast,
            rfn: f,
            ssa: f.ssa(),
            action_map: HashMap::new(),
            datamap: CASTDataMap::new(f),
        }
    }

    // XXX For debugging
    fn dummy_goto(&mut self) -> NodeIndex {
        self.last_action = self.ast.dummy_goto(self.last_action);
        self.last_action
    }

    // XXX For debugging
    fn dummy_action(&mut self, s: String) -> NodeIndex {
        self.last_action = self.ast.dummy(self.last_action, s);
        self.last_action
    }

    fn declare_vars(&mut self) {
        for (ref name, _) in self.datamap.reg_map.iter() {
            let _ = self.ast.var(&name, None);
        }
        for ref val in self.datamap.consts.iter() {
            let _ = self.ast.var(&val, None);
        }
    }

    fn assign(&mut self, dst: NodeIndex, src: NodeIndex) -> NodeIndex {
        self.last_action = self.ast.assign(dst, src, self.last_action);
        self.last_action
    }

    fn call_action(&mut self, func: &str) -> NodeIndex {
        self.last_action = self.ast.call_func(func, &[], self.last_action, None);
        self.last_action
    }

    fn recover_action(&mut self, node: NodeIndex) -> NodeIndex {
        assert!(self.is_recover_action(node));
        let op = self.ssa.opcode(node).unwrap_or(MOpcode::OpInvalid);
        radeco_trace!("CASTBuilder::recover {:?} @ {:?}", op, node);
        match op {
            MOpcode::OpCall => {
                // TODO Add proper argument, require prototype from RadecoFunction
                self.call_action("func")
            },
            MOpcode::OpStore => {
                let ops = self.ssa.operands_of(node);
                let (dst, src) = {
                    let dst = if let Some(&_dst) = self.datamap.var_map.get(&ops[1]) {
                        self.ast.derefed_node(_dst)
                    } else {
                        None
                    };
                    // XXX
                    let src = self.datamap.var_map.get(&ops[2]).map(|n| *n);
                    (dst, src)
                };
                if let (Some(d), Some(s)) = (dst, src) {
                    self.assign(d, s)
                } else {
                    self.dummy_action(format!("{:?} @ {:?}", op, node))
                }
            },
            _ => unreachable!(),
        }
    }

    fn is_recover_action(&self, node: NodeIndex) -> bool {
        let op = self.ssa.opcode(node).unwrap_or(MOpcode::OpInvalid);
        match op {
            MOpcode::OpCall | MOpcode::OpStore => true,
            _ => false,
        }
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

    pub fn recover_code(rfn: &'a RadecoFunction) -> SimpleCAST {
        let mut builder = Self::new(rfn);
        // Recover values
        let data_graph = CASTDataMap::recover_data(rfn, &mut builder.ast);
        builder.datamap = data_graph;
        builder.declare_vars();
        // Recover control flow graph
        builder.cfg_from_blocks(builder.ssa.entry_node().unwrap(), &mut HashSet::new());
        builder.replace_tmp_with_goto();
        builder.ast
    }

    fn cfg_from_nodes(&mut self, block: NodeIndex) {
        let nodes = self.ssa.nodes_in(block);
        for node in nodes {
            if self.is_recover_action(node) {
                let n = self.recover_action(node);
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

pub struct CASTDataMap<'a> {
    rfn: &'a RadecoFunction,
    ssa: &'a SSAStorage,
    // Hashmap from node of SSAStorage to one of self.data_graph
    // a map from node of data_graph to one of SimpleCAST's value
    pub var_map: HashMap<NodeIndex, NodeIndex>,
    // a map from node of data_graph to one of SimpleCAST's register
    pub reg_map: HashMap<String, NodeIndex>,
    pub consts: HashSet<String>,
    seen: HashSet<NodeIndex>,
}

impl<'a> CASTDataMap<'a> {
    pub fn new(rfn: &'a RadecoFunction) -> CASTDataMap<'a> {
        CASTDataMap {
            ssa: rfn.ssa(),
            rfn: rfn,
            var_map: HashMap::new(),
            reg_map: HashMap::new(),
            consts: HashSet::new(),
            seen: HashSet::new(),
        }
    }

    // Returns data map from SSAStorage's NodeIndex to SimpleCAST's NodeIndex
    pub fn recover_data(rfn: &'a RadecoFunction, ast: &mut SimpleCAST) -> Self {
        let mut s = Self::new(rfn);
        s.prepare_consts(ast);
        s.prepare_regs(ast);
        for node in s.ssa.inorder_walk() {
            if s.ssa.is_phi(node) {
                s.handle_phi(node);
            } else if s.ssa.is_expr(node) {
                s.update_values(node, ast);
            }
        }
        s
    }

    fn handle_binop(&mut self, ret_node: NodeIndex, ops: Vec<NodeIndex>,
                    expr: c_simple::Expr, ast: &mut SimpleCAST) {
        // XXX for debbuging, do not use filter_map
        let ops_mapped = ops.iter()
            .map(|op| self.var_map.get(op).map(|n| *n).unwrap_or(ast.unknown))
            .collect::<Vec<_>>();
        assert!(ops.len() == ops_mapped.len());
        let expr_node = ast.expr(ops_mapped.as_slice(), expr.clone());
        radeco_trace!("Add {:?} to {:?}, Operator: {:?}", ret_node, expr_node, expr);
        self.var_map.insert(ret_node, expr_node);
    }

    fn handle_uniop(&mut self, ret_node: NodeIndex, op: NodeIndex,
                    expr: c_simple::Expr, ast: &mut SimpleCAST) {
        if let Some(&n) = self.var_map.get(&op) {
            let expr_node = ast.expr(&[n], expr);
            self.var_map.insert(ret_node, expr_node);
        } else {
            radeco_warn!("Operand not found: {:?}", op);
        }
    }

    fn deref(&self, node: NodeIndex, ast: &mut SimpleCAST) -> NodeIndex {
        radeco_trace!("DeRef {:?}", node);
        let n = self.var_map.get(&node).expect("Cannot be None");
        ast.deref(*n)
    }

    fn handle_phi(&mut self, node: NodeIndex) {
        assert!(self.ssa.is_phi(node));
        radeco_trace!("CASTBuilder::handle_phi {:?}", node);
        if self.seen.contains(&node) {
            return;
        }
        let ops = self.ssa.operands_of(node);
        // TODO
        // XXX
        if let Some(&head) = ops.into_iter()
           .filter_map(|n| self.var_map.get(&n))
           .next() {
           self.var_map.insert(node, head);
        }
    }

    fn update_values(&mut self, ret_node: NodeIndex, ast: &mut SimpleCAST) {
        assert!(self.ssa.is_expr(ret_node));
        radeco_trace!("CASTBuilder::update_values {:?}", ret_node);
        if self.seen.contains(&ret_node) {
            return;
        }
        self.seen.insert(ret_node);
        let regs = self.ssa.registers(ret_node);
        if regs.len() > 0 {
            let ast_node = ast.constant(&regs[0], None);
            self.var_map.insert(ret_node, ast_node);
            return;
        }
        let ops = self.ssa.operands_of(ret_node);

        radeco_trace!("CASTBuilder::update_values opcode: {:?}", self.ssa.opcode(ret_node));
        // TODO update
        match self.ssa.opcode(ret_node).unwrap_or(MOpcode::OpInvalid) {
            MOpcode::OpStore => {
                assert!(ops.len() == 3);
                let deref_node = self.deref(ops[1], ast);
            }
            MOpcode::OpLoad => {
                radeco_trace!("OpLoad: {:?}", ops);
                let deref_node = self.deref(ops[1], ast);
                self.var_map.insert(ret_node, deref_node);
            },
            MOpcode::OpAdd => self.handle_binop(ret_node, ops, c_simple::Expr::Add, ast),
            MOpcode::OpAnd => self.handle_binop(ret_node, ops, c_simple::Expr::And, ast),
            MOpcode::OpDiv => self.handle_binop(ret_node, ops, c_simple::Expr::Div, ast),
            MOpcode::OpEq => self.handle_binop(ret_node, ops, c_simple::Expr::Eq, ast),
            MOpcode::OpGt => self.handle_binop(ret_node, ops, c_simple::Expr::Gt, ast),
            // XXX Shl might be wrong operator
            MOpcode::OpLsl => self.handle_binop(ret_node, ops, c_simple::Expr::Shl, ast),
            // XXX Shr might be wrong operator
            MOpcode::OpLsr => self.handle_binop(ret_node, ops, c_simple::Expr::Shr, ast),
            MOpcode::OpLt => self.handle_binop(ret_node, ops, c_simple::Expr::Lt, ast),
            MOpcode::OpMod => self.handle_binop(ret_node, ops, c_simple::Expr::Mod, ast),
            MOpcode::OpMul => self.handle_binop(ret_node, ops, c_simple::Expr::Mul, ast),
            // TODO Add `Narrow` info
            MOpcode::OpNarrow(size) => self.handle_uniop(ret_node, ops[0],
                                                         c_simple::Expr::Cast(size as usize), ast),
            MOpcode::OpNot => self.handle_binop(ret_node, ops, c_simple::Expr::Not, ast),
            MOpcode::OpOr => self.handle_binop(ret_node, ops, c_simple::Expr::Or, ast),
            MOpcode::OpRol => unimplemented!(),
            MOpcode::OpRor => unimplemented!(),
            // TODO Add `SignExt`
            MOpcode::OpSignExt(size) => self.handle_uniop(ret_node, ops[0],
                                                          c_simple::Expr::Cast(size as usize), ast),
            MOpcode::OpSub => self.handle_binop(ret_node, ops, c_simple::Expr::Sub, ast),
            MOpcode::OpXor => self.handle_binop(ret_node, ops, c_simple::Expr::Xor, ast),
            // TODO Add `ZeroExt`
            MOpcode::OpZeroExt(size) => self.handle_uniop(ret_node, ops[0],
                                                          c_simple::Expr::Cast(size as usize), ast),
            MOpcode::OpCall => {
                self.update_data_graph_by_call(ret_node);
            },
            _ => {},
        }
    }
    fn update_data_graph_by_call(&mut self, call_node: NodeIndex) {
        radeco_trace!("CASTBuilder::update_data_graph_by_call {:?}", call_node);
        let reg_map = utils::call_rets(call_node, self.ssa);
        for (idx, (node, vt)) in reg_map.into_iter() {
            let name = self.ssa.regfile.get_name(idx).unwrap_or("mem").to_string();
            // TODO
        }
    }

    fn prepare_consts(&mut self, ast: &mut SimpleCAST) {
        for (&val, &node) in self.ssa.constants.iter() {
            if let Ok(n) = self.ssa.node_data(node) {
                // TODO add type
                let ast_node = ast.constant(&val.to_string(), None);
                self.consts.insert(val.to_string());
                self.var_map.insert(node, ast_node);
            } else {
                radeco_warn!("Invalid constant");
            }
        }
    }

    fn prepare_regs(&mut self, ast: &mut SimpleCAST) {
        for walk_node in self.ssa.inorder_walk() {
            // TODO avoid unwrap
            let reg_state = self.ssa.registers_in(walk_node);
            if reg_state.is_none() {
                continue;
            }
            let reg_map = utils::register_state_info(reg_state.unwrap(), self.ssa);
            for (idx, (node, vt)) in reg_map.into_iter() {
                let name = self.ssa.regfile.get_name(idx).unwrap_or("mem").to_string();
                // XXX
                let ast_node = ast.constant(&name, None);
                // XXX
                self.reg_map.insert(name, ast_node);
                radeco_trace!("Add register {:?}", node);
                self.var_map.insert(node, ast_node);
            }
        }
    }
}
