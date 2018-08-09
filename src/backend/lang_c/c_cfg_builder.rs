//! This module is for recovering CCFG from RadecoFunction.
//!
//! Usage of this module is to call `c_cfg_builder::recover_simple_ast(rfn)`
//! where `rfn` is an instance of `RadecoFunction`, the function returns an instance of
//! CCFG and we can obtain higher level representation than Radeco IR.

use super::c_ast;
use super::c_ast::Ty;
use super::c_cfg::{CCFG, CCFGRef};
use frontend::radeco_containers::RadecoFunction;
use middle::ir::{MOpcode, MAddress};
use middle::ssa::cfg_traits::CFG;
use middle::ssa::ssa_traits::{SSA, SSAWalk};
use middle::ssa::ssastorage::{NodeData, SSAStorage};
use middle::ssa::utils;
use std::collections::{HashMap, HashSet};
use petgraph::graph::NodeIndex;

fn is_debug() -> bool {
    cfg!(feature = "trace_log")
}

/// This constructs CCFG from an instance of RadecoFunction.
pub fn recover_c_cfg(
    rfn: &RadecoFunction,
    fname_map: &HashMap<u64, String>,
    strings: &HashMap<u64, String>,
) -> CCFG {
    let mut builder = CCFGBuilder::new(rfn, fname_map);
    let data_graph = CCFGDataMap::recover_data(rfn, &mut builder.cfg, strings);
    builder.datamap = data_graph;
    builder.cfg_from_ssa();
    builder.insert_jumps();
    builder.cfg
}

fn ret_value_string(rfn: &RadecoFunction) -> Option<String> {
    let ret_reg_opt = rfn.callconv.clone();
    if ret_reg_opt.is_none() {
        return None;
    }
    ret_reg_opt.unwrap().ret
}

type SSARef = NodeIndex;
// CCFGBuilder constructs CCFG from RadecoFunction
struct CCFGBuilder<'a> {
    cfg: CCFG,
    last_action: CCFGRef,
    rfn: &'a RadecoFunction,
    // SSA of RadecoFunction
    ssa: &'a SSAStorage,
    fname_map: &'a HashMap<u64, String>,
    action_map: HashMap<SSARef, CCFGRef>,
    datamap: CCFGDataMap<'a>,
}

impl<'a> CCFGBuilder<'a> {
    fn new(rfn: &'a RadecoFunction, fname_map: &'a HashMap<u64, String>) -> CCFGBuilder<'a> {
        let cfg = CCFG::new(rfn.name.as_ref());
        CCFGBuilder {
            last_action: cfg.entry,
            cfg: cfg,
            rfn: rfn,
            ssa: rfn.ssa(),
            fname_map: fname_map,
            action_map: HashMap::new(),
            datamap: CCFGDataMap::new(rfn),
        }
    }

    fn basic_block(&mut self) -> CCFGRef {
        self.last_action = self.cfg.basic_block(self.last_action);
        self.last_action
    }

    fn assign(&mut self, dst: CCFGRef, src: CCFGRef) -> CCFGRef {
        self.last_action = self.cfg.assign(dst, src, self.last_action);
        self.last_action
    }

    // Retrieve CCFG's return value node of function call
    fn return_node(&self, call_node: SSARef) -> Option<SSARef> {
        let ret_reg_name_opt = ret_value_string(self.rfn);
        if ret_reg_name_opt.is_none() {
            return None;
        }
        let ret_reg_name = ret_reg_name_opt.unwrap();
        let reg_map = utils::call_rets(call_node, self.ssa);
        for (idx, (node, _)) in reg_map.into_iter() {
            if let Some(name) = self.ssa.regfile.get_name(idx) {
                if name == ret_reg_name {
                    return self.datamap.var_map.get(&node).cloned();
                }
            }
        }
        return None;
    }

    fn args_inorder(&self, call_node: SSARef) -> Vec<SSARef> {
        let call_info = utils::call_info(call_node, self.ssa).expect("This should not be `None`");
        if self.rfn.callconv.is_none() {
            return Vec::new();
        }
        let regs_order = self.rfn
            .callconv
            .clone()
            .unwrap()
            .args
            .unwrap_or(Vec::new())
            .into_iter()
            .enumerate()
            .map(|(i, s)| (s, i))
            .collect::<HashMap<_, _>>();
        let mut args = Vec::new();
        let reg_map = call_info.register_args;
        for (idx, node) in reg_map.into_iter() {
            let name = self.ssa.regfile.get_name(idx).unwrap_or("mem").to_string();
            if let Some(&i) = regs_order.get(&name) {
                args.push((i, node));
            }
        }
        args.sort_by_key(|k| k.0);
        args.into_iter().map(|(_, n)| n).collect()
    }

    fn call_action(&mut self, call_node: SSARef) -> CCFGRef {
        let call_info = utils::call_info(call_node, self.ssa).expect("This should not be `None`");
        let callee_node = call_info.target;
        let func_name = {
            if self.datamap.const_nodes.contains(&callee_node) {
                let addr = self.ssa.constant_value(callee_node).unwrap_or(0);
                self.fname_map.get(&addr).cloned().unwrap_or(
                    "invalid".to_string(),
                )
            } else {
                "unknown".to_string()
            }
        };
        let args = self.args_inorder(call_node)
            .into_iter()
            .map(|n| {
                self.datamap.var_map.get(&n).cloned().unwrap_or(
                    self.cfg.unknown,
                )
            })
            .collect::<Vec<_>>();
        let ret_val_node = self.return_node(call_node);
        self.last_action = self.cfg.call_func(
            &func_name,
            args.as_slice(),
            self.last_action,
            ret_val_node,
        );
        self.last_action
    }

    fn addr_str(&self, node: SSARef) -> String {
        self.ssa.address(node).map(|a| format!("{}", a)).unwrap_or(
            "unknown".to_string(),
        )
    }

    fn recover_action(&mut self, node: SSARef) -> CCFGRef {
        debug_assert!(self.is_recover_action(node));
        let op = self.ssa.opcode(node).unwrap_or(MOpcode::OpInvalid);
        radeco_trace!("CCFGBuilder::recover {:?} @ {:?}", op, node);
        match op {
            MOpcode::OpCall => {
                let ret = self.call_action(node);
                if is_debug() {
                    let addr = self.addr_str(node);
                    let ops_dbg = self.ssa.operands_of(node);
                    self.cfg.debug_info_at(
                        ret,
                        format!("Call {:?} @ {}", ops_dbg, addr),
                    );
                }
                ret
            }
            MOpcode::OpStore => {
                let ops = self.ssa.operands_of(node);
                let dst = self.datamap
                    .var_map
                    .get(&ops[1])
                    .map(|&x| self.cfg.derefed_node(x).unwrap_or(x))
                    .unwrap_or(self.cfg.unknown);
                let src = self.datamap.var_map.get(&ops[2]).cloned().unwrap_or(
                    self.cfg.unknown,
                );
                let ret = self.assign(dst, src);
                if is_debug() {
                    let addr = self.addr_str(node);
                    self.cfg.debug_info_at(
                        ret,
                        format!("*({:?}) = {:?} @ {}", dst, src, addr),
                    );
                }
                ret
            }
            _ => unreachable!(),
        }
    }

    fn is_recover_action(&self, node: SSARef) -> bool {
        let op = self.ssa.opcode(node).unwrap_or(MOpcode::OpInvalid);
        match op {
            MOpcode::OpCall | MOpcode::OpStore => true,
            _ => false,
        }
    }

    fn get_block_addr(&self, block: SSARef) -> Option<MAddress> {
        match self.ssa.g[block] {
            NodeData::BasicBlock(addr, _) => Some(addr),
            _ => None,
        }
    }

    fn gen_label(&self, block: SSARef) -> String {
        if let Some(addr) = self.get_block_addr(block) {
            format!("addr_{:}", addr).to_string()
        } else {
            "addr_unknown".to_string()
        }
    }

    fn handle_goto(&mut self, _next: SSARef, succ: SSARef) {
        radeco_trace!("CCFGBuilder::handle goto");
        let next = self.action_map.get(&_next).cloned().expect(
            "The node should be \
             added to action_map",
        );
        let succ_node = self.action_map.get(&succ).cloned().expect(
            "This should not be None",
        );
        let label = self.gen_label(succ);
        let goto_node = self.cfg.insert_goto_before(next, succ_node, &label);
        if is_debug() {
            let addr = self.addr_str(_next);
            self.cfg.debug_info_at(
                goto_node,
                format!("JMP {:?} @ {}", succ_node, addr),
            );
        }
    }

    fn handle_if(&mut self, _prev: SSARef, selector: SSARef, true_node: SSARef,
                 false_node: SSARef) {
        radeco_trace!("CCFGBuilder::handle_if");
        let prev = self.action_map.get(&_prev)
            .and_then(|&n| self.cfg.preds_of(n).first().cloned())
            .expect("This should not be `None`");
        // Add goto statement as `if then` node
        let goto_then = {
            let dst_node = self.action_map.get(&true_node).cloned().expect(
                "This should not \
                 be None",
            );
            // Edge from `unknown` will be removed later.
            let unknown = self.cfg.unknown;
            let label = self.gen_label(true_node);
            self.cfg.add_goto(dst_node, &label, unknown)
        };
        // Add goto statement as `if else` node
        let goto_else = {
            let dst_node = self.action_map.get(&false_node).cloned().expect(
                "This should not \
                 be None",
            );
            // Edge from `unknown` will be removed later.
            let unknown = self.cfg.unknown;
            let label = self.gen_label(false_node);
            self.cfg.add_goto(dst_node, &label, unknown)
        };
        // Add condition node to if statement
        let cond = self.datamap.var_map.get(&selector).cloned().unwrap_or(
            self.cfg.unknown,
        );
        let if_node = self.cfg.insert_conditional(cond, goto_then, Some(goto_else), prev);
        if is_debug() {
            let addr = self.addr_str(prev);
            self.cfg.debug_info_at(
                goto_then,
                format!("IF JMP {:?} @ {}", if_node, addr),
            );
        }
    }

    fn handle_return(&mut self, block: SSARef) {
        let prev = self.action_map.get(&block)
            .and_then(|&n| self.cfg.preds_of(n).first().cloned())
            .expect("This should not be `None`");
        // TODO specify return value if it exists
        self.cfg.add_return(None, prev);
    }

    fn insert_jump(&mut self, cur_block: SSARef, prev_block: SSARef) {
        if let Some(succ) = self.ssa.unconditional_block(prev_block) {
            if let Some(_) = self.ssa.selector_in(prev_block) {
                // TODO
                radeco_trace!("CCFGBuilder::insert_jump INDIRET JMP");
            } else if self.ssa.exit_node().map_or(false, |en| en == succ) {
                self.handle_return(cur_block);
            } else {
                self.handle_goto(cur_block, succ);
            }
        } else if let Some(blk_cond_info) = self.ssa.conditional_blocks(prev_block) {
            if let Some(selector) = self.ssa.selector_in(prev_block) {
                self.handle_if(cur_block, selector, blk_cond_info.true_side,
                               blk_cond_info.false_side);
            } else {
                radeco_warn!(
                    "block with conditional successors has no selector {:?}",
                    prev_block
                );
            }
        } else {
            radeco_err!("Unreachable node {:?}", prev_block);
        }
    }

    // Insert goto, if statements
    fn insert_jumps(&mut self) {
        let mut last = None;
        let entry_node = entry_node_err!(self.ssa);
        let exit_node = exit_node_err!(self.ssa);
        for cur_node in self.ssa.inorder_walk() {
            if cur_node == entry_node {
                continue;
            }
            if self.ssa.is_action(cur_node) {
                if let Some(prev_block) = last {
                    self.insert_jump(cur_node, prev_block);
                }
                last = Some(cur_node);
            }
        }
        if let Some(prev_block) = last {
            if prev_block != exit_node {
                self.insert_jump(exit_node, prev_block);
            }
        }
    }

    fn cfg_from_ssa(&mut self) {
        for node in self.ssa.inorder_walk() {
            if self.is_recover_action(node) {
                let n = self.recover_action(node);
                self.action_map.insert(node, n);
            } else if self.ssa.is_action(node) {
                let n = self.basic_block();
                self.action_map.insert(node, n);
            };
        }
    }
}

struct CCFGDataMap<'a> {
    rfn: &'a RadecoFunction,
    ssa: &'a SSAStorage,
    // Hashmap from node of SSAStorage to one of self.data_graph
    // a map from node of data_graph to one of CCFG's value
    pub var_map: HashMap<SSARef, CCFGRef>,
    // a map from node of data_graph to one of CCFG's register
    pub reg_map: HashMap<String, CCFGRef>,
    pub const_nodes: HashSet<SSARef>,
    seen: HashSet<SSARef>,
}

impl<'a> CCFGDataMap<'a> {
    fn new(rfn: &'a RadecoFunction) -> CCFGDataMap<'a> {
        CCFGDataMap {
            ssa: rfn.ssa(),
            rfn: rfn,
            var_map: HashMap::new(),
            reg_map: HashMap::new(),
            const_nodes: HashSet::new(),
            seen: HashSet::new(),
        }
    }

    fn recover_data(
        rfn: &'a RadecoFunction,
        ast: &mut CCFG,
        strings: &'a HashMap<u64, String>,
    ) -> Self {
        let mut s = Self::new(rfn);
        s.prepare_consts(ast, strings);
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

    fn handle_binop(
        &mut self,
        ret_node: SSARef,
        ops: Vec<SSARef>,
        expr: c_ast::Expr,
        ast: &mut CCFG,
    ) {
        debug_assert!(ops.len() == 2);
        let ops_mapped = ops.iter()
            .map(|op| self.var_map.get(op).map(|n| *n).unwrap_or(ast.unknown))
            .collect::<Vec<_>>();
        let expr_node = ast.expr(ops_mapped.as_slice(), expr.clone());
        radeco_trace!(
            "Add {:?} to {:?}, Operator: {:?}",
            ret_node,
            expr_node,
            expr
        );
        self.var_map.insert(ret_node, expr_node);
    }

    fn handle_uniop(
        &mut self,
        ret_node: SSARef,
        op: SSARef,
        expr: c_ast::Expr,
        ast: &mut CCFG,
    ) {
        if let Some(&n) = self.var_map.get(&op) {
            let expr_node = ast.expr(&[n], expr);
            self.var_map.insert(ret_node, expr_node);
        } else {
            radeco_warn!("Operand not found: {:?}", op);
        }
    }

    fn handle_cast(
        &mut self,
        ret_node: SSARef,
        op: SSARef,
        expr: c_ast::Expr,
        ast: &mut CCFG,
    ) {
        if self.const_nodes.contains(&op) {
            let cfg_node = self.var_map.get(&op).cloned().unwrap_or(ast.unknown);
            self.var_map.insert(ret_node, cfg_node);
        } else {
            self.handle_uniop(ret_node, op, expr, ast);
        }
    }

    fn deref(&self, node: SSARef, ast: &mut CCFG) -> CCFGRef {
        radeco_trace!("DeRef {:?}", node);
        let n = self.var_map.get(&node).cloned().unwrap_or(ast.unknown);
        ast.deref(n)
    }

    fn handle_phi(&mut self, node: SSARef) {
        debug_assert!(self.ssa.is_phi(node));
        radeco_trace!("CCFGBuilder::handle_phi {:?}", node);
        let ops = self.ssa.operands_of(node);
        // Take first available/mappable node of CCFG's node from phi node
        if let Some(&head) = ops.into_iter().filter_map(|n| self.var_map.get(&n)).next() {
            self.var_map.insert(node, head);
        }
    }

    fn type_from_str(type_str: &str) -> Option<Ty> {
        // TODO More types
        match type_str {
            "int" => Some(Ty::new(c_ast::BTy::Int, true, 0)),
            _ => None,
        }
    }

    fn update_values(&mut self, ret_node: SSARef, ast: &mut CCFG) {
        debug_assert!(self.ssa.is_expr(ret_node));
        radeco_trace!("CCFGBuilder::update_values {:?}", ret_node);
        if self.seen.contains(&ret_node) {
            return;
        }
        self.seen.insert(ret_node);
        // Checking whether `ret_node` is a local variable.
        if let Some(bindings) = self.rfn.local_at(ret_node, true) {
            // TODO add type
            let type_info = Self::type_from_str(&bindings[0].type_str);
            let cfg_node = ast.var(bindings[0].name(), type_info);
            self.var_map.insert(ret_node, cfg_node);
            return;
        }
        let ops = self.ssa.operands_of(ret_node);

        radeco_trace!(
            "CCFGBuilder::update_values opcode: {:?}",
            self.ssa.opcode(ret_node)
        );
        match self.ssa.opcode(ret_node).unwrap_or(MOpcode::OpInvalid) {
            MOpcode::OpStore => {
                debug_assert!(ops.len() == 3);
                // Variables do not need Deref
                if self.rfn.local_at(ops[1], true).is_none() {
                    self.deref(ops[1], ast);
                }
            }
            MOpcode::OpLoad => {
                // Variables do not need Deref
                if self.rfn.local_at(ops[1], true).is_none() {
                    let deref_node = self.deref(ops[1], ast);
                    self.var_map.insert(ret_node, deref_node);
                } else {
                    let cfg_node = *self.var_map.get(&ops[1]).expect("This can not be `None`");
                    self.var_map.insert(ret_node, cfg_node);
                }
            }
            MOpcode::OpAdd => self.handle_binop(ret_node, ops, c_ast::Expr::Add, ast),
            MOpcode::OpAnd => self.handle_binop(ret_node, ops, c_ast::Expr::And, ast),
            MOpcode::OpDiv => self.handle_binop(ret_node, ops, c_ast::Expr::Div, ast),
            MOpcode::OpEq => self.handle_binop(ret_node, ops, c_ast::Expr::Eq, ast),
            MOpcode::OpGt => self.handle_binop(ret_node, ops, c_ast::Expr::Gt, ast),
            // XXX Shl might be wrong operator
            MOpcode::OpLsl => self.handle_binop(ret_node, ops, c_ast::Expr::Shl, ast),
            // XXX Shr might be wrong operator
            MOpcode::OpLsr => self.handle_binop(ret_node, ops, c_ast::Expr::Shr, ast),
            MOpcode::OpLt => self.handle_binop(ret_node, ops, c_ast::Expr::Lt, ast),
            MOpcode::OpMod => self.handle_binop(ret_node, ops, c_ast::Expr::Mod, ast),
            MOpcode::OpMul => self.handle_binop(ret_node, ops, c_ast::Expr::Mul, ast),
            // TODO Add `Narrow` info
            MOpcode::OpNarrow(size) => {
                self.handle_cast(ret_node, ops[0], c_ast::Expr::Cast(size as usize), ast)
            }
            MOpcode::OpNot => self.handle_uniop(ret_node, ops[0], c_ast::Expr::Not, ast),
            MOpcode::OpOr => self.handle_binop(ret_node, ops, c_ast::Expr::Or, ast),
            MOpcode::OpRol => unimplemented!(),
            MOpcode::OpRor => unimplemented!(),
            // TODO Add `SignExt`
            MOpcode::OpSignExt(size) => {
                self.handle_cast(ret_node, ops[0], c_ast::Expr::Cast(size as usize), ast)
            }
            MOpcode::OpSub => self.handle_binop(ret_node, ops, c_ast::Expr::Sub, ast),
            MOpcode::OpXor => self.handle_binop(ret_node, ops, c_ast::Expr::Xor, ast),
            // TODO Add `ZeroExt`
            MOpcode::OpZeroExt(size) => {
                self.handle_cast(ret_node, ops[0], c_ast::Expr::Cast(size as usize), ast)
            }
            MOpcode::OpCall => {
                self.update_data_graph_by_call(ret_node, ast);
            }
            _ => {}
        }
    }

    fn update_data_graph_by_call(&mut self, call_node: SSARef, ast: &mut CCFG) {
        radeco_trace!("CCFGBuilder::update_data_graph_by_call {:?}", call_node);
        let ret_reg_name_opt = ret_value_string(self.rfn);
        if ret_reg_name_opt.is_none() {
            return;
        }
        let ret_reg_name = ret_reg_name_opt.unwrap();
        let reg_map = utils::call_rets(call_node, self.ssa);
        for (idx, (node, _)) in reg_map.into_iter() {
            // TODO Add data dependencies for registers
            let name = self.ssa.regfile.get_name(idx).unwrap_or("mem").to_string();
            if name == ret_reg_name {
                // TODO add type
                let cfg_node = ast.var("tmp", None);
                self.var_map.insert(node, cfg_node);
            }
        }
    }

    fn prepare_consts(&mut self, ast: &mut CCFG, strings: &HashMap<u64, String>) {
        for (&val, &node) in self.ssa.constants.iter() {
            if self.ssa.node_data(node).is_ok() {
                // TODO add type
                let cfg_node = if let Some(s) = strings.get(&val) {
                    ast.constant(&format!("\"{}\"", s), None)
                } else {
                    ast.constant(&val.to_string(), None)
                };
                self.const_nodes.insert(node);
                self.var_map.insert(node, cfg_node);
            } else {
                radeco_warn!("Invalid constant");
            }
        }
    }

    fn prepare_regs(&mut self, ast: &mut CCFG) {
        for walk_node in self.ssa.inorder_walk() {
            let reg_state = self.ssa.registers_in(walk_node);
            if reg_state.is_none() {
                continue;
            }
            let reg_map = utils::register_state_info(reg_state.unwrap(), self.ssa);
            for (idx, (node, _)) in reg_map.into_iter() {
                let name = self.ssa.regfile.get_name(idx).unwrap_or("mem").to_string();
                // XXX CCFG::constant may not be proper method for registering regs.
                let cfg_node = ast.constant(&name, None);
                radeco_trace!("Add register {:?}", node);
                self.var_map.insert(node, cfg_node);
                // XXX Maybe not needed
                self.reg_map.insert(name, cfg_node);
            }
        }
    }
}

struct CCFGBuilderVerifier {}

// type Verifier = Fn(SSARef, &mut CCFG, &mut CCFGDataMap) -> Result<(), String>;
impl CCFGBuilderVerifier {
    const DELIM: &'static str = "; ";

    fn verify(builder: &mut CCFGBuilder) -> Result<(), String> {
        let ssa = builder.ssa.clone();
        let mut errors = Vec::new();
        for node in ssa.inorder_walk() {
            match ssa.opcode(node) {
                Some(MOpcode::OpCall) => {
                    if let Err(err) = Self::verify_args_inorder_at(builder, node) {
                        errors.push(err);
                    }
                    if let Err(err) = Self::verify_call_action_at(builder, node) {
                        errors.push(err);
                    }
                }
                Some(MOpcode::OpStore) => {
                    if let Err(err) = Self::verify_assign_at(builder, node) {
                        errors.push(err);
                    }
                }
                _ => {}
            }
        }
        if errors.len() > 0 {
            Err(errors.join(Self::DELIM))
        } else {
            Ok(())
        }
    }

    // All argument node exist in SSAStorage
    fn verify_args_inorder_at(builder: &CCFGBuilder, call_node: SSARef) -> Result<(), String> {
        let mut errors = Vec::new();
        let args = builder.args_inorder(call_node);
        for arg in args {
            if let Err(debug) = builder.ssa.node_data(arg) {
                let err = format!("{:?}", debug);
                errors.push(err);
            }
        }
        if errors.len() > 0 {
            Err(errors.join(Self::DELIM))
        } else {
            Ok(())
        }
    }

    // Verify `assign` made `CCFG::Action(ActionNode::Assignment)` node.
    fn verify_assign_at(builder: &mut CCFGBuilder, node: SSARef) -> Result<(), String> {
        let ops = builder.ssa.operands_of(node);
        let dst = builder.datamap.var_map.get(&ops[1]).map(|&x| {
            builder.cfg.derefed_node(x).unwrap_or(x)
        });
        let src = builder.datamap.var_map.get(&ops[2]).cloned();
        if src.is_none() {
            return Err("Failed to get src operand node from CCFG".to_string());
        }
        if dst.is_none() {
            return Err("Failed to get dst operand node from CCFG".to_string());
        }
        let assign_node = builder.assign(dst.unwrap(), src.unwrap());
        let is_err = builder.last_action != assign_node || !builder.cfg.is_assign_node(assign_node);
        if is_err {
            Err("Failed to append assign action".to_string())
        } else {
            Ok(())
        }
    }

    // Verify `call_action` made `CCFG::Action(ActionNode::Call(_))` node.
    fn verify_call_action_at(
        builder: &mut CCFGBuilder,
        call_node: SSARef,
    ) -> Result<(), String> {
        let call_node = builder.call_action(call_node);
        let is_err = builder.last_action != call_node || !builder.cfg.is_call_node(call_node);
        if is_err {
            Err("`CCFGBuilder::call_action` is failed.".to_string())
        } else {
            Ok(())
        }
    }
}

struct CCFGDataMapVerifier {}

type Verifier = Fn(SSARef, &mut CCFG, &mut CCFGDataMap) -> Result<(), String>;
impl CCFGDataMapVerifier {
    const DELIM: &'static str = "; ";

    fn verify_datamap(
        datamap: &mut CCFGDataMap,
        ast: &mut CCFG,
        strings: &HashMap<u64, String>,
    ) -> Result<(), String> {
        Self::verify_prepare(ast, datamap, strings)?;
        Self::verify_ops(ast, datamap)?;
        Ok(())
    }

    fn verify_prepare(
        ast: &mut CCFG,
        datamap: &mut CCFGDataMap,
        strings: &HashMap<u64, String>,
    ) -> Result<(), String> {
        datamap.prepare_consts(ast, strings);
        Self::verify_prepare_consts(ast, datamap, strings)?;
        datamap.prepare_regs(ast);
        Self::verify_prepare_regs(datamap)?;
        Ok(())
    }

    fn verify_ops(ast: &mut CCFG, datamap: &mut CCFGDataMap) -> Result<(), String> {
        Self::verify_handler_each_node(
            ast,
            datamap,
            &Self::verify_handle_uniop,
            "Handle unary operator",
        )?;

        Self::verify_handler_each_node(
            ast,
            datamap,
            &Self::verify_handle_binop,
            "Handle binary operator",
        )?;
        Self::verify_handler_each_node(
            ast,
            datamap,
            &Self::verify_handle_cast,
            "Handle casting operator",
        )?;
        Ok(())
    }

    fn verify_prepare_consts(
        ast: &CCFG,
        datamap: &CCFGDataMap,
        strings: &HashMap<u64, String>,
    ) -> Result<(), String> {
        let mut errors = Vec::new();
        // All nodes of datamap.const_nodes should be constant node of SSAStorage
        for &const_node in &datamap.const_nodes {
            if !datamap.ssa.is_constant(const_node) {
                errors.push(format!("Invalid constant node: {:?}", const_node));
            }
        }

        // All values of constant nodes between SSAStorage and CCFG should be same.
        for (&node, &cfg_node) in &datamap.var_map {
            let val = if let Some(tmp_val) = datamap.ssa.constant_value(node) {
                let ret = if let Some(s) = strings.get(&tmp_val) {
                    format!("\"{}\"", s)
                } else {
                    tmp_val.to_string()
                };
                Some(ret)
            } else {
                let err = format!("Invalid constant node: {:?}", node);
                errors.push(err);
                None
            };
            let const_opt = ast.constant_of(cfg_node);
            if const_opt.is_none() {
                let err = format!("No ValueNode::Constant({:?}) is found", cfg_node);
                errors.push(err);
            }
            if val.is_none() || const_opt.is_none() {
                continue;
            }
            let v = val.unwrap().to_string();
            let c = const_opt.unwrap();
            if v != c {
                let err = format!("Mismatched values `{:?}` and `{:?}`", v, c);
                errors.push(err);
            }
        }

        if errors.len() > 0 {
            Err(errors.join(Self::DELIM))
        } else {
            Ok(())
        }
    }

    fn verify_prepare_regs_of(
        datamap: &CCFGDataMap,
        node: SSARef,
        name: String,
    ) -> Result<(), String> {
        let mut errors = Vec::new();
        if datamap.var_map.get(&node).is_none() {
            let err = format!("Invalid register node: {:?}", node);
            errors.push(err);
        }
        // Checking if name is a key of reg_map.
        // reg_map.get(&name) is not needed to be same to
        // cfg_node of var_map.get(&node)
        if !datamap.reg_map.contains_key(&name) {
            let err = format!("Invalid register name: {:?}", name);
            errors.push(err);
        }
        if errors.len() > 0 {
            Err(errors.join(Self::DELIM))
        } else {
            Ok(())
        }
    }

    fn verify_prepare_regs(datamap: &CCFGDataMap) -> Result<(), String> {
        let mut errors = Vec::new();
        for walk_node in datamap.ssa.inorder_walk() {
            let reg_state = datamap.ssa.registers_in(walk_node);
            if reg_state.is_none() {
                continue;
            }
            let reg_map = utils::register_state_info(reg_state.unwrap(), datamap.ssa);
            for (idx, (node, _)) in reg_map.into_iter() {
                let name = datamap
                    .ssa
                    .regfile
                    .get_name(idx)
                    .unwrap_or("mem")
                    .to_string();
                let res = Self::verify_prepare_regs_of(datamap, node, name);
                if let Err(e) = res {
                    errors.push(e);
                }
            }
        }
        if errors.len() > 0 {
            Err(errors.join(Self::DELIM))
        } else {
            Ok(())
        }
    }

    fn verify_handler_each_node(
        ast: &mut CCFG,
        datamap: &mut CCFGDataMap,
        verifier: &Verifier,
        name: &str,
    ) -> Result<(), String> {
        let mut errors = Vec::new();
        for node in datamap.ssa.inorder_walk() {
            if let Err(err) = verifier(node, ast, datamap) {
                errors.push(err);
            }
        }
        if errors.len() > 0 {
            Err(format!(
                "{} @ {}",
                errors.join(Self::DELIM),
                name.to_string()
            ))
        } else {
            Ok(())
        }
    }

    fn verify_handle_uniop(
        node: SSARef,
        ast: &mut CCFG,
        datamap: &mut CCFGDataMap,
    ) -> Result<(), String> {
        // Ensure `handle_uniop` insert node as key into var_map.
        let expr = c_ast::Expr::Not;
        let operand_node = datamap
            .var_map
            .iter()
            .map(|(n, _)| *n)
            .filter(|n| *n != node)
            .next()
            .unwrap();
        // Erase the key so as to ensure whether the key will be correctly inserted
        // by handle_uniop
        datamap.var_map.remove(&node);
        datamap.handle_uniop(node, operand_node, expr, ast);
        if datamap.var_map.get(&node).is_none() {
            Err(format!("Failed to handle unary operator: {:?}", node))
        } else {
            Ok(())
        }
    }

    fn verify_handle_binop(
        node: SSARef,
        ast: &mut CCFG,
        datamap: &mut CCFGDataMap,
    ) -> Result<(), String> {
        // Ensure `handle_binop` insert node as key into var_map.
        let expr = c_ast::Expr::Add;
        let operand_nodes = datamap
            .var_map
            .iter()
            .map(|(n, _)| *n)
            .filter(|n| *n != node)
            .take(2)
            .collect();
        // Erase the key so as to ensure whether the key will be correctly inserted
        // by handle_binop
        datamap.var_map.remove(&node);
        datamap.handle_binop(node, operand_nodes, expr, ast);
        if datamap.var_map.get(&node).is_none() {
            Err(format!("Failed to handle binary operator: {:?}", node))
        } else {
            Ok(())
        }
    }

    fn verify_handle_cast(
        node: SSARef,
        ast: &mut CCFG,
        datamap: &mut CCFGDataMap,
    ) -> Result<(), String> {
        // Ensure `handle_cast` insert node as key into var_map.
        let expr = c_ast::Expr::Cast(8);
        let operand_node = datamap
            .var_map
            .iter()
            .map(|(n, _)| *n)
            .filter(|n| *n != node)
            .next()
            .unwrap()
            .clone();
        // Erase the key so as to ensure whether the key will be correctly inserted
        // by handle_uniop
        datamap.var_map.remove(&node);
        datamap.handle_cast(node, operand_node, expr, ast);
        if datamap.var_map.get(&node).is_none() {
            Err(format!("Failed to handle cast operator: {:?}", node))
        } else {
            Ok(())
        }
    }
}

#[cfg(test)]
mod test {
    use backend::lang_c::c_cfg;
    use backend::lang_c::c_cfg_builder::{CCFGBuilder, CCFGDataMap, CCFGBuilderVerifier,
                                                CCFGDataMapVerifier};
    use frontend::radeco_containers::RadecoFunction;
    use frontend::radeco_source::SourceErr;
    use middle::ir_reader;
    use middle::regfile::SubRegisterFile;
    use r2api::structs::LRegInfo;
    use serde_json;
    use std::collections::HashMap;
    use std::fs::File;
    use std::io::prelude::*;
    use std::path::PathBuf;
    use std::sync::Arc;

    fn register_profile() -> Result<LRegInfo, SourceErr> {
        let regfile_path = "./test_files/x86_register_profile.json";
        let path = PathBuf::from(regfile_path);
        let mut f = File::open(path).expect("Failed to open file");
        let mut json_str = String::new();
        let _ = f.read_to_string(&mut json_str).expect(
            "Failed to read file",
        );
        Ok(serde_json::from_str(&json_str)?)
    }

    fn load(name: &str) -> RadecoFunction {
        let ssa = {
            let regfile = Arc::new(SubRegisterFile::new(&register_profile().expect(
                "Unable to load register profile",
            )));
            let mut f = File::open(name).expect("file not found");
            let mut ir_str = String::new();
            f.read_to_string(&mut ir_str).expect(
                "something went wrong reading \
                 the file",
            );
            ir_reader::parse_il(&ir_str, regfile)
        };
        let mut rfn = RadecoFunction::default();
        *rfn.ssa_mut() = ssa;
        rfn
    }

    const FILES: [&'static str; 2] = ["./test_files/bin1_main_ssa", "./test_files/loopy_main_ssa"];

    #[test]
    fn c_ast_data_map_test() {
        for file in FILES.iter() {
            let mut rfn = load("./test_files/bin1_main_ssa");
            let mut datamap = CCFGDataMap::new(&rfn);
            let mut cast = c_cfg::CCFG::new(rfn.name.as_ref());
            CCFGDataMapVerifier::verify_datamap(&mut datamap, &mut cast, &HashMap::new())
                .expect(&format!("CASTBDataMap verification failed {}", file));
        }
    }

    #[test]
    fn c_ast_builder_test() {
        for file in FILES.iter() {
            let mut rfn = load("./test_files/bin1_main_ssa");
            let dummy_map = HashMap::new();
            let mut builder = CCFGBuilder::new(&rfn, &dummy_map);
            let data_graph = CCFGDataMap::recover_data(&rfn, &mut builder.cfg, &dummy_map);
            builder.datamap = data_graph;
            CCFGBuilderVerifier::verify(&mut builder).expect(&format!(
                "CCFGBuilder \
                 verification \
                 failed {}",
                file
            ));
        }
    }
}
