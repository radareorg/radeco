use std::collections::{HashMap, HashSet};
use frontend::radeco_containers::RadecoFunction;
use middle::ir::MOpcode;
use middle::ssa::utils;
use middle::ssa::ssastorage::{NodeData, SSAStorage};
use middle::ssa::ssa_traits::{SSA, SSAExtra, SSAMod, SSAWalk, ValueInfo};
use middle::ssa::cfg_traits::CFG;
use super::c_simple_ast::{ValueNode, SimpleCAST};
use super::c_simple;
use petgraph::visit::EdgeRef;
use petgraph::graph::{Graph, NodeIndex, EdgeIndex, Edges, EdgeReference};
use petgraph::{EdgeDirection, Direction, Directed};

pub struct CASTDataGraph<'a> {
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

impl<'a> CASTDataGraph<'a> {
    pub fn new(rfn: &'a RadecoFunction) -> CASTDataGraph<'a> {
        CASTDataGraph {
            ssa: rfn.ssa(),
            rfn: rfn,
            var_map: HashMap::new(),
            reg_map: HashMap::new(),
            consts: HashSet::new(),
            seen: HashSet::new(),
        }
    }

    // Returns data map from SSAStorage's NodeIndex to SimpleCAST's NodeIndex
    // pub fn yo(ast: &'a mut SimpleCAST, ssa: &'a SSAStorage) -> HashMap<NodeIndex, NodeIndex> {
    pub fn yo(rfn: &'a RadecoFunction, ast: &mut SimpleCAST) -> Self {
        let mut s = Self::new(rfn);
        s.prepare_consts(ast);
        s.prepare_regs(ast);
        s.recover_data(ast);
        s
    }

    fn handle_binop(&mut self, ret_node: NodeIndex, ops: Vec<NodeIndex>,
                    expr: c_simple::Expr, ast: &mut SimpleCAST) {
        // XXX for debbuging, do not use filter_map
        let ops_mapped = ops.into_iter()
            .filter_map(|op| self.var_map.get(&op))
            .map(|n| *n)
            .collect::<Vec<_>>();
        let expr_node = ast.expr(ops_mapped.as_slice(), expr);
        radeco_trace!("Add {:?} to {:?}", ret_node, expr_node);
        self.var_map.insert(ret_node, expr_node);
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
        if let Some(head) = ops.into_iter()
           .filter_map(|n| self.var_map.get(&n))
           .map(|n| *n)
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
            MOpcode::OpLsl => {
                // TODO
                //unimplemented!()
            },
            MOpcode::OpLsr => {
                // TODO
                //unimplemented!()
            },
            MOpcode::OpLt => self.handle_binop(ret_node, ops, c_simple::Expr::Lt, ast),
            MOpcode::OpMod => self.handle_binop(ret_node, ops, c_simple::Expr::Mod, ast),
            MOpcode::OpMul => self.handle_binop(ret_node, ops, c_simple::Expr::Mul, ast),
            MOpcode::OpNarrow(size) => {
                // TODO
            },
            MOpcode::OpNot => self.handle_binop(ret_node, ops, c_simple::Expr::Not, ast),
            MOpcode::OpOr => self.handle_binop(ret_node, ops, c_simple::Expr::Or, ast),
            MOpcode::OpRol => unimplemented!(),
            MOpcode::OpRor => unimplemented!(),
            MOpcode::OpSignExt(size) => {
                // TODO
            },
            MOpcode::OpSub => self.handle_binop(ret_node, ops, c_simple::Expr::Sub, ast),
            MOpcode::OpXor => self.handle_binop(ret_node, ops, c_simple::Expr::Xor, ast),
            MOpcode::OpZeroExt(size) => {
                // TODO
            },
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
            // XXX
            let ast_node = ast.constant(&name, None);
            self.reg_map.insert(name, ast_node);
            self.var_map.insert(node, ast_node);
        }
    }

    fn recover_data(&mut self, ast: &mut SimpleCAST) {
        for node in self.ssa.inorder_walk() {
            if self.ssa.is_phi(node) {
                self.handle_phi(node);
            } else if self.ssa.is_expr(node) {
                self.update_values(node, ast);
            }
        }
    }

}

