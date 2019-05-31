// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Module that implements constant propagation on the SSA IR.
//!
//! This module implements 'Sparse Conditional Constant Propagation (SCCP)' algorithm to
//! perform the propagation. For more details, please refer:
//!    * https://www.cs.utexas.edu/~lin/cs380c/wegman.pdf.
//!

use crate::analysis::analyzer::{
    Action, Analyzer, AnalyzerInfo, AnalyzerKind, AnalyzerResult, Change, FuncAnalyzer,
};
use crate::frontend::radeco_containers::RadecoFunction;
use crate::middle::ir::{MArity, MOpcode, WidthSpec};
use crate::middle::ssa::cfg_traits::{CFGMod, CFG};
use crate::middle::ssa::graph_traits::{ConditionInfo, Graph};
use crate::middle::ssa::ssa_traits::{NodeData, NodeType, ValueInfo, ValueType};
use crate::middle::ssa::ssa_traits::{SSAMod, SSA};
use crate::middle::ssa::ssastorage::SSAStorage;

use std::any::Any;
use std::collections::{HashMap, VecDeque};
use std::u64;

#[macro_export]
macro_rules! node_data_from_g {
    ($g:ident, $i:ident) => {
        $g.node_data(*$i).unwrap_or_else(|_x| {
            radeco_err!("RegisterState found, {:?}", _x);
            NodeData {
                vt: ValueInfo::new(ValueType::Invalid, WidthSpec::Unknown),
                nt: NodeType::Undefined,
            }
        })
    };
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum LatticeValue {
    Top,
    Bottom,
    Const(u64),
}

impl LatticeValue {
    fn is_undefined(&self) -> bool {
        *self == LatticeValue::Top
    }

    fn is_overdefined(&self) -> bool {
        *self == LatticeValue::Bottom
    }

    #[allow(dead_code)]
    fn is_constant(&self) -> bool {
        !(self.is_undefined() || self.is_overdefined())
    }
}

fn meet(v1: &LatticeValue, v2: &LatticeValue) -> LatticeValue {
    // Any ^ Top    = Any
    // Any ^ Bottom = Bottom
    //   C ^ C      = C      (C = Constant)
    //   C ^ D      = Bottom (C, D = Constant and C != D).

    match *v1 {
        LatticeValue::Top => return *v2,
        LatticeValue::Bottom => return LatticeValue::Bottom,
        _ => {}
    }

    match *v2 {
        LatticeValue::Top => return *v1,
        LatticeValue::Bottom => return LatticeValue::Bottom,
        _ => {}
    }

    if *v1 != *v2 {
        return LatticeValue::Bottom;
    }

    *v1
}

const NAME: &str = "sccp";
const REQUIRES: &[AnalyzerKind] = &[];

pub const INFO: AnalyzerInfo = AnalyzerInfo {
    name: NAME,
    kind: AnalyzerKind::SCCP,
    requires: REQUIRES,
    uses_policy: false,
};

#[derive(Debug)]
pub struct SCCP {
    ssa_worklist: VecDeque<<SSAStorage as SSA>::ValueRef>,
    cfg_worklist: VecDeque<<SSAStorage as CFG>::CFEdgeRef>,
    executable: HashMap<<SSAStorage as CFG>::CFEdgeRef, bool>,
    expr_val: HashMap<<SSAStorage as SSA>::ValueRef, LatticeValue>,
}

impl SCCP {
    pub fn new() -> SCCP {
        SCCP {
            ssa_worklist: VecDeque::new(),
            cfg_worklist: VecDeque::new(),
            executable: HashMap::new(),
            expr_val: HashMap::new(),
        }
    }

    pub fn dump(&self) {
        println!("{:?}", self.expr_val);
    }

    fn visit_phi(&mut self, g: &SSAStorage, i: &<SSAStorage as SSA>::ValueRef) -> LatticeValue {
        let operands = g.operands_of(*i);
        let mut phi_val = self.get_value(g, i);

        // If "overdefined" return it.
        if phi_val.is_overdefined() {
            return LatticeValue::Bottom;
        }

        let invalid_block = g.invalid_action().expect("Invalid Action is not defind");
        let parent_block = g.block_for(*i).unwrap_or(invalid_block);
        for op in &operands {
            let operand_block = g.block_for(*op).unwrap_or(invalid_block);
            let op_val = self.get_value(g, op);

            if op_val.is_undefined() {
                continue;
            }

            //if op_val.is_overdefined() {
            //    return LatticeValue::Bottom;
            //}
            // Only operand which could be executable will be considered.
            // Even these operands are overdefined.

            let edge = g.find_edges_between(operand_block, parent_block);
            if edge.len() == 0 {
                continue;
            }
            assert_eq!(edge.len(), 1);
            if !self.is_executable(&edge[0])
                && edge[0] != g.invalid_edge().expect("Invalid Edge is not defined")
            {
                continue;
            }
            //TODO: Not sure what to do when the edge is invalid

            phi_val = meet(&phi_val, &op_val);
        }
        phi_val
    }

    fn evaluate_control_flow(&mut self, g: &SSAStorage, i: &<SSAStorage as SSA>::ValueRef) {
        assert!(g.is_selector(*i));

        let cond_val = self.get_value(g, i);
        let block = g.selector_for(*i).unwrap_or_else(|| {
            radeco_err!("Victim value is not a selector");
            g.invalid_action().unwrap()
        });
        let invalid_edge = g.invalid_edge().expect("Invalid Edge is not defined");
        let conditional_branches = if let Some(branches) = g.conditional_edges(block) {
            branches
        } else {
            ConditionInfo::new(invalid_edge, invalid_edge)
        };
        let true_branch = conditional_branches.true_side;
        let false_branch = conditional_branches.false_side;
        match cond_val {
            LatticeValue::Bottom => {
                self.cfg_worklist.push_back(true_branch);
                self.cfg_worklist.push_back(false_branch);
            }
            LatticeValue::Top => {
                radeco_warn!("`cond_val` is undefined");
                self.cfg_worklist.push_back(true_branch);
                self.cfg_worklist.push_back(false_branch);
            }
            LatticeValue::Const(cval) => {
                if cval == 0 {
                    self.cfgwl_push(&true_branch);
                } else {
                    self.cfgwl_push(&false_branch);
                }
            }
        }
    }

    fn evaluate_unary_op(
        &mut self,
        g: &SSAStorage,
        i: &<SSAStorage as SSA>::ValueRef,
        opcode: MOpcode,
    ) -> LatticeValue {
        let operand = g.operands_of(*i);
        let operand = if operand.is_empty() {
            return LatticeValue::Top;
        } else {
            operand[0]
        };

        let val = self.get_value(g, &operand);
        let const_val = if let LatticeValue::Const(cval) = val {
            cval
        } else {
            return val;
        };

        let mut val: u64 = match opcode {
            MOpcode::OpZeroExt(_) | MOpcode::OpSignExt(_) => {
                // Nothing to do in case of widen as the value cannot change.
                const_val
            }
            MOpcode::OpNarrow(size) => {
                // Max size is 64. Therefore, we can _never_ narrow to 64.
                assert!(size < 64);
                let mask = (1 << (size)) - 1;
                const_val & mask
            }
            MOpcode::OpMov => const_val as u64,
            MOpcode::OpNot => !const_val as u64,
            MOpcode::OpCall => {
                return LatticeValue::Bottom;
            }
            _ => unreachable!(),
        };

        // We should consider width.
        let ndata = node_data_from_g!(g, i);
        let w = ndata.vt.width().get_width().unwrap_or(64);
        if w < 64 {
            val = val & ((1 << (w)) - 1);
        }

        LatticeValue::Const(val)
    }

    fn evaluate_binary_op(
        &mut self,
        g: &SSAStorage,
        i: &<SSAStorage as SSA>::ValueRef,
        opcode: MOpcode,
    ) -> LatticeValue {
        // Do not reason about load/stores.
        match opcode {
            MOpcode::OpLoad | MOpcode::OpStore => return LatticeValue::Bottom,
            _ => {}
        }

        let operands = g
            .operands_of(*i)
            .iter()
            .map(|x| self.get_value(g, x))
            .collect::<Vec<_>>();

        let lhs = operands[0];
        let rhs = operands[1];

        let lhs_val = if let LatticeValue::Const(cval) = lhs {
            cval
        } else {
            return lhs;
        };
        let rhs_val = if let LatticeValue::Const(cval) = rhs {
            cval
        } else {
            return rhs;
        };

        let mut val: u64 = match opcode {
            MOpcode::OpAdd => {
                // lhs_val + rhs_val
                // we should avoid integer overflow panic
                let remained: u64 = u64::MAX - lhs_val;
                if remained >= rhs_val {
                    lhs_val + rhs_val
                } else {
                    (rhs_val - remained - 1) as u64
                }
            }
            MOpcode::OpSub => {
                if lhs_val >= rhs_val {
                    lhs_val - rhs_val
                } else {
                    // rhs_val - lhs_val
                    // This will never integer overflow
                    (u64::MAX - rhs_val + lhs_val + 1) as u64
                }
            }
            MOpcode::OpMul => {
                // TODO: how to handle integer overflow.
                lhs_val * rhs_val
            }
            MOpcode::OpDiv => lhs_val / rhs_val,
            MOpcode::OpMod => lhs_val % rhs_val,
            MOpcode::OpAnd => lhs_val & rhs_val,
            MOpcode::OpOr => lhs_val | rhs_val,
            MOpcode::OpXor => lhs_val ^ rhs_val,
            MOpcode::OpEq => (lhs_val == rhs_val) as u64,
            MOpcode::OpGt => (lhs_val > rhs_val) as u64,
            MOpcode::OpLt => (lhs_val < rhs_val) as u64,
            MOpcode::OpLsl => lhs_val << rhs_val,
            MOpcode::OpLsr => lhs_val >> rhs_val,
            _ => unreachable!(),
        };

        // We should consider width.
        let ndata = node_data_from_g!(g, i);
        let w = ndata.vt.width().get_width().unwrap_or(64);
        if w < 64 {
            val = val & ((1 << (w)) - 1);
        }

        LatticeValue::Const(val)
    }

    fn evaluate_ternary_op(
        &mut self,
        _i: &<SSAStorage as SSA>::ValueRef,
        opcode: MOpcode,
    ) -> LatticeValue {
        // Do not reason about stores.
        match opcode {
            MOpcode::OpStore => return LatticeValue::Bottom,
            _ => unimplemented!(),
        }
    }

    fn visit_expression(
        &mut self,
        g: &SSAStorage,
        i: &<SSAStorage as SSA>::ValueRef,
    ) -> LatticeValue {
        let expr = g.node_data(*i).unwrap_or_else(|_x| {
            radeco_err!("RegisterState found, {:?}", _x);
            NodeData {
                vt: ValueInfo::new(ValueType::Invalid, WidthSpec::Unknown),
                nt: NodeType::Undefined,
            }
        });
        let opcode = if let NodeType::Op(opcode) = expr.nt {
            opcode
        } else {
            radeco_err!("Found something other than an expression!");
            MOpcode::OpInvalid
        };

        let val = if let MOpcode::OpConst(v) = opcode {
            LatticeValue::Const(v as u64)
        } else {
            match opcode.arity() {
                MArity::Unary => self.evaluate_unary_op(g, i, opcode),
                MArity::Binary => self.evaluate_binary_op(g, i, opcode),
                _ => self.evaluate_ternary_op(i, opcode),
            }
        };

        // If expression is a `Selector` it means that it's value can affect the
        // control flow.
        // Hence evaluate the control flow to add edges to the cfgwl.
        // TODO: Handle the case where the selector of a block may belong to a
        // different block.
        if g.is_selector(*i) {
            self.evaluate_control_flow(g, i);
        }

        val
    }

    /// ////////////////////////////////////////////////////////////////////////
    /// / Helper functions.
    /// ////////////////////////////////////////////////////////////////////////

    fn is_executable(&self, i: &<SSAStorage as CFG>::CFEdgeRef) -> bool {
        self.executable.get(i).cloned().unwrap_or(false)
    }

    fn mark_executable(&mut self, i: &<SSAStorage as CFG>::CFEdgeRef) {
        let n = self.executable.entry(*i).or_insert(false);
        *n = true;
    }

    // Determines the Initial value
    fn init_val(&self, g: &SSAStorage, i: &<SSAStorage as SSA>::ValueRef) -> LatticeValue {
        //TODO replace unwrap
        let node_data = g.node_data(*i).unwrap();
        match node_data.nt {
            NodeType::Op(MOpcode::OpConst(v)) => LatticeValue::Const(v),
            NodeType::Undefined => LatticeValue::Bottom,
            _ => LatticeValue::Top,
        }
    }

    fn get_value(&mut self, g: &SSAStorage, i: &<SSAStorage as SSA>::ValueRef) -> LatticeValue {
        if self.expr_val.contains_key(i) {
            return self.expr_val[i];
        }

        let v = self.init_val(g, i);
        self.expr_val.insert(*i, v);
        v
    }

    fn set_value(&mut self, i: &<SSAStorage as SSA>::ValueRef, v: LatticeValue) {
        let n = self.expr_val.entry(*i).or_insert(LatticeValue::Top);
        *n = v;
    }

    fn is_block_executable(&self, g: &SSAStorage, i: &<SSAStorage as CFG>::ActionRef) -> bool {
        // entry_node is always reachable.
        if *i == entry_node_err!(&g) {
            return true;
        }
        let incoming = g.incoming_edges(*i);
        for &(ref edge, _) in &incoming {
            if self.is_executable(edge) {
                return true;
            }
        }
        false
    }

    fn ssawl_push(&mut self, g: &SSAStorage, i: &<SSAStorage as SSA>::ValueRef) {
        if !g.is_expr(*i) {
            return;
        }
        let owner_block = g.block_for(*i).unwrap_or_else(|| {
            radeco_err!("Value node doesn't belong to any block");
            g.invalid_action().unwrap()
        });
        if self.is_block_executable(g, &owner_block) {
            self.ssa_worklist.push_back(*i);
        }
    }

    fn cfgwl_push(&mut self, i: &<SSAStorage as CFG>::CFEdgeRef) {
        self.cfg_worklist.push_back(*i);
    }
}

impl Analyzer for SCCP {
    fn info(&self) -> &'static AnalyzerInfo {
        &INFO
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl FuncAnalyzer for SCCP {
    fn analyze<T: FnMut(Box<dyn Change>) -> Action>(
        &mut self,
        rfn: &mut RadecoFunction,
        _policy: Option<T>,
    ) -> Option<Box<dyn AnalyzerResult>> {
        let mut g = rfn.ssa_mut();

        {
            let entry_node = entry_node_err!(g);
            let edges = g.outgoing_edges(entry_node);
            for &(ref next, _) in &edges {
                self.cfgwl_push(next);
            }
            for arg in g.operands_of(registers_in_err!(g, entry_node)) {
                self.set_value(&arg, LatticeValue::Bottom);
            }
        }

        while !self.ssa_worklist.is_empty() || !self.cfg_worklist.is_empty() {
            while let Some(edge) = self.cfg_worklist.pop_front() {
                if !self.is_executable(&edge) {
                    self.mark_executable(&edge);
                    let block = g
                        .edge_info(edge)
                        .unwrap_or_else(|| g.edge_info(g.invalid_edge().unwrap()).unwrap())
                        .target;
                    let phis = g.phis_in(block);
                    for phi in &phis {
                        let v = self.visit_phi(&mut g, phi);
                        self.set_value(phi, v);
                    }

                    let visits = g.incoming_edges(block).iter().fold(0, |acc, &e| {
                        if self.is_executable(&e.0) {
                            acc + 1
                        } else {
                            acc
                        }
                    });

                    // If this is the first visit to the block.
                    if visits == 1 {
                        for expr in g.exprs_in(block) {
                            let val = self.visit_expression(&mut g, &expr);
                            self.set_value(&expr, val);
                            for use_ in g.uses_of(expr) {
                                self.ssawl_push(&mut g, &use_);
                            }
                        }
                        if let Some(selector) = g.selector_in(block) {
                            let val = self.visit_expression(&mut g, &selector);
                            self.set_value(&selector, val);
                        }
                    }

                    if let Some(next_edge) = g.unconditional_edge(block) {
                        self.cfgwl_push(&next_edge);
                    }
                }
            } // End of cfgwl

            while let Some(e) = self.ssa_worklist.pop_front() {
                let t = if g.is_expr(e) {
                    let block_of = g.block_for(e).unwrap_or_else(|| {
                        radeco_err!("Value node doesn't belong to any block");
                        g.invalid_action().unwrap()
                    });
                    if self.is_block_executable(&mut g, &block_of) {
                        self.visit_expression(&mut g, &e)
                    } else {
                        self.get_value(&mut g, &e)
                    }
                } else {
                    self.visit_phi(&mut g, &e)
                };

                if t != self.get_value(&mut g, &e) {
                    self.set_value(&e, t);
                    for use_ in &g.uses_of(e) {
                        self.ssawl_push(&mut g, use_);
                    }
                }
            } // End of ssawl
        } // End of while-loop

        for (k, v) in &self.expr_val {
            if g.constant(*k).is_some() {
                continue;
            }
            if let LatticeValue::Const(val) = *v {
                radeco_trace!("{:?} with {:?} --> Const {:#}", k, g.node_data(*k), val);
                let ndata = node_data_from_g!(g, k);
                let w = ndata.vt.width().get_width();
                // BUG: Width may be changed just using a simple replace.
                let const_node = g.insert_const(val, w).unwrap_or_else(|| {
                    radeco_err!("Cannot insert new constants");
                    g.invalid_value().unwrap()
                });
                g.replace_value(*k, const_node);
            }
        }
        let blocks = g.blocks();
        let mut remove_edges = Vec::<<SSAStorage as CFG>::CFEdgeRef>::new();
        let mut remove_blocks = Vec::<<SSAStorage as CFG>::ActionRef>::new();
        for block in &blocks {
            let edges = g.outgoing_edges(*block);
            for &(ref edge, _) in &edges {
                if !self.is_executable(edge) {
                    remove_edges.push(*edge);
                }
            }
            // TODO: Make this automatic in dce.
            if !self.is_block_executable(g, block) {
                remove_blocks.push(*block);
            }
        }
        for edge in &remove_edges {
            g.remove_data_edge(*edge);
        }
        for block in &remove_blocks {
            g.remove_block(*block);
        }

        None
    }
}

#[cfg(test)]
mod test {
    use super::{meet, LatticeValue};

    #[test]
    fn test_meet() {
        let t = LatticeValue::Top;
        let b = LatticeValue::Bottom;
        let c1 = LatticeValue::Const(1);
        let c2 = LatticeValue::Const(2);

        assert_eq!(meet(&t, &t), t);
        assert_eq!(meet(&t, &b), b);
        assert_eq!(meet(&t, &c1), c1);
        assert_eq!(meet(&t, &c2), c2);
        assert_eq!(meet(&c1, &b), b);
        assert_eq!(meet(&c2, &c1), b);
        assert_eq!(meet(&c1, &c1), c1);
    }
}
