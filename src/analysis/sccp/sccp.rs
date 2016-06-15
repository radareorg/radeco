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

use std::collections::{HashMap, VecDeque};
use middle::ssa::ssa_traits::{SSA, SSAMod};
use middle::ssa::ssa_traits::NodeType;
use middle::ir::{MArity, MOpcode};
use middle::ssa::cfg_traits::CFG;

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

pub struct Analyzer<T: SSAMod + SSA + Clone> {
    ssa_worklist: VecDeque<T::ValueRef>,
    cfg_worklist: VecDeque<T::CFEdgeRef>,
    executable: HashMap<T::CFEdgeRef, bool>,
    expr_val: HashMap<T::ValueRef, LatticeValue>,
    g: T,
}

impl<T: SSA + SSAMod + Clone> Analyzer<T> {
    pub fn new(g: &mut T) -> Analyzer<T> {
        Analyzer {
            ssa_worklist: VecDeque::new(),
            cfg_worklist: VecDeque::new(),
            executable: HashMap::new(),
            expr_val: HashMap::new(),
            g: g.clone(),
        }
    }

    pub fn dump(&self) {
    }

    fn visit_phi(&mut self, i: &T::ValueRef) -> LatticeValue {
        let operands = self.g.get_operands(i);
        let mut phi_val = self.get_value(i);

        // If "overdefined" return it.
        if phi_val.is_overdefined() {
            return LatticeValue::Bottom;
        }

        let parent_block = self.g.get_block(i);
        for op in &operands {
            let operand_block = self.g.get_block(&op);
            let op_val = self.get_value(&op);

            if op_val.is_undefined() {
                continue;
            }

            if op_val.is_overdefined() {
                return LatticeValue::Bottom;
            }

            let edge = self.g.find_edge(&operand_block, &parent_block);
            if !self.is_executable(&edge) && edge != self.g.invalid_edge() {
                continue;
            }

            phi_val = meet(&phi_val, &op_val);
        }
        phi_val
    }

    fn evaluate_control_flow(&mut self, i: &T::ValueRef) {
        assert!(self.g.is_selector(i));

        let cond_val = self.get_value(i);
        let block = self.g.selects_for(i);
        let true_branch = self.g.true_edge_of(&block);
        let false_branch = self.g.false_edge_of(&block);
        match cond_val {
            LatticeValue::Bottom => {
                self.cfg_worklist.push_back(true_branch);
                self.cfg_worklist.push_back(false_branch);
            }
            LatticeValue::Top => {
                // TODO: Not really sure what to do here.
            }
            LatticeValue::Const(cval) => {
                if cval == 1 {
                    self.cfgwl_push(&true_branch);
                } else {
                    self.cfgwl_push(&false_branch);
                }
            }
        }
    }

    fn evaluate_unary_op(&mut self, i: &T::ValueRef, opcode: MOpcode) -> LatticeValue {
        let operand = self.g.get_operands(i);
        let operand = if operand.is_empty() {
            return LatticeValue::Top;
        } else {
            operand[0]
        };

        let val = self.get_value(&operand);
        let const_val = if let LatticeValue::Const(cval) = val {
            cval
        } else {
            return val;
        };

        let val = match opcode {
            MOpcode::OpWiden(_) => {
                // Nothing to do in case of widen as the value cannot change.
                const_val
            }
            MOpcode::OpNarrow(size) => {
                // Max size is 64. Therefore, we can _never_ narrow to 64.
                assert!(size < 64);
                let mask = (1 << (size)) - 1;
                const_val & mask
            }
            MOpcode::OpNot => {
                !const_val as u64
            }
            _ => unreachable!(),
        };

        LatticeValue::Const(val)
    }

    fn evaluate_binary_op(&mut self, i: &T::ValueRef, opcode: MOpcode) -> LatticeValue {
        // Do not reason about load/stores.
        match opcode {
            MOpcode::OpLoad | MOpcode::OpStore => return LatticeValue::Bottom,
            _ => { },
        }

        let operands = self.g.get_operands(i).iter().map(|x| self.get_value(x)).collect::<Vec<_>>();

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

        let val = match opcode {
            MOpcode::OpAdd => {
                lhs_val + rhs_val
            }
            MOpcode::OpSub => {
                if lhs_val > rhs_val {
                    lhs_val - rhs_val
                } else {
                    rhs_val - lhs_val
                }
            }
            MOpcode::OpMul => {
                lhs_val * rhs_val
            }
            MOpcode::OpDiv => {
                lhs_val / rhs_val
            }
            MOpcode::OpMod => {
                lhs_val % rhs_val
            }
            MOpcode::OpAnd => {
                lhs_val & rhs_val
            }
            MOpcode::OpOr => {
                lhs_val | rhs_val
            }
            MOpcode::OpXor => {
                lhs_val ^ rhs_val
            }
            MOpcode::OpCmp => {
                (lhs_val == rhs_val) as u64
            }
            MOpcode::OpGt => {
                (lhs_val > rhs_val) as u64
            }
            MOpcode::OpLt => {
                (lhs_val < rhs_val) as u64
            }
            MOpcode::OpLsl => {
                lhs_val << rhs_val
            }
            MOpcode::OpLsr => {
                lhs_val >> rhs_val
            }
            _ => unreachable!(),
        };

        LatticeValue::Const(val)
    }

    fn visit_expression(&mut self, i: &T::ValueRef) -> LatticeValue {
        let expr = self.g.get_node_data(i).unwrap();
        let opcode = if let NodeType::Op(opcode) = expr.nt {
            opcode
        } else {
            panic!("Found something other than an expression!");
        };

        if let MOpcode::OpConst(v) = opcode {
            return LatticeValue::Const(v as u64);
        }

        let val = match opcode.arity() {
            MArity::Unary => self.evaluate_unary_op(i, opcode),
            MArity::Binary => self.evaluate_binary_op(i, opcode),
            _ => unimplemented!(),
        };

        // If expression is a `Selector` it means that it's value can affect the
        // control flow.
        // Hence evaluate the control flow to add edges to the cfgwl.
        // TODO: Handle the case where the selector of a block may belong to a
        // different block.
        if self.g.is_selector(i) {
            self.evaluate_control_flow(i);
        }

        val
    }

    pub fn analyze(&mut self) {

        {
            let start_node = self.g.start_node();
            let edges = self.g.edges_of(&start_node);
            for &(ref next, _) in &edges {
                self.cfgwl_push(next);
            }
        }

        while !self.ssa_worklist.is_empty() || !self.cfg_worklist.is_empty() {
            while let Some(edge) = self.cfg_worklist.pop_front() {
                if !self.is_executable(&edge) {
                    self.mark_executable(&edge);
                    let block = self.g.target_of(&edge);
                    let phis = self.g.get_phis(&block);
                    for phi in &phis {
                        let v = self.visit_phi(phi);
                        self.set_value(phi, v);
                    }

                    let visits = self.g.incoming_edges(&block).iter().fold(0, |acc, &e| {
                        if self.is_executable(&e.0) {
                            acc + 1
                        } else {
                            acc
                        }
                    });

                    // If this is the first visit to the block.
                    if visits == 1 {
                        for expr in self.g.exprs_in(&block) {
                            let val = self.visit_expression(&expr);
                            self.set_value(&expr, val);
                            for use_ in self.g.get_uses(&expr) {
                                self.ssawl_push(&use_);
                            }
                        }
                    }

                    let next_edge = self.g.next_edge_of(&block);
                    if next_edge != self.g.invalid_edge() {
                        self.cfgwl_push(&next_edge);
                    }
                }
            } // End of cfgwl

            while let Some(e) = self.ssa_worklist.pop_front() {
                let t = if self.g.is_expr(&e) {
                    let block_of = self.g.block_of(&e);
                    if self.is_block_executable(&block_of) {
                        self.visit_expression(&e)
                    } else {
                        self.get_value(&e)
                    }
                } else {
                    self.visit_phi(&e)
                };

                if t != self.get_value(&e) {
                    self.set_value(&e, t);
                    for use_ in &self.g.get_uses(&e) {
                        self.ssawl_push(use_);
                    }
                }
            } // End of ssawl
        } // End of while-loop
    }

    pub fn emit_ssa(&mut self) -> T {
        for (k, v) in &self.expr_val {
            if let LatticeValue::Const(val) = *v {
                let newnode = self.g.add_const(val);
                self.g.replace(*k, newnode);
            }
        }
        let blocks = self.g.blocks();
        let mut remove_edges = Vec::<T::CFEdgeRef>::new();
        let mut remove_blocks = Vec::<T::ActionRef>::new();
        for block in &blocks {
            let edges = self.g.edges_of(block);
            for &(ref edge, _) in &edges {
                if !self.is_executable(edge) {
                    remove_edges.push(*edge);
                }
            }
            // TODO: Make this automatic in dce.
            if !self.is_block_executable(block) {
                remove_blocks.push(*block);
            }
        }
        for edge in &remove_edges {
            self.g.remove_edge(edge);
        }
        for block in &remove_blocks {
            self.g.remove_block(*block);
        }
        self.g.clone()
    }

    /// ////////////////////////////////////////////////////////////////////////
    /// / Helper functions.
    /// ////////////////////////////////////////////////////////////////////////

    fn is_executable(&self, i: &T::CFEdgeRef) -> bool {
        self.executable.get(i).cloned().unwrap_or(false)
    }

    fn mark_executable(&mut self, i: &T::CFEdgeRef) {
        let n = self.executable.entry(*i).or_insert(false);
        *n = true;
    }

    // Determines the Initial value
    fn init_val(&self, i: &T::ValueRef) -> LatticeValue {
        let node_data = self.g.get_node_data(i).unwrap();
        match node_data.nt {
            NodeType::Op(MOpcode::OpConst(v)) => LatticeValue::Const(v),
            NodeType::Undefined => LatticeValue::Bottom,
            _ => LatticeValue::Top,
        }
    }

    fn get_value(&mut self, i: &T::ValueRef) -> LatticeValue {
        if self.expr_val.contains_key(i) {
            return self.expr_val[i];
        }

        let v = self.init_val(i);
        self.expr_val.insert(*i, v);
        v
    }

    fn set_value(&mut self, i: &T::ValueRef, v: LatticeValue) {
        let n = self.expr_val.entry(*i).or_insert(LatticeValue::Top);
        *n = v;
    }

    fn is_block_executable(&self, i: &T::ActionRef) -> bool {
        // start_node is always reachable.
        if *i == self.g.start_node() {
            return true;
        }
        let incoming = self.g.incoming_edges(i);
        for &(ref edge, _) in &incoming {
            if self.is_executable(edge) {
                return true;
            }
        }
        false
    }

    fn ssawl_push(&mut self, i: &T::ValueRef) {
        if !self.g.is_expr(i) {
            return;
        }
        let owner_block = self.g.get_block(&i);
        if self.is_block_executable(&owner_block) {
            self.ssa_worklist.push_back(*i);
        }
    }

    fn cfgwl_push(&mut self, i: &T::CFEdgeRef) {
        self.cfg_worklist.push_back(*i);
    }
}

#[cfg(test)]
mod test {
    use super::{LatticeValue, meet};

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
