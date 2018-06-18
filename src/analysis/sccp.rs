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
use std::u64;
use middle::ssa::ssa_traits::{SSA, SSAMod};
use middle::ssa::ssa_traits::{NodeData, NodeType, ValueInfo, ValueType};
use middle::ssa::graph_traits::{Graph, ConditionInfo};
use middle::ir::{MArity, MOpcode, WidthSpec, MAddress};

#[macro_export]
macro_rules! node_data_from_g {
    ($self:ident, $i:ident) => {
        $self.g.node_data(*$i).unwrap_or_else(|x| {
            radeco_err!("RegisterState found, {:?}", x);
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

pub struct Analyzer<T> 
    where T: Clone +
        SSAMod<ActionRef=<T as Graph>::GraphNodeRef, 
                    CFEdgeRef=<T as Graph>::GraphEdgeRef> +
        SSA<ActionRef=<T as Graph>::GraphNodeRef, 
                    CFEdgeRef=<T as Graph>::GraphEdgeRef> 
{
    ssa_worklist: VecDeque<T::ValueRef>,
    cfg_worklist: VecDeque<T::CFEdgeRef>,
    executable: HashMap<T::CFEdgeRef, bool>,
    expr_val: HashMap<T::ValueRef, LatticeValue>,
    g: T,
}

impl<T> Analyzer<T> 
    where T: Clone +
        SSAMod<ActionRef=<T as Graph>::GraphNodeRef, 
                    CFEdgeRef=<T as Graph>::GraphEdgeRef> +
        SSA<ActionRef=<T as Graph>::GraphNodeRef, 
                    CFEdgeRef=<T as Graph>::GraphEdgeRef> 
{
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
        println!("{:?}", self.expr_val);
    }

    fn visit_phi(&mut self, i: &T::ValueRef) -> LatticeValue {
        let operands = self.g.operands_of(*i);
        let mut phi_val = self.get_value(i);

        // If "overdefined" return it.
        if phi_val.is_overdefined() {
            return LatticeValue::Bottom;
        }

        let invalid_block = self.g.invalid_action().expect("Invalid Action is not defind");
        let parent_block = self.g.block_for(*i).unwrap_or(invalid_block);
        for op in &operands {
            let operand_block = self.g.block_for(*op).unwrap_or(invalid_block);
            let op_val = self.get_value(op);

            if op_val.is_undefined() {
                continue;
            }

            //if op_val.is_overdefined() {
            //    return LatticeValue::Bottom;
            //}
            // Only operand which could be executable will be considered.
            // Even these operands are overdefined.

            let edge = self.g.find_edges_between(operand_block, parent_block);
            if edge.len() == 0 {
                continue;
            }
            assert_eq!(edge.len(), 1);
            if !self.is_executable(&edge[0]) && 
                edge[0] != self.g.invalid_edge().expect("Invalid Edge is not defined") {
                continue;
            }
            //TODO: Not sure what to do when the edge is invalid

            phi_val = meet(&phi_val, &op_val);
        }
        phi_val
    }

    fn evaluate_control_flow(&mut self, i: &T::ValueRef) {
        assert!(self.g.is_selector(*i));

        let cond_val = self.get_value(i);
        let block = self.g.selector_for(*i).unwrap_or_else(|| {
                        radeco_err!("Victim value is not a selector");
                        self.g.invalid_action().unwrap()
        });
        let invalid_edge = self.g.invalid_edge().expect("Invalid Edge is not defined");
        let conditional_branches = 
            if let Some(branches) = self.g.conditional_edges(block) {
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
        let operand = self.g.operands_of(*i);
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
            MOpcode::OpNot => {
                !const_val as u64
            }
            MOpcode::OpCall => {
                return LatticeValue::Bottom;
            }
            _ => unreachable!(),
        };

        // We should consider width.
        let ndata = node_data_from_g!(self, i);
        let w = ndata.vt.width().get_width().unwrap_or(64);
        if w < 64 {
            val = val & ((1 << (w)) - 1);
        }

        LatticeValue::Const(val)
    }

    fn evaluate_binary_op(&mut self, i: &T::ValueRef, opcode: MOpcode) -> LatticeValue {
        // Do not reason about load/stores.
        match opcode {
            MOpcode::OpLoad | MOpcode::OpStore => return LatticeValue::Bottom,
            _ => { },
        }

        let operands = self.g.operands_of(*i).iter().map(|x| self.get_value(x)).collect::<Vec<_>>();

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
            MOpcode::OpEq => {
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

        // We should consider width.
        let ndata = node_data_from_g!(self, i);
        let w = ndata.vt.width().get_width().unwrap_or(64);
        if w < 64 {
            val = val & ((1 << (w)) - 1);
        }

        LatticeValue::Const(val)
    }

    fn evaluate_ternary_op(&mut self, i: &T::ValueRef, opcode: MOpcode) -> LatticeValue {
        // Do not reason about stores.
        match opcode {
            MOpcode::OpStore => return LatticeValue::Bottom,
            _ => unimplemented!(),
        }
    }

    fn visit_expression(&mut self, i: &T::ValueRef) -> LatticeValue {
        let expr = self.g.node_data(*i).unwrap_or_else(|x| {
            radeco_err!("RegisterState found, {:?}", x);
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

        if let MOpcode::OpConst(v) = opcode {
            return LatticeValue::Const(v as u64);
        }

        let val = match opcode.arity() {
            MArity::Unary => self.evaluate_unary_op(i, opcode),
            MArity::Binary => self.evaluate_binary_op(i, opcode),
            _ => self.evaluate_ternary_op(i, opcode),
        };

        // If expression is a `Selector` it means that it's value can affect the
        // control flow.
        // Hence evaluate the control flow to add edges to the cfgwl.
        // TODO: Handle the case where the selector of a block may belong to a
        // different block.
        if self.g.is_selector(*i) {
            self.evaluate_control_flow(i);
        }

        val
    }

    pub fn analyze(&mut self) {

        {
            let entry_node = entry_node_err!(self.g);
            let edges = self.g.outgoing_edges(entry_node);
            for &(ref next, _) in &edges {
                self.cfgwl_push(next);
            }
        }

        while !self.ssa_worklist.is_empty() || !self.cfg_worklist.is_empty() {
            while let Some(edge) = self.cfg_worklist.pop_front() {
                if !self.is_executable(&edge) {
                    self.mark_executable(&edge);
                    let block = self.g.edge_info(edge).unwrap_or_else(|| {
                        self.g.edge_info(self.g.invalid_edge().unwrap()).unwrap()
                    }).target;
                    let phis = self.g.phis_in(block);
                    for phi in &phis {
                        let v = self.visit_phi(phi);
                        self.set_value(phi, v);
                    }

                    let visits = self.g.incoming_edges(block).iter().fold(0, |acc, &e| {
                        if self.is_executable(&e.0) {
                            acc + 1
                        } else {
                            acc
                        }
                    });

                    // If this is the first visit to the block.
                    if visits == 1 {
                        for expr in self.g.exprs_in(block) {
                            let val = self.visit_expression(&expr);
                            self.set_value(&expr, val);
                            for use_ in self.g.uses_of(expr) {
                                self.ssawl_push(&use_);
                            }
                        }
                    }

                    if let Some(next_edge) = self.g.unconditional_edge(block) {
                        self.cfgwl_push(&next_edge);
                    }
                }
            } // End of cfgwl

            while let Some(e) = self.ssa_worklist.pop_front() {
                let t = if self.g.is_expr(e) {
                    let block_of = self.g.block_for(e)
                                            .unwrap_or_else(|| {
                                            radeco_err!("Value node doesn't belong to any block");
                                            self.g.invalid_action().unwrap()
                    });
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
                    for use_ in &self.g.uses_of(e) {
                        self.ssawl_push(use_);
                    }
                }
            } // End of ssawl
        } // End of while-loop
    }

    pub fn emit_ssa(&mut self) -> T {
        for (k, v) in &self.expr_val {
            if self.g.constant(*k).is_some() {
                continue;
            }
            if let LatticeValue::Const(val) = *v {
                radeco_trace!("{:?} with {:?} --> Const {:#}", 
                              k, self.g.node_data(*k), val);
                // BUG: Width may be changed just using a simple replace.
                let const_node = self.g.insert_const(val)
                                    .unwrap_or_else(|| {
                                        radeco_err!("Cannot insert new constants");
                                        self.g.invalid_value().unwrap()
                                    });
                let ndata = node_data_from_g!(self, k);
                let w = ndata.vt.width().get_width().unwrap_or(64);
                let new_node = if w == 64 {
                    const_node
                } else {
                    // val should not be larger than the k node could be.  
                    assert!(w < 64 && val < (1 << (w)));
                    let block = self.g.block_for(*k)
                                        .unwrap_or_else(|| {
                                            radeco_err!("No block information found");
                                            self.g.invalid_action().unwrap()
                                        });
                    let address = self.g.address(*k)
                                        .unwrap_or_else(|| {
                                            radeco_err!("No address information found");
                                            MAddress::invalid_address()
                                        });
                    let opcode = MOpcode::OpNarrow(w as u16);
                    let new_node = self.g.insert_op(opcode, ndata.vt, None)
                                        .unwrap_or_else(|| {
                                            radeco_err!("Cannot insert new values");
                                            self.g.invalid_value().unwrap()
                                        });
                    self.g.insert_into_block(new_node, block, address);
                    self.g.op_use(new_node, 0, const_node);
                    new_node
                };
                self.g.replace_value(*k, new_node);
            }
        }
        let blocks = self.g.blocks();
        let mut remove_edges = Vec::<T::CFEdgeRef>::new();
        let mut remove_blocks = Vec::<T::ActionRef>::new();
        for block in &blocks {
            let edges = self.g.outgoing_edges(*block);
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
            self.g.remove_data_edge(*edge);
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
        //TODO replace unwrap
        let node_data = self.g.node_data(*i).unwrap();
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
        // entry_node is always reachable.
        if *i == entry_node_err!(&self.g) {
            return true;
        }
        let incoming = self.g.incoming_edges(*i);
        for &(ref edge, _) in &incoming {
            if self.is_executable(edge) {
                return true;
            }
        }
        false
    }

    fn ssawl_push(&mut self, i: &T::ValueRef) {
        if !self.g.is_expr(*i) {
            return;
        }
        let owner_block = self.g.block_for(*i)
                                .unwrap_or_else(|| {
                                    radeco_err!("Value node doesn't belong to any block");
                                    self.g.invalid_action().unwrap()
                                });
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
