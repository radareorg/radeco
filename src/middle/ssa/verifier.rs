// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Implements a pass that goes over the ssa and checks if the ssa is still
//! valid.
//!
//! This is only for verification and to catch potential mistakes.
use petgraph::graph::NodeIndex;

use super::cfg_traits::CFG;
use super::ssa_traits::{SSA, SSAMod, ValueType};
use super::ssastorage::EdgeData;
use super::ssastorage::NodeData;
use super::ssa_traits::NodeData as TNodeData;
use super::ssa_traits::NodeType as TNodeType;
use super::error::SSAErr;

use super::ssastorage::SSAStorage;
use middle::ir::{MArity, MOpcode};
use std::result;
use std::fmt::Debug;

pub type VResult<T> = result::Result<(), SSAErr<T>>;

pub trait Verify: SSA + Sized + Debug {
    fn verify_block(&self, i: &Self::ActionRef) -> VResult<Self>;
    fn verify_expr(&self, i: &Self::ValueRef) -> VResult<Self>;
}

pub trait VerifiedAdd: SSAMod {
    fn verified_add_op(&mut self,
                       block: Self::ActionRef,
                       opc: MOpcode,
                       vt: ValueType,
                       args: &[Self::ValueRef],
                       addr: Option<u64>)
                       -> Self::ValueRef;
}

impl<T: Verify + SSAMod + Debug> VerifiedAdd for T {
    fn verified_add_op(&mut self,
                       block: Self::ActionRef,
                       opc: MOpcode,
                       vt: ValueType,
                       args: &[Self::ValueRef],
                       addr: Option<u64>)
                       -> Self::ValueRef {
        assert!(opc.allowed_in_ssa());
        // XXX
        let op = self.add_op(opc, vt, addr);
        for (i, arg) in args.iter().enumerate() {
            self.op_use(op, i as u8, *arg);
        }
        let q = self.verify_expr(&op);
        if let Err(ref e) = q {
            panic!(format!("{:?}", e));
        }
        op
    }
}

macro_rules! check {
	($cond: expr, $ssaerr: expr) => (
		if !$cond { return Err($ssaerr) }
	);
}

impl Verify for SSAStorage {
    fn verify_block(&self, block: &NodeIndex) -> VResult<Self> {
        let _ = self.node_count();

        let edges = self.edges_of(block);

        println!("Block {:?}", block);
        println!("Edges {:?}", edges);

        // Every BB can have a maximum of 2 Outgoing CFG Edges.
        // TODO: Relax this assumption when we have support for switch.
        check!(edges.len() < 3,
               SSAErr::WrongNumEdges(*block, 3, edges.len()));

        let mut edgecases = [false; 256];

        for edge in edges.iter() {
            let target = self.target_of(&edge.0);
            check!(self.is_action(target),
                   SSAErr::InvalidType("Block".to_owned()));
            check!(!edgecases[edge.1 as usize],
                   SSAErr::InvalidControl(*block, edge.0));
            edgecases[edge.1 as usize] = true;
        }

        for edge in edges.iter() {
            match edge.1 {
                0 | 1 => {
                    // Things to lookout for:
                    //  * There must be a minimum of two edges.
                    //  * There _must_ be a selector.
                    //  * The jump targets must not be the same block.
                    check!(edges.len() == 2,
                           SSAErr::WrongNumEdges(*block, 2, edges.len()));
                    let other_edge = match edge.1 {
                        0 => self.true_edge_of(block),
                        1 => self.false_edge_of(block),
                        _ => unreachable!(),
                    };
                    let target_1 = self.target_of(&edge.0);
                    let target_2 = self.target_of(&other_edge);
                    check!(target_1 != target_2, SSAErr::InvalidControl(*block, edge.0));
                    // No need to test the next edge.
                    break;
                }
                2 => {
                    // Things to lookout for:
                    //  * There can be only one Unconditional Edge.
                    //  * There can be no selector.
                    //  * Make sure we have not introduced an unconditional jump
                    //    which self-loops.
                    let _ = self.target_of(&edge.0);
                    check!(edges.len() == 1,
                           SSAErr::WrongNumEdges(*block, 1, edges.len()));

                    // TODO: Re-enable validity check if needed.
                    // check!(target_block.index() < node_count,
                    // SSAErr::InvalidTarget(*block, *edge, target_block));
                    // check!(*block != target_block,
                    // SSAErr::InvalidTarget(*block, *edge,
                    // target_block));
                }
                _ => panic!("Found something other than a control edge!"),
            }
        }


        let selector = self.selector_of(block);
        if edges.len() == 2 {
            check!(selector.is_some(), SSAErr::NoSelector(*block));
        } else {
            check!(selector.is_none(),
                   SSAErr::UnexpectedSelector(*block, selector.unwrap()));
        }

        // Make sure that this block is reachable.
        // TODO: Re-enable this after DCE. Make this Non-Fatal.
        // let incoming = self.incoming_edges(block);
        // check!((incoming.len() > 0) || *block == self.start_node(),
        //     SSAErr::UnreachableBlock(*block));
        Ok(())
    }

    fn verify_expr(&self, exi: &NodeIndex) -> VResult<Self> {
        println!("Node {:?} with {:?}", exi, self.get_node_data(exi));
        println!("Args: {:?}", self.get_operands(exi));
        for arg in &self.get_operands(exi) {
            println!("\targ: {:?} with {:?}", arg, self.get_node_data(arg));
        }
        if let Ok(ndata) = self.get_node_data(exi) { 
            match (ndata.nt, ndata.vt) {
                (TNodeType::Op(opcode), ValueType::Integer { width: w }) => { 
                    let extract_width = |x: TNodeData| -> u16 {
                        match x.vt {
                            ValueType::Integer { width: w } => w,
                        }
                    };

                    let opfilter = |&x: &NodeIndex| -> bool {
                        if let Some(op) = self.get_opcode(&x) {
                            match op {
                                MOpcode::OpLoad | MOpcode::OpStore => false,
                                _ => true,
                            }
                        } else {
                            true
                        }
                    };

                    let mut operands = self.get_operands(exi);
                    let op_len = operands.len();
                    let n = match opcode.arity() {
                        MArity::Zero => 0,
                        MArity::Unary => 1,
                        MArity::Binary => 2,
                        _ => unreachable!(),
                    };

                    // TODO: OpStore is unclear that it could be binary or ternary.
                    if opcode != MOpcode::OpStore &&
                        opcode != MOpcode::OpCall {
                        check!(op_len == n, SSAErr::WrongNumOperands(*exi, n, op_len));
                    }

                    // TODO: We do not consider OpStore and OpLoad's width.
                    operands.retain(&opfilter);

                    if n == 0 || operands.len() == 0 {
                        return Ok(());
                    }
                    match opcode {
                        MOpcode::OpNarrow(w0) => {
                            let opw = self.get_node_data(&operands[0]).map(&extract_width).unwrap();
                            check!(opw > w0, SSAErr::IncompatibleWidth(*exi, opw, w0));
                            check!(w == w0, SSAErr::IncompatibleWidth(*exi, w, w0));
                        }
                        MOpcode::OpWiden(w0) => {
                            let opw = self.get_node_data(&operands[0]).map(&extract_width).unwrap();
                            check!(opw < w0, SSAErr::IncompatibleWidth(*exi, opw, w0));
                            check!(w == w0, SSAErr::IncompatibleWidth(*exi, w, w0));
                        }
                        MOpcode::OpCmp |
                        MOpcode::OpGt |
                        MOpcode::OpLt => {
                            check!(w == 1, SSAErr::IncompatibleWidth(*exi, 1, w));
                        }
                        // TODO: Width of OpStore and OpLoad now is not certain.
                        MOpcode::OpCall | MOpcode::OpStore | MOpcode::OpLoad => {}
                        _ => {
                            // All operands to an expr must have the same width.
                            let w0 = self.get_node_data(&operands[0]).map(&extract_width).unwrap();
                            check!(w0 == w, SSAErr::IncompatibleWidth(*exi, w, w0));
                            for op in operands.iter() {
                                let w1 = self.get_node_data(op).map(&extract_width).unwrap();
                                check!(w == w1, SSAErr::IncompatibleWidth(*exi, w, w1));
                            }
                        }
                    }
                }
                _ => check!(false, SSAErr::InvalidExpr(*exi)),
            }
            Ok(())
        } else {
            Err(SSAErr::InvalidExpr(*exi))
        } 
    }
}

pub fn verify<T>(ssa: &T) -> VResult<T>
    where T: Verify + Debug
{
    let blocks = ssa.blocks();
    for block in &blocks {
        println!("Block {:?} is in {:#}", block, ssa.address(block).unwrap());
    }
    for block in blocks.iter() {
        // assert the qualities of the block first.
        try!(ssa.verify_block(block));
        // Iterate through each node in the block and assert their properties.
        let exprs = ssa.exprs_in(block);
        for expr in exprs.iter() {
            try!(ssa.verify_expr(expr));
        }
    }
    Ok(())
}
