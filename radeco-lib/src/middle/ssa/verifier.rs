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
use std::collections::{HashMap, VecDeque};
use std::fmt::Debug;
use std::result;

use super::cfg_traits::CFG;
use super::error::SSAErr;
use super::graph_traits::Graph;
use super::ssa_traits::NodeType as TNodeType;
use super::ssa_traits::SSA;
use super::ssastorage::SSAStorage;

use crate::middle::ir::{MArity, MOpcode};

pub type VResult<T> = result::Result<(), SSAErr<T>>;

pub trait Verify: SSA + Sized + Debug {
    fn verify_block(&self, i: &Self::ActionRef) -> VResult<Self>;
    fn verify_expr(&self, i: &Self::ValueRef) -> VResult<Self>;
    fn verify_SCC(
        &self,
        _: &Self::ValueRef,
        _: &mut u64,
        _: &mut HashMap<NodeIndex, u64>,
        _: &mut HashMap<NodeIndex, u64>,
        _: &mut VecDeque<NodeIndex>,
    ) -> VResult<Self>;
}

// TODO: Implement verified_add_op and use it in construction process.
//pub trait VerifiedAdd: SSAMod {
//    fn verified_add_op(&mut self,
//                       block: Self::ActionRef,
//                       opc: MOpcode,
//                       vt: ValueType,
//                       args: &[Self::ValueRef],
//                       addr: Option<u64>)
//                       -> Self::ValueRef;
//}
//
//impl<T: Verify + SSAMod + Debug> VerifiedAdd for T {
//    fn verified_add_op(&mut self,
//                       block: Self::ActionRef,
//                       opc: MOpcode,
//                       vt: ValueType,
//                       args: &[Self::ValueRef],
//                       addr: Option<u64>)
//                       -> Self::ValueRef {
//        assert!(opc.allowed_in_ssa());
//        // XXX
//        let op = self.add_op(opc, vt, addr);
//        for (i, arg) in args.iter().enumerate() {
//            self.op_use(op, i as u8, *arg);
//        }
//        let q = self.verify_expr(&op);
//        if let Err(ref e) = q {
//            panic!(format!("{:?}", e));
//        }
//        op
//    }
//}

macro_rules! check {
    ($cond: expr, $ssaerr: expr) => {
        if !$cond {
            return Err($ssaerr);
        }
    };
}

impl Verify for SSAStorage {
    fn verify_block(&self, block: &NodeIndex) -> VResult<Self> {
        let _ = self.nodes_count();

        let edges = self.outgoing_edges(*block);

        radeco_trace!("ssa verify|Block {:?}", block);
        radeco_trace!("ssa verify|Edges {:?}", edges);

        // Every BB can have a maximum of 2 Outgoing CFG Edges.
        // TODO: Relax this assumption when we have support for switch.
        check!(
            edges.len() < 3,
            SSAErr::WrongNumEdges(*block, 3, edges.len())
        );

        let mut edgecases = [false; 256];

        for edge in edges.iter() {
            let target = self.edge_info(edge.0).expect("Less-endpoints edge").target;
            check!(
                self.is_action(target),
                SSAErr::InvalidType("Block".to_owned())
            );
            check!(
                !edgecases[edge.1 as usize],
                SSAErr::InvalidControl(*block, edge.0)
            );
            edgecases[edge.1 as usize] = true;
        }

        for edge in edges.iter() {
            match edge.1 {
                0 | 1 => {
                    // Things to lookout for:
                    //  * There must be a minimum of two edges.
                    //  * There _must_ be a selector.
                    //  * The jump targets must not be the same block.
                    check!(
                        edges.len() == 2,
                        SSAErr::WrongNumEdges(*block, 2, edges.len())
                    );
                    let branches = self
                        .conditional_edges(*block)
                        .expect("No conditonal edges is found");
                    let other_edge = match edge.1 {
                        0 => branches.true_side,
                        1 => branches.false_side,
                        _ => unreachable!(),
                    };
                    let target_1 = self.edge_info(edge.0).expect("Less-endpoints edge").target;
                    let target_2 = self
                        .edge_info(other_edge)
                        .expect("Less-endpoints edge")
                        .target;
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
                    let _ = self.edge_info(edge.0).expect("Less-endpoints edge").target;
                    check!(
                        edges.len() == 1,
                        SSAErr::WrongNumEdges(*block, 1, edges.len())
                    );

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

        let selector = self.selector_in(*block);
        if edges.len() == 2 {
            check!(selector.is_some(), SSAErr::NoSelector(*block));
        } else {
            //check!(selector.is_none(),
            //SSAErr::UnexpectedSelector(*block, selector.unwrap()));
        }

        // Make sure that this block is reachable.
        // TODO: Re-enable this after DCE. Make this Non-Fatal.
        // let incoming = self.incoming_edges(block);
        // check!((incoming.len() > 0) || *block == self.start_node(),
        //     SSAErr::UnreachableBlock(*block));
        Ok(())
    }

    fn verify_expr(&self, exi: &NodeIndex) -> VResult<Self> {
        radeco_trace!("ssa verify|Node {:?} with {:?}", exi, self.node_data(*exi));
        radeco_trace!("ssa verify|Args: {:?}", self.operands_of(*exi));
        for _arg in &self.operands_of(*exi) {
            radeco_trace!(
                "ssa verify|\targ: {:?} with {:?}",
                _arg,
                self.node_data(*_arg)
            );
        }
        if let Ok(ndata) = self.node_data(*exi) {
            match (ndata.nt, ndata.vt) {
                (TNodeType::Op(opcode), vi) => {
                    let w = vi.width().get_width().unwrap_or(64);

                    let opfilter = |&x: &NodeIndex| -> bool {
                        if let Some(op) = self.opcode(x) {
                            match op {
                                MOpcode::OpLoad | MOpcode::OpStore => false,
                                _ => true,
                            }
                        } else {
                            true
                        }
                    };

                    let mut operands = self.operands_of(*exi);
                    let op_len = operands.len();
                    let n = match opcode.arity() {
                        MArity::Zero => 0,
                        MArity::Unary => 1,
                        MArity::Binary => 2,
                        MArity::Ternary => 3,
                    };

                    if opcode != MOpcode::OpCall
                        && opcode.idx() != MOpcode::OpCustom(String::new()).idx()
                    {
                        check!(op_len == n, SSAErr::WrongNumOperands(*exi, n, op_len));
                    }

                    // TODO: We do not consider OpStore and OpLoad's width now.
                    operands.retain(&opfilter);

                    if n == 0 || operands.len() == 0 {
                        return Ok(());
                    }
                    match opcode {
                        MOpcode::OpNarrow(w0) => {
                            let opw = self
                                .node_data(operands[0])
                                .map(|vi| vi.vt.width().get_width().unwrap_or(64))
                                .unwrap();
                            check!(opw > w0, SSAErr::IncompatibleWidth(*exi, opw, w0));
                            check!(w == w0, SSAErr::IncompatibleWidth(*exi, w, w0));
                        }
                        MOpcode::OpZeroExt(w0) | MOpcode::OpSignExt(w0) => {
                            let opw = self
                                .node_data(operands[0])
                                .map(|vi| vi.vt.width().get_width().unwrap_or(64))
                                .unwrap();
                            check!(opw < w0, SSAErr::IncompatibleWidth(*exi, opw, w0));
                            check!(w == w0, SSAErr::IncompatibleWidth(*exi, w, w0));
                        }
                        MOpcode::OpEq | MOpcode::OpGt | MOpcode::OpLt => {
                            check!(w == 1, SSAErr::IncompatibleWidth(*exi, 1, w));
                        }
                        // TODO: Width of OpStore and OpLoad now is not certain.
                        MOpcode::OpCall | MOpcode::OpStore | MOpcode::OpLoad => {}
                        _ => {
                            // All operands to an expr must have the same width.
                            let w0 = self
                                .node_data(operands[0])
                                .map(|vi| vi.vt.width().get_width().unwrap_or(64))
                                .unwrap();
                            check!(w0 == w, SSAErr::IncompatibleWidth(*exi, w, w0));
                            for op in operands.iter() {
                                let w1 = self
                                    .node_data(*op)
                                    .map(|vi| vi.vt.width().get_width().unwrap_or(64))
                                    .unwrap();
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

    // Use tarjan algorithm to calculate SCC in SSA.
    // It will find following wrong situation:
    //      * Opcode node use itself
    //      * Opcode node has a back edge use
    //      * Phi SCC is not reachable
    fn verify_SCC(
        &self,
        exi: &Self::ValueRef,
        timestamp: &mut u64,
        DFN: &mut HashMap<NodeIndex, u64>,
        LOW: &mut HashMap<NodeIndex, u64>,
        stack: &mut VecDeque<NodeIndex>,
    ) -> VResult<Self> {
        // Handle exi itself.
        DFN.insert(*exi, *timestamp);
        LOW.insert(*exi, *timestamp);
        *timestamp += 1;
        stack.push_back(*exi);

        // DFS with its operands.
        let operands = self.operands_of(*exi);
        for op in &operands {
            check!(op != exi || self.is_phi(*exi), SSAErr::BackUse(*exi, *op));
            let low_exi = LOW.get(exi).cloned().unwrap();
            if !DFN.contains_key(op) {
                self.verify_SCC(op, timestamp, DFN, LOW, stack)?;
                let low_op = LOW.get(op).cloned().unwrap();
                if low_op < low_exi {
                    LOW.insert(*exi, low_op);
                } else if stack.contains(op) {
                    radeco_trace!(
                        "Verify_SCC_BackUse| {:?} with {:?} --> {:?} with {:?}",
                        exi,
                        self.node_data(*exi),
                        op,
                        self.node_data(*op)
                    );
                    check!(self.is_phi(*exi), SSAErr::BackUse(*exi, *op));
                    let dfn_op = DFN.get(op).cloned().unwrap();
                    if dfn_op < low_exi {
                        LOW.insert(*exi, dfn_op);
                    }
                }
            }
        }

        let mut SCC: Vec<Self::ValueRef> = Vec::new();

        // Why no do-while loop in Rust, wanna cry
        let dfn = DFN.get(exi).unwrap();
        let low = LOW.get(exi).unwrap();
        radeco_trace!("Verify_SCC| {:?} | DFN: {} | LOW: {}", exi, dfn, low);
        if dfn == low {
            while let Some(node) = stack.pop_back() {
                SCC.push(node);
                if *exi == node {
                    break;
                }
            }
            let mut reachable = false;
            // That will be caused by phi nodes.
            for node in &SCC {
                let operands = self.operands_of(*node);
                radeco_trace!(
                    "Verify_SCC_PhiSCC|{:?} with {:?}",
                    node,
                    self.node_data(*node)
                );
                radeco_trace!("Verify_SCC_PhiScc|\tOperands: {:?}", operands);
                for op in &operands {
                    if !SCC.contains(op) {
                        reachable = true;
                        break;
                    }
                }
                if reachable {
                    break;
                }
            }
            // if it's a multi-node SCC, it must be reachable, or it's just a node.
            check!(reachable || SCC.len() == 1, SSAErr::UnreachablePhiSCC(SCC));
        }

        Ok(())
    }
}

pub fn verify<T>(ssa: &T) -> VResult<T>
where
    T: Verify + Debug,
{
    let blocks = ssa.blocks();
    for block in blocks.iter() {
        // assert the qualities of the block first.
        ssa.verify_block(block)?;
        // Iterate through each node in the block and assert their properties.
        let exprs = ssa.exprs_in(*block);
        for expr in exprs.iter() {
            ssa.verify_expr(expr)?;
        }
    }

    let mut DFN: HashMap<NodeIndex, u64> = HashMap::new();
    let mut LOW: HashMap<NodeIndex, u64> = HashMap::new();
    let mut stack: VecDeque<NodeIndex> = VecDeque::new();
    let mut timestamp: u64 = 0;

    // Find the start point. RegisterState for exit_node could be the best choose.
    // Because it could reach all the nodes in SSA.
    let exi = exit_node_err!(ssa);
    let register = ssa.registers_in(exi);
    if register.is_none() {
        return Err(SSAErr::Other("No register state node found"));
    }
    let register = register.unwrap();
    ssa.verify_SCC(&register, &mut timestamp, &mut DFN, &mut LOW, &mut stack)?;
    Ok(())
}
