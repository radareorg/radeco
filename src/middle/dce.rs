// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Dead code elimination

use middle::ssa::graph_traits::Graph;
use middle::ssa::ssa_traits::{SSAExtra, SSAMod};
use middle::ssa::ssa_traits::NodeType;
use petgraph::graph::NodeIndex;
use std::collections::VecDeque;

/// Removes SSA nodes that are not used by any other node.
/// The algorithm will not consider whether the uses keeping a node alive
/// are in code that is actually executed or not. For a better analysis
/// look at `analysis::constant_propagation`.
pub fn collect<T>(ssa: &mut T)
    where T: Clone + SSAExtra +
        SSAMod<ActionRef=<T as Graph>::GraphNodeRef, CFEdgeRef=<T as Graph>::GraphEdgeRef>
{
    mark(ssa);
    sweep(ssa);
}

/// Marks node for removal. This method does not remove nodes.
pub fn mark<T>(ssa: &mut T)
    where T: Clone + SSAExtra +
        SSAMod<ActionRef=<T as Graph>::GraphNodeRef, CFEdgeRef=<T as Graph>::GraphEdgeRef>
{
    let nodes = ssa.values();
    let exit_node = ssa.exit_node().expect("Incomplete CFG graph");
    let roots = ssa.registers_in(exit_node).expect("No register state node found");
    let mut queue = VecDeque::<T::ValueRef>::new();
    for node in &nodes {
        if let Ok(ref result) = ssa.node_data(*node) {
            if let NodeType::Op(ref op) = result.nt {
                if op.has_sideeffects() || ssa.is_selector(*node) {
                    queue.push_back(*node);
                }
            }
        } else {
            ssa.mark(node);
        }
    }
    ssa.clear_mark(&roots);
    queue.extend(&[roots]);
    while let Some(ni) = queue.pop_front() {
        if ssa.is_marked(&ni) {
            continue;
        }
        ssa.mark(&ni);
        queue.extend(ssa.operands_of(ni));
    }
}

/// Sweeps away the un-marked nodes
pub fn sweep<T>(ssa: &mut T)
    where T: Clone + SSAExtra +
        SSAMod<ActionRef=<T as Graph>::GraphNodeRef, CFEdgeRef=<T as Graph>::GraphEdgeRef>
{
    for node in &ssa.values() {
        if !ssa.is_marked(node) {
            ssa.remove_value(*node);
        }
        ssa.clear_mark(node);
    }

    // Remove empty blocks and redirect control flows.
    let blocks = ssa.blocks();
    for block in &blocks {
        // Do not touch start or exit nodes
        if *block == ssa.entry_node().expect("Incomplete CFG graph") ||
           *block == ssa.exit_node().expect("Incomplete CFG graph") {
            continue;
        }

        let remove_block = if ssa.exprs_in(*block).is_empty() && ssa.phis_in(*block).is_empty() {
            let incoming = ssa.incoming_edges(*block);
            let outgoing = ssa.outgoing_edges(*block);
            // Two cases.
            if outgoing.len() == 1 {
                let new_target = ssa.edge_info(outgoing[0].0).expect("Less-endpoints edge").target;
                for &(ie, ref i) in &incoming {
                    let new_src = ssa.edge_info(ie).expect("Less-endpoints edge").source;
                    ssa.remove_control_edge(ie);
                    ssa.insert_control_edge(new_src, new_target, *i);
                }
                ssa.remove_control_edge(outgoing[0].0);
                true
            } else {
                // Incoming edge has to be an unconditional,
                // else we cannot re-route.
                // TODO: This is currently unimplemented!
                false
            }
        } else {
            false
        };
        if remove_block {
            radeco_trace!("dce_rm_empty|{:?}", block);
            ssa.remove_block(*block);
        }
    }
}
