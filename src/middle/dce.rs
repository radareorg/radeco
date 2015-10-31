// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Dead code elimination

use std::collections::VecDeque;
use middle::ssa::{SSA, SSAExtra, SSAMod};
use middle::ssa::ssa_traits::NodeType;

/// Removes SSA nodes that are not used by any other node.
/// The algorithm will not consider whether the uses keeping a node alive
/// are in code that is actually executed or not. For a better analysis
/// look at `analysis::constant_propagation`.
pub fn collect<T: Clone + SSAMod + SSAExtra>(ssa: &mut T) {
    mark(ssa);
    sweep(ssa);
}

/// Marks node for removal. This method does not remove nodes
pub fn mark<T: Clone + SSAMod + SSAExtra>(ssa: &mut T) {
    let nodes = ssa.nodes();
    let exit_node = ssa.exit_node();
    let roots = ssa.registers_at(&exit_node);
    let mut queue = VecDeque::<T::ValueRef>::new();
    for node in &nodes {
        if let Ok(ref result) = ssa.get_node_data(node) {
            if let NodeType::Op(ref op) = result.nt {
                if op.has_sideeffects() {
                    ssa.mark(node);
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
        queue.extend(ssa.args_of(ni));
    }
}

/// Sweeps away the un-marked nodes
pub fn sweep<T: Clone + SSAMod + SSAExtra>(ssa: &mut T) {
    for node in &ssa.nodes() {
        if !ssa.is_marked(node) {
            ssa.remove(*node);
        }
        ssa.clear_mark(node);
    }
}
