// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Dead code elimination

use petgraph::graph::NodeIndex;
use std::collections::VecDeque;

use middle::ssa::{SSAMod, SSA};
use middle::ssa::ssa_traits::NodeType;

/// Removes SSA nodes that are not used by any other node.
/// The algorithm will not consider whether the uses keeping a node alive
/// are in code that is actually executed or not. For a better analysis
/// look at `analysis::constant_propagation`.
pub fn collect<'a, T>(ssa: &mut T)
where T: Clone +
         SSAMod<ValueRef=NodeIndex, ActionRef=NodeIndex>
{
	let exit_node = ssa.exit_node();
	let roots = ssa.registers_at(&exit_node);
	if exit_node == ssa.invalid_value() { panic!(); }
	if roots == ssa.invalid_value() { panic!(); }

	let maxindex = ssa.node_count();
	let mut reachable = Vec::with_capacity(maxindex);
	let mut queue: VecDeque<NodeIndex> = VecDeque::new();
	for i in 0..maxindex {
		let reach = ssa.get_node_data(&NodeIndex::new(i))
		               .map(|nd| 
							if let NodeType::Op(op) = nd.nt {
								op.has_sideeffects()
							} else {
								false
							}).unwrap_or(true);
		reachable.push(reach);
	}

	reachable[roots.index()] = false;
	queue.extend(&[roots]);
	while let Some(ni) = queue.pop_front() {
		let i = ni.index();

		if reachable[i] {
			continue;
		}

		reachable[i] = true;
		queue.extend(ssa.args_of(ni));
	}

	for i in 0..reachable.len() {
		if !reachable[i] {
			ssa.remove(NodeIndex::new(i));
		}
	}
	ssa.cleanup();
}

