use std::collections::VecDeque;
use petgraph::graph::NodeIndex;
use middle::ssa::{NodeData, SSAMod, SSA};

pub fn collect<'a, T>(ssa: &mut T) where T:
	SSAMod<ValueRef=NodeIndex, ActionRef=NodeIndex> +
	Clone
{
	let exit_node = ssa.exit_node();
	let roots = ssa.registers_at(exit_node);
	if exit_node == ssa.invalid_value() { panic!(); }
	if roots == ssa.invalid_value() { panic!(); }

	let maxindex = ssa.node_count();
	let mut reachable = Vec::with_capacity(maxindex);
	let mut queue: VecDeque<NodeIndex> = VecDeque::new();
	for i in 0..maxindex {
		reachable.push(match ssa.get_node_data(&NodeIndex::new(i)) {
			NodeData::Op(op, _) => op.has_sideeffects(),
			NodeData::Invalid   => true,
			_                   => false,
		});
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

