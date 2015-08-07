use std::collections::VecDeque;
use petgraph::graph::NodeIndex;
use middle::ssa::{NodeData, SSAMod, SSA};

pub struct DCE<'a, T: 'a + SSA + SSAMod<ValueRef=NodeIndex, ActionRef=NodeIndex> + Clone> {
	ssa: &'a mut T,
	roots: T::ValueRef,
}

impl<'a, T> DCE<'a, T>
where T: SSA +
SSAMod<ValueRef=NodeIndex, ActionRef=NodeIndex> +
Clone {
	
	pub fn new(ssa: &'a mut T) -> DCE<'a, T> {
		let exit_node = ssa.exit_node();
		let roots = ssa.registers_at(exit_node);
		DCE {
			ssa: ssa,
			roots: roots.clone(),
		}
	}

	pub fn collect(&mut self) {
		let maxindex = self.ssa.node_count();
		let mut reachable = Vec::with_capacity(maxindex);
		let mut queue: VecDeque<NodeIndex> = VecDeque::new();
		for i in 0..maxindex {
			reachable.push(match self.ssa.get_node_data(&NodeIndex::new(i)) {
				NodeData::Op(_, _)      => false,
				NodeData::Comment(_)    => true,
				NodeData::Const(_)      => false,
				NodeData::Phi(_)        => false,
				NodeData::Undefined     => false,
				NodeData::Removed       => true,
				NodeData::BasicBlock(_) => true,
				NodeData::RegisterState => true,
			});
		}
		reachable[self.roots.index()] = false;
		queue.extend(&[self.roots]);
		while let Some(ni) = queue.pop_front() {
			let i = ni.index();

			if reachable[i] {
				continue;
			}

			reachable[i] = true;
			queue.extend(self.ssa.args_of(ni));
		}
		for i in 0..reachable.len() {
			if !reachable[i] {
				self.ssa.remove(NodeIndex::new(i));
			}
		}
		self.ssa.cleanup();
	}
}


