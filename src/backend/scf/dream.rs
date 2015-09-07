// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

use petgraph::Graph;
use petgraph::graph::{NodeIndex, EdgeIndex};

use super::{AST, AST_};
use analysis::dom::DomInfo;

struct GraphSlice<T> {
	source: NodeIndex,
	sink:   NodeIndex,
	graph:  Graph<NodeIndex. EdgeIndex>
}

impl GraphSlice {
	fn from()
}

enum Marker {
	Unvisited,
	Visited,
	VisitedOnStack,
	VisitedCycle,
	VisitedOnStackCycle,
}

impl Marker {
	fn cycle(&self) -> Self { match *self {
		VisitedOnStack | VisitedOnStackCycle => true,
		_ => false
	}}
	fn visited(&self) -> Self { match *self {
		Unvisited => false,
		_ => true
	}}
	fn push(&mut self) -> bool {
		let descend = !self.visited();
		*self = match *self {
			Unvisited           => VisitedOnStack,
			Visited             => VisitedOnStack,
			VisitedOnStack      => VisitedOnStackCycle,
			VisitedCycle        => VisitedCycle,
			VisitedOnStackCycle => VisitedOnStackCycle,
		};
		descend
	}
	fn pop(&mut self) {
		*self = match *self {
			Unvisited           => panic!(),
			Visited             => panic!(),
			VisitedOnStack      => Visited,
			VisitedCycle        => VisitedCycle,
			VisitedOnStackCycle => VisitedCycle,
		};
	}
}

struct BB<T: SSA> {
	index:    T::ActionRef,
	ast:      Option<AST_<T::ActionRef, T::ValueRef>>,
	selector: Option<T::ValueRef>,
}

type CFG<T: SSA> = Graph<BB, u8>;

struct CFG2<T: SSA> {
	// TODO: better name
	cfg:        CFG<T>
	markers:    Vec<Marker>,
	post_order: Vec<NodeIndex>
}

impl CFG2<T: SSA> {
	fn build_cfg<T: SSA>(ssa: &T) -> CFG<T> {
		let cfg = Graph::new();

	}

	fn dfs<T: SSA>(cfg: &mut CFG<T>, i: NodeIndex) {
		let descend = (&mut cfg[i]).push();
		if descend {
			self.post_order.push(i);

		}
		(&mut cfg[i]).pop();
	}
}

struct Dreamer<T: SSA> {
	ssa:     &T,
	cfg:     &mut CFG<T>,
	dominfo: DomInfo,
}

impl<T: SSA> Dreamer<T> {
	pub fn new(ssa: &T) -> Self {
		let mut d = Dreamer {
			ssa:     ssa,
			cfg:     build_cfg(ssa),
			domtree: DomInfo::new(),
		};
		d.build_dom_tree(ssa, ssa.start_node())
	}

	fn acyclic_region(&mut self) {

	}

	fn cyclic_region(&mut self) {
		unimplemented!();
	}

	fn dfs(&mut self, i: NodeIndex) {

		let direction = EdgeDirection::Outgoing;
		let neighbors_iter = g.neighbors_directed(node, direction)
			.collect::<Vec<NodeIndex>>();

		for n in neighbors_iter.iter() {
			self.dfs(g, n.clone());
		}

		self.visited.push(node.clone());
		self.post_order.push(node.clone());	
	}

	pub fn run(&mut self) {

	}
}
