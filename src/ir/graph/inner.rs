use std::cmp::Eq;
use std::ops::{Add, Not};
use std::mem;

use super::super::traits::{Manipulation, NavigationInternal};

pub trait InnerIndexType : Clone + Copy + Add<Output=Self> + Not<Output=Self> + Eq + {
	// replace with zero trait once stable
	fn zero() -> Self;
	fn as_usize(self) -> usize;
	fn from_usize(usize) -> Self;
	fn invalid() -> Self { !(Self::zero()) }
}

impl InnerIndexType for i8 {
	fn zero() -> i8 { 0 }
	fn as_usize(self) -> usize { self as usize }
	fn from_usize(v: usize) -> i8 { v as i8 }
}

impl InnerIndexType for i16 {
	fn zero() -> i16 { 0 }
	fn as_usize(self) -> usize { self as usize }
	fn from_usize(v: usize) -> i16 { v as i16 }
}

impl InnerIndexType for i32 {
	fn zero() -> i32 { 0 }
	fn as_usize(self) -> usize { self as usize }
	fn from_usize(v: usize) -> i32 { v as i32 }
}

impl InnerIndexType for i64 {
	fn zero() -> i64 { 0 }
	fn as_usize(self) -> usize { self as usize }
	fn from_usize(v: usize) -> i64 { v as i64 }
}

impl InnerIndexType for isize {
	fn zero() -> isize { 0 }
	fn as_usize(self) -> usize { self as usize }
	fn from_usize(v: usize) -> isize { v as isize }
}


#[derive(Clone, Copy)]
pub struct InnerEdgeLight<I: InnerIndexType> { target: I }
#[derive(Clone, Copy)]
pub struct InnerEdgeLinked<I: InnerIndexType> { target: I, next: I, prev: I }

pub trait InnerEdgeTrait: Copy {
	type NodeAux;
	type Index;
}

impl<I: InnerIndexType> InnerEdgeTrait for InnerEdgeLight<I> {
	type NodeAux = ();
	type Index = I;
}

impl<I: InnerIndexType> InnerEdgeTrait for InnerEdgeLinked<I> {
	type NodeAux = I;
	type Index = I;
}

struct InnerNode<Node, Edge: InnerEdgeTrait> {
	data:      Node,
	aux:       Edge::NodeAux,
	edges:     [Edge; 2],
	num_edges: u8
}

pub struct InnerGraph<Node, Edge: InnerEdgeTrait> {
	nodes: Vec<InnerNode<Node, Edge>>
}

impl<Node, Edge: InnerEdgeTrait> InnerGraph<Node, Edge> {
	pub fn new() -> InnerGraph<Node, Edge> {
		InnerGraph::<Node, Edge> {
			nodes: Vec::new()
		}
	}
}

trait NPSet<Index> {
	fn next_set(&mut self, Index, Index);
	fn prev_set(&mut self, Index, Index);
}

impl<N, Index: InnerIndexType> NPSet<Index> for InnerNode<N, InnerEdgeLinked<Index>> {
	fn next_set(&mut self, target: Index, value: Index) {
		for x in 0..self.num_edges {
			let edge = &mut self.edges[x as usize];
			if edge.target == target {
				edge.next = value;
			}
		}
	}
	fn prev_set(&mut self, target: Index, value: Index) {
		for x in 0..self.num_edges {
			let edge = &mut self.edges[x as usize];
			if edge.target == target {
				edge.prev = value;
			}
		}
	}
}

impl<Index: InnerIndexType, N> Manipulation<Index, u8> for InnerGraph<N, InnerEdgeLight<Index>> {
	fn arg_mod(&mut self, i: Index, x: u8, mut target: Index) -> Index {
		mem::swap(&mut target, &mut self.nodes[i.as_usize()].edges[x as usize].target);
		return target
	}
}

impl<Index: InnerIndexType, N> Manipulation<Index, u8> for InnerGraph<N, InnerEdgeLinked<Index>> {
	fn arg_mod(&mut self, i: Index, x: u8, mut target: Index) -> Index {
		let edgecopy = self.nodes[i.as_usize()].edges[x as usize];

		if edgecopy.target != target {
			let ni = edgecopy.next;
			let pi = edgecopy.prev;

			if ni != Index::invalid() {
				self.nodes[ni.as_usize()].prev_set(edgecopy.target, pi);
			}

			if pi != Index::invalid() {
				self.nodes[pi.as_usize()].next_set(edgecopy.target, ni);
			} else {
				self.nodes[edgecopy.target.as_usize()].aux = ni;
			}

			let node = &mut self.nodes[i.as_usize()];
			let edge = &mut node.edges[x as usize];
			edge.target = target;

			//arg_ins(i, )
			for ox in 0..node.num_edges {
				// check if this node already has an edge pointing to target
			}
		}

		return target
	}
}


impl<Index: InnerIndexType, N> NavigationInternal<Index> for InnerGraph<N, InnerEdgeLight<Index>> {
	fn add_uses_to(&self, i: Index, r: &mut Vec<Index>) {
		for (n, ref node) in self.nodes.iter().enumerate() {
			for x in 0..node.num_edges {
				if node.edges[x as usize].target == i {
					r.push(Index::from_usize(n));
					break
				}
			}
		}
	}

	fn add_args_to(&self, i: Index, r: &mut Vec<Index>) {
		for node in &self.nodes {
			for x in 0..node.num_edges {
				r.push(node.edges[x as usize].target)
			}
		}
	}
}

impl<Index: InnerIndexType, N> NavigationInternal<Index> for InnerGraph<N, InnerEdgeLinked<Index>> {
	fn add_uses_to(&self, t: Index, r: &mut Vec<Index>) {
		let mut user: Index = self.nodes[t.as_usize()].aux;
		while user != Index::invalid() {
			r.push(user);

			let node = &self.nodes[user.as_usize()];

			for x in 0..node.num_edges {
				if node.edges[x as usize].target == t {
					user = node.edges[x as usize].next;
					break
				}
			}
		}
	}

	fn add_args_to(&self, t: Index, r: &mut Vec<Index>) {
		for node in &self.nodes {
			for x in 0..node.num_edges {
				r.push(node.edges[x as usize].target)
			}
		}
	}
}
