use std::mem;

use super::super::traits::{Manipulation, NavigationInternal};
use super::indextype::IndexType;

#[derive(Clone, Copy)]
pub struct InnerEdgeLight<I: IndexType> { target: I }
#[derive(Clone, Copy)]
pub struct InnerEdgeLinked<I: IndexType> { target: I, next: I, prev: I }

pub trait InnerEdgeTrait: Copy {
	type NodeAux;
	type Index;
}

impl<I: IndexType> InnerEdgeTrait for InnerEdgeLight<I> {
	type NodeAux = ();
	type Index = I;
}

impl<I: IndexType> InnerEdgeTrait for InnerEdgeLinked<I> {
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
	fn similar_edge(&self, Index) -> Option<&InnerEdgeLinked<Index>>;
}

impl<N, Index: IndexType> NPSet<Index> for InnerNode<N, InnerEdgeLinked<Index>> {
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
	fn similar_edge(&self, target: Index) -> Option<&InnerEdgeLinked<Index>> {
		for x in 0..self.num_edges {
			let edge = &self.edges[x as usize];
			if edge.target == target {
				return Option::Some(edge)
			}
		}
		return Option::None
	}
}

impl<Index: IndexType, N> Manipulation<Index, u8> for InnerGraph<N, InnerEdgeLight<Index>> {
	fn arg_ins(&mut self, i: Index, x: u8, mut target: Index) -> Index {
		mem::swap(&mut target, &mut self.nodes[i.as_usize()].edges[x as usize].target);
		return target
	}
	fn arg_mod(&mut self, i: Index, x: u8, target: Index) -> Index {
		self.arg_ins(i, x, target)
	}
}

impl<Index: IndexType, N> Manipulation<Index, u8> for InnerGraph<N, InnerEdgeLinked<Index>> {

	fn arg_ins(&mut self, mut i: Index, x: u8, target: Index) -> Index {
		match self.nodes[i.as_usize()].similar_edge (target) {
			Option::Some(linkededge) => {
				return i // TODO
			},
			Option::None => {}
		}

 		let node = &mut self.nodes[i.as_usize()];
		mem::swap(&mut i, &mut node.aux);
		let edge = &mut node.edges[x as usize];
		// TODO
		i
	}

	fn arg_mod(&mut self, i: Index, x: u8, target: Index) -> Index {
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

			{
				let node = &mut self.nodes[i.as_usize()];
				let edge = &mut node.edges[x as usize];
				edge.target = target;
			}

			self.arg_ins(i, x, target);
		}

		return target
	}
}


impl<Index: IndexType, N> NavigationInternal<Index> for InnerGraph<N, InnerEdgeLight<Index>> {
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

impl<Index: IndexType, N> NavigationInternal<Index> for InnerGraph<N, InnerEdgeLinked<Index>> {
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
