use std::mem;
use std::fmt::Debug;

use super::super::traits::{Manipulation, NavigationInternal};
use super::indextype::IndexType;

#[derive(Clone, Copy, Debug)]
pub struct InnerEdgeLight<I: IndexType> { target: I }
#[derive(Clone, Copy, Debug)]
pub struct InnerEdgeLinked<I: IndexType> { target: I, next: I, prev: I }

pub trait InnerEdgeTrait: Copy + Default + Debug {
	type NodeAux: Debug;
	type Index: IndexType;
	fn default_aux() -> Self::NodeAux;
}

impl<I: IndexType> InnerEdgeTrait for InnerEdgeLight<I> {
	type NodeAux = ();
	type Index = I;
	fn default_aux() -> () {}
}

impl<I: IndexType> InnerEdgeTrait for InnerEdgeLinked<I> {
	type NodeAux = I;
	type Index = I;
	fn default_aux() -> I {I::invalid()}
}

impl<I: IndexType> Default for InnerEdgeLight<I> {
	fn default() -> Self { InnerEdgeLight::<I> { target: I::invalid() } }
}

impl<I: IndexType> Default for InnerEdgeLinked<I> {
	fn default() -> Self { InnerEdgeLinked::<I> { target: I::invalid(), next: I::invalid(), prev: I::invalid() } }
}

#[derive(Debug)]
struct InnerNode<Node, Edge: InnerEdgeTrait> {
	data:      Node,
	aux:       Edge::NodeAux,
	edges:     [Edge; 2],
	num_edges: u8
}

pub struct InnerGraph<Node: Debug, Edge: InnerEdgeTrait> {
	nodes: Vec<InnerNode<Node, Edge>>
}

impl<Node: Debug, Edge: InnerEdgeTrait> InnerGraph<Node, Edge> {
	pub fn new() -> InnerGraph<Node, Edge> {
		InnerGraph::<Node, Edge> {
			nodes: Vec::new()
		}
	}
	pub fn dump_node(&self, i: Edge::Index) {
		println!("{:?}", self.nodes[i.as_usize()]);
	}
}

pub trait TempTrait<Node, Edge: InnerEdgeTrait> {
	fn add(&mut self, Node, &[Edge::Index]) -> Edge::Index;
}

impl<Node: Debug, Edge: InnerEdgeTrait> TempTrait<Node, Edge> for InnerGraph<Node, Edge>
	where InnerGraph<Node, Edge>: Manipulation<Edge::Index, u8>
{
	fn add(&mut self, data: Node, args: &[Edge::Index]) -> Edge::Index {
		// change Edge::Index to usze here?
		let index = self.nodes.len();

		self.nodes.push(InnerNode::<Node, Edge> {
			data:  data,
			aux:   Edge::default_aux(),
			edges: [Edge::default(); 2],
			num_edges: 0
		});

		for (i, &target) in args.iter().enumerate() {
			if i == 2 { break }

			self.arg_ins(
				Edge::Index::from_usize(index),
				i as u8, target);

			self.nodes[index].num_edges = i as u8+1;
		}
		return Edge::Index::from_usize(index)
	}
}

trait NPSet<Index> {
	fn next_set(&mut self, Index, Index);
	fn prev_set(&mut self, Index, Index);
	fn similar_edge(&self, Index) -> Option<InnerEdgeLinked<Index>>;
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
	fn similar_edge(&self, target: Index) -> Option<InnerEdgeLinked<Index>> {
		for x in 0..self.num_edges {
			let edge = &self.edges[x as usize];
			if edge.target == target {
				return Option::Some(*edge)
			}
		}
		return Option::None
	}
}

impl<Index: IndexType, N: Debug> Manipulation<Index, u8> for InnerGraph<N, InnerEdgeLight<Index>> {
	fn arg_ins(&mut self, i: Index, x: u8, target: Index) {
		self.arg_mod(i, x, target);
	}
	fn arg_mod(&mut self, i: Index, x: u8, mut target: Index) -> Index {
		mem::swap(&mut target, &mut self.nodes[i.as_usize()].edges[x as usize].target);
		return target
	}
}

impl<Index: IndexType, N: Debug> Manipulation<Index, u8> for InnerGraph<N, InnerEdgeLinked<Index>> {

	fn arg_ins(&mut self, i: Index, x: u8, target: Index) {
		let next: Index;

		{
			let node = &mut self.nodes[i.as_usize()];
			if let Option::Some(linkededge) = node.similar_edge (target) {
				node.edges[x as usize] = linkededge;
				return;
			}
		}

		{
			let node = &mut self.nodes[target.as_usize()];
			next = node.aux;
			node.aux = i;
		}

		{
			let node = &mut self.nodes[i.as_usize()];
			node.edges[x as usize] = InnerEdgeLinked {
				target: target,
				next:   next,
				prev:   Index::invalid()
			};
		}


		if next != Index::invalid() {
			self.nodes[next.as_usize()].prev_set(target, i);
		}
	}

	fn arg_mod(&mut self, i: Index, x: u8, target: Index) -> Index {
		let edgecopy = self.nodes[i.as_usize()].edges[x as usize];
		let oldtarget = edgecopy.target;

		if oldtarget != target {
			let ni = edgecopy.next;
			let pi = edgecopy.prev;

			if ni != Index::invalid() {
				self.nodes[ni.as_usize()].prev_set(oldtarget, pi);
			}

			if pi != Index::invalid() {
				self.nodes[pi.as_usize()].next_set(oldtarget, ni);
			} else {
				self.nodes[oldtarget.as_usize()].aux = ni;
			}

			{
				let node = &mut self.nodes[i.as_usize()];
				let edge = &mut node.edges[x as usize];
				edge.target = target;
			}

			self.arg_ins(i, x, target);
			return oldtarget;
		}

		return target
	}
}


impl<Index: IndexType, N: Debug> NavigationInternal<Index> for InnerGraph<N, InnerEdgeLight<Index>> {
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
		let node = &self.nodes[i.as_usize()];
		for x in 0..node.num_edges {
			r.push(node.edges[x as usize].target)
		}
	}
}

impl<Index: IndexType, N: Debug> NavigationInternal<Index> for InnerGraph<N, InnerEdgeLinked<Index>> {
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
		let node = &self.nodes[t.as_usize()];
		for x in 0..node.num_edges {
			r.push(node.edges[x as usize].target)
		}
	}
}

mod test {
	use ir::traits::{Navigation, NavigationInternal};
	use super::super::indextype::IndexType;
	use super::*;

	#[test]
	fn construct_light() {
		construct::<InnerEdgeLight<i16>>();
	}

	#[test]
	fn construct_linked() {
		construct::<InnerEdgeLinked<i16>>();
	}

	#[derive(Debug)]
	enum TestInstr { Phi, NotPhi }

	fn construct<EdgeType: InnerEdgeTrait>() where
		InnerGraph<TestInstr, EdgeType>: TempTrait<TestInstr, EdgeType> + Navigation<EdgeType::Index>
	{
		let mut graph = InnerGraph::<TestInstr, EdgeType>::new();
		let i1 = graph.add(TestInstr::NotPhi, &[]);
		let i2 = graph.add(TestInstr::NotPhi, &[]);
		let i3 = graph.add(TestInstr::NotPhi, &[i1, i2]);

		graph.dump_node(i1);
		graph.dump_node(i2);
		graph.dump_node(i3);

		assert_eq!(graph.args_of(i1), &[]);
		assert_eq!(graph.args_of(i2), &[]);
		assert_eq!(graph.args_of(i3), &[i1, i2]);

		assert_eq!(graph.uses_of(i1), &[i3]);
		assert_eq!(graph.uses_of(i2), &[i3]);
		assert_eq!(graph.uses_of(i3), &[]);
	}
}
