use std::cell::Cell;
use std::fmt::Debug;
use std::iter::Map;
use std::mem;
use std::ops::Index;
use std::slice;

use super::super::traits::{Manipulation, Navigation, NavigationInternal};
use super::indextype::IndexType;

#[derive(Clone, Copy, Debug)]
pub struct InnerEdgeLight<I: IndexType> { target: I }
#[derive(Clone, Copy, Debug)]
pub struct InnerEdgeLinked<I: IndexType> { target: I, prev: I, next: I }

pub trait InnerEdgeTrait: Copy + Default + Debug {
	type NodeAux: Sized + Debug + 'static;
	type Index: IndexType;
	fn default_aux(i: Self::Index) -> Self::NodeAux;
	fn default_aux_usize(i: usize) -> Self::NodeAux { Self::default_aux(Self::Index::from_usize(i)) }
}

impl<I: IndexType> InnerEdgeTrait for InnerEdgeLight<I> {
	type NodeAux = ();
	type Index = I;
	#[allow(unused_variables)]
	fn default_aux(i: I) -> () {}
}

impl<I: IndexType> InnerEdgeTrait for InnerEdgeLinked<I> {
	type NodeAux = I;
	type Index = I;
	fn default_aux(i: I) -> I { i }
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

pub trait AuxQuery<Edge: InnerEdgeTrait> {
	fn access_aux<'a>(&'a mut self, i: Edge::Index) -> &'a Cell<Edge::NodeAux>;
}

pub trait ManipulationWithExternal<Edge: InnerEdgeTrait, U> {
	fn arg_ins(&mut self, Edge::Index, U, Edge::Index, &mut AuxQuery<Edge>);
	fn arg_mod(&mut self, Edge::Index, U, Edge::Index, &mut AuxQuery<Edge>) -> Edge::Index;
}

pub struct InnerGraph<Node: Debug, Edge: InnerEdgeTrait> {
	nodes: Vec<InnerNode<Node, Edge>>,
}

pub struct InnerGraphIter<'a, Node: Debug + 'a, Edge: InnerEdgeTrait + 'a> {
	iter: slice::Iter<'a, InnerNode<Node, Edge>>
}

// helper function, so we can explicitly say that T is sized
// with the current rustc the bound is not propagated properly
fn mutref_to_cell<T: Sized>(r: &mut T) -> &Cell<T> {
	unsafe { mem::transmute(r) }
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
	pub fn lookup<'a>(&'a self, i: Edge::Index) -> &'a Node {
		&self.nodes[i.as_usize()].data
	}
	pub fn access_aux<'a>(&'a mut self, i: Edge::Index, external: &'a mut AuxQuery<Edge>) -> &'a Cell<Edge::NodeAux> {
		if i >= Edge::Index::zero() {
			let node = &mut self.nodes[i.as_usize()];
			// See https://github.com/rust-lang/rfcs/issues/1106 for next line
			return mutref_to_cell(&mut node.aux)
		} else {
			return external.access_aux(i)
		}
	}
	pub fn iter<'a>(&'a self) -> InnerGraphIter<'a, Node, Edge> {
		InnerGraphIter{iter: self.nodes.iter()}
	}
}

impl<'a, Node: Debug, Edge: InnerEdgeTrait> Iterator for InnerGraphIter<'a, Node, Edge> {
	type Item = &'a Node;
	fn next(&mut self) -> Option<&'a Node> {
		self.iter.next().map(|node|&node.data)
	}
}

impl<Node: Debug, Edge: InnerEdgeTrait> Index<usize> for InnerGraph<Node, Edge> {
	type Output = Node;
	fn index<'a>(&'a self, index: usize) -> &'a Node {
		&self.nodes[index].data
	}
}

pub trait HasAdd<Node, Edge: InnerEdgeTrait> {
	fn add(&mut self, &mut AuxQuery<Edge>, Node, &[Edge::Index]) -> Edge::Index;
}

impl<Node: Debug, Edge: InnerEdgeTrait> HasAdd<Node, Edge> for InnerGraph<Node, Edge>
	where InnerGraph<Node, Edge>: ManipulationWithExternal<Edge, u8>
{
	fn add(&mut self, external: &mut AuxQuery<Edge>, data: Node, args: &[Edge::Index]) -> Edge::Index {
		// change Edge::Index to usze here?
		let index = self.nodes.len();

		self.nodes.push(InnerNode::<Node, Edge> {
			data:  data,
			aux:   Edge::default_aux(Edge::Index::from_usize(index)),
			edges: [Edge::default(); 2],
			num_edges: 0
		});

		for (i, &target) in args.iter().enumerate() {
			if i == 2 { break }

			self.arg_ins(
				Edge::Index::from_usize(index),
				i as u8, target, external);

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

impl<Index: IndexType, N: Debug>
	ManipulationWithExternal<InnerEdgeLight<Index>, u8>
	for InnerGraph<N, InnerEdgeLight<Index>>
{
	fn arg_ins(&mut self, i: Index, x: u8, target: Index, _external: &mut AuxQuery<InnerEdgeLight<Index>>) {
		self.arg_mod(i, x, target, _external);
	}
	fn arg_mod(&mut self, i: Index, x: u8, mut target: Index, _external: &mut AuxQuery<InnerEdgeLight<Index>>) -> Index {
		mem::swap(&mut target, &mut self.nodes[i.as_usize()].edges[x as usize].target);
		return target
	}
}

impl<Index: IndexType, N: Debug>
	ManipulationWithExternal<InnerEdgeLinked<Index>, u8>
	for InnerGraph<N, InnerEdgeLinked<Index>>
{
	fn arg_ins(&mut self, i: Index, x: u8, target: Index, external: &mut AuxQuery<InnerEdgeLinked<Index>>) {
		let mut next: Index;

		{
			let node = &mut self.nodes[i.as_usize()];
			if let Option::Some(linkededge) = node.similar_edge (target) {
				node.edges[x as usize] = linkededge;
				return;
			}
		}

		{
			let aux: &Cell<Index> = self.access_aux(target, external);
			next = aux.get();
			aux.set(i);
			if next == target { next = i }
		}

		{
			let node = &mut self.nodes[i.as_usize()];
			node.edges[x as usize] = InnerEdgeLinked {
				target: target,
				next:   next,
				prev:   i
			};
		}


		if next != Index::invalid() {
			self.nodes[next.as_usize()].prev_set(target, i);
		}
	}

	fn arg_mod(&mut self, i: Index, x: u8, target: Index, external: &mut AuxQuery<InnerEdgeLinked<Index>>) -> Index {
		let edgecopy = self.nodes[i.as_usize()].edges[x as usize];
		let oldtarget = edgecopy.target;

		if oldtarget != target {
			// TODO: Make Option<Index> accessors for prev/next
			let ni = edgecopy.next;
			let pi = edgecopy.prev;

			if ni != i {
				self.nodes[ni.as_usize()].prev_set(oldtarget, if pi == i {ni} else {pi});
			}

			if pi != i {
				self.nodes[pi.as_usize()].next_set(oldtarget, if ni == i {pi} else {ni});
			} else {
				self.access_aux(oldtarget, external).set(if ni == i { oldtarget } else { ni })
			}

			{
				let node = &mut self.nodes[i.as_usize()];
				let edge = &mut node.edges[x as usize];
				edge.target = target;
			}

			self.arg_ins(i, x, target, external);
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
		// TODO: let user pass 'aux' so negative indices can be handled as well
		let mut user: Index = self.nodes[t.as_usize()].aux;
		let mut prev_user: Index = t;

		//let mut expect_prev: Index = user;

		while user != prev_user {
			prev_user = user;

			r.push(user);

			let node = &self.nodes[user.as_usize()];

			for x in 0..node.num_edges {
				if node.edges[x as usize].target == t {
					// assert_eq!(node.edges[x as usize].prev, expect_prev);
					user = node.edges[x as usize].next;
					break
				}
			}

			//expect_prev = prev_user;
		}
	}

	fn add_args_to(&self, t: Index, r: &mut Vec<Index>) {
		let node = &self.nodes[t.as_usize()];
		for x in 0..node.num_edges {
			r.push(node.edges[x as usize].target)
		}
	}
}

pub trait InnerGraphWithMethods<Instruction, EdgeType: InnerEdgeTrait>:
	HasAdd<Instruction, EdgeType> +
	Navigation<EdgeType::Index> +
	ManipulationWithExternal<EdgeType, u8>  {}

impl<Instruction: Debug, EdgeType: InnerEdgeTrait> InnerGraphWithMethods<Instruction, EdgeType>
	for InnerGraph<Instruction, EdgeType>
	where InnerGraph<Instruction, EdgeType>:
	HasAdd<Instruction, EdgeType> +
	Navigation<EdgeType::Index> +
	ManipulationWithExternal<EdgeType, u8> {}

#[cfg(test)]
mod test {
	use std::collections::HashSet;
	use std::cell::Cell;
	use ir::traits::{Manipulation, Navigation};
	use super::super::indextype::IndexType;
	use super::mutref_to_cell;
	use super::*;

	#[derive(Debug)]
	enum TestInstr { Phi, NotPhi }

	struct TestAuxQuery<Edge: InnerEdgeTrait>(Vec<Edge::NodeAux>);

	impl<Edge: InnerEdgeTrait> AuxQuery<Edge> for TestAuxQuery<Edge> {
		fn access_aux<'a>(&'a mut self, i: Edge::Index) -> &'a Cell<Edge::NodeAux> {
			mutref_to_cell(&mut self.0[!i.as_usize()])
		}
	}

	fn construct<EdgeType: InnerEdgeTrait>() where
		InnerGraph<TestInstr, EdgeType>:
			HasAdd<TestInstr, EdgeType> +
			Navigation<EdgeType::Index> +
			ManipulationWithExternal<EdgeType, u8>
	{
		let mut external = TestAuxQuery::<EdgeType>(Vec::new());
		let mut graph = InnerGraph::<TestInstr, EdgeType>::new();

		let i0 = graph.add(&mut external, TestInstr::NotPhi, &[]);
		let i1 = graph.add(&mut external, TestInstr::NotPhi, &[]);
		let i2 = graph.add(&mut external, TestInstr::NotPhi, &[i0, i1]);

		println!("");
		graph.dump_node(i0);
		graph.dump_node(i1);
		graph.dump_node(i2);

		assert_eq!(graph.args_of(i0), &[]);
		assert_eq!(graph.args_of(i1), &[]);
		assert_eq!(graph.args_of(i2), &[i0, i1]);

		assert_eq!(graph.uses_of(i0), &[i2]);
		assert_eq!(graph.uses_of(i1), &[i2]);
		assert_eq!(graph.uses_of(i2), &[]);

		let i3 = graph.add(&mut external, TestInstr::NotPhi, &[i1, i2]);
		let i4 = graph.add(&mut external, TestInstr::NotPhi, &[i1, i3]);

		println!("");
		graph.dump_node(i0);
		graph.dump_node(i1);
		graph.dump_node(i2);
		graph.dump_node(i3);
		graph.dump_node(i4);

		assert_eq!(graph.args_of(i0), &[]);
		assert_eq!(graph.args_of(i1), &[]);
		assert_eq!(graph.args_of(i2), &[i0, i1]);
		assert_eq!(graph.args_of(i3), &[i1, i2]);
		assert_eq!(graph.args_of(i4), &[i1, i3]);

		assert_eq!(graph.uses_of(i0), &[i2]);
		assert_eq!(
			graph.uses_of(i1). iter().collect::<HashSet<_>>(),
			[i2, i3, i4].      iter().collect::<HashSet<_>>()
		);
		assert_eq!(graph.uses_of(i2), &[i3]);
		assert_eq!(graph.uses_of(i3), &[i4]);
		assert_eq!(graph.uses_of(i4), &[]);

		graph.arg_mod(i3, 1, i1, &mut external);

		assert_eq!(
			graph.uses_of(i1). iter().collect::<HashSet<_>>(),
			[i2, i3, i4].      iter().collect::<HashSet<_>>()
		);
		assert_eq!(graph.uses_of(i2), &[]);
		assert_eq!(graph.args_of(i3), &[i1, i1]);
	}


	fn external<EdgeType: InnerEdgeTrait>() where
		InnerGraph<TestInstr, EdgeType>:
			HasAdd<TestInstr, EdgeType> +
			Navigation<EdgeType::Index> +
			ManipulationWithExternal<EdgeType, u8>
	{
		let mut external = TestAuxQuery::<EdgeType>(Vec::new());
		let mut graph = InnerGraph::<TestInstr, EdgeType>::new();

		external.0.push(EdgeType::default_aux_usize(!0));
		external.0.push(EdgeType::default_aux_usize(!1));

		let m1 = EdgeType::Index::from_usize(!1);
		let m0 = EdgeType::Index::from_usize(!0);

		let i0 = graph.add(&mut external, TestInstr::NotPhi, &[m0]);
		let i1 = graph.add(&mut external, TestInstr::NotPhi, &[m1, m0]);
		let i2 = graph.add(&mut external, TestInstr::NotPhi, &[i0, m1]);
		let i3 = graph.add(&mut external, TestInstr::NotPhi, &[m0, m0]);

		assert_eq!(graph.args_of(i0), &[m0]);
		assert_eq!(graph.args_of(i1), &[m1, m0]);
		assert_eq!(graph.args_of(i2), &[i0, m1]);
		assert_eq!(graph.args_of(i3), &[m0, m0]);

		// TODO
		//assert_eq!(graph.uses_of(!1), &[i1, i2]);
		//assert_eq!(graph.uses_of(!0), &[i0, i2, i3]);
		assert_eq!(graph.uses_of(i0), &[i2]);
		assert_eq!(graph.uses_of(i1), &[]);
		assert_eq!(graph.uses_of(i2), &[]);
		assert_eq!(graph.uses_of(i3), &[]);

		println!("");
		println!("{:?} {:?}", external.0[0], external.0[1]);
		graph.dump_node(i0);
		graph.dump_node(i1);
		graph.dump_node(i2);
		graph.dump_node(i3);
	}

	#[test]
	fn construct_light() {
		construct::<InnerEdgeLight<i16>>();
	}

	#[test]
	fn construct_linked() {
		construct::<InnerEdgeLinked<i16>>();
	}

	#[test]
	fn external_light() {
		external::<InnerEdgeLight<i16>>();
	}

	#[test]
	fn external_linked() {
		external::<InnerEdgeLinked<i16>>();
	}

}
