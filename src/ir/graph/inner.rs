use std::ops::Add;
use super::super::traits::NavigationInternal;

pub trait InnerIndexType : Clone + Copy + Add<Output=Self> {
	// replace with zero trait once stable
	fn zero() -> Self;
}
impl InnerIndexType for i8 { fn zero() -> i8 { 0 } }
impl InnerIndexType for i16 { fn zero() -> i16 { 0 } }
impl InnerIndexType for i32 { fn zero() -> i32 { 0 } }
impl InnerIndexType for i64 { fn zero() -> i64 { 0 } }
impl InnerIndexType for isize { fn zero() -> isize { 0 } }

pub struct InnerEdgeLight<I: InnerIndexType> { target: I }
pub struct InnerEdgeLinked<I: InnerIndexType> { target: I, next: I, prev: I }

pub trait InnerEdgeTrait {
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

pub struct InnerGraph<Node, Edge: InnerEdgeTrait> {
	nodes: Vec<(Node, Edge::NodeAux, [Edge; 2], u8)>
}

impl<Node, Edge: InnerEdgeTrait> InnerGraph<Node, Edge> {
	pub fn new() -> InnerGraph<Node, Edge> {
		InnerGraph::<Node, Edge> {
			nodes: Vec::new()
		}
	}
}

impl<Index: InnerIndexType, N> NavigationInternal<Index> for InnerGraph<N, InnerEdgeLight<Index>> {
	fn add_uses_to(&self, i: Index, r: &mut Vec<Index>) {
		//
	}

	fn add_args_to(&self, i: Index, r: &mut Vec<Index>) {
		for &(ref node, (), ref edges, num_edges) in &self.nodes {
			for x in 0..num_edges {
				r.push(edges[x as usize].target)
			}
		}
	}
}

impl<Index: InnerIndexType, N> NavigationInternal<Index> for InnerGraph<N, InnerEdgeLinked<Index>> {
	fn add_uses_to(&self, t: Index, r: &mut Vec<Index>) {
		//
	}

	fn add_args_to(&self, t: Index, r: &mut Vec<Index>) {
		for &(ref node, ref first_user, ref edges, num_edges) in &self.nodes {
			for x in 0..num_edges {
				r.push(edges[x as usize].target)
			}
		}
	}
}
