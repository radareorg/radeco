use super::traits::NavigationInternal;

struct InnerEdgeLight<I> { target: I }
struct InnerEdgeLinked<I> { target: I, next: I, prev: I }

trait InnerEdgeTrait<Index> {
	fn target(&self) -> Index;
	type NodeAux;
}

impl<I> InnerEdgeTrait<I> for InnerEdgeLight<I> {
	fn target(&self) { self.target }
	type NodeAux = ();
}

impl<I> InnerEdgeTrait<I> for InnerEdgeLinked<I> {
	fn target(&self) { self.target }
	type NodeAux = I;
}

struct InnerGraph<Node, Edge: InnerEdgeTrait<Index>, Index> {
	nodes: Vec<(Node, Edge::NodeAux, [Edge; 2], u8)>
}

impl<Index, N> NavigationInternal<Index> for InnerGraph<N, InnerEdgeLight<Index>> {
	fn add_uses_to(&self, i: Index, r: &Vec<Index>) {
		//
	}

	fn add_args_to(&self, i: Index, r: &Vec<Index>) {
		for &(ref node, (), ref edges, num_edges) in &self.nodes {
			for x in 0..num_edges {
				r.push(edges[x].target)
			}
		}
	}
}

impl<Index, N> NavigationInternal<Index> for InnerGraph<N, InnerEdgeLinked<Index>> {
	fn add_uses_to(&self, t: Index, r: &Vec<Index>) {
		//
	}

	fn add_args_to(&self, t: Index, r: &Vec<Index>) {
		for &(ref node, (), ref edges, num_edges) in &self.nodes {
			for x in 0..num_edges {
				r.push(edges[x].target)
			}
		}
	}
}
