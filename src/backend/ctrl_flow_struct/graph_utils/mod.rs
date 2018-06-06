//! Various generic graph algorithms
//!
//! Anything that doesn't need to know anything about the node/edge weights of
//! a graph goes here.

pub mod ix_bit_set;
mod ncd;
#[cfg(test)]
mod tests;

pub use self::ncd::nearest_common_dominator;

use self::ix_bit_set::{IndexLike, IxBitSet};

use petgraph::graph::IndexType;
use petgraph::stable_graph::{EdgeIndex, NodeIndex, StableDiGraph};
use petgraph::visit::{
    Dfs, EdgeRef, IntoEdges, IntoNeighbors, IntoNodeIdentifiers, VisitMap, Visitable, Walker,
};

use std::iter;

pub enum DfsEvent<G>
where
    G: IntoEdges,
{
    Discover(G::NodeId),
    TreeEdge(G::EdgeRef),
    BackEdge(G::EdgeRef),
    CrossForwardEdge(G::EdgeRef),
    Finish(G::NodeId),
}

struct DfsState<G, F>
where
    G: IntoEdges + Visitable,
    F: FnMut(DfsEvent<G>) -> (),
{
    graph: G,
    discovered: G::Map,
    finished: G::Map,
    visitor: F,
}

impl<G, F> DfsState<G, F>
where
    G: IntoEdges + Visitable,
    F: FnMut(DfsEvent<G>) -> (),
{
    /// https://docs.rs/petgraph/0.4.12/src/petgraph/visit/dfsvisit.rs.html#164-189
    fn go_rec(&mut self, u: G::NodeId) -> () {
        if self.discovered.visit(u) {
            (self.visitor)(DfsEvent::Discover(u));
            for e in self.graph.edges(u) {
                let v = e.target();
                if !self.discovered.is_visited(&v) {
                    (self.visitor)(DfsEvent::TreeEdge(e));
                    self.go_rec(v);
                } else if !self.finished.is_visited(&v) {
                    (self.visitor)(DfsEvent::BackEdge(e));
                } else {
                    (self.visitor)(DfsEvent::CrossForwardEdge(e));
                }
            }
            let _first_finish = self.finished.visit(u);
            debug_assert!(_first_finish);
            (self.visitor)(DfsEvent::Finish(u));
        }
    }
}

#[allow(unused_imports)]
use petgraph;
/// Like [`petgraph::visit::depth_first_search`], but with edges.
pub fn depth_first_search<G, F>(graph: G, start: G::NodeId, visitor: F) -> ()
where
    G: IntoEdges + Visitable,
    F: FnMut(DfsEvent<G>) -> (),
{
    DfsState {
        graph,
        discovered: graph.visit_map(),
        finished: graph.visit_map(),
        visitor,
    }.go_rec(start);
}

/// Returns the union of all simple paths from `start` to a node in `end_set`,
/// including the nodes in `end_set`.
/// Also returns a topological ordering of this subgraph.
pub fn slice<G>(
    graph: G,
    start: G::NodeId,
    end_set: &IxBitSet<G::NodeId>,
) -> (IxBitSet<G::NodeId>, IxBitSet<G::EdgeId>, Vec<G::NodeId>)
where
    G: IntoEdges + Visitable,
    G::NodeId: IndexLike,
    G::EdgeId: IndexLike,
{
    let mut ret_nodes = IxBitSet::new();
    let mut ret_edges = IxBitSet::new();
    // we push nodes into this in post-order, then reverse at the end
    let mut ret_topo_order = Vec::new();

    ret_nodes.insert(start);

    let mut cur_stack = NonEmptyPath::new(start);
    depth_first_search(graph, start, |ev| {
        use self::DfsEvent::*;
        match ev {
            TreeEdge(te) => {
                cur_stack.segments.push((te.id(), te.target()));
                if end_set.contains(te.target()) {
                    // found a simple path from `start` to `end`
                    ret_nodes.extend(cur_stack.nodes());
                    ret_edges.extend(cur_stack.edges());
                }
            }
            CrossForwardEdge(cfe) => {
                debug_assert!(cur_stack.nodes().all(|n| n != cfe.target()));
                debug_assert!(cur_stack.edges().all(|e| e != cfe.id()));
                if ret_nodes.contains(cfe.target()) {
                    // found a simple path from `start` to a node already in the
                    // slice, which is on a simple path to `end`
                    ret_nodes.extend(cur_stack.nodes());
                    ret_edges.extend(cur_stack.edges());
                    ret_edges.insert(cfe.id());
                }
            }
            Finish(f) => {
                if f != start {
                    let _popped = cur_stack.segments.pop();
                    debug_assert!(_popped.unwrap().1 == f);
                    if ret_nodes.contains(f) {
                        ret_topo_order.push(f);
                    }
                } else {
                    debug_assert!(cur_stack.segments.is_empty());
                }
            }
            _ => (),
        }
    });

    ret_topo_order.push(start);

    ret_topo_order.reverse();
    (ret_nodes, ret_edges, ret_topo_order)
}

pub fn retarget_edge<N, E>(
    graph: &mut StableDiGraph<N, E>,
    edge: EdgeIndex,
    new_target: NodeIndex,
) -> EdgeIndex {
    let source = graph.edge_endpoints(edge).expect("no edge").0;
    let w = graph.remove_edge(edge).expect("no edge");
    graph.add_edge(source, new_target, w)
}

/// Returns the union of the successors of each node in `set` differenced
/// with `set`.
pub fn strict_successors_of_set<G>(graph: G, set: &IxBitSet<G::NodeId>) -> IxBitSet<G::NodeId>
where
    G: IntoNeighbors,
    G::NodeId: IndexLike,
{
    set.iter()
        .flat_map(|n| graph.neighbors(n))
        .filter(|&n| !set.contains(n))
        .collect()
}

/// Returns the set of nodes that `h` dominates.
pub fn dominated_by<G>(graph: G, entry: G::NodeId, h: G::NodeId) -> IxBitSet<G::NodeId>
where
    G: IntoNeighbors + IntoNodeIdentifiers + Visitable,
    G::NodeId: IndexLike,
{
    if entry == h {
        graph.node_identifiers().collect()
    } else {
        let mut dfs = Dfs::new(graph, entry);
        dfs.discovered.visit(h);
        let inv_dom_set: IxBitSet<_> = dfs.iter(graph).collect();
        graph
            .node_identifiers()
            .filter(|&n| !inv_dom_set.contains(n))
            .collect()
    }
}

// ideally this would be <G: IntoEdges> so we can enforce that this is actually
// a path, but Reversed doesn't implement that
pub struct NonEmptyPath<NI: Copy, EI: Copy> {
    pub start: NI,
    pub segments: Vec<(EI, NI)>,
}

impl<NI: Copy, EI: Copy> NonEmptyPath<NI, EI> {
    pub fn new(start: NI) -> Self {
        Self {
            start,
            segments: Vec::new(),
        }
    }

    pub fn last_node(&self) -> NI {
        self.segments.last().map_or(self.start, |s| s.1)
    }

    pub fn nodes(&self) -> impl Iterator<Item = NI> + '_ {
        iter::once(self.start).chain(self.segments.iter().map(|&(_, n)| n))
    }

    pub fn edges(&self) -> impl Iterator<Item = EI> + '_ {
        self.segments.iter().map(|&(e, _)| e)
    }
}

impl<Ix: IndexType> IndexLike for NodeIndex<Ix> {
    fn new(x: usize) -> Self {
        Self::new(x)
    }
    fn index(self) -> usize {
        self.index()
    }
}

impl<Ix: IndexType> IndexLike for EdgeIndex<Ix> {
    fn new(x: usize) -> Self {
        Self::new(x)
    }
    fn index(self) -> usize {
        self.index()
    }
}
