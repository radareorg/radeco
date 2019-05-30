//! Various generic graph algorithms
//!
//! Anything that doesn't need to know anything about the node/edge weights of
//! a graph goes here.

pub mod ix_bit_set;
mod ncd;
#[cfg(test)]
mod test;

pub use self::ncd::nearest_common_dominator;

use self::ix_bit_set::{IndexLike, IxBitSet};

use petgraph::graph::IndexType;
use petgraph::prelude::*;
use petgraph::visit::{
    IntoEdges, IntoEdgesDirected, IntoNeighbors, IntoNeighborsDirected, IntoNodeIdentifiers,
    VisitMap, Visitable, Walker,
};

use std::collections::HashMap;
use std::hash::Hash;
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
    }
    .go_rec(start);
}

/// see [`slice`]
pub struct GraphSlice<N: IndexLike, E: IndexLike> {
    pub nodes: IxBitSet<N>,
    pub edges: IxBitSet<E>,
    pub topo_order: Vec<N>,
}

/// Returns the union of all simple paths from `start` to a node in `end_set`,
/// including the nodes in `end_set`.
/// Also returns a topological ordering of this subgraph.
pub fn slice<G>(
    graph: G,
    start: G::NodeId,
    end_set: &IxBitSet<G::NodeId>,
) -> GraphSlice<G::NodeId, G::EdgeId>
where
    G: IntoEdges + Visitable,
    G::NodeId: IndexLike,
    G::EdgeId: IndexLike,
{
    // we push nodes into `topo_order` in post-order, then reverse at the end
    let mut ret = GraphSlice {
        nodes: IxBitSet::new(),
        edges: IxBitSet::new(),
        topo_order: Vec::new(),
    };

    ret.nodes.insert(start);

    let mut cur_stack = NonEmptyPath::new(start);
    depth_first_search(graph, start, |ev| {
        use self::DfsEvent::*;
        match ev {
            TreeEdge(te) => {
                cur_stack.segments.push((te.id(), te.target()));
                if end_set.contains(te.target()) {
                    // found a simple path from `start` to `end`
                    ret.nodes.extend(cur_stack.nodes());
                    ret.edges.extend(cur_stack.edges());
                }
            }
            CrossForwardEdge(cfe) => {
                debug_assert!(cur_stack.nodes().all(|n| n != cfe.target()));
                debug_assert!(cur_stack.edges().all(|e| e != cfe.id()));
                if ret.nodes.contains(cfe.target()) {
                    // found a simple path from `start` to a node already in the
                    // slice, which is on a simple path to `end`
                    ret.nodes.extend(cur_stack.nodes());
                    ret.edges.extend(cur_stack.edges());
                    ret.edges.insert(cfe.id());
                }
            }
            Finish(f) => {
                if f != start {
                    let _popped = cur_stack.segments.pop();
                    debug_assert!(_popped.unwrap().1 == f);
                    if ret.nodes.contains(f) {
                        ret.topo_order.push(f);
                    }
                } else {
                    debug_assert!(cur_stack.segments.is_empty());
                }
            }
            _ => (),
        }
    });

    ret.topo_order.push(start);

    ret.topo_order.reverse();
    ret
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

/// Computes the transitive closure of the given directed acyclic graph.
/// Returns, for each node, the set of nodes reachable from that node.
pub fn dag_transitive_closure<G>(graph: G) -> HashMap<G::NodeId, IxBitSet<G::NodeId>>
where
    G: IntoNeighbors + IntoNodeIdentifiers + Visitable,
    G::NodeId: IndexLike + Hash + Eq,
{
    // https://algowiki-project.org/en/Purdom%27s_algorithm#Implementation_scheme_of_the_serial_algorithm
    // Stage 3: transitive closure of directed acyclic graph

    let mut ret = HashMap::new();

    let mut dfspo = DfsPostOrder::empty(graph).iter(graph);
    for n in graph.node_identifiers() {
        if !dfspo.inner_ref().discovered.is_visited(&n) {
            dfspo.inner_mut().move_to(n);
            for v in &mut dfspo {
                let mut ret_v = IxBitSet::new();
                ret_v.insert(v);
                for w in graph.neighbors(v) {
                    ret_v.union_with(&ret[&w]);
                }
                ret.insert(v, ret_v);
            }
        }
    }

    ret
}

/// Contracts the nodes in `nodes` into a single node, maintaining all internal
/// and external edges. Node and edge weights can be changed by `node_map` and
/// `edge_map` before they are inserted into the graph given to `contract`.
pub fn contract_nodes_and_map<N, N2, E, E2, FN, FE, FC>(
    graph: &mut StableDiGraph<N, E>,
    nodes: &IxBitSet<NodeIndex>,
    mut node_map: FN,
    mut edge_map: FE,
    contract: FC,
) -> NodeIndex
where
    FN: FnMut(NodeIndex, N) -> N2,
    FE: FnMut(EdgeIndex, E) -> E2,
    FC: FnOnce(StableDiGraph<N2, E2>) -> N,
{
    debug_assert!(!nodes.is_empty());

    let mut preds = Vec::new();
    let mut succs = Vec::new();
    let mut internal_edges = Vec::new();

    // remove and store every edge incident to every node
    for n in nodes {
        {
            let mut neighbors = graph.neighbors_directed(n, Outgoing).detach();
            while let Some((edge, succ)) = neighbors.next(graph) {
                let weight = graph.remove_edge(edge).unwrap();
                if nodes.contains(succ) {
                    internal_edges.push((n, succ, edge_map(edge, weight)));
                } else {
                    succs.push((succ, weight));
                }
            }
        }
        {
            let mut neighbors = graph.neighbors_directed(n, Incoming).detach();
            while let Some((edge, pred)) = neighbors.next(graph) {
                let weight = graph.remove_edge(edge).unwrap();
                if nodes.contains(pred) {
                    internal_edges.push((pred, n, edge_map(edge, weight)));
                } else {
                    preds.push((pred, weight));
                }
            }
        }
    }

    let mut old_new_map = HashMap::with_capacity(nodes.len());
    let mut subgraph = StableDiGraph::with_capacity(nodes.len(), internal_edges.len());

    // move nodes
    for old_node in nodes {
        debug_assert!(graph.neighbors_undirected(old_node).next().is_none());
        let weight = graph.remove_node(old_node).unwrap();
        let new_node = subgraph.add_node(node_map(old_node, weight));
        old_new_map.insert(old_node, new_node);
    }

    // restore internal edges
    for (src, dst, weight) in internal_edges {
        subgraph.add_edge(old_new_map[&src], old_new_map[&dst], weight);
    }

    let contracted = graph.add_node(contract(subgraph));

    // restore external edges
    for (pred, weight) in preds {
        graph.add_edge(pred, contracted, weight);
    }
    for (succ, weight) in succs {
        graph.add_edge(contracted, succ, weight);
    }

    contracted
}

/// Returns if `node` has no incoming edges.
pub fn is_source<G: IntoNeighborsDirected>(graph: G, node: G::NodeId) -> bool {
    graph.neighbors_directed(node, Incoming).next().is_none()
}

/// Returns if `node` has no outgoing edges.
pub fn is_sink<G: IntoNeighborsDirected>(graph: G, node: G::NodeId) -> bool {
    graph.neighbors_directed(node, Outgoing).next().is_none()
}

pub fn edges_from_region_to_node<'a, G>(
    graph: G,
    src_region: &'a IxBitSet<G::NodeId>,
    tgt_node: G::NodeId,
) -> impl Iterator<Item = G::EdgeId> + 'a
where
    G: IntoEdgesDirected + 'a,
    G::NodeId: IndexLike,
{
    // `move` required b/c https://github.com/rust-lang/rust/issues/36569
    graph
        .edges_directed(tgt_node, Incoming)
        .filter(move |e| src_region.contains(e.source()))
        .map(|e| e.id())
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
