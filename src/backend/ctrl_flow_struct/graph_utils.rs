use fixedbitset::FixedBitSet;
use petgraph::graph::GraphIndex;
use petgraph::visit::{
    EdgeRef, FilterNode, GraphBase, IntoEdges, NodeIndexable, VisitMap, Visitable,
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
            let first_finish = self.finished.visit(u);
            debug_assert!(first_finish);
            (self.visitor)(DfsEvent::Finish(u));
        }
    }
}

/// Like [`petgraph::visit::depth_first_search`], but with edges.
pub fn depth_first_search<G, F>(graph: G, start: G::NodeId, mut visitor: F) -> ()
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
pub fn slice<G, F>(
    graph: G,
    start: G::NodeId,
    end_set: F,
) -> (FixedBitSet, FixedBitSet, Vec<G::NodeId>)
where
    G: IntoEdges + Visitable + NodeIndexable,
    G::NodeId: GraphIndex,
    G::EdgeId: GraphIndex,
    F: FilterNode<G::NodeId>,
{
    let mut ret_nodes = FixedBitSet::with_capacity(graph.node_bound());
    // petgraph doesn't expose `edge_bound` :(
    let mut ret_edges = FixedBitSet::with_capacity(0);
    // we push nodes into this in post-order, then reverse at the end
    let mut ret_topo_order = Vec::new();

    ret_nodes.put(start.index());

    let mut cur_node_stack = vec![start];
    let mut cur_edge_stack = Vec::new();
    depth_first_search(graph, start, |ev| {
        use self::DfsEvent::*;
        match ev {
            TreeEdge(te) => {
                cur_edge_stack.push(te.id());
                cur_node_stack.push(te.target());
                if end_set.include_node(te.target()) {
                    // found a simple path from `start` to `end`
                    ret_nodes.extend(cur_node_stack.iter().map(|n| n.index()));
                    ret_edges.extend(cur_edge_stack.iter().map(|e| e.index()));
                }
            }
            CrossForwardEdge(cfe) => {
                debug_assert!(cur_node_stack.iter().all(|&n| n != cfe.target()));
                debug_assert!(cur_edge_stack.iter().all(|&e| e != cfe.id()));
                debug_assert!(cur_node_stack[0] == start);
                if ret_nodes[cfe.target().index()] {
                    // found a simple path from `start` to a node already in the
                    // slice, which is on a simple path to `end`
                    ret_nodes.extend(cur_node_stack.iter().map(|n| n.index()));
                    ret_edges.extend(cur_edge_stack.iter().map(|e| e.index()));
                    ret_edges.extend(iter::once(cfe.id().index()));
                }
            }
            Finish(f) => {
                let _pop_n = cur_node_stack.pop();
                let _pop_e = cur_edge_stack.pop();
                debug_assert!(_pop_n == Some(f));
                debug_assert!(_pop_e.is_some() == (f != start));
                if ret_nodes[f.index()] {
                    ret_topo_order.push(f);
                }
            }
            _ => (),
        }
    });

    ret_topo_order.reverse();
    (ret_nodes, ret_edges, ret_topo_order)
}

#[cfg(test)]
mod tests {
    use super::*;
    use petgraph::algo;
    use petgraph::prelude::{EdgeIndex, NodeIndex, Outgoing, StableDiGraph};
    use petgraph::stable_graph;
    use petgraph::visit::IntoEdgeReferences;

    use quickcheck::TestResult;
    use std::collections::HashMap;
    use std::hash::Hash;
    use std::iter;

    /// Tests that `slice` returns an acyclic graph and a topological ordering.
    #[quickcheck]
    fn qc_slice(
        mut graph: StableDiGraph<(), ()>,
        start_i: usize,
        end_is: Vec<usize>,
    ) -> TestResult {
        let nodes: Vec<_> = graph.node_indices().collect();
        if nodes.is_empty() {
            return TestResult::discard();
        }
        let start = nodes[start_i % nodes.len()];
        let ends: FixedBitSet = end_is
            .into_iter()
            .map(|end_i| nodes[end_i % nodes.len()].index())
            .collect();
        println!("start: {:?}", start);
        println!("ends: {:?}", ends);
        let (slice_nodes, slice_edges, slice_topo_order) = slice(&graph, start, ends);
        let mut trace_nodes = FixedBitSet::with_capacity(graph.node_bound());
        trace_nodes.extend(slice_topo_order.iter().map(|n| n.index()));
        if trace_nodes != slice_nodes {
            println!("wrong nodes in topo_order:");
            println!(
                "  real: {:?}",
                slice_nodes
                    .ones()
                    .map(|i| NodeIndex::new(i))
                    .collect::<Vec<NodeIndex>>()
            );
            println!("  order: {:?}", slice_topo_order);
            return TestResult::failed();
        }
        graph.retain_nodes(|_, n| slice_nodes[n.index()]);
        graph.retain_edges(|_, e| slice_edges[e.index()]);
        if algo::is_cyclic_directed(&graph) {
            println!("cyclic slice:");
            println!("  slice: {:?}", graph);
            return TestResult::failed();
        }
        if !is_topological_ordering(&graph, &slice_topo_order) {
            println!("bad topological ordering: {:?}", slice_topo_order);
            println!("  slice: {:?}", graph);
            return TestResult::failed();
        }
        TestResult::passed()
    }

    /// Tests that `slice`ing a connected rooted acyclic graph is a no-op.
    #[quickcheck]
    fn qc_slice_acyclic(mut graph: StableDiGraph<(), ()>, root_i: usize) -> TestResult {
        let nodes: Vec<_> = graph.node_indices().collect();
        if nodes.is_empty() {
            return TestResult::discard();
        }
        let root = nodes[root_i % nodes.len()];
        let mut reachable = FixedBitSet::with_capacity(graph.node_bound());
        let mut back_edges = FixedBitSet::with_capacity(0);
        depth_first_search(&graph, root, |ev| {
            use super::DfsEvent::*;
            match ev {
                Discover(n) => reachable.set(n.index(), true),
                BackEdge(e) => back_edges.extend(iter::once(e.id().index())),
                _ => (),
            }
        });
        graph.retain_nodes(|_, n| reachable[n.index()]);
        graph.retain_edges(|_, e| !back_edges[e.index()]);
        println!("graph: {:?}", graph);
        println!("root: {:?}", root);
        assert!(!algo::is_cyclic_directed(&graph));
        let (slice_nodes, slice_edges, _) = slice(&graph, root, |n| {
            graph.edges_directed(n, Outgoing).next().is_none()
        });
        println!("slice_nodes: {:?}", slice_nodes.ones().collect::<Vec<_>>());
        println!(
            "slice_edges: {:?}",
            slice_edges
                .ones()
                .map(|ei| graph.edge_endpoints(EdgeIndex::new(ei)).unwrap())
                .collect::<Vec<_>>()
        );
        TestResult::from_bool(
            graph.node_indices().all(|n| slice_nodes[n.index()])
                && graph.edge_indices().all(|e| slice_edges[e.index()]),
        )
    }

    fn is_topological_ordering<G>(graph: G, topo_order: &[G::NodeId]) -> bool
    where
        G: IntoEdgeReferences,
        G::NodeId: Eq + Hash,
    {
        let order_idx: HashMap<_, _> = topo_order
            .into_iter()
            .enumerate()
            .map(|(i, n)| (n, i))
            .collect();
        graph
            .edge_references()
            .all(|e| order_idx[&e.source()] < order_idx[&e.target()])
    }
}
