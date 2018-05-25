use fixedbitset::FixedBitSet;
use petgraph::prelude::{NodeIndex, StableDiGraph};
use petgraph::visit::{EdgeRef, GraphBase, IntoEdges, NodeIndexable, VisitMap, Visitable};

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

/// Returns the union of all simple paths from `start` to `end`.
fn slice<N, E>(
    graph: &StableDiGraph<N, E>,
    start: NodeIndex,
    end: NodeIndex,
) -> (FixedBitSet, FixedBitSet) {
    let mut ret_nodes = FixedBitSet::with_capacity(graph.node_bound());
    // petgraph doesn't expose `edge_bound` :(
    let mut ret_edges = FixedBitSet::with_capacity(0);

    let mut cur_node_stack = vec![start];
    let mut cur_edge_stack = Vec::new();
    depth_first_search(graph, start, |ev| {
        use self::DfsEvent::*;
        match ev {
            TreeEdge(te) => {
                cur_edge_stack.push(te.id());
                cur_node_stack.push(te.target());
                if te.target() == end {
                    // found a simple path from `start` to `end`
                    ret_nodes.extend(cur_node_stack.iter().map(|n| n.index()));
                    ret_edges.extend(cur_edge_stack.iter().map(|e| e.index()));
                }
            }
            CrossForwardEdge(cfe) => {
                debug_assert!(cur_node_stack.iter().all(|&n| n != cfe.target()));
                debug_assert!(cur_edge_stack.iter().all(|&e| e != cfe.id()));
                if ret_nodes[cfe.target().index()] {
                    // found a simple path from `start` to a node already in the
                    // slice, which is on a simple path to `end`
                    ret_nodes.extend(cur_node_stack.iter().map(|n| n.index()));
                    ret_edges.extend(cur_edge_stack.iter().map(|e| e.index()));
                }
            }
            Finish(f) => {
                let _pop_n = cur_node_stack.pop();
                let _pop_e = cur_edge_stack.pop();
                debug_assert!(_pop_n == Some(f));
                if f != start {
                    debug_assert!(graph.edge_endpoints(_pop_e.unwrap()).unwrap().1 == f);
                }
            }
            _ => (),
        }
    });

    (ret_nodes, ret_edges)
}

#[cfg(test)]
mod tests {
    use super::*;
    use petgraph::algo;
    use petgraph::prelude::StableDiGraph;
    use petgraph::stable_graph;
    use petgraph::visit::{EdgeFiltered, NodeFiltered};

    use quickcheck::TestResult;

    #[quickcheck]
    fn qc_slice(graph: StableDiGraph<(), ()>, start_i: usize, end_i: usize) -> TestResult {
        let nodes: Vec<_> = graph.node_indices().collect();
        if nodes.is_empty() {
            return TestResult::discard();
        }
        let start = nodes[start_i % nodes.len()];
        let end = nodes[end_i % nodes.len()];
        let (slice_nodes, slice_edges) = slice(&graph, start, end);
        let subgraph = NodeFiltered(&graph, slice_nodes);
        let subgraph = EdgeFiltered::from_fn(&subgraph, |e| slice_edges[e.id().index()]);
        TestResult::from_bool(!algo::is_cyclic_directed(&subgraph))
    }
}
