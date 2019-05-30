use super::*;
use petgraph::algo;
use petgraph::prelude::{Outgoing, StableDiGraph};
use petgraph::visit::IntoEdgeReferences;

use quickcheck::TestResult;
use std::collections::HashMap;
use std::hash::Hash;
use std::iter::FromIterator;

/// Tests that `slice` returns an acyclic graph and a topological ordering.
#[quickcheck]
fn qc_slice(mut graph: StableDiGraph<(), ()>, start_i: usize, end_is: Vec<usize>) -> TestResult {
    let nodes: Vec<_> = graph.node_indices().collect();
    if nodes.is_empty() {
        return TestResult::discard();
    }
    let start = nodes[start_i % nodes.len()];
    let ends: IxBitSet<_> = end_is
        .into_iter()
        .map(|end_i| nodes[end_i % nodes.len()])
        .collect();
    println!("start: {:?}", start);
    println!("ends: {:?}", ends);
    let slice = slice(&graph, start, &ends);
    let trace_nodes = IxBitSet::from_iter(&slice.topo_order);
    if trace_nodes != slice.nodes {
        println!("wrong nodes in topo_order:");
        println!("  real: {:?}", slice.nodes);
        println!("  order: {:?}", slice.topo_order);
        return TestResult::failed();
    }
    graph.retain_nodes(|_, n| slice.nodes.contains(n));
    graph.retain_edges(|_, e| slice.edges.contains(e));
    if algo::is_cyclic_directed(&graph) {
        println!("cyclic slice:");
        println!("  slice: {:?}", graph);
        return TestResult::failed();
    }
    if !is_topological_ordering(&graph, &slice.topo_order) {
        println!("bad topological ordering: {:?}", slice.topo_order);
        println!("  slice: {:?}", graph);
        return TestResult::failed();
    }
    TestResult::passed()
}

/// Tests that `slice`ing a connected rooted acyclic graph is a no-op.
#[quickcheck]
fn qc_slice_acyclic(mut graph: StableDiGraph<(), ()>, root_i: usize) -> TestResult {
    let root = if let Some(root) = mk_rooted_stable_graph(&mut graph, root_i, true) {
        root
    } else {
        return TestResult::discard();
    };
    println!("graph: {:?}", graph);
    println!("root: {:?}", root);
    assert!(!algo::is_cyclic_directed(&graph));
    let sinks: IxBitSet<_> = graph
        .node_indices()
        .filter(|&n| graph.edges_directed(n, Outgoing).next().is_none())
        .collect();
    let slice = slice(&graph, root, &sinks);
    println!("slice_nodes: {:?}", slice.nodes);
    println!(
        "slice_edges: {:?}",
        slice
            .edges
            .iter()
            .map(|e| graph.edge_endpoints(e).unwrap())
            .collect::<Vec<_>>()
    );
    TestResult::from_bool(
        graph.node_indices().all(|n| slice.nodes.contains(n))
            && graph.edge_indices().all(|e| slice.edges.contains(e)),
    )
}

/// Tests that nearest common dominator works
#[quickcheck]
fn qc_nearest_common_dominator(
    mut graph: StableDiGraph<(), ()>,
    root_i: usize,
    in_node_is: Vec<usize>,
) -> TestResult {
    if in_node_is.is_empty() {
        return TestResult::discard();
    }
    let root = if let Some(root) = mk_rooted_stable_graph(&mut graph, root_i, true) {
        root
    } else {
        return TestResult::discard();
    };

    let nodes: Vec<_> = graph.node_indices().collect();
    let in_nodes: IxBitSet<_> = in_node_is
        .into_iter()
        .map(|in_node_i| nodes[in_node_i % nodes.len()])
        .collect();

    println!("graph: {:?}", graph);
    println!("root: {:?}", root);
    println!("in_nodes: {:?}", in_nodes);

    let dominators = algo::dominators::simple_fast(&graph, root);

    let ncd = nearest_common_dominator(&graph, root, &in_nodes);

    let mut common_dominators = in_nodes
        .iter()
        .map(|u| IxBitSet::from_iter(dominators.dominators(u).unwrap()))
        .fold(IxBitSet::from_iter(graph.node_indices()), |mut acc, x| {
            acc.intersect_with(&x);
            acc
        });
    // `ncd` dominates every node in `in_nodes`
    if !common_dominators.remove(ncd) {
        return TestResult::failed();
    }
    // `ncd` does not dominate any other common dominator
    for cand in &common_dominators {
        if dominators.dominators(cand).unwrap().any(|d| d == ncd) {
            return TestResult::failed();
        }
    }

    TestResult::passed()
}

#[quickcheck]
fn qc_dominated_by(mut graph: StableDiGraph<(), ()>, root_i: usize, h_i: usize) -> TestResult {
    let root = if let Some(root) = mk_rooted_stable_graph(&mut graph, root_i, false) {
        root
    } else {
        return TestResult::discard();
    };
    let nodes: Vec<_> = graph.node_indices().collect();
    let h = nodes[h_i % nodes.len()];

    let dominators = algo::dominators::simple_fast(&graph, root);
    let true_dom_set: IxBitSet<_> = graph
        .node_indices()
        .filter(|&n| dominators.dominators(n).unwrap().any(|d| d == h))
        .collect();

    let dom_set = dominated_by(&graph, root, h);

    TestResult::from_bool(dom_set == true_dom_set)
}

#[quickcheck]
fn qc_dag_transitive_closure(graph: StableDiGraph<(), ()>) -> TestResult {
    if algo::is_cyclic_directed(&graph) {
        return TestResult::discard();
    }

    let reachable_map = dag_transitive_closure(&graph);

    if reachable_map.len() != graph.node_count() {
        return TestResult::failed();
    }
    TestResult::from_bool(graph.node_indices().all(|n| {
        let dfs_reachable: IxBitSet<_> = Dfs::new(&graph, n).iter(&graph).collect();
        dfs_reachable == reachable_map[&n]
    }))
}

fn mk_rooted_stable_graph(
    graph: &mut StableDiGraph<(), ()>,
    root_i: usize,
    prune_cycles: bool,
) -> Option<NodeIndex> {
    let nodes: Vec<_> = graph.node_indices().collect();
    if nodes.is_empty() {
        return None;
    }
    let root = nodes[root_i % nodes.len()];
    let mut reachable = IxBitSet::new();
    let mut back_edges = IxBitSet::new();
    depth_first_search(&*graph, root, |ev| {
        use super::DfsEvent::*;
        match ev {
            Discover(n) => {
                reachable.insert(n);
            }
            BackEdge(e) => {
                back_edges.insert(e.id());
            }
            _ => (),
        }
    });
    if graph.node_indices().any(|n| !reachable.contains(n)) {
        return None;
    }
    // graph.retain_nodes(|_, n| reachable.contains(n));
    if prune_cycles {
        // discarding cyclic graphs make tests take too long
        // instead we just remove cycles
        graph.retain_edges(|_, e| !back_edges.contains(e));
    }
    Some(root)
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
