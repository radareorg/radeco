use super::condition::*;
use super::*;

// #[test]
fn nmg_example() {
    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(node("A"));
    let c1 = graph.add_node(node("c1"));
    let c2 = graph.add_node(node("c2"));
    let c3 = graph.add_node(node("c3"));
    let b1 = graph.add_node(node("b1"));
    let b2 = graph.add_node(node("b2"));
    let d1 = graph.add_node(node("d1"));
    let d2 = graph.add_node(node("d2"));
    let d3 = graph.add_node(node("d3"));
    let n1 = graph.add_node(node("n1"));
    let n2 = graph.add_node(node("n2"));
    let n3 = graph.add_node(node("n3"));
    let n4 = graph.add_node(node("n4"));
    let n5 = graph.add_node(node("n5"));
    let n6 = graph.add_node(node("n6"));
    let n7 = graph.add_node(node("n7"));
    let n8 = graph.add_node(node("n8"));
    let n9 = graph.add_node(node("n9"));

    graph.add_edge(entry, c1, cond("A"));
    graph.add_edge(entry, b1, cond("-A"));
    // R1
    graph.add_edge(c1, n1, cond("c1"));
    graph.add_edge(n1, c1, None);
    graph.add_edge(c1, c2, cond("-c1"));
    graph.add_edge(c2, n2, cond("c2"));
    graph.add_edge(n2, n9, None);
    graph.add_edge(c2, n3, cond("-c2"));
    graph.add_edge(n3, c3, None);
    graph.add_edge(c3, c1, cond("c3"));
    graph.add_edge(c3, n9, cond("-c3"));
    // R2
    graph.add_edge(b1, b2, cond("b1"));
    graph.add_edge(b2, n6, cond("b2"));
    graph.add_edge(n6, n7, None);
    graph.add_edge(n7, d1, None);
    graph.add_edge(b2, n5, cond("-b2"));
    graph.add_edge(n5, n7, None);
    graph.add_edge(b1, n4, cond("-b1"));
    graph.add_edge(n4, n5, None);
    // R3
    graph.add_edge(d1, d3, cond("d1"));
    graph.add_edge(d3, n8, cond("d3"));
    graph.add_edge(n8, d1, None);
    graph.add_edge(d3, n9, cond("-d3"));
    graph.add_edge(d1, d2, cond("-d1"));
    graph.add_edge(d2, n8, cond("d2"));
    graph.add_edge(d2, n9, cond("-d2"));

    for n in graph.node_indices() {
        println!("{:?}: {:?}", n, graph[n]);
    }

    let cstore = ConditionStorage::new();
    let cctx = ConditionContext::new(&cstore);
    let cfg = ControlFlowGraph { graph, entry, cctx };
    cfg.structure_whole();
}

// #[test]
fn abnormal_entries() {
    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(node("entry"));
    let n1 = graph.add_node(node("n1"));
    let n2 = graph.add_node(node("n2"));
    let n3 = graph.add_node(node("n3"));
    let n4 = graph.add_node(node("n4"));
    let n5 = graph.add_node(node("n5"));
    let f = graph.add_node(node("f"));
    let l1 = graph.add_node(node("l1"));
    let l2 = graph.add_node(node("l2"));
    let l3 = graph.add_node(node("l3"));

    graph.add_edge(entry, l1, cond("e1"));
    graph.add_edge(entry, n1, cond("-e1"));
    graph.add_edge(n1, n2, cond("n1"));
    graph.add_edge(n2, n3, cond("n2"));
    graph.add_edge(n3, n4, cond("n3"));
    graph.add_edge(n4, n5, cond("n4"));
    graph.add_edge(n5, f, cond("n5"));
    // loop
    graph.add_edge(l1, l2, cond("l1"));
    graph.add_edge(l2, l3, None);
    graph.add_edge(l3, l1, None);
    // loop exit
    graph.add_edge(l1, f, cond("-l1"));
    // abnormal entries
    graph.add_edge(n1, l1, cond("-n1"));
    graph.add_edge(n2, l2, cond("-n2"));
    graph.add_edge(n3, l3, cond("-n3"));
    graph.add_edge(n4, l2, cond("-n4"));
    graph.add_edge(n5, l2, cond("-n5"));

    for n in graph.node_indices() {
        println!("{:?}: {:?}", n, graph[n]);
    }

    let cstore = ConditionStorage::new();
    let cctx = ConditionContext::new(&cstore);
    let cfg = ControlFlowGraph { graph, entry, cctx };
    cfg.structure_whole();
}

fn cond(c: &str) -> Option<SimpleCondition> {
    Some(SimpleCondition(c.to_owned()))
}

fn node(n: &str) -> CfgNode {
    CfgNode::Code(AstNode::BasicBlock(n.to_owned()))
}
