use super::*;

// #[test]
fn nmg_example() {
    let mut graph = StableDiGraph::new();
    let entry = graph.add_node(AstNode::BasicBlock("A".to_owned()));
    let c1 = graph.add_node(AstNode::BasicBlock("c1".to_owned()));
    let c2 = graph.add_node(AstNode::BasicBlock("c2".to_owned()));
    let c3 = graph.add_node(AstNode::BasicBlock("c3".to_owned()));
    let b1 = graph.add_node(AstNode::BasicBlock("b1".to_owned()));
    let b2 = graph.add_node(AstNode::BasicBlock("b2".to_owned()));
    let d1 = graph.add_node(AstNode::BasicBlock("d1".to_owned()));
    let d2 = graph.add_node(AstNode::BasicBlock("d2".to_owned()));
    let d3 = graph.add_node(AstNode::BasicBlock("d3".to_owned()));
    let n1 = graph.add_node(AstNode::BasicBlock("n1".to_owned()));
    let n2 = graph.add_node(AstNode::BasicBlock("n2".to_owned()));
    let n3 = graph.add_node(AstNode::BasicBlock("n3".to_owned()));
    let n4 = graph.add_node(AstNode::BasicBlock("n4".to_owned()));
    let n5 = graph.add_node(AstNode::BasicBlock("n5".to_owned()));
    let n6 = graph.add_node(AstNode::BasicBlock("n6".to_owned()));
    let n7 = graph.add_node(AstNode::BasicBlock("n7".to_owned()));
    let n8 = graph.add_node(AstNode::BasicBlock("n8".to_owned()));
    let n9 = graph.add_node(AstNode::BasicBlock("n9".to_owned()));

    graph.add_edge(entry, c1, SimpleCondition("A".to_owned()));
    graph.add_edge(entry, b1, SimpleCondition("-A".to_owned()));
    // R1
    graph.add_edge(c1, n1, SimpleCondition("c1".to_owned()));
    graph.add_edge(n1, c1, SimpleCondition("".to_owned()));
    graph.add_edge(c1, c2, SimpleCondition("-c1".to_owned()));
    graph.add_edge(c2, n2, SimpleCondition("c2".to_owned()));
    graph.add_edge(n2, n9, SimpleCondition("".to_owned()));
    graph.add_edge(c2, n3, SimpleCondition("-c2".to_owned()));
    graph.add_edge(n3, c3, SimpleCondition("".to_owned()));
    graph.add_edge(c3, c1, SimpleCondition("c3".to_owned()));
    graph.add_edge(c3, n9, SimpleCondition("-c3".to_owned()));
    // R2
    graph.add_edge(b1, b2, SimpleCondition("b1".to_owned()));
    graph.add_edge(b2, n6, SimpleCondition("b2".to_owned()));
    graph.add_edge(n6, n7, SimpleCondition("".to_owned()));
    graph.add_edge(n7, d1, SimpleCondition("".to_owned()));
    graph.add_edge(b2, n5, SimpleCondition("-b2".to_owned()));
    graph.add_edge(n5, n7, SimpleCondition("".to_owned()));
    graph.add_edge(b1, n4, SimpleCondition("-b1".to_owned()));
    graph.add_edge(n4, n5, SimpleCondition("".to_owned()));
    // R3
    graph.add_edge(d1, d3, SimpleCondition("d1".to_owned()));
    graph.add_edge(d3, n8, SimpleCondition("d3".to_owned()));
    graph.add_edge(n8, d1, SimpleCondition("".to_owned()));
    graph.add_edge(d3, n9, SimpleCondition("-d3".to_owned()));
    graph.add_edge(d1, d2, SimpleCondition("-d1".to_owned()));
    graph.add_edge(d2, n8, SimpleCondition("d2".to_owned()));
    graph.add_edge(d2, n9, SimpleCondition("-d2".to_owned()));

    let cfg = ControlFlowGraph { graph, entry };
    cfg.structure_whole();
}
