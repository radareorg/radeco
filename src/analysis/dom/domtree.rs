//! Implements Dominance frontier and dominance tree computation.

use std::collections::{HashMap, HashSet};
use petgraph::graph::{Graph, NodeIndex, Edge};
use petgraph::EdgeDirection;
use super::index::InternalIndex;

use super::super::super::middle::dot::{GraphDot, EdgeInfo, Label};

#[derive(Debug)]
pub struct DFSVisitor {
    post_order: Vec<NodeIndex>,
    pre_order:  Vec<NodeIndex>,
    visited:    Vec<NodeIndex>,
}

impl DFSVisitor {
    pub fn new() -> DFSVisitor {
        DFSVisitor {
            post_order: Vec::new(),
            pre_order:  Vec::new(),
            visited:    Vec::new(),
        }
    }

    pub fn dfs<N, E>(&mut self, g: &Graph<N, E>, node: NodeIndex) {
        if self.visited.contains(&node) {
            return;
        }

        self.pre_order.push(node.clone());
        let neighbors_iter = g.neighbors_directed(node, EdgeDirection::Outgoing)
                              .collect::<Vec<NodeIndex>>();

        for n in neighbors_iter.iter() {
            self.dfs(g, n.clone());
        }

        self.visited.push(node.clone());
        self.post_order.push(node.clone());
    }

    pub fn post_order(&self) -> Vec<NodeIndex> {
        self.post_order.clone()
    }

    pub fn pre_order(&self) -> Vec<NodeIndex> {
        self.pre_order.clone()
    }
}

pub struct DomTree {
    idom: Vec<InternalIndex>,
    map:  HashMap<InternalIndex, NodeIndex>,
    rmap: HashMap<NodeIndex, InternalIndex>,
    g:    Graph<NodeIndex, u8>,
    preds_map: HashMap<NodeIndex, Vec<InternalIndex>>,
    dom_frontier: Option<HashMap<NodeIndex, HashSet<NodeIndex>>>,
}

// Note: No method should leak out or accept the internal numbering.
impl DomTree {
    fn new() -> DomTree {
        DomTree {
            idom:         Vec::new(),
            map:          HashMap::new(),
            rmap:         HashMap::new(),
            g:            Graph::new(),
            preds_map:    HashMap::new(),
            dom_frontier: None,
        }
    }

    pub fn graph(&self) -> &Graph<NodeIndex, u8> {
        &self.g
    }

    pub fn doms(&self, i: NodeIndex) -> Vec<NodeIndex> {
        let internal_index = self.rmap.get(&i).unwrap();
        let mut idom = *internal_index;
        let mut doms = Vec::<InternalIndex>::new();
        while idom != self.idom[idom] {
            doms.push(idom.clone());
            idom = self.idom[idom];
        }

        doms.push(idom.clone());
        doms.iter().map(|d| self.map.get(d).unwrap().clone()).collect()
    }

    pub fn idom(&self, i: NodeIndex) -> NodeIndex {
        let internal_index = self.rmap.get(&i).unwrap();
        let internal_node = self.idom[*internal_index];
        self.map.get(&internal_node).unwrap().clone()
    }

    fn intersect(idom: &Vec<InternalIndex>, i: &InternalIndex, j: &InternalIndex) -> InternalIndex {
        let mut f1 = *i;
        let mut f2 = *j;
        while f1 != f2 {
            while f1 < f2 {
                f1 = idom[f1];
            }
            while f2 < f1 {
                f2 = idom[f2];
            }
        }
        return f1;
    }

    pub fn build_dom_tree<N, E>(g: &Graph<N, E>, start_node: NodeIndex) -> DomTree {
        let mut tree = DomTree::new();

        {
            let idom     = &mut (tree.idom);
            let map      = &mut (tree.map);
            let rmap     = &mut (tree.rmap);
            let dom_tree = &mut (tree.g);
            let preds_map = &mut (tree.preds_map);
            
            let mut v      = DFSVisitor::new();
            let node_count = g.node_count();
            let invalid    = node_count;

            v.dfs(&g,start_node);
            
            for i in 0..node_count {
                v.dfs(&g, NodeIndex::new(i));
            }
            
            // Tuple of (NodeIndex, post-order numbering).
            let mut nodes_iter = v.post_order().iter()
                                               .cloned()
                                               .zip(0..node_count)
                                               .collect::<Vec<(NodeIndex, usize)>>();

            let internal_invalid = InternalIndex::new(invalid.clone(), NodeIndex::new(invalid));
            let mut start_index = internal_invalid;
            let mut dom_tree_nodes: Vec<NodeIndex> = Vec::new();

            for item in nodes_iter.iter() {
                if item.0 == start_node {
                    start_index = InternalIndex::new(item.1, item.0.clone());
                    idom.push(start_index);
                } else {
                    idom.push(internal_invalid);
                }

                dom_tree_nodes.push(dom_tree.add_node(NodeIndex::new(item.1)));
                rmap.insert(item.0.clone(), InternalIndex::new(item.1, item.0.clone()));
                map.insert(InternalIndex::new(item.1, item.0.clone()), item.0.clone());
            }

            {
                let tmp = Vec::<InternalIndex>::new();
                preds_map.insert(start_node, tmp);
            }

            nodes_iter.remove(start_index.index());
            nodes_iter.reverse();
            
            let mut changed = true;
            while changed {
                changed = false;
                for n in nodes_iter.iter() {
                    let node: NodeIndex = n.0;
                    let preds_iter = g.neighbors_directed(node.clone(), EdgeDirection::Incoming)
                                      .map(|x| rmap.get(&x).unwrap().clone());
                    let preds = preds_map.entry(node.clone())
                                         .or_insert(preds_iter.collect::<Vec<InternalIndex>>());
                    
                    let mut new_idom = preds[0];
                    for p in preds.iter() {
                        if idom[*p] < internal_invalid {
                            new_idom = *p;
                            break;
                        }
                    }

                    for p in preds.iter() {
                        if idom[*p] != internal_invalid {
                            new_idom = DomTree::intersect(&idom, &new_idom, p);
                        }
                    }

                    if idom[n.1] != new_idom {
                        idom[n.1] = new_idom;
                        changed = true;
                    }
                }
            }

            for i in (0..idom.len()).map(|n| InternalIndex::new(n, NodeIndex::new(node_count))) {
                if i == idom[i] { continue; }
                let n1 = dom_tree_nodes[map.get(&idom[i]).unwrap().index()];
                let n2 = dom_tree_nodes[map.get(&i).unwrap().index()];
                dom_tree.add_edge(n1, n2, 0);
            }
        }
        return tree;
    }

    pub fn compute_dominance_frontier(&mut self) {
        let node_count = self.idom.len();
        let mut frontier_map = HashMap::<NodeIndex, HashSet<NodeIndex>>::new();
        for node in (0..node_count).map(|x| NodeIndex::new(x)) {
            let internal_index = self.rmap.get(&node).unwrap();
            let preds = self.preds_map.get(&node).unwrap();
            if preds.len() < 2 {
                continue;
            }
            for p in preds {
                let mut runner = *p;
                while runner != self.idom[*internal_index] {
                    let runner_index = self.map.get(&runner).unwrap();
                    frontier_map.entry(*runner_index)
                                .or_insert(HashSet::new())
                                .insert(node);

                    runner = self.idom[runner];
                }
            }
        }
        self.dom_frontier = Some(frontier_map);
    }

    pub fn dom_frontier(&self, n: NodeIndex) -> HashSet<NodeIndex> {
        self.dom_frontier.clone().unwrap().get(&n).unwrap().clone()
    }
}

///////////////////////////////////////////////////////////////////////////////
//// Implementation of Traits to emit dot for dom.
///////////////////////////////////////////////////////////////////////////////

impl GraphDot for DomTree {
    type NodeType = NodeIndex;
    type EdgeType = Edge<u8>;

    fn configure(&self) -> String {
        format!("digraph cfg {{\nsplines=\"true\";\n")
    }

    fn nodes(&self) -> Vec<Self::NodeType> {
        let res = self.g.raw_nodes().iter().map(|e| e.weight.clone()).collect();
        res
    }

    fn edges(&self) -> Vec<Self::EdgeType> {
        let res = self.g.raw_edges().to_vec();
        res
    }

    fn get_node(&self, n: usize) -> Option<&Self::NodeType> {
        self.g.node_weight(NodeIndex::new(n))
    }
}

impl EdgeInfo for Edge<u8> {
    fn source(&self) -> usize {
        self.source().index()
    }

    fn target(&self) -> usize {
        self.target().index()
    }
}

impl Label for Edge<u8> {
    fn label(&self) -> String {
        ";\n".to_string()
    }
     
    fn name(&self) -> Option<String> {
        None
    }
}

impl Label for NodeIndex {
    fn label(&self) -> String {
        let tmp = format!("n{}", self.index());
        format!("{} [label={}];\n", tmp, tmp)
    }

    fn name(&self) -> Option<String> {
        Some(format!("n{}", self.index()))
    }
}


#[cfg(test)]
mod test {

    use super::*;
    use petgraph::graph::{Graph, NodeIndex};
    use std::collections::{HashSet};

    #[test]
    fn dfs() {
        let mut g = Graph::<u64, u64>::new();
        let mut n: Vec<NodeIndex> = Vec::new();
        for i in 0..13 {
            n.push(g.add_node(i));
        }
        
        g.add_edge(n[0] , n[6] , 0);
        g.add_edge(n[0] , n[1] , 0);
        g.add_edge(n[0] , n[5] , 0);
        g.add_edge(n[5] , n[4] , 0);
        g.add_edge(n[2] , n[3] , 0);
        g.add_edge(n[2] , n[0] , 0);
        g.add_edge(n[3] , n[5] , 0);
        g.add_edge(n[6] , n[4] , 0);
        g.add_edge(n[6] , n[9] , 0);
        g.add_edge(n[9] , n[10], 0);
        g.add_edge(n[9] , n[11], 0);
        g.add_edge(n[9] , n[12], 0);
        g.add_edge(n[11], n[12], 0);
        g.add_edge(n[8] , n[7] , 0);
        g.add_edge(n[7] , n[6] , 0);

        let count = g.node_count();
        
        let mut dfs = DFSVisitor::new();
        for i in 0..count {
            dfs.dfs(&g, n[i]);
        }

        let order = vec![4, 5, 1, 12, 11, 10, 9, 6, 0, 3, 2, 7, 8];
        for (i, j) in dfs.post_order().iter().zip(order) {
            assert_eq!(i.index(), j)
        }
        
        let order = vec![0, 5, 4, 1, 6, 9, 12, 11, 10, 2, 3, 7, 8];
        for (i, j) in dfs.pre_order().iter().zip(order) {
            assert_eq!(i.index(), j)
        }
    }

    #[test]
    fn dom() {
        let mut g = Graph::<NodeIndex, u64>::new();
        let n0 = g.add_node(NodeIndex::new(0));
        let n1 = g.add_node(NodeIndex::new(1));
        let n2 = g.add_node(NodeIndex::new(2));
        let n3 = g.add_node(NodeIndex::new(3));
        let n4 = g.add_node(NodeIndex::new(4));
        let n5 = g.add_node(NodeIndex::new(5));
        let n6 = g.add_node(NodeIndex::new(6));
        let n7 = g.add_node(NodeIndex::new(7));
        let n8 = g.add_node(NodeIndex::new(8));
        let n9 = g.add_node(NodeIndex::new(9));

        g.add_edge(n0, n2, 0);
        g.add_edge(n2, n3, 0);
        g.add_edge(n2, n4, 0);
        g.add_edge(n4, n3, 0);
        g.add_edge(n4, n5, 0);
        g.add_edge(n5, n3, 0);
        g.add_edge(n5, n6, 0);
        g.add_edge(n6, n8, 0);
        g.add_edge(n6, n9, 0);
        g.add_edge(n8, n7, 0);
        g.add_edge(n9, n7, 0);
        g.add_edge(n3, n7, 0);
        g.add_edge(n7, n1, 0);

        let dom = DomTree::build_dom_tree(&mut g, n0);
        let doms_n1 = [n1, n7, n2, n0];
        let doms_n9 = [n9, n6, n5, n4, n2, n0];
        let doms_n3 = [n3, n2, n0];
        
        assert_eq!(dom.doms(n1), doms_n1);
        assert_eq!(dom.doms(n9), doms_n9);
        assert_eq!(dom.doms(n3), doms_n3);
    }

    #[test]
    fn dom_frontier() {
        let mut g = Graph::<char, u8>::new();
        let a = g.add_node('A');
        let b = g.add_node('B');
        let c = g.add_node('C');
        let d = g.add_node('D');
        let e = g.add_node('E');
        let f = g.add_node('F');
        let _g = g.add_node('G');

        g.add_edge(a, b, 0);
        g.add_edge(a, _g, 0);
        g.add_edge(b, c, 0);
        g.add_edge(b, e, 0);
        g.add_edge(c, d, 0);
        g.add_edge(d, e, 0);
        g.add_edge(e, f, 0);
        g.add_edge(f, _g, 0);

        let mut dom = DomTree::build_dom_tree(&mut g, a);
        dom.compute_dominance_frontier();

        let res = dom.dom_frontier(f);
        let mut dom_front_f = HashSet::new();
        dom_front_f.insert(_g);

        assert_eq!(dom_front_f.len(), res.len());
        for i in res.iter().zip(dom_front_f) {
            assert_eq!(*i.0, i.1);
        }
        
        let res = dom.dom_frontier(d);
        let mut dom_front_d = HashSet::new();
        dom_front_d.insert(e);
        
        assert_eq!(dom_front_d.len(), res.len());
        for i in res.iter().zip(dom_front_d) {
            assert_eq!(*i.0, i.1);
        }
    }
}
