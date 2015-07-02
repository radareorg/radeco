//! Implements Dominance frontier and dominance tree computation.

#![allow(dead_code, unused_variables, unused_imports, unused_mut)]

use std::collections::{BTreeSet, BTreeMap, HashMap};
use petgraph::graph::{Graph, NodeIndex, EdgeIndex, Edge};
use petgraph::EdgeDirection;
use petgraph::{Dfs, Bfs};
use super::super::middle::dot::{GraphDot, EdgeInfo, Label};

//use super::dot::{GraphDot, EdgeInfo, Label};

// Non-public struct. Not intended to be used outside.
// The index in the DomTree = postorder numbering of the node in the original cfg.
type DomTreeIndex = usize;

pub struct DomTree {
    idom: Vec<usize>,
    // map from the internal numbering to original NodeIndex in cfg.
    map: HashMap<usize, NodeIndex>,
    // map from original NodeIndex in the cfg to internal numbering.
    rmap: HashMap<NodeIndex, usize>,
    g: Graph<NodeIndex, u8>,
}

#[derive(Debug)]
pub struct DFSVisitor {
    pub post_order: Vec<NodeIndex>,
    pub pre_order: Vec<NodeIndex>,
    visited: Vec<NodeIndex>,
}

impl DFSVisitor {
    pub fn new() -> DFSVisitor {
        DFSVisitor {
            post_order: Vec::new(),
            pre_order: Vec::new(),
            visited: Vec::new(),
        }
    }

    pub fn dfs<N, E>(&mut self, g: &Graph<N, E>, node: NodeIndex) {
        // If this node is already visited, return.
        if self.visited.contains(&node) {
            return;
        }

        self.pre_order.push(node.clone());


        // recursive call to perform dfs on children
        let neighbors_iter = g.neighbors_directed(node, EdgeDirection::Outgoing)
                              .collect::<Vec<NodeIndex>>();

        for n in neighbors_iter.iter() {
            self.dfs(g, n.clone());
        }

        self.visited.push(node.clone());
        self.post_order.push(node.clone());

    }
}


//trait PostOrder {
    //fn post_order(&self, start_node: usize) -> Vec<NodeIndex>;
    //fn dfs(&self, node: NodeIndex) -> Vec<NodeIndex>;
//}

//impl<N, E> PostOrder for Graph<N, E> {
    //fn post_order(&self, start_node: usize) -> Vec<NodeIndex> {
        //let n0 = NodeIndex::new(start_node);
        //let count = self.node_count();
        //let res = self.dfs(n0);

        //// If all nodes have been visited, return the resulting vector.
        //if res.count() == count {
            //return res;
        //}

        //// Else, visit the other nodes.
        //for i in 0..count {
            //if !res.contains(i) {
                //self.dfs(NodeIndex::new(i))
                    //.iter()
                    //.map(|e| res.push(e.clone()));
            //}
        //}

        //return res;
    //}

//}

// Note: No method should leak out or accept the internal numbering.
impl DomTree {
    fn new() -> DomTree {
        DomTree {
            idom: Vec::new(),
            map: HashMap::new(),
            rmap: HashMap::new(),
            g: Graph::new(),
        }
    }

    pub fn doms(&self, i: NodeIndex) -> Vec<NodeIndex> {
        Vec::new()
    }

    pub fn idom(&self, i: NodeIndex) -> NodeIndex {
        let internal_index = self.rmap.get(&i).unwrap();
        let internal_node = self.idom[*internal_index];
        self.map.get(&internal_node).unwrap().clone()
    }

    fn intersect(idom: &Vec<usize>, i: &usize, j: &usize) -> usize {
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

    //fn postorder<N, E>(g: &Graph<N, E>, start_node: NodeIndex) -> Vec<NodeIndex> {

    //}

    pub fn build_dom_tree<N, E>(g: &Graph<N, E>, start_node: NodeIndex) -> DomTree {
        let mut tree = DomTree::new();
        {
            let idom = &mut (tree.idom);
            let map = &mut (tree.map);
            let rmap = &mut (tree.rmap);
            let domg = &mut (tree.g);

            let mut v = DFSVisitor::new();
            let node_count = g.node_count();
            let invalid = node_count;

            // Compute the post-order for 'g'.
            v.dfs(&g,start_node);

            // In case the graph is disconnected (usually not the case with cfgs), visit all the
            // unvisited nodes.
            for i in 0..node_count {
                v.dfs(&g, NodeIndex::new(i));
            }

            let post_order = v.post_order.clone();

            // Tuple of (NodeIndex, post-order numbering).
            let mut nodes_iter = post_order.iter()
                                       .cloned()
                                       .zip(0..node_count)
                                       .collect::<Vec<(NodeIndex, usize)>>();

            let mut start_index = 0;
            let mut domg_nodes: Vec<NodeIndex> = Vec::new();
            for item in nodes_iter.iter() {
                if item.0 == start_node {
                    idom.push(item.1);
                    start_index = item.1;
                } else {
                    idom.push(invalid.clone());
                }

                domg_nodes.push(domg.add_node(item.0.clone()));
                //map.insert(item.1, item.0.clone());
                rmap.insert(item.0.clone(), item.1);
            }

            nodes_iter.remove(start_index);
            let mut changed = true;
            nodes_iter.reverse();
            while changed {
                changed = false;
                for n in nodes_iter.iter() {
                    let node: NodeIndex = n.0;
                    let mut preds = g.neighbors_directed(node.clone(), EdgeDirection::Incoming)
                                     .map(|x| rmap.get(&x).unwrap().clone())
                                     .collect::<Vec<usize>>();
                    let mut pindex = 0;
                    for i in 0..preds.len() {
                        let p = preds[i];
                        if idom[p] < invalid {
                            pindex = i;
                            break;
                        }
                    }

                    let mut new_idom = preds[pindex];
                    preds.remove(pindex);

                    for p in preds.iter() {
                        if idom[*p] != invalid {
                            new_idom = DomTree::intersect(&idom, &new_idom, p);
                        }
                    }

                    if idom[n.1] != new_idom {
                        idom[n.1] = new_idom;
                        changed = true;
                    }
                }
            }

            for i in 0..idom.len() {
                if i == idom[i] { continue; }
                domg.add_edge(domg_nodes[idom[i]], domg_nodes[i], 0);
            }
            
        }

        tree
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
    use petgraph::graph::{Graph, NodeIndex, EdgeIndex};
    use petgraph::{Dfs};
    use std::io::prelude::*;
    use std::fs::File;

    use super::super::super::middle::dot;

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
        for (i, j) in dfs.post_order.iter().zip(order) {
            assert_eq!(i.index(), j)
        }
        
        let order = vec![0, 5, 4, 1, 6, 9, 12, 11, 10, 2, 3, 7, 8];
        for (i, j) in dfs.pre_order.iter().zip(order) {
            assert_eq!(i.index(), j)
        }
    }

    #[test]
    fn dom_new() {
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

        let _dot = dot::emit_dot(&dom);
        let mut dot_file = File::create("dom-new.dot").ok().expect("Error. Cannot create file!\n");
        dot_file.write_all(_dot.as_bytes()).ok().expect("Error. Cannot write file!\n");
    }
}
