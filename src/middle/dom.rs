//! Implements Dominance frontier and dominance tree computation.
//!
//! Note: This is completely unstable and maybe merged with someother module in the future.

#![allow(dead_code, unused_variables, unused_imports, unused_mut)]

use std::collections::{BTreeSet, BTreeMap};
use petgraph::graph::{Graph, NodeIndex, EdgeIndex, Edge};
use petgraph::EdgeDirection;
use petgraph::{Dfs};

use super::dot::{GraphDot, EdgeInfo, Label};

// Design ideas:
//  - We can compute a dominator tree and further the dominance frontier by using the algorithms
//    outlined in: 
//        * http://www.cs.rice.edu/~keith/Embed/dom.pdf
//
//  - We can use the same graph as the CFG and overlay the dominator tree information by indicating
//  edges that belong to the dominator tree differently.

pub type DomTree = Graph<NodeIndex, u64>;

#[derive(Clone, Debug)]
pub struct DomResult {
    pub g:    DomTree,
    pub doms: BTreeMap<NodeIndex, BTreeSet<NodeIndex>>,
}

impl DomResult {
    fn new() -> DomResult {
        DomResult {
            g: DomTree::new(),
            doms: BTreeMap::new(),
        }
    }
}

pub fn build_dom_tree(g: &Graph<NodeIndex, u64>) -> DomResult {
    let mut res: DomResult = DomResult::new();
    
    {
        let mut d = &mut (res.g);
        let mut doms = &mut (res.doms);

        // Make a set of all nodes except the start.
        let mut g_iter = g.raw_nodes().iter();
        let mut tmp_iter;
        let start_n = g_iter.next().unwrap().weight.clone();

        tmp_iter = g_iter.clone();
        let mut node_set = BTreeSet::new();

        while let Some(ref node) = tmp_iter.next() {
            node_set.insert(node.weight.clone());
        }
        
        d.add_node(start_n.clone());
        
        {
            let mut tmp_set = BTreeSet::new();
            tmp_set.insert(start_n.clone());
            doms.insert(start_n.clone(), tmp_set);
        }
   
        // Add all the nodes to the DomTree.
        tmp_iter = g_iter.clone();
        while let Some(ref node) = tmp_iter.next() {
            d.add_node(node.weight.clone());
            doms.insert(node.weight.clone(), node_set.clone());
        }

        let mut change = true;

        while change {
            change = false;
            while let Some(ref node) = g_iter.next() {
                let mut preds = g.neighbors_directed((node.clone()).weight, EdgeDirection::Incoming);
                let mut tmp_set = BTreeSet::new();
                
                if let Some(ref n) = preds.next() {
                    tmp_set = doms.get(&(g.node_weight(*n).unwrap())).unwrap().clone();
                }

                while let Some(ref n) = preds.next() {
                    let other_set = doms.get(&(g.node_weight(*n).unwrap())).unwrap();
                    let intersect: Vec<NodeIndex> = tmp_set.intersection(other_set).cloned().collect();
                    tmp_set.clear();
                    for e in intersect.iter(){ 
                        tmp_set.insert(e.clone());
                    }
                }

                tmp_set.insert(node.weight);
                
                let set = doms.get_mut(&(node.weight)).unwrap();
                let tmp: Vec<NodeIndex> = tmp_set.difference(set).cloned().collect();

                if !(tmp.is_empty()) {
                    change = true;
                    *set = tmp_set.clone();
                }
            }
        }

        for (key, value) in doms.iter() {
            let list: Vec<NodeIndex>  = g.neighbors_directed(key.clone(), EdgeDirection::Incoming).collect();
            for v in value.iter() {
                // If v is in the adjacency list of key, add an edge.
                // (As we need only immediate dominators for the dom tree).
                if list.contains(v) {
                    d.add_edge(*v, *key, 0);
                }
            }
        }
    }

    res
}

///////////////////////////////////////////////////////////////////////////////
//// Implementation of Traits to emit dot for dom.
///////////////////////////////////////////////////////////////////////////////

impl GraphDot for DomResult {
    type NodeType = NodeIndex;
    type EdgeType = Edge<u64>;

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

impl EdgeInfo for Edge<u64> {
    fn source(&self) -> usize {
        self.source().index()
    }

    fn target(&self) -> usize {
        self.target().index()
    }
}

impl Label for Edge<u64> {
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
    use super::super::dot;
    use std::io::prelude::*;
    use std::fs::File;

    #[test]
    fn dummy() {
        let mut g = Graph::<NodeIndex, u64>::new();
        let a = g.add_node(NodeIndex::new(0));
        let b = g.add_node(NodeIndex::new(1));
        let c = g.add_node(NodeIndex::new(2));
        let d = g.add_node(NodeIndex::new(3));
        let e = g.add_node(NodeIndex::new(4));
        let f = g.add_node(NodeIndex::new(5));
        let _g = g.add_node(NodeIndex::new(6));

        g.add_edge(a, b, 0);
        g.add_edge(a, _g, 0);
        g.add_edge(b, c, 0);
        g.add_edge(b, e, 0);
        g.add_edge(c, d, 0);
        g.add_edge(d, e, 0);
        g.add_edge(e, f, 0);
        g.add_edge(f, _g, 0);

        let dom = build_dom_tree(&mut g);
        let _dot = dot::emit_dot(&dom);
        let mut dot_file = File::create("dom.dot").ok().expect("Error. Cannot create file!\n");
        dot_file.write_all(_dot.as_bytes()).ok().expect("Error. Cannot write file!\n");
    }
}
