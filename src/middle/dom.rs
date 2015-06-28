//! Implements Dominance frontier and dominance tree computation.
//!
//! Note: This is completely unstable and maybe merged with someother module in the future.

#![allow(dead_code, unused_variables, unused_imports, unused_mut)]

use std::collections::{BTreeSet, BTreeMap};
use petgraph::graph::{Graph, NodeIndex, EdgeIndex};
use petgraph::EdgeDirection;
use petgraph::{Dfs};

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
            for v in value.iter() {
                d.add_edge(*key, *v, 0);
            }
        }
    }

    res
}

#[cfg(test)]
mod test {
    use super::*;
    use petgraph::graph::{Graph, NodeIndex, EdgeIndex};
    use petgraph::{Dfs};

    #[test]
    fn dummy() {
        let mut g = Graph::<NodeIndex, u64>::new();
        let n0 = g.add_node(NodeIndex::new(0));
        let n1 = g.add_node(NodeIndex::new(1));
        g.add_edge(n0, n1, 0);

        build_dom_tree(&mut g);
    }
}
