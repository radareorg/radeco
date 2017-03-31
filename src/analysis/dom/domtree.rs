// See the COPYING file at the top-level directory of this distribution.
// Copyright (c) 2015, The Radare Project. All rights reserved.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Implements Dominance frontier and dominance tree computation.

use super::bimap::BiMap;
use middle::dot::{DotAttrBlock, GraphDot};
use petgraph::{graph, EdgeDirection};
use petgraph::algo::dominators;
use petgraph::algo::dominators::{Dominators, DominatorsIter};
use petgraph::graph::Graph;
use petgraph::graph::NodeIndex;

use petgraph::visit::{Dfs, Bfs, Walker};
use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Iter;
use std::default;

// TODO: make the functoins work on domintor tree / post dominator tree by passing them
// an enum i.e call dominators(n, Domination:Pre), dominators(n, Domination:Post) to
// find the dominators / post dominators of a given node

/// Contains dominance info for a given graph
/// If you want to find a NodeIndex from a usize value, use the node_map.first_to_second
#[derive(Clone, Debug)]
struct DomminanceInfo<N, E> {
    dominators: Dominators<NodeIndex>,

    post_dominators: Dominators<NodeIndex>,
    // Dominance frontier for each node
    frontier_map: HashMap<NodeIndex, HashSet<NodeIndex>>,
    // Predecessors of each node
    preds_map: HashMap<NodeIndex, Vec<NodeIndex>>,
    // Needed to acess elements by dfs order in an O(1) operation
    node_map: BiMap<usize, NodeIndex>,
    // Graph
    g: Option<graph::Graph<N, E>>,
}

impl<N, E> DomminanceInfo<N, E> {
    /// Computes info about dominance relation in a given graph
    fn new(g: &Graph<N, E>, start_node: graph::NodeIndex) -> DomminanceInfo<N, E>
        where E: Clone,
              N: Clone
    {

        let mut node_map = DomminanceInfo::create_dfs_indexing(g, start_node);

        // Computes the dominators
        let mut dom = dominators::simple_fast(g, start_node);
        let preds_map = DomminanceInfo::compute_preds(g, start_node);

        // TODO: check if this is correct, I assume that the last node visited by dfs is guaranteed to be
        //  the exit node, as all execution paths must arrive at the exit node
        let last_node_index: usize = g.node_count() - 1;
        let exit_node: NodeIndex = *node_map.second_from_first(&last_node_index).unwrap();

        let mut clone = (*g).clone();
        clone.reverse();

        let mut pdom = dominators::simple_fast(&clone, exit_node);

        DomminanceInfo {
            dominators: dom,
            post_dominators: pdom,
            preds_map: preds_map,
            node_map: node_map,
            frontier_map: HashMap::new(),
            g: Some(g.clone()),
        }
    }

    /// Store nodes in the node_map table as k,v -> dfs_visit_index, NodeIndex
    fn create_dfs_indexing(g: &Graph<N, E>, start_node: NodeIndex) -> BiMap<usize, NodeIndex> {

        let mut node_map: BiMap<usize, NodeIndex> = BiMap::new();

        // DFS is used as the access will be by dfs index, as in the code flow
        let mut dfs_visitor = Dfs::new(&g, start_node);
        let mut dfs_index: usize = 0;
        while let Some(node) = dfs_visitor.next(&g) {
            node_map.insert(dfs_index, node);
            dfs_index += 1;
        }
        node_map
    }

    /// Returns the immediate dominator of a given node
    fn immediate_dominator(&self, node: graph::NodeIndex) -> Option<NodeIndex> {
        // This is an O(1) operation, no need to keep another hashmap
        self.dominators.immediate_dominator(node)
    }

    /// Returns a vector of dominators for a given node
    fn dominators(&self, node: graph::NodeIndex) -> Option<Vec<NodeIndex>> {
        let dom_iter = self.dominators.dominators(node);
        let mut results: Vec<NodeIndex> = Vec::new();

        // Unwrap the iterator and put its content in a vector, else return None
        dom_iter.and_then(|iter| Some(iter.collect::<Vec<NodeIndex>>())).or_else(|| None)
    }

    /// Computes all predecessors for all the nodes in the graph and adds them to
    /// a hashmap
    fn compute_preds(g: &Graph<N, E>, start_node: NodeIndex) -> HashMap<NodeIndex, Vec<NodeIndex>> {
        // Foreach node n, find all the nodes that point to n
        // Store all that data in a HashMap
        let mut pred_map: HashMap<NodeIndex, Vec<NodeIndex>> = HashMap::new();
        let mut visitor = Dfs::new(&g, start_node);

        while let Some(node) = visitor.next(&g) {

            // NodeIndex -> usize_dfs_index
            let preds_vec: Vec<NodeIndex> =
                g.neighbors_directed(node, EdgeDirection::Incoming).clone().collect();
            pred_map.insert(node, preds_vec);

        }
        pred_map
    }

    /// Dominance Frontier of a node x (DF(x)) is the set of all
    /// branches (u; v) in the CFG, where x dominates u, but x does not
    /// dominate v, i.e the boundaries of x's dominance
    fn compute_dominance_frontier(&mut self) {

        // for usize, NodeIndex
        for (k, node_index) in self.node_map.first_iter() {

            let ref preds: Vec<NodeIndex> = self.preds_map[node_index];

            if preds.len() < 2 {
                continue; // Nothing to do
            }

            for pred in preds {
                let mut temp = *pred;

                while temp != self.immediate_dominator(*node_index).unwrap() {
                    self.frontier_map.entry(*pred).or_insert_with(HashSet::new).insert(*node_index);
                    temp = self.immediate_dominator(temp).unwrap();
                }
            }
        }
    }

    /// Returns the dominance frontier for a given node
    fn dominance_frontier(&self, node: graph::NodeIndex) -> Option<&HashSet<NodeIndex>> {
        self.frontier_map.get(&node)
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use petgraph::graph::{Graph, NodeIndex};
    use std::collections::HashSet;

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

        let dom = DomminanceInfo::new(&mut g, n0);
        let doms_n1 = [n1, n7, n2, n0];
        let doms_n9 = [n9, n6, n5, n4, n2, n0];
        let doms_n3 = [n3, n2, n0];

        assert_eq!(dom.dominators(n1).unwrap(), doms_n1);
        assert_eq!(dom.dominators(n9).unwrap(), doms_n9);
        assert_eq!(dom.dominators(n3).unwrap(), doms_n3);
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

        let mut dom = DomminanceInfo::new(&mut g, a);
        dom.compute_dominance_frontier();

        let res = dom.dominance_frontier(f).unwrap();
        let mut dom_front_f = HashSet::new();
        dom_front_f.insert(_g);

        assert_eq!(dom_front_f.len(), res.len());
        for i in res.iter().zip(dom_front_f) {
            assert_eq!(*i.0, i.1);
        }

        let res = dom.dominance_frontier(d).unwrap();
        let mut dom_front_d = HashSet::new();
        dom_front_d.insert(e);

        assert_eq!(dom_front_d.len(), res.len());
        for i in res.iter().zip(dom_front_d) {
            assert_eq!(*i.0, i.1);
        }
    }
}
