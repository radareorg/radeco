// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Implements Dominance frontier and dominance tree computation.

use std::collections::{HashMap, HashSet};
use petgraph::graph::Graph;
use petgraph::graph;
use petgraph::EdgeDirection;

use middle::dot::{DotAttrBlock, GraphDot};
use super::index::InternalIndex;

#[derive(Clone, Debug)]
/// Depth first visitor that stores pre- and post- order traversal over a
/// Graph.
pub struct DFSVisitor {
    post_order: Vec<graph::NodeIndex>,
    pre_order: Vec<graph::NodeIndex>,
    visited: Vec<graph::NodeIndex>,
}

impl DFSVisitor {
    pub fn new() -> DFSVisitor {
        DFSVisitor {
            post_order: Vec::new(),
            pre_order: Vec::new(),
            visited: Vec::new(),
        }
    }

    pub fn dfs<N, E>(&mut self, g: &Graph<N, E>, node: graph::NodeIndex) {
        if self.visited.contains(&node) {
            return;
        }

        self.pre_order.push(node.clone());
        let direction = EdgeDirection::Outgoing;
        let neighbors_iter = g.neighbors_directed(node, direction)
                              .collect::<Vec<graph::NodeIndex>>();

        for n in neighbors_iter.iter() {
            self.dfs(g, n.clone());
        }

        self.visited.push(node.clone());
        self.post_order.push(node.clone());
    }

    pub fn post_order(&self) -> Vec<graph::NodeIndex> {
        self.post_order.clone()
    }

    pub fn pre_order(&self) -> Vec<graph::NodeIndex> {
        self.pre_order.clone()
    }
}

#[derive(Clone, Debug)]
/// Wrapper struct that consolidates the Dom and PostDom information for a
/// Graph.
pub struct DomInfo {
    dom: Option<DomTree>,
    postdom: Option<DomTree>,
}

impl DomInfo {
    pub fn new() -> DomInfo {
        DomInfo {
            dom: None,
            postdom: None,
        }
    }

    /// Constructs the DomTree and computes the dominance frontier for `g`.
    pub fn build_dom_tree<N, E>(&mut self, g: &Graph<N, E>, i: graph::NodeIndex)
        where N: Clone,
              E: Clone
    {

        let mut dom = DomTree::build_dom_tree(g, i);
        dom.compute_dominance_frontier();
        self.dom = Some(dom);
    }

    /// Constructs the Post-DomTree and computes the postdominance frontier for `g`.
    pub fn build_postdom_tree<N, E>(&mut self, g: &Graph<N, E>, i: graph::NodeIndex)
        where N: Clone,
              E: Clone
    {

        let mut _g = (*g).clone();
        _g.reverse();
        let mut postdom = DomTree::build_dom_tree(&_g, i);
        postdom.compute_dominance_frontier();
        self.postdom = Some(postdom);
    }

    /// Returns all the dominators of the node with graph::NodeIndex `i`.
    pub fn doms(&self, i: graph::NodeIndex) -> Vec<graph::NodeIndex> {
        match self.dom {
            None => panic!("dom not computed."),
            Some(ref d) => d.doms(i),
        }
    }

    /// Returns the immediate dominator of the node with graph::NodeIndex `i`.
    pub fn idom(&self, i: graph::NodeIndex) -> graph::NodeIndex {
        match self.dom {
            None => panic!("dom not computed."),
            Some(ref d) => d.idom(i),
        }
    }

    /// Returns all nodes in the dominance frontier of `i`.
    pub fn dom_frontier(&self, i: graph::NodeIndex) -> HashSet<graph::NodeIndex> {
        match self.dom {
            None => panic!("dom not computed."),
            Some(ref d) => d.dom_frontier(i),
        }
    }

    /// Returns all postdominators of `i`.
    pub fn postdoms(&self, i: graph::NodeIndex) -> Vec<graph::NodeIndex> {
        match self.postdom {
            None => panic!("postdom not computed."),
            Some(ref d) => d.doms(i),
        }
    }

    /// Returns the immediate postdominator of `i`.
    pub fn ipostdom(&self, i: graph::NodeIndex) -> graph::NodeIndex {
        match self.postdom {
            None => panic!("postdom not computed."),
            Some(ref d) => d.idom(i),
        }
    }

    /// Returns all nodes in the postdominance frontier of `i`.
    pub fn postdom_frontier(&self, i: graph::NodeIndex) -> HashSet<graph::NodeIndex> {
        match self.postdom {
            None => panic!("postdom not computed."),
            Some(ref d) => d.dom_frontier(i),
        }
    }
}

#[derive(Clone, Debug)]
/// Stores dominator information for a graph.
///
/// Provides finer grained control over the analysis of a Graph and base for
/// DomInfo.
///
/// Module level documentation provides more information.
pub struct DomTree {
    idom: Vec<InternalIndex>,
    rmap: HashMap<graph::NodeIndex, InternalIndex>,
    g: Graph<graph::NodeIndex, u8>,
    preds_map: HashMap<graph::NodeIndex, Vec<InternalIndex>>,
    dom_frontier: Option<HashMap<graph::NodeIndex, HashSet<graph::NodeIndex>>>,
}

impl DomTree {
    fn new() -> DomTree {
        DomTree {
            idom: Vec::new(),
            rmap: HashMap::new(),
            g: Graph::new(),
            preds_map: HashMap::new(),
            dom_frontier: None,
        }
    }

    pub fn graph(&self) -> &Graph<graph::NodeIndex, u8> {
        &self.g
    }

    pub fn doms(&self, i: graph::NodeIndex) -> Vec<graph::NodeIndex> {
        assert!(self.idom.len() > 0,
                "Call to DomTree::doms before 
									  DomTree::build_dom_tree.");

        let internal_index = self.rmap.get(&i).unwrap();
        let mut idom = *internal_index;
        let mut doms = Vec::<InternalIndex>::new();
        while idom != self.idom[idom] {
            doms.push(idom.clone());
            idom = self.idom[idom];
        }

        doms.push(idom.clone());
        doms.iter().map(|d| d.external()).collect()
    }

    pub fn idom(&self, i: graph::NodeIndex) -> graph::NodeIndex {
        assert!(self.idom.len() > 0,
                "Call to DomTree::idom before 
				DomTree::build_dom_tree.");

        let internal_index = self.rmap.get(&i).unwrap();
        let internal_node = self.idom[*internal_index];
        internal_node.external()
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

    pub fn build_dom_tree<N, E>(g: &Graph<N, E>, start_node: graph::NodeIndex) -> DomTree
        where N: Clone,
              E: Clone
    {

        let mut tree = DomTree::new();

        {
            let idom = &mut (tree.idom);
            let rmap = &mut (tree.rmap);
            let dom_tree = &mut (tree.g);
            let preds_map = &mut (tree.preds_map);
            let mut v = DFSVisitor::new();
            let node_count = g.node_count();

            // compute postorder numbering.
            v.dfs(&g, start_node);
            for i in 0..node_count {
                v.dfs(&g, graph::NodeIndex::new(i));
            }

            // Tuple of (graph::NodeIndex, post-order numbering).
            let mut nodes_iter = v.post_order()
                                  .iter()
                                  .cloned()
                                  .zip(0..node_count)
                                  .collect::<Vec<_>>();
            let invalid_index = InternalIndex::new(node_count, graph::NodeIndex::new(node_count));
            let mut start_index = invalid_index;
            let mut dom_tree_nodes: Vec<InternalIndex> = Vec::new();

            // Initializations for nodes.
            for item in nodes_iter.iter() {
                let i = InternalIndex::new(item.1, item.0.clone());

                if item.0 == start_node {
                    start_index = i;
                    idom.push(start_index);
                } else {
                    idom.push(invalid_index);
                }

                dom_tree.add_node(graph::NodeIndex::new(item.1));
                dom_tree_nodes.push(i);
                rmap.insert(item.0.clone(), i);
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
                    let node: graph::NodeIndex = n.0;
                    let preds_iter = g.neighbors_directed(node.clone(), EdgeDirection::Incoming)
                                      .map(|x| rmap.get(&x).unwrap().clone());
                    let preds = preds_map.entry(node.clone())
                                         .or_insert(preds_iter.collect::<Vec<_>>());
                    let mut new_idom = invalid_index;
                    for p in preds.iter() {
                        if idom[*p] < invalid_index {
                            new_idom = *p;
                            break;
                        }
                    }
                    // Make sure we found a node.
                    assert!(new_idom != invalid_index);
                    for p in preds.iter() {
                        if idom[*p] != invalid_index {
                            new_idom = DomTree::intersect(&idom, &new_idom, p);
                        }
                    }
                    if idom[n.1] != new_idom {
                        idom[n.1] = new_idom;
                        changed = true;
                    }
                }
            }

            // Add edges to the graph based on the idom information.
            for i in dom_tree_nodes.iter() {
                if *i == idom[*i] {
                    continue;
                }
                dom_tree.add_edge(idom[*i].external(), i.external(), 0);
            }
        }

        return tree;
    }

    pub fn compute_dominance_frontier(&mut self) {
        assert!(self.idom.len() > 0,
                "Call to DomTree::compute_dominance_frontier 
				before DomTree::build_dom_tree.");

        let node_count = self.idom.len();
        let mut frontier_map = HashMap::<graph::NodeIndex, HashSet<graph::NodeIndex>>::new();
        for node in (0..node_count).map(|x| graph::NodeIndex::new(x)) {
            let internal_index = self.rmap.get(&node).unwrap();
            let preds = self.preds_map.get(&node).unwrap();

            if preds.len() < 2 {
                continue;
            }

            for p in preds {
                let mut runner = *p;
                while runner != self.idom[*internal_index] {
                    let runner_index = runner.external();
                    frontier_map.entry(runner_index).or_insert(HashSet::new()).insert(node);

                    runner = self.idom[runner];
                }
            }
        }

        self.dom_frontier = Some(frontier_map);
    }

    pub fn dom_frontier(&self, n: graph::NodeIndex) -> HashSet<graph::NodeIndex> {
        assert!(self.dom_frontier != None, "Uninitialized dom_frontier.");
        self.dom_frontier.clone().unwrap().get(&n).unwrap().clone()
    }
}

/// ////////////////////////////////////////////////////////////////////////////
/// / Implementation of Traits to emit dot for dom.
/// ////////////////////////////////////////////////////////////////////////////

impl GraphDot for DomTree {
	type NodeIndex = graph::NodeIndex;
	type EdgeIndex = u8;

    fn node_count(&self) -> usize {
        self.g.node_count()
    }

    fn edge_count(&self) -> usize {
        self.g.edge_count()
    }

    fn node_index_new(i: usize) -> Self::NodeIndex {
        graph::NodeIndex::new(i)
    }

    fn edge_index_new(i: usize) -> Self::EdgeIndex {
        i as u8
    }

    fn configure(&self) -> String {
        format!("digraph cfg {{\nsplines=\"true\";\n")
    }

    fn nodes(&self) -> Vec<Self::NodeIndex> {
        (0..self.node_count()).map(|n| graph::NodeIndex::new(n)).collect()
    }

    fn edge_source(&self, i: &Self::EdgeIndex) -> Self::NodeIndex {
        self.g.raw_edges()[*i as usize].source()
    }

    fn edge_target(&self, i: &Self::EdgeIndex) -> Self::NodeIndex {
        self.g.raw_edges()[*i as usize].target()
    }

    fn edge_attrs(&self, _: &Self::EdgeIndex) -> DotAttrBlock {
        DotAttrBlock::Raw("".to_string())
    }

    fn node_attrs(&self, i: &Self::NodeIndex) -> DotAttrBlock {
        let tmp = format!("n{}", i.index());
        DotAttrBlock::Raw(format!("[label={}]", tmp))
    }
}


#[cfg(test)]
mod test {

    use super::*;
    use petgraph::graph::{Graph, NodeIndex};
    use std::collections::HashSet;

    #[test]
    fn dfs() {
        let mut g = Graph::<u64, u64>::new();
        let mut n: Vec<NodeIndex> = Vec::new();
        for i in 0..13 {
            n.push(g.add_node(i));
        }

        g.add_edge(n[0], n[6], 0);
        g.add_edge(n[0], n[1], 0);
        g.add_edge(n[0], n[5], 0);
        g.add_edge(n[5], n[4], 0);
        g.add_edge(n[2], n[3], 0);
        g.add_edge(n[2], n[0], 0);
        g.add_edge(n[3], n[5], 0);
        g.add_edge(n[6], n[4], 0);
        g.add_edge(n[6], n[9], 0);
        g.add_edge(n[9], n[10], 0);
        g.add_edge(n[9], n[11], 0);
        g.add_edge(n[9], n[12], 0);
        g.add_edge(n[11], n[12], 0);
        g.add_edge(n[8], n[7], 0);
        g.add_edge(n[7], n[6], 0);

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
