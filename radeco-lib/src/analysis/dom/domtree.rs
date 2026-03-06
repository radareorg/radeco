// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Implements Dominance frontier and dominance tree computation.
//!
//! From the Wikipedia article on [Dominator (graph theory)](https://en.wikipedia.org/wiki/Dominator_(graph_theory)):
//!
//! ```text
//! A node `d` of a control-flow graph dominates a node `n` if every path from the entry node to `n` must go through `d`.
//!
//! The `dominance frontier` of a node `d` is the set of all nodes `n[i]` such that `d` dominates an immediate predecessor of `n[i]`, but `d` does not strictly dominate `n[i]`.
//! It is the set of nodes where `d`'s dominance stops.
//!
//! A `dominator tree` is a tree where each node's children are those nodes it immediately dominates.
//! The start node is the root of the tree.
//! ```

use petgraph::graph::Graph;
use petgraph::{graph, EdgeDirection};
use std::collections::{HashMap, HashSet};

use super::index::InternalIndex;
use crate::analysis::error::AnalysisError;
use crate::middle::dot::{DotAttrBlock, GraphDot};

/// Depth first visitor that stores pre- and post- order traversal over a
/// [Graph].
#[derive(Clone, Debug)]
pub struct DFSVisitor {
    post_order: Vec<graph::NodeIndex>,
    pre_order: Vec<graph::NodeIndex>,
    visited: Vec<graph::NodeIndex>,
}

impl DFSVisitor {
    /// Creates a new [DFSVisitor].
    pub const fn new() -> DFSVisitor {
        DFSVisitor {
            post_order: Vec::new(),
            pre_order: Vec::new(),
            visited: Vec::new(),
        }
    }

    /// Performs a depth-first-search on the [Graph] starting at the `node` index.
    pub fn dfs<N, E>(&mut self, g: &Graph<N, E>, node: graph::NodeIndex) {
        if self.visited.contains(&node) {
            return;
        }

        self.pre_order.push(node);
        let direction = EdgeDirection::Outgoing;
        let neighbors_iter = g
            .neighbors_directed(node, direction)
            .collect::<Vec<graph::NodeIndex>>();

        for n in &neighbors_iter {
            self.dfs(g, *n);
        }

        self.visited.push(node);
        self.post_order.push(node);
    }

    /// Gets a reference to the post-search order of node indices.
    pub fn post_order(&self) -> &[graph::NodeIndex] {
        &self.post_order
    }

    /// Gets a reference to the the pre-search order of node indices.
    pub fn pre_order(&self) -> &[graph::NodeIndex] {
        &self.pre_order
    }
}

impl Default for DFSVisitor {
    fn default() -> Self {
        Self::new()
    }
}

/// Wrapper struct that consolidates the `Dom` and `PostDom` information for a
/// [Graph].
#[derive(Clone, Debug)]
pub struct DomInfo {
    dom: Option<DomTree>,
    postdom: Option<DomTree>,
}

impl DomInfo {
    /// Creates a new [DomInfo].
    pub const fn new() -> DomInfo {
        DomInfo {
            dom: None,
            postdom: None,
        }
    }

    /// Constructs the DomTree and computes the dominance frontier for `g`.
    pub fn build_dom_tree<N, E>(
        &mut self,
        g: &Graph<N, E>,
        i: graph::NodeIndex,
    ) -> Result<(), AnalysisError>
    where
        N: Clone,
        E: Clone,
    {
        DomTree::build_dom_tree(g, i).and_then(|mut dom| {
            dom.compute_dominance_frontier().map(|_| {
                self.dom = Some(dom);
            })
        })
    }

    /// Constructs the Post-DomTree and computes the postdominance frontier for `g`.
    pub fn build_postdom_tree<N, E>(
        &mut self,
        g: &Graph<N, E>,
        i: graph::NodeIndex,
    ) -> Result<(), AnalysisError>
    where
        N: Clone,
        E: Clone,
    {
        let mut g_ = (*g).clone();
        g_.reverse();
        DomTree::build_dom_tree(&g_, i).and_then(|mut postdom| {
            postdom.compute_dominance_frontier().map(|_| {
                self.postdom = Some(postdom);
            })
        })
    }

    /// Returns all the dominators of the node with graph::NodeIndex `i`.
    pub fn doms(&self, i: graph::NodeIndex) -> Result<Vec<graph::NodeIndex>, AnalysisError> {
        match self.dom {
            None => Err(AnalysisError::domtree("dom not computed.")),
            Some(ref d) => d.doms(i),
        }
    }

    /// Returns the immediate dominator of the node with graph::NodeIndex `i`.
    pub fn idom(&self, i: graph::NodeIndex) -> Result<graph::NodeIndex, AnalysisError> {
        match self.dom {
            None => Err(AnalysisError::domtree("dom not computed.")),
            Some(ref d) => d.idom(i),
        }
    }

    /// Returns all nodes in the dominance frontier of `i`.
    pub fn dom_frontier(
        &self,
        i: graph::NodeIndex,
    ) -> Result<&HashSet<graph::NodeIndex>, AnalysisError> {
        match self.dom {
            None => Err(AnalysisError::domtree("dom not computed.")),
            Some(ref d) => d.dom_frontier(i),
        }
    }

    /// Returns all postdominators of `i`.
    pub fn postdoms(&self, i: graph::NodeIndex) -> Result<Vec<graph::NodeIndex>, AnalysisError> {
        match self.postdom {
            None => Err(AnalysisError::domtree("postdom not computed.")),
            Some(ref d) => d.doms(i),
        }
    }

    /// Returns the immediate postdominator of `i`.
    pub fn ipostdom(&self, i: graph::NodeIndex) -> Result<graph::NodeIndex, AnalysisError> {
        match self.postdom {
            None => Err(AnalysisError::domtree("postdom not computed.")),
            Some(ref d) => d.idom(i),
        }
    }

    /// Returns all nodes in the postdominance frontier of `i`.
    pub fn postdom_frontier(
        &self,
        i: graph::NodeIndex,
    ) -> Result<&HashSet<graph::NodeIndex>, AnalysisError> {
        match self.postdom {
            None => Err(AnalysisError::domtree("postdom not computed.")),
            Some(ref d) => d.dom_frontier(i),
        }
    }
}

impl Default for DomInfo {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug)]
/// Stores dominator information for a graph.
///
/// Provides finer grained control over the analysis of a Graph and base for
/// `DomInfo`.
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
    /// Creates a new [DomTree].
    pub fn new() -> DomTree {
        DomTree {
            idom: Vec::new(),
            rmap: HashMap::new(),
            g: Graph::new(),
            preds_map: HashMap::new(),
            dom_frontier: None,
        }
    }

    /// Gets a reference to the [DomTree] control-flow [Graph].
    pub fn graph(&self) -> &Graph<graph::NodeIndex, u8> {
        &self.g
    }

    /// Gets a list of all the dominator node indices.
    pub fn doms(&self, i: graph::NodeIndex) -> Result<Vec<graph::NodeIndex>, AnalysisError> {
        if self.idom.is_empty() {
            return Err(AnalysisError::domtree(
                "Call to DomTree::doms before DomTree::build_dom_tree.",
            ));
        }

        let internal_index = self.rmap[&i];
        let mut idom = internal_index;
        let mut doms = Vec::<graph::NodeIndex>::new();

        while idom != self.idom[idom] {
            doms.push(idom.external());
            idom = self.idom[idom];
        }

        doms.push(idom.external());
        Ok(doms)
    }

    /// Gets the internal dominator node index.
    pub fn idom(&self, i: graph::NodeIndex) -> Result<graph::NodeIndex, AnalysisError> {
        if self.idom.is_empty() {
            return Err(AnalysisError::domtree(
                "Call to DomTree::idom before DomTree::build_dom_tree.",
            ));
        }

        let internal_index = self.rmap[&i];
        Ok(self.idom[internal_index].external())
    }

    fn intersect(idom: &[InternalIndex], i: &InternalIndex, j: &InternalIndex) -> InternalIndex {
        let mut f1 = *i;
        let mut f2 = *j;
        while f1 != f2 {
            while f1 < f2 {
                f1 = idom[f1.index()];
            }
            while f2 < f1 {
                f2 = idom[f2.index()];
            }
        }
        f1
    }

    /// Builds the `dominator tree` from the provided [Graph] starting at the `start_node`.
    pub fn build_dom_tree<N, E>(
        g: &Graph<N, E>,
        start_node: graph::NodeIndex,
    ) -> Result<DomTree, AnalysisError>
    where
        N: Clone,
        E: Clone,
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
            v.dfs(g, start_node);
            for i in 0..node_count {
                v.dfs(g, graph::NodeIndex::new(i));
            }

            // Tuple of (graph::NodeIndex, post-order numbering).
            let mut nodes_iter = v
                .post_order()
                .iter()
                .cloned()
                .zip(0..node_count)
                .collect::<Vec<_>>();
            let invalid_index = InternalIndex::new(node_count, graph::NodeIndex::new(node_count));
            let mut start_index = invalid_index;
            let mut dom_tree_nodes: Vec<InternalIndex> = Vec::new();

            // Initializations for nodes.
            for item in &nodes_iter {
                let i = InternalIndex::new(item.1, item.0);

                if item.0 == start_node {
                    start_index = i;
                    idom.push(start_index);
                } else {
                    idom.push(invalid_index);
                }

                dom_tree.add_node(graph::NodeIndex::new(item.1));
                dom_tree_nodes.push(i);
                rmap.insert(item.0, i);
            }
            {
                let tmp = Vec::<InternalIndex>::new();
                preds_map.insert(start_node, tmp);
            }

            nodes_iter.remove(start_index.index());
            nodes_iter.reverse();
            let mut changed = true;
            //TODO not to use dummy
            let dummy = InternalIndex::new(0, graph::NodeIndex::new(0));
            while changed {
                changed = false;
                for n in &nodes_iter {
                    let node: graph::NodeIndex = n.0;
                    let preds_iter = g
                        .neighbors_directed(node, EdgeDirection::Incoming)
                        .map(|x| {
                            rmap.get(&x).unwrap_or_else(|| {
                                radeco_err!("DomTree.build_dom_tree: Node not found");
                                &dummy
                            })
                        });
                    let preds = preds_map
                        .entry(node)
                        .or_insert_with(|| preds_iter.cloned().collect::<Vec<_>>());
                    let mut new_idom = invalid_index;
                    for p in preds.iter() {
                        if idom[*p] < invalid_index {
                            new_idom = *p;
                            break;
                        }
                    }
                    // Make sure we found a node.
                    if new_idom == invalid_index {
                        return Err(AnalysisError::domtree(format!(
                            "invalid domtree index: {}",
                            new_idom.index()
                        )));
                    }
                    for p in preds.iter() {
                        if idom[*p] != invalid_index {
                            new_idom = DomTree::intersect(idom, &new_idom, p);
                        }
                    }
                    if idom[n.1] != new_idom {
                        idom[n.1] = new_idom;
                        changed = true;
                    }
                }
            }

            // Add edges to the graph based on the idom information.
            for i in &dom_tree_nodes {
                if *i == idom[*i] {
                    continue;
                }
                dom_tree.add_edge(idom[*i].external(), i.external(), 0);
            }
        }

        Ok(tree)
    }

    /// Computes the [DomTree] of the `dominance frontier`.
    ///
    /// For more information, see the module-level documentation.
    pub fn compute_dominance_frontier(&mut self) -> Result<(), AnalysisError> {
        if self.idom.is_empty() {
            return Err(AnalysisError::domtree(
                "Call to DomTree::compute_dominance_frontier before DomTree::build_dom_tree.",
            ));
        }

        let node_count = self.idom.len();
        let mut frontier_map = HashMap::<graph::NodeIndex, HashSet<graph::NodeIndex>>::new();
        for node in (0..node_count).map(graph::NodeIndex::new) {
            let internal_index = self.rmap[&node];
            let preds = &self.preds_map[&node];

            if preds.len() < 2 {
                continue;
            }

            for p in preds {
                let mut runner = *p;
                while runner != self.idom[internal_index] {
                    let runner_index = runner.external();
                    frontier_map
                        .entry(runner_index)
                        .or_insert_with(HashSet::new)
                        .insert(node);

                    runner = self.idom[runner];
                }
            }
        }

        self.dom_frontier = Some(frontier_map);

        Ok(())
    }

    /// Gets a reference to the `dominance frontier` at the `n` node index.
    ///
    /// Returns an `Err` if:
    ///
    /// - the `dom_frontier` is unintialized
    /// - there is no `dom_frontier` at index `n`
    pub fn dom_frontier(
        &self,
        n: graph::NodeIndex,
    ) -> Result<&HashSet<graph::NodeIndex>, AnalysisError> {
        self.dom_frontier
            .as_ref()
            .ok_or(AnalysisError::domtree("Uninitialized dom_frontier"))
            .map(|d| d.get(&n))
            .and_then(|d| {
                d.ok_or(AnalysisError::domtree(format!(
                    "no dom_frontier at index: {}",
                    n.index()
                )))
            })
    }
}

impl Default for DomTree {
    fn default() -> Self {
        Self::new()
    }
}

///////////////////////////////////////////////////////////////////////////////
// Implementation of Traits to emit dot for dom.
//////////////////////////////////////////////////////////////////////////////

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
        "digraph cfg {{\nsplines=\"true\";\n".to_owned()
    }

    fn nodes(&self) -> Vec<Self::NodeIndex> {
        (0..self.node_count()).map(Self::NodeIndex::new).collect()
    }

    fn edges(&self) -> Vec<Self::EdgeIndex> {
        (0..self.edge_count())
            .map(|i| i as Self::EdgeIndex)
            .collect()
    }

    fn edge_source(&self, i: &Self::EdgeIndex) -> Self::NodeIndex {
        self.g.raw_edges()[*i as usize].source()
    }

    fn edge_target(&self, i: &Self::EdgeIndex) -> Self::NodeIndex {
        self.g.raw_edges()[*i as usize].target()
    }

    fn edge_attrs(&self, _: &Self::EdgeIndex) -> DotAttrBlock {
        DotAttrBlock::Raw(String::new())
    }

    fn node_attrs(&self, i: &Self::NodeIndex) -> DotAttrBlock {
        let tmp = format!("n{}", i.index());
        DotAttrBlock::Raw(format!("[label={tmp}]"))
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
        for node in n.iter().take(count).cloned() {
            dfs.dfs(&g, node);
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

        let dom = DomTree::build_dom_tree(&g, n0).unwrap();
        let doms_n1 = [n1, n7, n2, n0];
        let doms_n9 = [n9, n6, n5, n4, n2, n0];
        let doms_n3 = [n3, n2, n0];

        assert_eq!(dom.doms(n1).as_deref(), Ok(doms_n1.as_ref()));
        assert_eq!(dom.doms(n9).as_deref(), Ok(doms_n9.as_ref()));
        assert_eq!(dom.doms(n3).as_deref(), Ok(doms_n3.as_ref()));
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

        let mut dom = DomTree::build_dom_tree(&g, a).unwrap();
        dom.compute_dominance_frontier().unwrap();

        let res = dom.dom_frontier(f).unwrap();
        let mut dom_front_f = HashSet::new();
        dom_front_f.insert(_g);

        assert_eq!(dom_front_f.len(), res.len());
        for i in res.iter().zip(dom_front_f) {
            assert_eq!(*i.0, i.1);
        }

        let res = dom.dom_frontier(d).unwrap();
        let mut dom_front_d = HashSet::new();
        dom_front_d.insert(e);

        assert_eq!(dom_front_d.len(), res.len());
        for i in res.iter().zip(dom_front_d) {
            assert_eq!(*i.0, i.1);
        }
    }
}
