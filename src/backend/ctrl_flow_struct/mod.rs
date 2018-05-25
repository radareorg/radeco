//! Recovers high-level control-flow constructs from a control-flow graph.
//! Implements the algorithm described in
//! [*No More Gotos*](https://doi.org/10.14722/ndss.2015.23185)

#![allow(unused)]

mod graph_utils;
#[cfg(test)]
mod tests;

use petgraph::algo::dominators;
use petgraph::stable_graph::{EdgeIndex, NodeIndex, StableDiGraph};
use petgraph::visit::*;

use fixedbitset::FixedBitSet;

use std::collections::HashMap;
use std::hash::Hash;
use std::iter;
use std::mem;

#[derive(Debug)]
struct ControlFlowGraph {
    graph: StableDiGraph<AstNode, SimpleCondition>,
    entry: NodeIndex,
}

#[derive(Debug)]
enum AstNode {
    BasicBlock(String), // XXX
    Seq(Vec<AstNode>),
    Cond(Condition, Box<AstNode>, Option<Box<AstNode>>),
    Loop(LoopType, Box<AstNode>),
    Switch(Variable, Vec<(ValueSet, AstNode)>, Box<AstNode>),
}

#[derive(Debug)]
enum LoopType {
    PreChecked(Condition),
    PostChecked(Condition),
    Endless,
}

type Variable = (); // XXX
type ValueSet = (); // XXX

#[derive(Debug)]
struct SimpleCondition(String); // XXX

#[derive(Debug)]
enum Condition {
    Simple(SimpleCondition),
    And(Vec<Condition>),
    Or(Vec<Condition>),
}

impl ControlFlowGraph {
    fn structure_whole(mut self) -> AstNode {
        let mut backedges = HashMap::new();
        let mut podfs_trace = Vec::new();
        graph_utils::depth_first_search(&self.graph, self.entry, |ev| {
            use self::graph_utils::DfsEvent::*;
            match ev {
                BackEdge(e) => backedges
                    .entry(e.target())
                    .or_insert(Vec::new())
                    .push(e.id()),
                Finish(n) => podfs_trace.push(n),
                _ => (),
            }
        });

        for n in podfs_trace {
            if let Some(backedges) = backedges.get(&n) {
                // loop
                // TODO
                println!("cycle: {:?}", self.graph[n]);
                for &backedge in backedges {
                    println!(
                        "  latch: {:?}",
                        self.graph[self.graph.edge_endpoints(backedge).unwrap().0],
                    );
                }
            } else {
                // acyclic
                let region = self.dominates_set(n);
                // single-block regions aren't interesting
                if region.count_ones(..) > 1 {
                    let succs = self.successors_of_set(&region);
                    let mut region_successors = succs.difference(&region);
                    if let Some(succ) = region_successors.next() {
                        if region_successors.next().is_none() {
                            // sese region
                            self.structure_acyclic_sese_region(n, NodeIndex::new(succ));
                        }
                    }
                }
            }
        }
        unimplemented!()
    }

    /// Convert the acyclic, single entry, single exit region bound by `header`
    /// and `successor` into an `AstNode`.
    fn structure_acyclic_sese_region(&mut self, header: NodeIndex, successor: NodeIndex) -> () {
        println!(
            "acyclic sese region: {:?} ==> {:?}",
            self.graph[header], self.graph[successor],
        );

        let mut region_postorder: Vec<_> = {
            let mut visitor = DfsPostOrder::new(&self.graph, header);
            // stop dfs at `successor`
            visitor.discovered.visit(successor);
            visitor.iter(&self.graph).collect()
        };

        // remove all region nodes from the cfg and add them to an AstNode::Seq
        let repl_ast: Vec<_> = region_postorder
            .into_iter()
            .rev()
            .map(|n| {
                let reaching_cond = Condition::Simple(SimpleCondition("".to_owned())); // XXX
                let n_ast = if n == header {
                    // we don't want to remove `header` since that will also remove
                    // incoming edges, which we need to keep
                    // instead we replace it with a dummy value that will be
                    // later replaced with the actual value
                    mem::replace(&mut self.graph[header], AstNode::Seq(Vec::new()))
                } else {
                    self.graph.remove_node(n).unwrap()
                };
                let n_cond_ast = AstNode::Cond(reaching_cond, Box::new(n_ast), None);
                println!("  {:?}", n_cond_ast);
                n_cond_ast
            })
            .collect();
        mem::replace(&mut self.graph[header], AstNode::Seq(repl_ast));

        // the region's successor is still this node's successor.
        self.graph
            .add_edge(header, successor, SimpleCondition("".to_owned()));
    }

    /// Returns the set of nodes that `h` dominates.
    fn dominates_set(&self, h: NodeIndex) -> FixedBitSet {
        let mut ret = self.mk_node_set();
        // TODO: this is horrifically inefficient
        let doms = dominators::simple_fast(&self.graph, self.entry);
        for (n, _) in self.graph.node_references() {
            if doms
                .dominators(n)
                .map(|mut ds| ds.any(|d| d == h))
                .unwrap_or(false)
            {
                ret.put(n.index());
            }
        }
        ret
    }

    /// Returns the union of the successors of each node in `set`.
    fn successors_of_set(&self, set: &FixedBitSet) -> FixedBitSet {
        let mut ret = self.mk_node_set();
        for ni in set.ones() {
            for succ in self.graph.neighbors(NodeIndex::new(ni)) {
                ret.put(succ.index());
            }
        }
        ret
    }

    fn mk_node_set(&self) -> FixedBitSet {
        FixedBitSet::with_capacity(self.graph.node_bound())
    }
}
