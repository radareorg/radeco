//! Recovers high-level control-flow constructs from a control-flow graph.
//! Implements the algorithm described in
//! [*No More Gotos*](https://doi.org/10.14722/ndss.2015.23185)

#![allow(unused)]

mod condition;
mod graph_utils;
#[cfg(test)]
mod tests;

use self::condition::*;

use petgraph::algo::dominators;
use petgraph::stable_graph::{EdgeIndex, NodeIndex, StableDiGraph};
use petgraph::visit::*;
use petgraph::Incoming;

use fixedbitset::FixedBitSet;

use std::collections::HashMap;
use std::hash::Hash;
use std::iter;
use std::mem;

#[derive(Debug)]
struct ControlFlowGraph<'cd> {
    graph: StableDiGraph<AstNode<'cd>, Option<SimpleCondition>>,
    entry: NodeIndex,
    cctx: ConditionContext<'cd, SimpleCondition>,
}

#[derive(Debug)]
enum AstNode<'cd> {
    BasicBlock(String), // XXX
    Seq(Vec<AstNode<'cd>>),
    Cond(Condition<'cd>, Box<AstNode<'cd>>, Option<Box<AstNode<'cd>>>),
    Loop(LoopType<'cd>, Box<AstNode<'cd>>),
    Switch(Variable, Vec<(ValueSet, AstNode<'cd>)>, Box<AstNode<'cd>>),
}

#[derive(Debug)]
enum LoopType<'cd> {
    PreChecked(Condition<'cd>),
    PostChecked(Condition<'cd>),
    Endless,
}

type Variable = (); // XXX
type ValueSet = (); // XXX

#[derive(Debug, Clone)]
struct SimpleCondition(String); // XXX

type Condition<'cd> = condition::BaseCondition<'cd, SimpleCondition>;

impl<'cd> ControlFlowGraph<'cd> {
    fn structure_whole(mut self) -> AstNode<'cd> {
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
            "structuring acyclic sese region: {:?} ==> {:?}",
            self.graph[header], self.graph[successor],
        );

        let (reaching_conds, mut region_topo_order) =
            self.reaching_conditions(header, |n| n == successor);
        // slice includes `successor`, but its not actually part of the region.
        let _popped = region_topo_order.pop();
        debug_assert!(_popped == Some(successor));

        // remove all region nodes from the cfg and add them to an AstNode::Seq
        let repl_ast: Vec<_> = region_topo_order
            .iter()
            .map(|&n| {
                let reaching_cond = reaching_conds[&n];
                let n_ast = if n == header {
                    // we don't want to remove `header` since that will also remove
                    // incoming edges, which we need to keep
                    // instead we replace it with a dummy value that will be
                    // later replaced with the actual value
                    mem::replace(&mut self.graph[header], AstNode::Seq(Vec::new()))
                } else {
                    self.graph.remove_node(n).unwrap()
                };
                AstNode::Cond(reaching_cond, Box::new(n_ast), None)
            })
            .collect();
        println!("  ast: {:#?}", repl_ast);
        mem::replace(&mut self.graph[header], AstNode::Seq(repl_ast));

        // the region's successor is still this node's successor.
        self.graph.add_edge(header, successor, None);
    }

    /// Computes the reaching condition for every node in the graph slice from
    /// `start` to `end_set`. Also returns a topological ordering of that slice.
    fn reaching_conditions<F: FilterNode<NodeIndex>>(
        &self,
        start: NodeIndex,
        end_set: F,
    ) -> (HashMap<NodeIndex, Condition<'cd>>, Vec<NodeIndex>) {
        let (slice_nodes, slice_edges, slice_topo_order) =
            graph_utils::slice(&self.graph, start, end_set);
        // {Node, Edge}Filtered don't implement IntoNeighborsDirected :(
        // https://github.com/bluss/petgraph/pull/219
        // Also EdgeFiltered<Reversed<_>, _> isn't Into{Neighbors, Edges}
        // because Reversed<_> isn't IntoEdges

        let mut ret = HashMap::with_capacity(slice_topo_order.len());

        {
            let mut iter = slice_topo_order.iter();
            let _first = iter.next();
            debug_assert!(_first == Some(&start));
            ret.insert(start, self.cctx.mk_true());

            for &n in iter {
                let reach_cond = self.cctx.mk_or_iter(
                    // manually restrict to slice
                    self.graph
                        .edges_directed(n, Incoming)
                        .filter(|e| slice_edges[e.id().index()])
                        .map(|e| {
                            // TODO: can we remove this edge to avoid cloning?
                            if let Some(ec) = &self.graph[e.id()] {
                                self.cctx
                                    .mk_and(ret[&e.source()], self.cctx.mk_simple(ec.clone()))
                            } else {
                                ret[&e.source()]
                            }
                        }),
                );
                let _old = ret.insert(n, reach_cond);
                debug_assert!(_old.is_none());
            }
        }

        (ret, slice_topo_order)
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
