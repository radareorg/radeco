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
    graph: StableDiGraph<CfgNode<'cd>, Option<SimpleCondition>>,
    entry: NodeIndex,
    cctx: ConditionContext<'cd, SimpleCondition>,
}

#[derive(Debug)]
enum CfgNode<'cd> {
    /// out-degree <= 1
    Code(AstNode<'cd>),
    /// out-degree >= 2
    Condition,
    /// only appears temporarily in the middle of algorithms
    Dummy(&'static str),
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
        let mut backedge_map = HashMap::new();
        let mut podfs_trace = Vec::new();
        graph_utils::depth_first_search(&self.graph, self.entry, |ev| {
            use self::graph_utils::DfsEvent::*;
            match ev {
                BackEdge(e) => backedge_map
                    .entry(e.target())
                    .or_insert(Vec::new())
                    .push(e.id()),
                Finish(n) => podfs_trace.push(n),
                _ => (),
            }
        });

        for n in podfs_trace {
            if let Some(backedges) = backedge_map.get(&n) {
                // loop
                println!("cycle: {:?}", self.graph[n]);
                for &backedge in backedges {
                    println!(
                        "  latch: {:?}",
                        self.graph[self.graph.edge_endpoints(backedge).unwrap().0],
                    );
                }
                let mut latch_nodes: FixedBitSet = backedges
                    .iter()
                    .map(|&e| self.graph.edge_endpoints(e).unwrap().0.index())
                    .collect();
                let (mut loop_nodes, _, _) = graph_utils::slice(&self.graph, n, latch_nodes);
                // retarget backedges to a "loop continue" node
                let loop_continue = self.graph.add_node(CfgNode::Dummy("loop continue"));
                for &backedge in backedges {
                    graph_utils::retarget_edge(&mut self.graph, backedge, loop_continue);
                }
                let header = self.funnel_abnormal_entries(n, &loop_nodes);
                // TODO
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
            .filter_map(|&n| {
                let cfg_node = if n == header {
                    // we don't want to remove `header` since that will also remove
                    // incoming edges, which we need to keep
                    // instead we replace it with a dummy value that will be
                    // later replaced with the actual value
                    mem::replace(&mut self.graph[header], CfgNode::Dummy("replaced header"))
                } else {
                    self.graph.remove_node(n).unwrap()
                };
                if let CfgNode::Code(ast) = cfg_node {
                    let reaching_cond = reaching_conds[&n];
                    Some(AstNode::Cond(reaching_cond, Box::new(ast), None))
                } else {
                    None
                }
            })
            .collect();
        println!("  ast: {:#?}", repl_ast);
        mem::replace(
            &mut self.graph[header],
            CfgNode::Code(AstNode::Seq(repl_ast)),
        );

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

    /// Transforms the loop into a single-entry loop.
    /// Returns the new loop header.
    fn funnel_abnormal_entries(
        &mut self,
        header: NodeIndex,
        loop_nodes: &FixedBitSet,
    ) -> NodeIndex {
        let mut entry_map = HashMap::new();
        for ni in loop_nodes.ones() {
            let n = NodeIndex::new(ni);
            for e in self.graph.edges_directed(n, Incoming) {
                if !loop_nodes[e.source().index()] {
                    entry_map.entry(n).or_insert(Vec::new()).push(e.id());
                }
            }
        }
        println!("entries: {:?}", entry_map);
        // loop must be reachable, so the header must have entries
        let header_entries = entry_map.remove(&header).unwrap();
        debug_assert!(!header_entries.is_empty());
        let abnormal_entry_map = entry_map;
        if abnormal_entry_map.is_empty() {
            // no abnormal entries
            return header;
        }
        let abnormal_entry_iter = (1..).zip(&abnormal_entry_map);

        let struct_var = "i".to_owned(); // XXX

        // make condition cascade
        let new_header = {
            let abnormal_entry_iter = abnormal_entry_iter.clone().map(|(n, (&t, _))| (n, t));

            let dummy_preheader = self.graph.add_node(CfgNode::Dummy("loop \"preheader\""));

            let mut prev_cascade_node = dummy_preheader;
            let mut prev_entry_target = header;
            let mut prev_out_cond = None;
            let mut prev_entry_num = 0;

            // we make the condition node for the *previous* entry target b/c
            // the current one might be the last one, which shouldn't get a
            // condition node because it's the only possible target
            for (entry_num, entry_target) in abnormal_entry_iter {
                let cascade_node = self.graph.add_node(CfgNode::Condition);
                self.graph
                    .add_edge(prev_cascade_node, cascade_node, prev_out_cond);
                self.graph.add_edge(
                    cascade_node,
                    prev_entry_target,
                    Some(SimpleCondition(format!(
                        "{} == {}",
                        struct_var, prev_entry_num
                    ))),
                ); // XXX
                let struct_reset = self.graph.add_node(CfgNode::Code(AstNode::BasicBlock(
                    format!("{} = 0", struct_var),
                ))); // XXX
                self.graph.add_edge(struct_reset, entry_target, None);
                prev_cascade_node = cascade_node;
                prev_entry_target = struct_reset;
                prev_out_cond = Some(SimpleCondition(format!(
                    "{} != {}",
                    struct_var, prev_entry_num
                ))); // XXX
                prev_entry_num = entry_num;
            }

            self.graph
                .add_edge(prev_cascade_node, prev_entry_target, prev_out_cond);

            // we always add an edge from dummy_preheader
            let new_header = self.graph.neighbors(dummy_preheader).next().unwrap();
            self.graph.remove_node(dummy_preheader);
            new_header
        };

        // redirect entries
        for (entry_num, entry_edges) in
            iter::once((0, &header_entries)).chain(abnormal_entry_iter.map(|(n, (_, e))| (n, e)))
        {
            let struct_assign = self
                .graph
                .add_node(CfgNode::Code(AstNode::BasicBlock(format!(
                    "{} = {}",
                    struct_var, entry_num
                )))); // XXX
            self.graph.add_edge(struct_assign, new_header, None);
            for &entry_edge in entry_edges {
                graph_utils::retarget_edge(&mut self.graph, entry_edge, struct_assign);
            }
        }

        new_header
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
