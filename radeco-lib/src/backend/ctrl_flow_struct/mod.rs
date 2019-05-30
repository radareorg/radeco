//! Recovers high-level control-flow constructs from a control-flow graph.
//! Implements the algorithm described in
//! [*No More Gotos*](https://doi.org/10.14722/ndss.2015.23185)

#![allow(dead_code)]

pub mod ast_context;
pub mod condition;
pub mod export;

mod ast;
mod dedup_conds;
mod graph_utils;
mod refinement;
#[cfg(test)]
mod test;

use self::ast::AstNode as AstNodeC;
use self::ast_context::*;
use self::graph_utils::ix_bit_set::IxBitSet;

use petgraph::prelude::*;
use petgraph::visit::{DfsPostOrder, NodeIndexable, Walker};

use std::collections::HashMap;
use std::fmt;
use std::iter;
use std::marker::PhantomData;
use std::mem;

/// Note: Conditions may be evaluated "eagerly". Thus, all conditions must always
/// be "safe" to evaluate, but may produce garbage.
pub struct ControlFlowGraph<'cd, A: AstContext> {
    graph: StableDiGraph<CfgNode<'cd, A>, CfgEdge>,
    entry: NodeIndex,
    cctx: CondContext<'cd, A>,
    actx: A,
}

type NodeSet = IxBitSet<NodeIndex>;
type EdgeSet = IxBitSet<EdgeIndex>;

pub enum CfgNode<'cd, A: AstContext> {
    /// out-degree <= 1
    Code(AstNode<'cd, A>),
    /// out-degree >= 2
    Condition(CondVar<'cd, A>),
    /// only appears temporarily in the middle of algorithms
    Dummy(&'static str),
}

#[derive(Copy, Clone, Debug)]
pub enum CfgEdge {
    True,
    False,
}

type CondVar<'cd, A> = condition::VarRef<'cd, <A as AstContext>::Condition>;
type Condition<'cd, A> = condition::Condition<'cd, <A as AstContext>::Condition>;
type CondContext<'cd, A> = condition::Context<'cd, <A as AstContext>::Condition>;
// hoping https://github.com/rust-lang/rust/issues/49683 lands soon
type AstNode<'cd, A> =
    ast::AstNode<<A as AstContext>::Block, Condition<'cd, A>, <A as AstContext>::Variable>;

impl<'cd, A: AstContextMut> ControlFlowGraph<'cd, A> {
    /// Preconditions:
    /// - `entry` must be a source
    /// - all nodes must be reachable from `entry`
    pub fn new(
        graph: StableDiGraph<CfgNode<'cd, A>, CfgEdge>,
        entry: NodeIndex,
        cctx: CondContext<'cd, A>,
        actx: A,
    ) -> Self {
        let ret = Self {
            graph,
            entry,
            cctx,
            actx,
        };
        ret.check();
        ret
    }

    #[cfg(not(debug_assertions))]
    fn check(&self) {}

    #[cfg(debug_assertions)]
    fn check(&self) {
        for n in self.graph.node_indices() {
            assert!(graph_utils::is_source(&self.graph, n) == (n == self.entry));
            match &self.graph[n] {
                CfgNode::Code(_) => assert!(self.graph.neighbors(n).count() <= 1),
                CfgNode::Condition(_) => assert!(self.graph.neighbors(n).count() == 2),
                CfgNode::Dummy(s) => panic!("found `CfgNode::Dummy({:?})`", s),
            }
        }
    }

    pub fn structure_whole(mut self) -> (AstNode<'cd, A>, A) {
        let mut loop_headers = NodeSet::new();
        let mut podfs_trace = Vec::new();
        graph_utils::depth_first_search(&self.graph, self.entry, |ev| {
            use self::graph_utils::DfsEvent::*;
            match ev {
                BackEdge(e) => {
                    loop_headers.insert(e.target());
                }
                Finish(n) => podfs_trace.push(n),
                _ => (),
            }
        });
        let (podfs_trace, loop_headers) = (podfs_trace, loop_headers);

        let mut visited = NodeSet::with_capacity(self.graph.node_bound());
        for &cur_node in &podfs_trace {
            visited.insert(cur_node);

            if loop_headers.contains(cur_node) {
                // loop

                // find latch nodes
                let mut backedges = EdgeSet::new();
                let mut latch_nodes = NodeSet::new();
                for edge in self.graph.edges_directed(cur_node, Incoming) {
                    // backedges are always from original graph nodes
                    if visited.contains(edge.source()) {
                        backedges.insert(edge.id());
                        latch_nodes.insert(edge.source());
                    }
                }

                // remove backedges
                for e in &backedges {
                    self.graph.remove_edge(e);
                }

                // regionify loop
                let mut loop_nodes = graph_utils::slice(&self.graph, cur_node, &latch_nodes).nodes;
                let loop_header = self.funnel_abnormal_entries(cur_node, &loop_nodes);
                let mut succ_nodes =
                    graph_utils::strict_successors_of_set(&self.graph, &loop_nodes);
                self.refine_loop(&mut loop_nodes, &mut succ_nodes);
                let loop_succ_opt = {
                    // pick the successor with the smallest post-order
                    let final_succ_opt = DfsPostOrder::new(&self.graph, self.entry)
                        .iter(&self.graph)
                        .find(|&n| succ_nodes.contains(n));
                    if let Some(final_succ) = final_succ_opt {
                        succ_nodes.remove(final_succ);
                        let loop_succ =
                            self.funnel_abnormal_exits(&mut loop_nodes, final_succ, &succ_nodes);
                        Some(loop_succ)
                    } else {
                        None
                    }
                };
                debug_assert!(
                    graph_utils::strict_successors_of_set(&self.graph, &loop_nodes).len() <= 1
                );

                let loop_body = self.structure_acyclic_sese_region(loop_header, &loop_nodes);
                let repl_ast = refinement::refine_loop::<A>(self.cctx, loop_body);
                self.graph[loop_header] = CfgNode::Code(repl_ast);
                if let Some(loop_succ) = loop_succ_opt {
                    self.graph.add_edge(loop_header, loop_succ, CfgEdge::True);
                }
            } else {
                // acyclic
                let region = graph_utils::dominated_by(&self.graph, self.entry, cur_node);
                // single-block regions aren't interesting
                if region.len() > 1 {
                    let succs = graph_utils::strict_successors_of_set(&self.graph, &region);
                    // `region` must have one or zero successors
                    if succs.len() <= 1 {
                        let opt_succ = succs.iter().next();
                        let repl_ast = self.structure_acyclic_sese_region(cur_node, &region);
                        self.graph[cur_node] = CfgNode::Code(repl_ast);
                        if let Some(succ) = opt_succ {
                            self.graph.add_edge(cur_node, succ, CfgEdge::True);
                        }
                    }
                }
            }
        }

        let ret = self.graph.remove_node(self.entry).unwrap();
        debug_assert!(self.graph.node_count() == 0);

        if let CfgNode::Code(ret) = ret {
            (ret, self.actx)
        } else {
            panic!("last node wasn't a Code node")
        }
    }

    /// Converts the given acyclic region headed by `header` into an `AstNode`.
    fn structure_acyclic_sese_region(
        &mut self,
        header: NodeIndex,
        region: &NodeSet,
    ) -> AstNode<'cd, A> {
        let slice = graph_utils::slice(&self.graph, header, region);
        let reaching_conds = self.reaching_conditions(&slice);

        let mut region_graph =
            StableDiGraph::with_capacity(slice.topo_order.len(), slice.edges.len());
        let mut old_new_map = HashMap::with_capacity(slice.topo_order.len());
        let mut region_conditions = Vec::new();

        // move all region nodes into `region_graph`.
        for &old_n in &slice.topo_order {
            let cfg_node = mem::replace(&mut self.graph[old_n], CfgNode::Dummy("sasr replaced"));
            if let CfgNode::Condition(c) = cfg_node {
                // record all conditions in the region
                region_conditions.push(c);
            }
            let new_node = match cfg_node {
                // refinement needs to be able to see `Break`s
                CfgNode::Code(AstNodeC::Break) => Some(AstNodeC::Break),
                // other nodes should be opaque
                CfgNode::Code(ast) => Some(AstNodeC::BasicBlock(ast)),
                _ => None,
            };
            let new_n = region_graph.add_node((reaching_conds[&old_n], new_node));
            old_new_map.insert(old_n, new_n);
        }
        let old_new_map = old_new_map;

        // copy over edges
        for e in &slice.edges {
            let (src, dst) = self.graph.edge_endpoints(e).unwrap();
            region_graph.add_edge(old_new_map[&src], old_new_map[&dst], ());
        }

        // remove region nodes from the cfg
        for &n in &slice.topo_order {
            // we don't want to remove `header` since that will also remove
            // incoming edges, which we need to keep
            if n != header {
                let _removed = self.graph.remove_node(n);
                debug_assert!(_removed.is_some());
            }
        }

        let ast = refinement::refine::<RegionAstContext<A>>(
            self.cctx,
            region_graph,
            old_new_map[&header],
        );

        let ast = dedup_conds::run(&mut self.actx, self.cctx, &region_conditions, ast);
        let ast = RegionAstContext::<A>::export(ast);
        refinement::simplify_ast_node::<A>(self.cctx, ast).unwrap_or_default()
    }

    /// Computes the reaching condition for every node in the given graph slice.
    fn reaching_conditions(
        &self,
        slice: &graph_utils::GraphSlice<NodeIndex, EdgeIndex>,
    ) -> HashMap<NodeIndex, Condition<'cd, A>> {
        // {Node, Edge}Filtered don't implement IntoNeighborsDirected :(
        // https://github.com/bluss/petgraph/pull/219
        // Also EdgeFiltered<Reversed<_>, _> isn't Into{Neighbors, Edges}
        // because Reversed<_> isn't IntoEdges

        let mut ret = HashMap::with_capacity(slice.topo_order.len());

        let mut iter = slice.topo_order.iter();
        if let Some(&start) = iter.next() {
            ret.insert(start, self.cctx.mk_true());
            for &n in iter {
                let reach_cond = self.cctx.mk_or_from_iter(
                    // manually restrict to slice
                    self.graph
                        .edges_directed(n, Incoming)
                        .filter(|e| slice.edges.contains(e.id()))
                        .map(|e| {
                            let src_cond = ret[&e.source()];
                            match (&self.graph[e.source()], e.weight()) {
                                (&CfgNode::Condition(c), CfgEdge::True) => {
                                    self.cctx.mk_and(src_cond, self.cctx.mk_var(c))
                                }
                                (&CfgNode::Condition(c), CfgEdge::False) => self
                                    .cctx
                                    .mk_and(src_cond, self.cctx.mk_not(self.cctx.mk_var(c))),
                                (_, CfgEdge::True) => src_cond,
                                (_, CfgEdge::False) => self.cctx.mk_false(),
                            }
                        }),
                );
                let _old = ret.insert(n, reach_cond);
                debug_assert!(_old.is_none());
            }
        }

        ret
    }

    /// Transforms the loop into a single-entry loop.
    /// Returns the new loop header.
    fn funnel_abnormal_entries(&mut self, header: NodeIndex, loop_nodes: &NodeSet) -> NodeIndex {
        let mut entry_map = HashMap::new();
        for n in loop_nodes {
            for e in self.graph.edges_directed(n, Incoming) {
                if !loop_nodes.contains(e.source()) {
                    entry_map.entry(n).or_insert(Vec::new()).push(e.id());
                }
            }
        }
        // loop must be reachable, so the header must have entries
        let header_entries = entry_map.remove(&header).unwrap();
        debug_assert!(!header_entries.is_empty());
        let abnormal_entry_map = entry_map;
        if abnormal_entry_map.is_empty() {
            // no abnormal entries
            return header;
        }
        let abnormal_entry_iter = (1..).zip(&abnormal_entry_map);

        let struct_var = self.actx.mk_fresh_var();

        // make condition cascade
        let new_header = {
            let abnormal_entry_iter = abnormal_entry_iter.clone().map(|(n, (&t, _))| (n, t));

            let dummy_preheader = self.graph.add_node(CfgNode::Dummy("loop \"preheader\""));

            let mut prev_cascade_node = dummy_preheader;
            let mut prev_entry_target = header;
            let mut prev_entry_num = 0;

            // we make the condition node for the *previous* entry target b/c
            // the current one might be the last one, which shouldn't get a
            // condition node because it's the only possible target
            for (entry_num, entry_target) in abnormal_entry_iter {
                let prev_cond_eq = self
                    .cctx
                    .new_var(self.actx.mk_cond_equals(&struct_var, prev_entry_num));
                let cascade_node = self.graph.add_node(CfgNode::Condition(prev_cond_eq));
                self.graph
                    .add_edge(prev_cascade_node, cascade_node, CfgEdge::False);
                self.graph
                    .add_edge(cascade_node, prev_entry_target, CfgEdge::True);

                let struct_reset = self.graph.add_node(CfgNode::Code(AstNodeC::BasicBlock(
                    self.actx.mk_var_assign(&struct_var, 0),
                )));
                self.graph
                    .add_edge(struct_reset, entry_target, CfgEdge::True);

                prev_cascade_node = cascade_node;
                prev_entry_target = struct_reset;
                prev_entry_num = entry_num;
            }

            self.graph
                .add_edge(prev_cascade_node, prev_entry_target, CfgEdge::False);

            // we always add an edge from dummy_preheader
            let new_header = self.graph.neighbors(dummy_preheader).next().unwrap();
            self.graph.remove_node(dummy_preheader);
            new_header
        };

        // redirect entries
        for (entry_num, entry_edges) in
            iter::once((0, &header_entries)).chain(abnormal_entry_iter.map(|(n, (_, e))| (n, e)))
        {
            let struct_assign = self.graph.add_node(CfgNode::Code(AstNodeC::BasicBlock(
                self.actx.mk_var_assign(&struct_var, entry_num),
            )));
            self.graph
                .add_edge(struct_assign, new_header, CfgEdge::True);
            for &entry_edge in entry_edges {
                graph_utils::retarget_edge(&mut self.graph, entry_edge, struct_assign);
            }
        }

        new_header
    }

    /// Incrementally adds nodes dominated by the loop to the loop until
    /// there's only one successor or there are no more nodes to add.
    fn refine_loop(&self, loop_nodes: &mut NodeSet, succ_nodes: &mut NodeSet) -> () {
        // reuse this `NodeSet` so we avoid allocating
        let mut new_nodes = NodeSet::new();
        while succ_nodes.len() > 1 {
            for n in &*succ_nodes {
                if self
                    .graph
                    .neighbors_directed(n, Incoming)
                    .all(|pred| loop_nodes.contains(pred))
                {
                    // post-pone removal from `succ_nodes` b/c rust ownership
                    loop_nodes.insert(n);
                    new_nodes.extend(self.graph.neighbors(n).filter(|&u| !loop_nodes.contains(u)));
                }
            }

            // do the removal
            succ_nodes.difference_with(&loop_nodes);

            if new_nodes.is_empty() {
                break;
            }
            succ_nodes.union_with(&new_nodes);
            new_nodes.clear();
        }
    }

    /// Transforms the loop so that all loop exits are `break`.
    /// Returns the new loop successor.
    fn funnel_abnormal_exits(
        &mut self,
        loop_nodes: &mut NodeSet,
        final_succ: NodeIndex,
        abn_succ_nodes: &NodeSet,
    ) -> NodeIndex {
        // replace "normal" exit edges with "break"
        {
            let exit_edges: Vec<_> =
                graph_utils::edges_from_region_to_node(&self.graph, &loop_nodes, final_succ)
                    .collect();
            for exit_edge in exit_edges {
                let break_node = self.graph.add_node(CfgNode::Code(AstNodeC::Break));
                graph_utils::retarget_edge(&mut self.graph, exit_edge, break_node);
                loop_nodes.insert(break_node);
            }
        }

        if abn_succ_nodes.is_empty() {
            // no abnormal exits
            return final_succ;
        }

        let abn_succ_iter = (1..).zip(abn_succ_nodes);
        let struct_var = self.actx.mk_fresh_var_zeroed();

        // replace abnormal exit edges with "break"
        for (exit_num, exit_target) in abn_succ_iter.clone() {
            let exit_edges: Vec<_> =
                graph_utils::edges_from_region_to_node(&self.graph, &loop_nodes, exit_target)
                    .collect();
            for exit_edge in exit_edges {
                let break_node = self.graph.add_node(CfgNode::Code(AstNodeC::Seq(vec![
                    AstNodeC::BasicBlock(self.actx.mk_var_assign(&struct_var, exit_num)),
                    AstNodeC::Break,
                ])));
                graph_utils::retarget_edge(&mut self.graph, exit_edge, break_node);
                loop_nodes.insert(break_node);
            }
        }

        let mut cur_succ = final_succ;

        // make condition cascade
        for (exit_num, exit_target) in abn_succ_iter.clone() {
            let cond = self
                .cctx
                .new_var(self.actx.mk_cond_equals(&struct_var, exit_num));
            let cascade_node = self.graph.add_node(CfgNode::Condition(cond));
            self.graph
                .add_edge(cascade_node, exit_target, CfgEdge::True);
            self.graph.add_edge(cascade_node, cur_succ, CfgEdge::False);

            cur_succ = cascade_node;
        }

        cur_succ
    }
}

struct RegionAstContext<'cd, A>(PhantomData<(&'cd (), A)>);

impl<'cd, A: AstContext> AstContext for RegionAstContext<'cd, A> {
    type Block = AstNode<'cd, A>;
    type Condition = A::Condition;
    type BoolVariable = A::BoolVariable;
    type Variable = A::Variable;
}

impl<'cd, A: AstContext> RegionAstContext<'cd, A> {
    fn export(ast: AstNode<'cd, Self>) -> AstNode<'cd, A> {
        use self::AstNodeC::*;
        match ast {
            BasicBlock(b) => b,
            Seq(seq) => Seq(seq.into_iter().map(Self::export).collect()),
            Cond(c, t, oe) => Cond(
                c,
                Box::new(Self::export(*t)),
                oe.map(|e| Box::new(Self::export(*e))),
            ),
            Loop(t, b) => Loop(t, Box::new(Self::export(*b))),
            Break => Break,
            Switch(v, cases, default) => Switch(
                v,
                cases
                    .into_iter()
                    .map(|(vs, a)| (vs, Self::export(a)))
                    .collect(),
                Box::new(Self::export(*default)),
            ),
        }
    }
}

pub fn mk_code_node<A: AstContext>(block: A::Block) -> CfgNode<'static, A> {
    CfgNode::Code(AstNodeC::BasicBlock(block))
}

pub fn mk_cond_node<'cd, A: AstContext>(
    cctx: CondContext<'cd, A>,
    cond: A::Condition,
) -> CfgNode<'cd, A> {
    CfgNode::Condition(cctx.new_var(cond))
}

pub fn empty_node<A: AstContext>() -> CfgNode<'static, A> {
    CfgNode::Code(AstNodeC::default())
}

impl<'cd, A> fmt::Debug for ControlFlowGraph<'cd, A>
where
    A: AstContext + fmt::Debug,
    A::Block: fmt::Debug,
    A::Variable: fmt::Debug,
    A::Condition: fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("ControlFlowGraph")
            .field("graph", &self.graph)
            .field("entry", &self.entry)
            .field("cctx", &self.cctx)
            .field("actx", &self.actx)
            .finish()
    }
}

impl<'cd, A> fmt::Debug for CfgNode<'cd, A>
where
    A: AstContext,
    A::Block: fmt::Debug,
    A::Variable: fmt::Debug,
    A::Condition: fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CfgNode::Code(c) => fmt.debug_tuple("Code").field(c).finish(),
            CfgNode::Condition(c) => fmt.debug_tuple("Condition").field(c).finish(),
            CfgNode::Dummy(s) => fmt.debug_tuple("Dummy").field(s).finish(),
        }
    }
}
