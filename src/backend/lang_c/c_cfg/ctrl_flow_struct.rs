use super::{ActionEdge, ActionNode, CCFGEdge, CCFGNode, CCFG};
use backend::ctrl_flow_struct as flstr;
use backend::ctrl_flow_struct::ast_context::{AstContext, AstContextMut};
use backend::lang_c::c_ast::{self, CAST};

use petgraph::graph::EdgeReference;
use petgraph::prelude::*;
use petgraph::visit::{Dfs, EdgeFiltered, IntoNodeReferences, VisitMap};

pub fn structure_and_convert(ccfg: CCFG) -> Option<CAST> {
    let cstore = flstr::condition::Storage::new();
    let flstr_cfg = import(cstore.cctx(), ccfg)?;
    let (flstr_ast, ccfg) = flstr_cfg.structure_whole();
    flstr::export::to_c_ast(&ccfg, flstr_ast)
        .map_err(|e| radeco_err!("{}", e))
        .ok()
}

fn import<'cd>(
    cctx: flstr::condition::Context<'cd, NodeIndex>,
    ccfg: CCFG,
) -> Option<flstr::ControlFlowGraph<'cd, CCFG>> {
    let (new_graph, entry) = {
        let reachable_actions = {
            let ef = EdgeFiltered::from_fn(&ccfg.g, |e| {
                // ignore `Normal` edges from `Goto` and `If` nodes
                match (&ccfg.g[e.source()], e.weight()) {
                    (CCFGNode::Action(ActionNode::Goto), CCFGEdge::Action(ActionEdge::Normal)) => {
                        false
                    }
                    (CCFGNode::Action(ActionNode::If), CCFGEdge::Action(ActionEdge::Normal)) => {
                        false
                    }
                    (_, CCFGEdge::Action(_)) => true,
                    _ => false,
                }
            });
            let mut dfs = Dfs::new(&ef, ccfg.entry);
            while let Some(_) = dfs.next(&ef) {}
            dfs.discovered
        };

        if ccfg
            .g
            .node_references()
            .filter(|(n, _)| reachable_actions.is_visited(n))
            .any(|(_, nw)| !is_action_node(nw))
        {
            return None;
        }

        let (new_graph, node_index_map) = try_filter_map_to_stable(
            &ccfg.g,
            |n, nw| {
                Some(if reachable_actions.is_visited(&n) {
                    if let CCFGNode::Action(ActionNode::If) = nw {
                        Some(flstr::mk_cond_node(cctx, ccfg.branch_condition(n)?))
                    } else {
                        Some(flstr::mk_code_node(n))
                    }
                } else {
                    None
                })
            },
            |e| {
                // `Some(_)`     <=> keep edge
                // `None`        <=> ignore edge
                // `return None` <=> error
                Some(match (&ccfg.g[e.source()], e.weight()) {
                    (CCFGNode::Action(ActionNode::If), CCFGEdge::Action(ActionEdge::IfThen)) => {
                        Some(flstr::CfgEdge::True)
                    }
                    (CCFGNode::Action(ActionNode::If), CCFGEdge::Action(ActionEdge::IfElse)) => {
                        Some(flstr::CfgEdge::False)
                    }
                    (CCFGNode::Action(ActionNode::If), CCFGEdge::Action(ActionEdge::Normal)) => {
                        None
                    }
                    (CCFGNode::Action(ActionNode::Goto), CCFGEdge::Action(ActionEdge::GotoDst)) => {
                        Some(flstr::CfgEdge::True)
                    }
                    (CCFGNode::Action(ActionNode::Goto), CCFGEdge::Action(ActionEdge::Normal)) => {
                        None
                    }
                    (_, CCFGEdge::Action(ActionEdge::Normal)) => Some(flstr::CfgEdge::True),
                    (_, CCFGEdge::Value(_)) => None,
                    (_, CCFGEdge::Action(_)) => return None,
                })
            },
        )?;

        (new_graph, node_index_map[ccfg.entry.index()])
    };

    Some(flstr::ControlFlowGraph::new(new_graph, entry, cctx, ccfg))
}

impl AstContext for CCFG {
    type Block = NodeIndex;
    type Variable = NodeIndex;
    type BoolVariable = NodeIndex;
    type Condition = NodeIndex;
}

fn var_ty() -> c_ast::Ty {
    c_ast::Ty::new(c_ast::BTy::Int, false, 0)
}

impl AstContextMut for CCFG {
    fn mk_fresh_var(&mut self) -> Self::Variable {
        // XXX: initialized to something
        let var = format!("i_{}", self.vars.len());
        self.var(&var, Some(var_ty()))
    }

    fn mk_fresh_var_zeroed(&mut self) -> Self::Variable {
        // XXX: initialized to zero
        let var = format!("i_{}", self.vars.len());
        self.var(&var, Some(var_ty()))
    }

    fn mk_fresh_bool_var(&mut self) -> Self::BoolVariable {
        // XXX: initialized to something
        let var = format!("c_{}", self.vars.len());
        self.var(&var, Some(var_ty()))
    }

    fn mk_cond_equals(&mut self, &var: &Self::Variable, val: u64) -> Self::Condition {
        let val = self.constant(&format!("{}", val), Some(var_ty()));
        self.expr(&[var, val], c_ast::Expr::Eq)
    }

    fn mk_cond_from_bool_var(&mut self, &var: &Self::BoolVariable) -> Self::Condition {
        var
    }

    fn mk_var_assign(&mut self, &var: &Self::Variable, val: u64) -> Self::Block {
        let val = self.constant(&format!("{}", val), Some(var_ty()));
        let unk = self.unknown;
        self.assign(var, val, unk)
    }

    fn mk_bool_var_assign(
        &mut self,
        &var: &Self::BoolVariable,
        &cond: &Self::Condition,
    ) -> Self::Block {
        let unk = self.unknown;
        self.assign(var, cond, unk)
    }
}

/// based on https://docs.rs/petgraph/0.4.12/src/petgraph/graph_impl/mod.rs.html#1293-1317
fn try_filter_map_to_stable<'a, N, E, F, G, N2, E2>(
    graph: &'a Graph<N, E>,
    mut node_map: F,
    mut edge_map: G,
) -> Option<(StableGraph<N2, E2>, Vec<NodeIndex>)>
where
    F: FnMut(NodeIndex, &'a N) -> Option<Option<N2>>,
    G: FnMut(EdgeReference<'a, E>) -> Option<Option<E2>>,
{
    let mut g = StableGraph::new();
    // mapping from old node index to new node index, end represents removed.
    let mut node_index_map = vec![NodeIndex::end(); graph.node_count()];
    for (i, node) in graph.node_references() {
        if let Some(nw) = node_map(i, node)? {
            node_index_map[i.index()] = g.add_node(nw);
        }
    }
    for edge in graph.edge_references() {
        // skip edge if any endpoint was removed
        let source = node_index_map[edge.source().index()];
        let target = node_index_map[edge.target().index()];
        if source != NodeIndex::end() && target != NodeIndex::end() {
            if let Some(ew) = edge_map(edge)? {
                g.add_edge(source, target, ew);
            }
        }
    }
    Some((g, node_index_map))
}

fn is_action_node(e: &CCFGNode) -> bool {
    match e {
        CCFGNode::Entry | CCFGNode::Action(_) => true,
        _ => false,
    }
}
