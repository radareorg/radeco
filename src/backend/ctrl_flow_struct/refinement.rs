use super::ast_context::AstContext;
use super::graph_utils;
use super::{AstNode, AstNodeC, CondContext, Condition, NodeSet};

use petgraph::algo;
use petgraph::prelude::*;
use petgraph::visit::{IntoNodeReferences, Topo};

use std::collections::HashMap;

pub(super) struct Refiner<'cd, A: AstContext> {
    pub cctx: CondContext<'cd, A>,
}
impl<'cd, A: AstContext> Copy for Refiner<'cd, A> {}
impl<'cd, A: AstContext> Clone for Refiner<'cd, A> {
    fn clone(&self) -> Self {
        *self
    }
}

pub(super) type RefinementAstNode<'cd, A> = (Condition<'cd, A>, Option<AstNode<'cd, A>>);

impl<'cd, A: AstContext> Refiner<'cd, A> {
    pub(super) fn refine_ast_seq(
        self,
        mut graph: StableDiGraph<RefinementAstNode<'cd, A>, ()>,
    ) -> AstNode<'cd, A> {
        // look for pairs of nodes whose reaching conditions are opposites
        loop {
            let mut if_else_cond = None;
            'l: for (_, &(ca, ref wa)) in graph.node_references() {
                if wa.is_some() {
                    for (_, &(cb, ref wb)) in graph.node_references() {
                        if wb.is_some() {
                            let not_cb = self.cctx.mk_not(cb);
                            if ca == not_cb {
                                if_else_cond = Some((ca, cb));
                                break 'l;
                            }
                        }
                    }
                }
            }

            if let Some((cond, not_cond)) = if_else_cond {
                let _changed = self.try_group_by_cond(cond, not_cond, &mut graph);
                debug_assert!(_changed);
            } else {
                break;
            }
        }

        // iterate through every node in topological order and try to group by
        // that condition
        'm: loop {
            let mut topo = Topo::new(&graph);
            loop {
                if let Some(n) = topo.next(&graph) {
                    let cond = graph[n].0;
                    let changed = self.try_group_by_cond(cond, self.cctx.mk_not(cond), &mut graph);
                    if changed {
                        // restart traversal
                        break;
                    }
                } else {
                    // no more groupings
                    break 'm;
                }
            }
        }

        let mut ast_seq = Vec::new();

        // remove all nodes in topological order
        let mut topo = Topo::new(&graph);
        while let Some(node) = topo.next(&graph) {
            let (cond, ast) = graph.remove_node(node).unwrap();
            ast_seq.push(mk_cond::<A>(cond, Box::new(ast.unwrap_or_default()), None));
        }

        self.simplify_ast_node(AstNodeC::Seq(ast_seq))
            .unwrap_or_default()
    }

    fn try_group_by_cond(
        self,
        cond: Condition<'cd, A>,
        not_cond: Condition<'cd, A>,
        graph: &mut StableDiGraph<RefinementAstNode<'cd, A>, ()>,
    ) -> bool {
        if cond.is_true() {
            return false;
        }
        let mut then_cands = HashMap::new();
        let mut else_cands = HashMap::new();
        let mut rest = Vec::with_capacity(graph.node_count());
        for node in graph.node_indices() {
            let orig_cond = graph[node].0;
            if let Some(new_cond) = self.cctx.remove_and(cond, orig_cond) {
                then_cands.insert(node, new_cond);
            } else if let Some(new_cond) = self.cctx.remove_and(not_cond, orig_cond) {
                else_cands.insert(node, new_cond);
            } else {
                rest.push(node);
            }
        }

        if then_cands.len() + else_cands.len() >= 2 {
            if then_cands.is_empty() {
                graph_utils::contract_nodes_and_map(
                    graph,
                    else_cands.keys().collect(),
                    |node, (_, ast)| (else_cands[&node], ast),
                    |_, _| (),
                    |else_graph| (not_cond, Some(self.refine_ast_seq(else_graph))),
                );
            } else {
                let then_node = graph_utils::contract_nodes_and_map(
                    graph,
                    then_cands.keys().collect(),
                    |node, (_, ast)| (then_cands[&node], ast),
                    |_, _| (),
                    |then_graph| (cond, Some(self.refine_ast_seq(then_graph))),
                );
                if !else_cands.is_empty() {
                    let else_node = graph_utils::contract_nodes_and_map(
                        graph,
                        else_cands.keys().collect(),
                        |node, (_, ast)| (else_cands[&node], ast),
                        |_, _| (),
                        |else_graph| (not_cond, Some(self.refine_ast_seq(else_graph))),
                    );

                    debug_assert!(graph.find_edge_undirected(then_node, else_node).is_none());
                    let preds: NodeSet = graph
                        .neighbors_directed(then_node, Incoming)
                        .chain(graph.neighbors_directed(else_node, Incoming))
                        .collect();
                    let succs: NodeSet = graph
                        .neighbors_directed(then_node, Outgoing)
                        .chain(graph.neighbors_directed(else_node, Outgoing))
                        .collect();

                    let (_, then_ast) = graph.remove_node(then_node).unwrap();
                    let (_, else_ast) = graph.remove_node(else_node).unwrap();
                    let if_node = graph.add_node((
                        self.cctx.mk_true(),
                        Some(AstNodeC::Cond(
                            cond,
                            Box::new(then_ast.unwrap_or_default()),
                            Some(Box::new(else_ast.unwrap_or_default())),
                        )),
                    ));

                    for p in &preds {
                        graph.add_edge(p, if_node, ());
                    }
                    for s in &succs {
                        graph.add_edge(if_node, s, ());
                    }
                }
            }
            debug_assert!(!algo::is_cyclic_directed(&*graph));
            true
        } else {
            false
        }
    }

    /// Performs trivial simplifications.
    fn simplify_ast_node(self, ast: AstNode<'cd, A>) -> Option<AstNode<'cd, A>> {
        use super::AstNodeC::*;
        match ast {
            BasicBlock(b) => Some(BasicBlock(b)),
            Seq(seq) => {
                let mut new_seq: Vec<_> = seq
                    .into_iter()
                    .flat_map(|a| {
                        if let Some(a) = self.simplify_ast_node(a) {
                            if let Seq(s) = a {
                                s
                            } else {
                                vec![a]
                            }
                        } else {
                            Vec::new()
                        }
                    })
                    .collect();
                match new_seq.len() {
                    0 => None,
                    1 => Some(new_seq.pop().unwrap()),
                    _ => Some(Seq(new_seq)),
                }
            }
            Cond(c, t, oe) => {
                let ot = self.simplify_ast_node(*t);
                let oe = oe.and_then(|e| self.simplify_ast_node(*e));
                if c.is_true() {
                    ot
                } else if c.is_false() {
                    oe
                } else {
                    if let Some(t) = ot {
                        Some(Cond(c, Box::new(t), oe.map(Box::new)))
                    } else {
                        if let Some(e) = oe {
                            Some(Cond(self.cctx.mk_not(c), Box::new(e), None))
                        } else {
                            None
                        }
                    }
                }
            }
            Loop(t, b) => {
                let b = self.simplify_ast_node(*b).unwrap_or_default();
                Some(Loop(t, Box::new(b)))
            }
            Switch(v, cases, default) => {
                let cases: Vec<_> = cases
                    .into_iter()
                    .filter_map(|(vs, a)| self.simplify_ast_node(a).map(|a| (vs, a)))
                    .collect();
                let default = self.simplify_ast_node(*default);
                if cases.is_empty() {
                    default
                } else {
                    Some(Switch(v, cases, Box::new(default.unwrap_or_default())))
                }
            }
        }
    }
}

fn mk_cond<'cd, A: AstContext>(
    cond: Condition<'cd, A>,
    then_node: Box<AstNode<'cd, A>>,
    else_node_opt: Option<Box<AstNode<'cd, A>>>,
) -> AstNode<'cd, A> {
    if cond.is_true() {
        *then_node
    } else {
        AstNodeC::Cond(cond, then_node, else_node_opt)
    }
}
