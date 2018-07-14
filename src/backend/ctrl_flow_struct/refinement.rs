use super::ast_context::AstContext;
use super::graph_utils;
use super::{AstNode, AstNodeC, CondContext, Condition, NodeSet};

use petgraph::algo;
use petgraph::prelude::*;
use petgraph::visit::{IntoNodeReferences, Topo, Walker};

use std::collections::HashMap;

pub(super) struct Refiner<'cd, A: AstContext> {
    pub cctx: CondContext<'cd, A>,
    pub graph: StableDiGraph<RefinementAstNode<'cd, A>, ()>,
}

pub(super) type RefinementAstNode<'cd, A> = (Condition<'cd, A>, Option<AstNode<'cd, A>>);

pub(super) fn refine<'cd, A: AstContext>(
    cctx: CondContext<'cd, A>,
    graph: StableDiGraph<RefinementAstNode<'cd, A>, ()>,
) -> AstNode<'cd, A> {
    Refiner::<A> { cctx, graph }.refine()
}

impl<'cd, A: AstContext> Refiner<'cd, A> {
    fn refine(mut self) -> AstNode<'cd, A> {
        let cctx = self.cctx;

        self.try_find_if_else_pair();
        self.try_find_if();
        // TODO: find switch
        self.try_find_if_else_cascade();

        let mut ast_seq = Vec::new();

        // remove all nodes in topological order
        let mut topo = Topo::new(&self.graph);
        while let Some(node) = topo.next(&self.graph) {
            let (cond, ast) = self.graph.remove_node(node).unwrap();
            ast_seq.push(mk_cond::<A>(cond, Box::new(ast.unwrap_or_default()), None));
        }

        simplify_ast_node::<A>(cctx, AstNodeC::Seq(ast_seq)).unwrap_or_default()
    }

    fn try_find_if_else_pair(&mut self) {
        let cctx = self.cctx;

        // look for pairs of nodes whose reaching conditions are opposites
        loop {
            let mut if_else_cond = None;
            'l: for (_, &(ca, ref wa)) in self.graph.node_references() {
                if wa.is_some() {
                    for (_, &(cb, ref wb)) in self.graph.node_references() {
                        if wb.is_some() {
                            let not_cb = cctx.mk_not(cb);
                            if ca == not_cb {
                                if_else_cond = Some((ca, cb));
                                break 'l;
                            }
                        }
                    }
                }
            }

            if let Some((cond, not_cond)) = if_else_cond {
                let _changed = self.try_group_by_cond(cond, not_cond);
                debug_assert!(_changed);
            } else {
                break;
            }
        }
    }

    fn try_find_if(&mut self) {
        let cctx = self.cctx;

        // iterate through every node in topological order and try to group by
        // that condition
        'm: loop {
            let mut topo = Topo::new(&self.graph);
            loop {
                if let Some(n) = topo.next(&self.graph) {
                    let cond = self.graph[n].0;
                    let changed = self.try_group_by_cond(cond, cctx.mk_not(cond));
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
    }

    fn try_group_by_cond(&mut self, cond: Condition<'cd, A>, not_cond: Condition<'cd, A>) -> bool {
        let cctx = self.cctx;

        if cond.is_true() {
            return false;
        }
        let mut then_cands = HashMap::new();
        let mut else_cands = HashMap::new();
        let mut rest = Vec::with_capacity(self.graph.node_count());
        for node in self.graph.node_indices() {
            let orig_cond = self.graph[node].0;
            if let Some(new_cond) = cctx.remove_and(cond, orig_cond) {
                then_cands.insert(node, new_cond);
            } else if let Some(new_cond) = cctx.remove_and(not_cond, orig_cond) {
                else_cands.insert(node, new_cond);
            } else {
                rest.push(node);
            }
        }

        let then_len = then_cands
            .keys()
            .filter(|&&n| self.graph[n].1.is_some())
            .count();
        let else_len = else_cands
            .keys()
            .filter(|&&n| self.graph[n].1.is_some())
            .count();

        if then_len + else_len >= 2 {
            if then_cands.is_empty() {
                graph_utils::contract_nodes_and_map(
                    &mut self.graph,
                    else_cands.keys().collect(),
                    |node, (_, ast)| (else_cands[&node], ast),
                    |_, _| (),
                    |else_graph| (not_cond, Some(refine::<A>(cctx, else_graph))),
                );
            } else {
                let then_node = graph_utils::contract_nodes_and_map(
                    &mut self.graph,
                    then_cands.keys().collect(),
                    |node, (_, ast)| (then_cands[&node], ast),
                    |_, _| (),
                    |then_graph| (cond, Some(refine::<A>(cctx, then_graph))),
                );
                if !else_cands.is_empty() {
                    let else_node = graph_utils::contract_nodes_and_map(
                        &mut self.graph,
                        else_cands.keys().collect(),
                        |node, (_, ast)| (else_cands[&node], ast),
                        |_, _| (),
                        |else_graph| (not_cond, Some(refine::<A>(cctx, else_graph))),
                    );

                    debug_assert!(
                        self.graph
                            .find_edge_undirected(then_node, else_node)
                            .is_none()
                    );
                    let preds: NodeSet = self
                        .graph
                        .neighbors_directed(then_node, Incoming)
                        .chain(self.graph.neighbors_directed(else_node, Incoming))
                        .collect();
                    let succs: NodeSet = self
                        .graph
                        .neighbors_directed(then_node, Outgoing)
                        .chain(self.graph.neighbors_directed(else_node, Outgoing))
                        .collect();

                    let (_, then_ast) = self.graph.remove_node(then_node).unwrap();
                    let (_, else_ast) = self.graph.remove_node(else_node).unwrap();
                    let if_node = self.graph.add_node((
                        cctx.mk_true(),
                        Some(AstNodeC::Cond(
                            cond,
                            Box::new(then_ast.unwrap_or_default()),
                            Some(Box::new(else_ast.unwrap_or_default())),
                        )),
                    ));

                    for p in &preds {
                        self.graph.add_edge(p, if_node, ());
                    }
                    for s in &succs {
                        self.graph.add_edge(if_node, s, ());
                    }
                }
            }
            debug_assert!(!algo::is_cyclic_directed(&self.graph));
            true
        } else {
            false
        }
    }

    fn try_find_if_else_cascade(&mut self) {
        let cctx = self.cctx;

        let mut order = Vec::new();
        for n in Topo::new(&self.graph).iter(&self.graph) {
            if self.graph[n].1.is_some() {
                order.push(n);
            }
        }
        let order = order;

        if order.len() < 2 {
            return;
        }

        let mut opt_cands = None;
        // find a set of nodes to structure as an if-else cascade
        let mut dfs = algo::DfsSpace::new(&self.graph);
        'n: for i in 0..order.len() {
            if self.graph[order[i]].0.is_true() {
                continue;
            }
            let init = &order[0..=i];
            let mut cands = NodeSet::new();
            let mut or_cand_conds = cctx.mk_false();
            for j in (0..init.len()).rev() {
                let n = init[j];
                if cands
                    .iter()
                    .all(|c| !algo::has_path_connecting(&self.graph, n, c, Some(&mut dfs)))
                {
                    cands.insert(n);
                    or_cand_conds = cctx.mk_or(or_cand_conds, self.graph[n].0);
                    if or_cand_conds.is_true() {
                        opt_cands = Some(cands);
                        break 'n;
                    }
                }
            }
        }

        if let Some(cands) = opt_cands {
            graph_utils::contract_nodes_and_map(
                &mut self.graph,
                cands,
                |_, n| n,
                |_, _| (),
                |mut casc_graph| {
                    debug_assert!(casc_graph.edge_count() == 0);
                    debug_assert!(casc_graph.node_count() >= 2);
                    debug_assert!(casc_graph.node_references().all(|(_, (_, x))| x.is_some()));

                    // `StableGraph` doesn't have `into_nodes_edges` :(
                    let mut nodes = Vec::new();
                    while let Some(n) = casc_graph.node_indices().next() {
                        let (cond, ast) = casc_graph.remove_node(n).unwrap();
                        nodes.push((cond, ast.unwrap()));
                    }
                    nodes.sort_unstable_by_key(|&(c, _)| c.complexity());

                    // build if-else cascade starting from last else block
                    let mut casc_ast = nodes.pop().unwrap().1;
                    for (cond, ast) in nodes.into_iter().rev() {
                        casc_ast = AstNodeC::Cond(cond, Box::new(ast), Some(Box::new(casc_ast)));
                    }

                    (cctx.mk_true(), Some(casc_ast))
                },
            );
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

/// Performs trivial simplifications.
fn simplify_ast_node<'cd, A: AstContext>(
    cctx: CondContext<'cd, A>,
    ast: AstNode<'cd, A>,
) -> Option<AstNode<'cd, A>> {
    use super::AstNodeC::*;
    match ast {
        BasicBlock(b) => Some(BasicBlock(b)),
        Seq(seq) => {
            let mut new_seq: Vec<_> = seq
                .into_iter()
                .flat_map(|a| {
                    if let Some(a) = simplify_ast_node::<A>(cctx, a) {
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
            let ot = simplify_ast_node::<A>(cctx, *t);
            let oe = oe.and_then(|e| simplify_ast_node::<A>(cctx, *e));
            if c.is_true() {
                ot
            } else if c.is_false() {
                oe
            } else {
                if let Some(t) = ot {
                    Some(Cond(c, Box::new(t), oe.map(Box::new)))
                } else {
                    if let Some(e) = oe {
                        Some(Cond(cctx.mk_not(c), Box::new(e), None))
                    } else {
                        None
                    }
                }
            }
        }
        Loop(t, b) => {
            let b = simplify_ast_node::<A>(cctx, *b).unwrap_or_default();
            Some(Loop(t, Box::new(b)))
        }
        Switch(v, cases, default) => {
            let cases: Vec<_> = cases
                .into_iter()
                .filter_map(|(vs, a)| simplify_ast_node::<A>(cctx, a).map(|a| (vs, a)))
                .collect();
            let default = simplify_ast_node::<A>(cctx, *default);
            if cases.is_empty() {
                default
            } else {
                Some(Switch(v, cases, Box::new(default.unwrap_or_default())))
            }
        }
    }
}
