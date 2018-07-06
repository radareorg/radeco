use super::ast_context::AstContext;
use super::{AstNode, CondContext, Condition, LoopType, NodeSet};

use petgraph::prelude::*;

use std::marker::PhantomData;
use std::mem;

pub(super) use self::RefinementAstContext as RAC;

pub(super) struct Refiner<'cd, A: AstContext> {
    pub cctx: CondContext<'cd, A>,
    pub reaching_conds: Vec<Condition<'cd, A>>,
    pub reachability: Vec<NodeSet>,
}

impl<'cd, A: AstContext> Refiner<'cd, A> {
    pub(super) fn refine_ast_seq(
        &self,
        mut ast_nodes: Vec<AstNode<'cd, RAC<A>>>,
    ) -> AstNode<'cd, RAC<A>> {
        // look for pairs of nodes whose reaching conditions are opposites
        loop {
            let mut if_else_cond = None;

            'l: for node_a in &ast_nodes {
                if let &AstNode::Cond(ca, _, None) = node_a {
                    for node_b in &ast_nodes {
                        if let &AstNode::Cond(cb, _, None) = node_b {
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
                self.try_group_by_cond(cond, not_cond, &mut ast_nodes);
            } else {
                break;
            }
        }

        if ast_nodes.len() == 1 {
            ast_nodes.pop().unwrap()
        } else {
            AstNode::Seq(ast_nodes)
        }
    }

    /// Tries to collect multiple nodes into a single
    /// `if (cond) { .. } else { .. }` node.
    fn try_group_by_cond(
        &self,
        cond: Condition<'cd, A>,
        not_cond: Condition<'cd, A>,
        ast_nodes: &mut Vec<AstNode<'cd, RAC<A>>>,
    ) {
        if cond.is_true() {
            return;
        }
        let mut then_cands = Vec::new();
        let mut else_cands = Vec::new();
        let mut rest = Vec::with_capacity(ast_nodes.len());
        for (i, node) in mem::replace(ast_nodes, Vec::new()).into_iter().enumerate() {
            if let AstNode::Cond(orig_cond, t, None) = node {
                if let Some(new_cond) = self.cctx.remove_and(cond, orig_cond) {
                    then_cands.push((i, orig_cond, new_cond, t));
                } else if let Some(new_cond) = self.cctx.remove_and(not_cond, orig_cond) {
                    else_cands.push((i, orig_cond, new_cond, t));
                } else {
                    rest.push(AstNode::Cond(orig_cond, t, None));
                }
            } else {
                rest.push(node);
            }
        }
        *ast_nodes = match (then_cands.len(), else_cands.len()) {
            (0, 0) => rest,
            (1, 0) => {
                // undo removal
                let (i, orig_cond, _, t) = then_cands.pop().unwrap();
                rest.insert(i, AstNode::Cond(orig_cond, t, None));
                rest
            }
            (0, 1) => {
                // undo removal
                let (i, orig_cond, _, t) = else_cands.pop().unwrap();
                rest.insert(i, AstNode::Cond(orig_cond, t, None));
                rest
            }
            _ => {
                // XXX: maintain topological order
                let else_node = if else_cands.is_empty() {
                    None
                } else {
                    let else_nodes = else_cands
                        .into_iter()
                        .map(|(_, _, new_cond, t)| mk_cond(new_cond, t, None))
                        .collect();
                    Some(Box::new(self.refine_ast_seq(else_nodes)))
                };
                let then_nodes = then_cands
                    .into_iter()
                    .map(|(_, _, new_cond, t)| mk_cond(new_cond, t, None))
                    .collect();
                rest.push(AstNode::Cond(
                    cond,
                    Box::new(self.refine_ast_seq(then_nodes)),
                    else_node,
                ));
                rest
            }
        }
    }
}

pub(super) fn mk_cond<'cd, A: AstContext>(
    cond: Condition<'cd, A>,
    then_node: Box<AstNode<'cd, A>>,
    else_node_opt: Option<Box<AstNode<'cd, A>>>,
) -> AstNode<'cd, A> {
    if cond.is_true() {
        *then_node
    } else {
        AstNode::Cond(cond, then_node, else_node_opt)
    }
}

// TODO: better name
pub(super) struct RefinementAstContext<A: AstContext> {
    _phantom: PhantomData<A>,
}

impl<A: AstContext> AstContext for RefinementAstContext<A> {
    type Block = (A::Block, NodeIndex);
    type Variable = A::Variable;
    type Condition = A::Condition;
}

impl<A: AstContext> RefinementAstContext<A> {
    pub(super) fn import_ast_node(node: AstNode<'_, A>, data: NodeIndex) -> AstNode<'_, Self> {
        use super::AstNode::*;
        match node {
            BasicBlock(b) => BasicBlock((b, data)),
            Seq(s) => Seq(s
                .into_iter()
                .map(|a| Self::import_ast_node(a, data))
                .collect()),
            Cond(c, t, e) => Cond(
                c,
                Box::new(Self::import_ast_node(*t, data)),
                e.map(|e| Box::new(Self::import_ast_node(*e, data))),
            ),
            Loop(t, b) => Loop(
                convert_loop_type(t),
                Box::new(Self::import_ast_node(*b, data)),
            ),
            Switch(v, c, d) => Switch(
                v,
                c.into_iter()
                    .map(|(s, a)| (s, Self::import_ast_node(a, data)))
                    .collect(),
                Box::new(Self::import_ast_node(*d, data)),
            ),
        }
    }

    pub(super) fn export_ast_node(node: AstNode<'_, Self>) -> AstNode<'_, A> {
        use super::AstNode::*;
        match node {
            BasicBlock((b, _)) => BasicBlock(b),
            Seq(s) => Seq(s.into_iter().map(|a| Self::export_ast_node(a)).collect()),
            Cond(c, t, e) => Cond(
                c,
                Box::new(Self::export_ast_node(*t)),
                e.map(|e| Box::new(Self::export_ast_node(*e))),
            ),
            Loop(t, b) => Loop(convert_loop_type(t), Box::new(Self::export_ast_node(*b))),
            Switch(v, c, d) => Switch(
                v,
                c.into_iter()
                    .map(|(s, a)| (s, Self::export_ast_node(a)))
                    .collect(),
                Box::new(Self::export_ast_node(*d)),
            ),
        }
    }
}

fn convert_loop_type<A, B>(t: LoopType<'_, A>) -> LoopType<'_, B>
where
    A: AstContext,
    B: AstContext<Condition = A::Condition>,
{
    use super::LoopType::*;
    match t {
        PreChecked(c) => PreChecked(c),
        PostChecked(c) => PostChecked(c),
        Endless => Endless,
    }
}
