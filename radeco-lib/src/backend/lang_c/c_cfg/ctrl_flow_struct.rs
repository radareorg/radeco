use super::{ActionEdge, ActionNode, CCFGEdge, CCFGNode, CCFGRef, ValueEdge, CCFG};
use crate::backend::ctrl_flow_struct as flstr;
use crate::backend::ctrl_flow_struct::ast_context::{AstContext, AstContextMut};
use crate::backend::lang_c::c_ast::{self, CAST};

use petgraph::stable_graph::StableDiGraph;
use petgraph::visit::EdgeRef;

use std::collections::hash_map::Entry;
use std::collections::HashMap;

pub fn structure_and_convert(ccfg: CCFG) -> Result<CAST, &'static str> {
    let cstore = flstr::condition::Storage::new();
    let flstr_cfg = Importer::new(cstore.cctx(), ccfg).run()?;
    let (flstr_ast, ccfg) = flstr_cfg.structure_whole();
    flstr::export::to_c_ast(&ccfg, flstr_ast)
}

struct Importer<'cd> {
    cctx: flstr::condition::Context<'cd, CCFGRef>,
    ccfg: CCFG,
    new_graph: StableDiGraph<flstr::CfgNode<'cd, CCFG>, flstr::CfgEdge>,
}

enum SuccInfo {
    Single(CCFGRef),
    Branch(CCFGRef, CCFGRef, CCFGRef),
}

impl<'cd> Importer<'cd> {
    fn new(cctx: flstr::condition::Context<'cd, CCFGRef>, ccfg: CCFG) -> Self {
        Self {
            cctx,
            ccfg,
            new_graph: StableDiGraph::new(),
        }
    }

    fn run(mut self) -> Result<flstr::ControlFlowGraph<'cd, CCFG>, &'static str> {
        let new_entry = self.new_graph.add_node(flstr::empty_node());
        let mut converted = HashMap::new();

        // do a DFS over CCFG, inserting nodes and edges into `new_graph` as we
        // discover them
        if let Some(first) = self.ccfg.next_action(self.ccfg.entry) {
            // list of edges to process
            let mut worklist = Vec::new();
            worklist.push((new_entry, flstr::CfgEdge::True, first));

            while let Some((f_pred_block, pred_edge_ty, cur_block)) = worklist.pop() {
                // find the node in `new_graph` corresponding to `cur_block` in
                // CCFG, adding a new node if necessary
                let f_cur_block = match converted.entry(cur_block) {
                    Entry::Occupied(oe) => *oe.into_mut(),
                    Entry::Vacant(ve) => {
                        let (block, opt_succs) = self.find_block(cur_block)?;

                        let f_cur_block = match opt_succs {
                            None => self.new_graph.add_node(flstr::mk_code_node(block)),
                            Some(SuccInfo::Single(next_block)) => {
                                let f_cur_block =
                                    self.new_graph.add_node(flstr::mk_code_node(block));
                                worklist.push((f_cur_block, flstr::CfgEdge::True, next_block));
                                f_cur_block
                            }
                            Some(SuccInfo::Branch(cond, then_block, else_block)) => {
                                // make the cond node
                                let f_cond_node = self
                                    .new_graph
                                    .add_node(flstr::mk_cond_node(self.cctx, cond));
                                worklist.push((f_cond_node, flstr::CfgEdge::True, then_block));
                                worklist.push((f_cond_node, flstr::CfgEdge::False, else_block));

                                // only insert a preceding code node if needed
                                if block.is_empty() {
                                    f_cond_node
                                } else {
                                    let f_cur_block =
                                        self.new_graph.add_node(flstr::mk_code_node(block));
                                    self.new_graph.add_edge(
                                        f_cur_block,
                                        f_cond_node,
                                        flstr::CfgEdge::True,
                                    );
                                    f_cur_block
                                }
                            }
                        };

                        *ve.insert(f_cur_block)
                    }
                };

                self.new_graph
                    .add_edge(f_pred_block, f_cur_block, pred_edge_ty);
            }
        }

        Ok(flstr::ControlFlowGraph::new(
            self.new_graph,
            new_entry,
            self.cctx,
            self.ccfg,
        ))
    }

    fn find_block(&self, block: CCFGRef) -> Result<(Vec<CCFGRef>, Option<SuccInfo>), &'static str> {
        if self.ccfg.g[block] != CCFGNode::Action(ActionNode::BasicBlock) {
            return Err("import: basic block doesn't begin with BasicBlock");
        }

        let mut ret = Vec::new();
        let mut cur_node = if let Some(start) = self.ccfg.next_action(block) {
            start
        } else {
            return Ok((Vec::new(), None));
        };

        loop {
            match self.ccfg.g[cur_node] {
                CCFGNode::Action(ActionNode::BasicBlock) => {
                    return Ok((ret, Some(SuccInfo::Single(cur_node))));
                }
                CCFGNode::Action(ActionNode::If) => {
                    let (condition, then_goto, else_goto) = if_info(&self.ccfg, cur_node)?;
                    let then_block = goto_next(&self.ccfg, then_goto)?;
                    let else_block = goto_next(&self.ccfg, else_goto)?;
                    return Ok((
                        ret,
                        Some(SuccInfo::Branch(condition, then_block, else_block)),
                    ));
                }
                CCFGNode::Action(ActionNode::Goto) => {
                    let next_block = goto_next(&self.ccfg, cur_node)?;
                    return Ok((ret, Some(SuccInfo::Single(next_block))));
                }
                CCFGNode::Action(_) => {
                    ret.push(cur_node);
                    if let Some(next) = self.ccfg.next_action(cur_node) {
                        cur_node = next;
                    } else {
                        return Ok((ret, None));
                    }
                }
                CCFGNode::Value(_) => return Err("import: found Value node"),
                _ => return Err("import: unknown node type"),
            }
        }
    }
}

fn goto_next(ccfg: &CCFG, goto_node: CCFGRef) -> Result<CCFGRef, &'static str> {
    ccfg.goto(goto_node)
        .ok_or("import: Goto node has no destination")
}

fn if_info(ccfg: &CCFG, if_node: CCFGRef) -> Result<(CCFGRef, CCFGRef, CCFGRef), &'static str> {
    let mut condition = None;
    let mut then_dst = None;
    let mut else_dst = None;
    for e in ccfg.g.edges(if_node) {
        match e.weight() {
            CCFGEdge::Value(ValueEdge::Conditional) => condition = Some(e.target()),
            CCFGEdge::Action(ActionEdge::IfThen) => then_dst = Some(e.target()),
            CCFGEdge::Action(ActionEdge::IfElse) => else_dst = Some(e.target()),
            _ => (),
        }
    }
    match (condition, then_dst, else_dst) {
        (Some(c), Some(t), Some(e)) => Ok((c, t, e)),
        _ => Err("import: If node has no condition, then node, and/or else node"),
    }
}

impl AstContext for CCFG {
    type Block = Vec<CCFGRef>;
    type Variable = CCFGRef;
    type BoolVariable = CCFGRef;
    type Condition = CCFGRef;
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
        vec![self.assign(var, val, unk)]
    }

    fn mk_bool_var_assign(
        &mut self,
        &var: &Self::BoolVariable,
        &cond: &Self::Condition,
    ) -> Self::Block {
        let unk = self.unknown;
        vec![self.assign(var, cond, unk)]
    }
}
