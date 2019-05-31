//! Common Subexpression Elimination (CSE)
//!
//! This module implements methods and structs to perform CSE.
//! However, this implement doesn't consider much about commutative
//! opcodes. On the other hand, considering too much will cause
//! a huge memory consume. Thus, a balanced solution should be
//! improved.

use std::any::Any;
use std::collections::HashMap;

use crate::analysis::analyzer::{
    Action, Analyzer, AnalyzerInfo, AnalyzerKind, AnalyzerResult, Change, FuncAnalyzer,
    ReplaceValue,
};
use crate::frontend::radeco_containers::RadecoFunction;

use crate::middle::ir::MOpcode;
use crate::middle::ssa::ssa_traits::SSA;
use crate::middle::ssa::ssa_traits::{NodeType, SSAMod, SSAWalk};
use crate::middle::ssa::ssastorage::SSAStorage;

#[derive(Debug)]
pub struct CSE {
    exprs: HashMap<String, Vec<<SSAStorage as SSA>::ValueRef>>,
    hashed: HashMap<<SSAStorage as SSA>::ValueRef, String>,
}

const NAME: &str = "cse";
const REQUIRES: &[AnalyzerKind] = &[];

pub const INFO: AnalyzerInfo = AnalyzerInfo {
    name: NAME,
    kind: AnalyzerKind::CSE,
    requires: REQUIRES,
    uses_policy: true,
};

impl CSE {
    pub fn new() -> CSE {
        CSE {
            exprs: HashMap::new(),
            hashed: HashMap::new(),
        }
    }

    fn hash_args(&self, ssa: &SSAStorage, args: &[<SSAStorage as SSA>::ValueRef]) -> String {
        let mut result = String::new();
        for arg in args {
            if let Ok(node_data) = ssa.node_data(*arg) {
                match node_data.nt {
                    NodeType::Op(opc) => match opc {
                        MOpcode::OpConst(val) => result.push_str(&format!("{}", val)),
                        _ => {
                            if let Some(hash) = self.hashed.get(arg) {
                                result.push_str(hash);
                            } else {
                                radeco_err!("Hash value not found!");
                                result.push_str(&format!("{:?}", arg));
                            }
                        }
                    },
                    _ => {
                        // NOTE: Phi functions and Comment node should be considered, otherwise,
                        // CSE will replace different op nodes, which only use phi/comment as
                        // operands, causing wrong analysis. Meanwhile, we take Invalid node
                        // into account, for it may not influence the answer. Actually, till
                        // now, there is no possibility to have an Invalid node.
                        // Attantion: any call node will cause a lot of Comment nodes as it return,
                        // to symbolize every register.
                        result.push_str(&format!("{:?}", arg));
                    }
                }
            }
        }
        result
    }

    // NOTE: Because we have sorted the operands, it's unnecessary to consider commutative opcodes.
    fn hash_string(&self, ssa: &SSAStorage, idx: &<SSAStorage as SSA>::ValueRef) -> Option<String> {
        if let Ok(node_data) = ssa.node_data(*idx) {
            if let NodeType::Op(opc) = node_data.nt {
                let args = ssa.operands_of(*idx);
                let hashed_args = self.hash_args(ssa, &args);
                let hs = format!("{}{}", opc, hashed_args);
                return Some(hs);
            }
        }
        None
    }
}

impl Analyzer for CSE {
    fn info(&self) -> &'static AnalyzerInfo {
        &INFO
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl FuncAnalyzer for CSE {
    fn analyze<T: FnMut(Box<dyn Change>) -> Action>(
        &mut self,
        func: &mut RadecoFunction,
        policy: Option<T>,
    ) -> Option<Box<dyn AnalyzerResult>> {
        let mut policy = policy.expect("A policy function must be provided");

        {
            let ssa = func.ssa_mut();

            for expr in ssa.inorder_walk() {
                let hs = match self.hash_string(ssa, &expr) {
                    Some(hs) => hs,
                    None => continue,
                };
                let mut replaced = false;
                if let Some(ex_idxs) = self.exprs.get(&hs).cloned() {
                    // NOTE: Though we can eliminate expression even if the two aren't in the same
                    // block, we do not do so cause it requires that block b1 (block of the
                    // replacer) must dominate block b2 (the replacee). Since we are currently not
                    // using the dominator information, this replacement may not be safe. Hence we
                    // restrict outselves to the case where both the expressions belong to the same
                    // block.
                    for ex_idx in &ex_idxs {
                        if ssa.block_for(*ex_idx) == ssa.block_for(expr) {
                            match policy(Box::new(ReplaceValue(expr, *ex_idx))) {
                                Action::Apply => {
                                    ssa.replace_value(expr, *ex_idx);
                                    replaced = true;
                                    break;
                                }
                                Action::Skip => (),
                                Action::Abort => return None,
                            }
                        }
                    }
                }

                if !replaced {
                    self.exprs
                        .entry(hs.clone())
                        .or_insert_with(Vec::new)
                        .push(expr);
                    self.hashed.insert(expr, hs.clone());
                }
            }
        }

        None
    }
}
