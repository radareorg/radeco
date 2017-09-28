//! Common Subexpression Elimination (CSE)
//!
//! This module implements methods and structs to perform CSE.
//! However, this implement doesn't consider much about commutative 
//! opcodes. One the other hand, considering too much will cause
//! a huge memory consume. Thus, a balanced solution should be 
//! improved.

use std::collections::HashMap;
use std::marker::PhantomData;

use middle::ssa::ssa_traits::{NodeType, SSA, SSAMod, SSAWalk};
use middle::ir::MOpcode;
use super::ssasort::Sorter;

#[derive(Debug)]
pub struct CSE<'a, I, S>
    where I: Iterator<Item = S::ValueRef>,
          S: 'a + SSAMod + SSA + SSAWalk<I>
{
    exprs: HashMap<String, Vec<S::ValueRef>>,
    hashed: HashMap<S::ValueRef, String>,
    ssa: &'a mut S,
    foo: PhantomData<I>,
}

impl<'a, I, S> CSE<'a, I, S>
where I: Iterator<Item = S::ValueRef>,
      S: 'a + SSAMod + SSA + SSAWalk<I>
{
    pub fn new(ssa: &'a mut S) -> CSE<'a, I, S> {
        CSE {
            exprs: HashMap::new(),
            hashed: HashMap::new(),
            ssa: ssa,
            foo: PhantomData,
        }
    }

    fn hash_args(&self, args: &[S::ValueRef]) -> String {
        let mut result = String::new();
        for arg in args {
            if let Ok(node_data) = self.ssa.node_data(*arg) {
                match node_data.nt {
                    NodeType::Op(opc) => {
                        match opc {
                            MOpcode::OpConst(val) => result.push_str(&format!("{}", val)),
                            _ => result.push_str(self.hashed.get(arg).expect("Hash value not found!")),
                        }
                    }
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
    fn hash_string(&self, idx: &S::ValueRef) -> Option<String> {
        if let Ok(node_data) = self.ssa.node_data(*idx) {
            if let NodeType::Op(opc) = node_data.nt {
                let args = self.ssa.operands_of(*idx);
                let hashed_args = self.hash_args(&args);
                let hs = format!("{}{}", opc, hashed_args);
                return Some(hs);
            }
        }
        None
    }

    pub fn run(&mut self) {
        {
            let mut sorter = Sorter::new(self.ssa);
            sorter.run();
        }
        // Sort the operands for commutative opcode first;

        for expr in self.ssa.inorder_walk() {
            let hs_ = self.hash_string(&expr);
            let mut replaced = false;
            if hs_.is_none() {
                break;
            }
            let hs = hs_.as_ref().unwrap();
            if let Some(ex_idxs) = self.exprs.get(hs).cloned() {
                // NOTE: Though we can eliminate expression even if the two aren't in the same
                // block, we do not do so cause it requires that block b1 (block of the
                // replacer) must dominate block b2 (the replacee). Since we are currently not
                // using the dominator information, this replacement may not be safe. Hence we
                // restrict outselves to the case where both the expressions belong to the same
                // block.
                for ex_idx in &ex_idxs {
                    if self.ssa.block_of(*ex_idx) == self.ssa.block_of(expr) {
                        self.ssa.replace_value(expr, *ex_idx);
                        replaced = true;
                        break;
                    }
                }
            }

            if !replaced {
                if let Some(ref hs) = hs_ {
                    self.exprs.entry(hs.clone()).or_insert_with(Vec::new).push(expr);
                    self.hashed.insert(expr, hs.clone());
                }
            }
        }
    }
}
