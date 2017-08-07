//! Common Subexpression Elimination (CSE)
//!
//! This module implements methods and structs to perform CSE.
//! 
//! This file is the new implement, considering the phi funcions,
//! and takes more care of commutative Op. However, this implement 
//! will consume too much memory. We just assume there will not 
//! be too many commutative opcodes in a single block.
//!
//! Additionally, a good solution should be sort the operands when
//! construct the SSA form.
use std::collections::HashMap;
use std::marker::PhantomData;

use middle::ssa::ssa_traits::{NodeType, SSA, SSAMod, SSAWalk};
use middle::ir::MOpcode;

#[derive(Debug)]
pub struct CSE<'a, I, S>
    where I: Iterator<Item = S::ValueRef>,
          S: 'a + SSAMod + SSA + SSAWalk<I>
{
    exprs: HashMap<String, Vec<S::ValueRef>>,
    hashed: HashMap<S::ValueRef, Vec<String>>,
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

    fn push_vec(&self, op1: &mut Vec<String>, op2: &Vec<String>) {
        let mut result: Vec<String> = Vec::new();
        while let Some(substr1) = op1.pop() {
            for substr2 in op2 {
                let substr = format!("{}{}", substr1, substr2);
                result.push(substr);
            }
        }
        op1.append(&mut result);
    }

    fn hash_args(&self, idx: &S::ValueRef, args: &[S::ValueRef]) -> Vec<String> {
        let mut result = vec![String::new()];
        for arg in args {
            if let Ok(node_data) = self.ssa.get_node_data(arg) {
                if self.ssa.block_of(idx) != self.ssa.block_of(arg) {
                    // NOTE: Considering the memory consume, we just pay attention on the args 
                    // in the same block of the expression, which may reduce the consume.
                    self.push_vec(&mut result, &vec![format!("{:?}", arg)]);
                    continue;
                }
                match node_data.nt {
                    NodeType::Op(opc) => {
                        match opc {
                            MOpcode::OpConst(val) => self.push_vec(
                                    &mut result,  &vec![format!("{}", val)]),
                            _ => self.push_vec(&mut result, 
                                    self.hashed.get(arg)
                                    .expect("Hash value not found!")),
                        }
                    }
                    NodeType::Phi => {
                        // NOTE: Phi functions should be considered, otherwise, CSE will replace 
                        // different op node, which only use phi functions as operands, causing 
                        // wrong analysis.
                        self.push_vec(&mut result, &vec![format!("{:?}", arg)]);
                    }
                    _ => {}
                }
            }
        }
        result
    }

    fn hash_string(&self, idx: &S::ValueRef) -> Vec<String> {
        let mut hashes: Vec<String> = Vec::new();
        if let Ok(node_data) = self.ssa.get_node_data(idx) {
            if let NodeType::Op(opc) = node_data.nt {
                hashes.push(String::from(format!("{}", opc)));
                let args = self.ssa.args_of(*idx);
                let mut hashed_args = self.hash_args(idx, &args);
                if opc.is_commutative() {
                    hashed_args.append(&mut self.hash_args(idx, &args.iter()
                                                          .cloned()
                                                          .rev()
                                                          .collect::<Vec<_>>()));
                }
                self.push_vec(&mut hashes, &hashed_args);
            }
        }
        hashes
    }

    pub fn run(&mut self) {
        let mut now_block: &S::ActionRef = &self.ssa.start_node();
        for expr in self.ssa.inorder_walk() {
            println!("{:?}", expr);
            println!("{:?}", self.ssa.exit_node());
            if now_block != &self.ssa.block_of(&expr) {
                now_block = &self.ssa.block_of(&expr);
                self.exprs = HashMap::new();
                self.hashed = HashMap::new();
            }
            let hashes = self.hash_string(&expr);
            let mut replaced = false;
            for hs in &hashes {
                if let Some(ex_idxs) = self.exprs.get(hs).cloned() {
                    // NOTE: Though we can eliminate expression even if the two aren't in the same
                    // block, we do not do so cause it requires that block b1 (block of the
                    // replacer) must dominate block b2 (the replacee). Since we are currently not
                    // using the dominator information, this replacement may not be safe. Hence we
                    // restrict outselves to the case where both the expressions belong to the same
                    // block.
                    for ex_idx in &ex_idxs {
                        if self.ssa.block_of(ex_idx) == self.ssa.block_of(&expr) {
                            self.ssa.replace(expr, *ex_idx);
                            replaced = true;
                            break;
                        }
                    }
                }
                if replaced {
                    break;
                }
            }

            if !replaced {
                for hs in &hashes {
                    self.exprs.entry(hs.clone()).or_insert_with(Vec::new).push(expr);
                    self.hashed.entry(expr).or_insert_with(Vec::new).push(hs.clone());
                }
            }
        }
    }
}
