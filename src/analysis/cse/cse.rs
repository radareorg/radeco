//! Common Subexpression Elimination (CSE)
//!
//! This moduile implements methods and structs to perform CSE.
use std::collections::HashMap;
use std::marker::PhantomData;

use middle::ssa::ssa_traits::{NodeData, NodeType, SSA, SSAMod, SSAWalk};
use middle::ssa::cfg_traits::{CFG, CFGMod};
use middle::ir::MOpcode;

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

    fn hash_args(&self, args: &Vec<S::ValueRef>) -> String {
        let mut result = String::new();
        for arg in args {
            if let Ok(node_data) = self.ssa.get_node_data(arg) {
                if let NodeType::Op(opc) = node_data.nt {
                    match opc {
                        MOpcode::OpConst(val) => result.push_str(&format!("{}", val)),
                        _ => result.push_str(self.hashed.get(arg).expect("Hash value not found!")),
                    }
                }
            }
        }
        result
    }

    fn hash_string(&self, idx: &S::ValueRef) -> Vec<Option<String>> {
        let hashes: &mut [Option<String>; 2] = &mut [None, None];
        if let Ok(node_data) = self.ssa.get_node_data(idx) {
            if let NodeType::Op(opc) = node_data.nt {
                let args = self.ssa.args_of(*idx);
                let hashed_args = self.hash_args(&args);
                let hs0 = format!("{}{}", opc, hashed_args);
                let hs1 = if opc.is_commutative() {
                    let hashed_args = self.hash_args(&args.iter()
                                                          .cloned()
                                                          .rev()
                                                          .collect::<Vec<_>>());
                    Some(format!("{}{}", opc, hashed_args))
                } else {
                    None
                };
                hashes[0] = Some(hs0);
                hashes[1] = hs1;
            }
        }
        hashes.to_vec()
    }

    pub fn run(&mut self) {
        for expr in self.ssa.inorder_walk() {
            let hashes = self.hash_string(&expr);
            let mut replaced = false;
            for hs_ in &hashes {
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
                        if self.ssa.block_of(&ex_idx) == self.ssa.block_of(&expr) {
                            self.ssa.replace(expr, *ex_idx);
                            replaced = true;
                            break;
                        }
                    }
                }
            }

            if !replaced {
                if let Some(ref hs0) = hashes[0] {
                    self.exprs.entry(hs0.clone()).or_insert(Vec::new()).push(expr);
                    self.hashed.insert(expr, hs0.clone());
                }
                if let Some(ref hs1) = hashes[1] {
                    self.exprs.entry(hs1.clone()).or_insert(Vec::new()).push(expr);
                }
            }
        }
    }
}
