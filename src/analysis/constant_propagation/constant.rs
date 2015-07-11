//! Module that implements constant propagation on the SSA IR.
//!
//! This module implements 'Sparse Conditional Constant' algorithm to
//! perform the propagation. For more details, please refer:
//!    * https://www.cs.utexas.edu/~lin/cs380c/wegman.pdf.
//!

use petgraph::graph::{NodeIndex};
use std::collections::{HashMap};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ExprVal {
    Top,
    Bottom,
    Const(i64),
}

fn meet(v1: &ExprVal, v2: &ExprVal) -> ExprVal {
    // Any ^ Top    = Any
    // Any ^ Bottom = Bottom
    //   C ^ C      = C      (C = Constant)
    //   C ^ D      = Bottom (C, D = Constant and C != D).

    match *v1 {
        ExprVal::Top    => return *v2,
        ExprVal::Bottom => return ExprVal::Bottom,
        _               => { },
    }

    match *v2 {
        ExprVal::Top    => return *v1,
        ExprVal::Bottom => return ExprVal::Bottom,
        _               => { },
    }

    if *v1 != *v2 {
        return ExprVal::Bottom;
    }

    return *v1;
}

pub trait SSAGraphIter {
    fn get_blocks(&self) -> Vec<NodeIndex>;
    fn start_node(&self) -> NodeIndex;
    fn get_exprs(&self, i: &NodeIndex) -> Vec<NodeIndex>;
    fn get_phis(&self, i: &NodeIndex) -> Vec<NodeIndex>;
    fn get_uses(&self, i: &NodeIndex) -> Vec<NodeIndex>;
    fn get_block(&self, i: &NodeIndex) -> NodeIndex;
    fn get_operands(&self, i: &NodeIndex) -> Vec<NodeIndex>;
    fn lhs(&self, i: &NodeIndex) -> NodeIndex;
    fn rhs(&self, i: &NodeIndex) -> NodeIndex;
}

pub struct Analyzer<T: SSAGraphIter + Clone> {
    ssa_worklist: Vec<NodeIndex>,
    cfg_worklist: Vec<NodeIndex>,
    executable: HashMap<NodeIndex, bool>,
    expr_val: HashMap<NodeIndex, ExprVal>,
    g: T,
}

impl<T: SSAGraphIter + Clone> Analyzer<T> {
    pub fn new(g: &mut T) -> Analyzer<T> {
        Analyzer {
            ssa_worklist: Vec::new(),
            cfg_worklist: Vec::new(),
            executable: HashMap::new(),
            expr_val: HashMap::new(),
            g: g.clone(),
        }
    }

    pub fn visit_phi(&mut self, i: &NodeIndex) -> ExprVal {
        let operands = self.g.get_operands(i);
        let mut phi_val = self.expr_val.get(i).unwrap().clone();
        for o in operands.iter() {
            let b = self.g.get_block(&o);
            let val = match *self.executable.get(&b).unwrap() {
                true => *self.expr_val.get(&o).unwrap(),
                false => ExprVal::Top,
            };
            phi_val = meet(&phi_val, &val);
        }

        return phi_val;
    }

    pub fn visit_expression(&mut self, i: &NodeIndex) -> ExprVal {
        ExprVal::Top
    }

    pub fn analyze(&mut self) {
        // Initializations
        self.cfg_worklist.push(self.g.start_node());
        for block in self.g.get_blocks().iter() {
            self.executable.insert(block.clone(), false);
            for expr in self.g.get_exprs(block) {
                self.expr_val.insert(expr.clone(), ExprVal::Top);
            }
        }

        // Iterative worklist.
        while self.ssa_worklist.len() > 0 || self.cfg_worklist.len() > 0 {
            while self.cfg_worklist.len() > 0 {
                let block = self.cfg_worklist.pop().unwrap();
                *(self.executable.get_mut(&block).unwrap()) = true;
                let phis = self.g.get_phis(&block);
                // Evaluate phis
                for phi in phis.iter() {
                    // Visit the phi's and set their values.
                    let v = self.visit_phi(phi);
                    *(self.expr_val.get_mut(phi).unwrap()) = v;
                }
                // Iterate through all the expression in the block.
                for expr in self.g.get_exprs(&block) {
                    let val = self.visit_expression(&expr);
                    *(self.expr_val.get_mut(&expr).unwrap()) = val;
                    for _use in self.g.get_uses(&expr) {
                        let owner_block = self.g.get_block(&_use);
                        if *self.executable.get(&owner_block).unwrap() {
                            self.ssa_worklist.push(_use);
                        }
                    }
                }
            }

            while self.ssa_worklist.len() > 0 {
                let e = self.ssa_worklist.pop().unwrap();
                // self.get the operation/expression to which this operand belongs to.
                let exprs = self.g.get_uses(&e);
                for expr in exprs.iter() {
                    let t = self.visit_expression(&expr);
                    if t != *self.expr_val.get(expr).unwrap() {
                        *(self.expr_val.get_mut(expr).unwrap()) = t;
                        for _use in self.g.get_uses(expr).iter() {
                            let b = self.g.get_block(_use);
                            if *self.executable.get(&b).unwrap() {
                                self.ssa_worklist.push(_use.clone())
                            }
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::{ExprVal, meet};

    #[test]
    fn test_meet() {
        let t = ExprVal::Top;
        let b = ExprVal::Bottom;
        let c1 = ExprVal::Const(1);
        let c2 = ExprVal::Const(2);

        assert_eq!(meet(&t,  &t) , t);
        assert_eq!(meet(&t,  &b) , b);
        assert_eq!(meet(&t,  &c1), c1);
        assert_eq!(meet(&t,  &c2), c2);
        assert_eq!(meet(&c1, &b) , b);
        assert_eq!(meet(&c2, &c1), b);
        assert_eq!(meet(&c1, &c1), c1);
    }
}
