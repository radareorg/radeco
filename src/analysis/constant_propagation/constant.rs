//! Module that implements constant propagation on the SSA IR.
//!
//! This module implements 'Sparse Conditional Constant' algorithm to
//! perform the propagation. For more details, please refer:
//!    * https://www.cs.utexas.edu/~lin/cs380c/wegman.pdf.
//!

use petgraph::graph::{NodeIndex};
use std::collections::{HashMap};

#[derive(Copy, Clone, Debug, PartialEq)]
enum ExprVal {
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
    fn get_ops(&self, i: &NodeIndex) -> NodeIndex;
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

    pub fn analyze(&mut self) {
        let mut g = &mut (self.g);
        let mut ssa_worklist = &mut (self.ssa_worklist);
        let mut cfg_worklist = &mut (self.cfg_worklist);
        let mut executable = &mut (self.executable);
        let mut expr_val = &mut (self.expr_val);

        // Initializations
        cfg_worklist.push(g.start_node());
        for block in g.get_blocks().iter() {
            executable.insert(block.clone(), false);
            for expr in g.get_exprs(block) {
                expr_val.insert(expr.clone(), ExprVal::Top);
            }
        }

        // Iterative worklist.
        while ssa_worklist.len() > 0 || cfg_worklist.len() > 0 {
            while cfg_worklist.len() > 0 {
                let block = cfg_worklist.pop().unwrap();
                *(executable.get_mut(&block).unwrap()) = true;
                let phis = g.get_phis(&block);
                // Evaluate phis
                for phi in phis.iter() {
                    // TODO: Actually do the meet here.
                }
                // Iterate through all the expression in the block.
                for expr in g.get_exprs(&block) {
                    // TODO: Evaluate expr and update expr_val.
                    for _use in g.get_uses(&expr) {
                        let owner_block = g.get_block(&_use);
                        if *executable.get(&owner_block).unwrap() {
                            ssa_worklist.push(_use);
                        }
                    }
                }
            }

            while ssa_worklist.len() > 0 {
                let e = ssa_worklist.pop().unwrap();
                // Get the operation/expression to which this operand belongs to.
                let op = g.get_ops(&e);
                // TODO: Evaluate the expression.
                let t = ExprVal::Top;
                if t != *expr_val.get(&op).unwrap() {
                    *(expr_val.get_mut(&op).unwrap()) = ExprVal::Top;
                    for _use in g.get_uses(&op).iter() {
                        let b = g.get_block(_use);
                        if *executable.get(&b).unwrap() {
                            ssa_worklist.push(_use.clone())
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
