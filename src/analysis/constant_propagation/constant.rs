//! Module that implements constant propagation on the SSA IR.
//!
//! This module implements 'Sparse Conditional Constant' algorithm to
//! perform the propagation. For more details, please refer:
//!    * https://www.cs.utexas.edu/~lin/cs380c/wegman.pdf.
//!

use std::collections::{HashMap};
use ::middle::ssa::{SSA, NodeData};
use ::middle::ir::{MOpcode, MArity};

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

pub struct Analyzer<T: SSA + Clone> {
    ssa_worklist: Vec<T::ValueRef>,
    cfg_worklist: Vec<T::ActionRef>,
    executable: HashMap<T::ActionRef, bool>,
    expr_val: HashMap<T::ValueRef, ExprVal>,
    g: T,
}

impl<T: SSA + Clone> Analyzer<T> {
    pub fn new(g: &mut T) -> Analyzer<T> {
        Analyzer {
            ssa_worklist: Vec::new(),
            cfg_worklist: Vec::new(),
            executable: HashMap::new(),
            expr_val: HashMap::new(),
            g: g.clone(),
        }
    }

    pub fn visit_phi(&mut self, i: &T::ValueRef) -> ExprVal {
        let operands = self.g.get_operands(i);
        let mut phi_val = self.expr_val.get(i).unwrap().clone();
        for o in operands.iter() {
            let b = self.g.get_block(&o);
            let val = match *self.executable.get(&b).unwrap() {
                true => *self.expr_val.get(&o).unwrap(),
                false => continue,
            };
            phi_val = meet(&phi_val, &val);
        }
        return phi_val;
    }

    pub fn evaluate_control_flow(&mut self, i: &T::ValueRef, opcode: MOpcode) -> ExprVal {
        match opcode {
            MOpcode::OpJmp
            | MOpcode::OpCall => {
                // TODO: Experimental.
                let target = self.g.get_target(i);
                self.cfg_worklist.push(target);
            },
            MOpcode::OpCJmp => {
                let operands = self.g.get_operands(i);
                let cond = operands[0].clone();
                let cond_val = self.expr_val.get(&cond).unwrap();
                //let target = operands[1];
                //let target_val = self.expr_val.get(&target).unwrap();
                let true_branch = self.g.get_true_branch(i);
                let false_branch = self.g.get_false_branch(i);

                match *cond_val {
                    ExprVal::Bottom => {
                        self.cfg_worklist.push(true_branch);
                        self.cfg_worklist.push(false_branch);
                    },
                    ExprVal::Top => {
                        // TODO: Not really sure what to do here.
                        return ExprVal::Top;
                    },
                    ExprVal::Const(cval) => {
                        if cval == 0 {
                            self.cfg_worklist.push(true_branch);
                        } else {
                            self.cfg_worklist.push(false_branch);
                        }
                    },
                }
            },
            _ => unreachable!()
        }

        return ExprVal::Top;
    }

    pub fn evaluate_unary_op(&mut self, i: &T::ValueRef, opcode: MOpcode) -> ExprVal {
        let operand = self.g.get_operands(i)[0].clone();
        let val = self.expr_val.get(&operand).unwrap();
        let const_val = if let ExprVal::Const(cval)  = *val { cval } else { return *val; };
        let _val = match opcode {
            MOpcode::OpNarrow(size)
                | MOpcode::OpWiden(size) => { 
                    let mask = (2 << (size + 1)) - 1;
                const_val & mask
            },
            MOpcode::OpNot  => { 
                !const_val as i64
            },
            _ => unreachable!(),
        };

        ExprVal::Const(_val)
    }

    pub fn evaluate_binary_op(&mut self, i: &T::ValueRef, opcode: MOpcode) -> ExprVal {
        let operands = self.g.get_operands(i)
                             .iter()
                             .map(|x| self.expr_val.get(x).unwrap())
                             .collect::<Vec<_>>();

        let lhs = operands[0];
        let rhs = operands[1];

        let lhs_val = if let ExprVal::Const(cval) = *lhs { cval } else { return *lhs; };
        let rhs_val = if let ExprVal::Const(cval) = *rhs { cval } else { return *rhs; };
        
        let _val = match opcode {
            MOpcode::OpAdd        => { lhs_val + rhs_val },
            MOpcode::OpSub        => { lhs_val - rhs_val },
            MOpcode::OpMul        => { lhs_val * rhs_val },
            MOpcode::OpDiv        => { lhs_val / rhs_val },
            MOpcode::OpMod        => { lhs_val % rhs_val },
            MOpcode::OpAnd        => { lhs_val & rhs_val },
            MOpcode::OpOr         => { lhs_val | rhs_val },
            MOpcode::OpXor        => { lhs_val ^ rhs_val },
            MOpcode::OpCmp        => { (lhs_val == rhs_val) as i64 },
            MOpcode::OpGt         => { (lhs_val > rhs_val) as i64 },
            MOpcode::OpLt         => { (lhs_val < rhs_val) as i64 },
            MOpcode::OpLteq       => { (lhs_val <= rhs_val) as i64 },
            MOpcode::OpGteq       => { (lhs_val >= rhs_val) as i64 },
            MOpcode::OpLsl        => { lhs_val << rhs_val },
            MOpcode::OpLsr        => { lhs_val >> rhs_val },
            _ => unreachable!(),
        };

        ExprVal::Const(_val)
    }

    // TODO:
    //  * Evaluate the expression actually if all it's operands are constants.
    //  * If the expression is a branch, then add the appropriate edges to the cfg_worklist.
    //  * If the expression is a jump, then place the target edge on the cfg_worklist.
    pub fn visit_expression(&mut self, i: &T::ValueRef) -> ExprVal {
        // Get the actual node corresponding to the NodeIndex.
        let expr = self.g.get_node_data(i);

        // Handle NodeData::Const as it is trivial.
        if let NodeData::Const(val) = expr {
            return ExprVal::Const(val as i64);
        }

        // After this point, it has to be NodeData::Op.
        let opcode = if let NodeData::Op(_opcode, _) = expr {
            _opcode.clone()
        } else { 
            panic!("Found something other than an Operator or Constant");
        };

        // Handle conditionals and jump statements separately.
        match opcode {
            MOpcode::OpJmp  |
            MOpcode::OpCJmp |
            MOpcode::OpCall => {
                   return self.evaluate_control_flow(i, opcode);
            }
            _ => { },
        }

        match opcode.arity() {
            MArity::Unary => self.evaluate_unary_op(i, opcode),
            MArity::Binary => self.evaluate_binary_op(i, opcode),
            _ => unimplemented!(),
        }
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
