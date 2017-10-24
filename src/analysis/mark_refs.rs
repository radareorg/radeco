//! Data flow analysis to mark ssa nodes as references or scalars.
//!
//! This analysis makes some assumptions about how references are forged/used:
//!   1. Every subtree has exactly one leaf that is a reference,
//!   that is, two references are never used in an operation (such as add, sub etc.)
//!   Indeed, there is nothing that prevents someone from code that does this, but is generally
//!   not an accepted practice.
//!
//!   2. References are not used in operations such as mul, left shift, etc.


use frontend::radeco_containers::RadecoFunction;
use middle::ir::MOpcode;
use middle::ssa::cfg_traits::CFG;
use middle::ssa::ssa_traits::{ValueInfo, ValueType, SSA};

use middle::ssa::ssastorage::{NodeData, SSAStorage};
use petgraph::graph::NodeIndex;

use std::collections::{HashSet, VecDeque};
use std::fmt::Debug;

#[derive(Clone, Copy, Debug)]
pub struct ReferenceMarker { }

// T_a = C
//     = R
//     = op1 U op2
//
//   C = op1 U op2 => op1 = C /\ op2 = C
//   R = op1 U op2 => (op1 = R /\ op2 = C) \/ (op1 = C /\ op2 = R)
//

#[derive(Debug, Clone)]
pub enum Constraint {
    Union(usize, usize),
    // Solve eq
    Equality(usize, Box<Constraint>),
    // Solve assert
    Assertion(Box<Constraint>),
    Or(Box<Constraint>, Box<Constraint>),
    And(Box<Constraint>, Box<Constraint>),
    // No solve
    Value(ValueType),
}

#[derive(Default, Debug)]
pub struct ConstraintSet<T: Debug> {
    bindings: Vec<(T, ValueType)>,
    set: VecDeque<Constraint>,
}

impl<T: Clone + Debug> ConstraintSet<T> {
    pub fn add_constraint(&mut self, c: Constraint) {
        self.set.push_back(c);
    }

    pub fn bind(&mut self, n: &[T]) {
        for i in n {
            self.bindings.push((i.clone(), ValueType::Unresolved));
        }
    }

    pub fn solve(&mut self) {
        let mut first_false = 0;
        while !self.set.is_empty() {
            let constraint = self.set.pop_front().expect("Cannot be `None`");
            // See if we went a complete circle. If we did, this means that the equations in the
            // set are no longer solvable.
            println!("Solving: {:?}", constraint);
            if first_false > self.set.len() {
                break;
            }

            // Match to see if this is an Eq constraint or an assert
            let res = match constraint {
                Constraint::Equality(_, _) => self.solve_eq(&constraint),
                Constraint::Equality(_, box _) => self.solve_eq(&constraint),
                Constraint::Assertion(_) => self.solve_assert(&constraint),
                _ => false,
            };

            if !res {
                // It wasn't solved. Push it back in.
                self.set.push_back(constraint);
                first_false += 1;
            } else {
                first_false = 0;
            }
        }
    }

    fn solve_eq(&mut self, constraint: &Constraint) -> bool {
        println!("Solving Eq: {:?}!", constraint);
        match *constraint {
            Constraint::Equality(ni, box ref cs) => {
                match *cs {
                    Constraint::Value(vt) => {
                        self.bindings[ni].1 = vt;
                        true
                    }
                    Constraint::Union(op1, op2) => {
                        println!("UNION!");
                        match self.bindings[ni].1 {
                            ValueType::Scalar => {
                                // Both, op1 and op2, are scalars
                                self.bindings[op1].1 = ValueType::Scalar;
                                self.bindings[op2].1 = ValueType::Scalar;
                                true
                            },
                            ValueType::Reference => {
                                // Assert some facts
                                let cons = Constraint::Or(
                                    Box::new(Constraint::And(Box::new(Constraint::Equality(op1, Box::new(Constraint::Value(ValueType::Reference)))),
                                                             Box::new(Constraint::Equality(op2, Box::new(Constraint::Value(ValueType::Scalar)))))),
                                    Box::new(Constraint::And(Box::new(Constraint::Equality(op2, Box::new(Constraint::Value(ValueType::Reference)))),
                                                             Box::new(Constraint::Equality(op1, Box::new(Constraint::Value(ValueType::Scalar))))))
                                    );
                                self.set.push_back(Constraint::Assertion(Box::new(cons)));
                                println!("Added new assertion!");
                                true
                            },
                            ValueType::Unresolved => {
                                // Look at the Union
                                let op1_vt = self.bindings[op1].1;
                                let op2_vt = self.bindings[op2].1;
                                let (vt, retval)  = match (op1_vt, op2_vt) {
                                    (ValueType::Invalid, _)
                                    | (_, ValueType::Invalid)
                                    | (ValueType::Reference, ValueType::Reference) => {
                                        (ValueType::Invalid, true)
                                    }

                                    (ValueType::Reference, _) => { 
                                        self.bindings[op2].1 = ValueType::Scalar;
                                        (ValueType::Reference , true)
                                    }

                                    (_, ValueType::Reference) => {
                                        self.bindings[op1].1 = ValueType::Scalar;
                                        (ValueType::Reference, true)
                                    },

                                    (ValueType::Scalar, ValueType::Scalar) => { (ValueType::Scalar, true) }
                                    (_, _) => { (ValueType::Unresolved, false) }
                                };
                                self.bindings[ni].1 = vt;
                                retval
                            },
                            _ => { false },
                        }
                    }
                    _ => { false }
                }
            }
            _ => { false }
        }
    }

    fn solve_assert(&mut self, constraint: &Constraint) -> bool {
        // Will be of the form, Assertion(<inner>)
        // inner will be of the form Or(<c1>, <c2>)
        if let &Constraint::Assertion(box Constraint::Or(box ref c1, box ref c2)) = constraint {
            // c1, c2 will be of the form And(Eq(ci1), Eq(ci2))
            [c1, c2].iter().any(|&ci| {
                if let Constraint::And(
                    box Constraint::Equality(op1, box Constraint::Value(vt1)),
                    box Constraint::Equality(op2, box Constraint::Value(vt2))) = *ci
                {
                    if self.bindings[op1].1 == vt1 {
                        self.bindings[op2].1 = vt2;
                        true
                    } else if self.bindings[op2].1 == vt2 {
                        self.bindings[op1].1 = vt1;
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            })
        } else {
            false
        }
    }

}



impl ReferenceMarker {
    fn compute_result(&self, op: &[ValueType]) -> ValueType {
        match (op[0], op[1]) {
            (ValueType::Invalid, _) |
            (_, ValueType::Invalid) |
            (ValueType::Reference, ValueType::Reference) => ValueType::Invalid,

            (ValueType::Reference, _) |
            (_, ValueType::Reference) => ValueType::Reference,

            (ValueType::Scalar, ValueType::Scalar) => ValueType::Scalar,

            (_, _) => ValueType::Unresolved,
        }
    }

    // Used to return indices nodes which may potentially be a reference
    // and need to be checked.
    // Only propagates to `uses` of `ni`
    fn propagate_upwards(&self, ni: NodeIndex) -> Vec<NodeIndex> {
        unimplemented!()
    }

    // Used to return indices nodes which may potentially be a reference
    // and need to be checked.
    // Only propagates to `args` of `ni`
    fn propagate_downwards(&self, ni: NodeIndex) -> Vec<NodeIndex> {
        unimplemented!()
    }

    fn compute_result_arr(&self, ops: &[ValueType]) -> ValueType {
        unimplemented!()
        // ops.fold(ValueType::Unresolved, |acc, &v| { compute_result(&[acc, x]) })
    }

    fn resolve_and_mark_subtree(&self, ssa: &mut SSAStorage, subtree: &NodeIndex) -> ValueType {
        unimplemented!()
        // let mut operands = ssa.operands_of(*subtree);
        // let rvt = self.compute_result_arr(operands
        // iter()
        // map(|&n| ssa.g[n].get_valueinfo()
        // map(|v| v.value_type())
        // expect(""))
        // cloned()
        // collect::<Vec<_>>()
        // as_slice());
        // match rvt {
        // / All operands are scalar
        // ValueType::Scalar => ValueType::Scalar,
        // / Exactly one operand is a reference
        // ValueType::Reference => ValueType::Reference,
        // / More than one operand has been identified as a reference
        // ValueType::Invalid => ValueType::Invalid,
        // / Unresolved, need more information/analysis to determine which operand is a reference
        // ValueType::Unresolved => {
        // / Need to go deeper!
        // let mut vtys = Vec::new();
        // / Check the opcode to determine the next action.
        // let nd  = ssa.g[current].clone();

        // match opc {
        // / Either operand can be ref
        // MOpcode::OpAdd |
        // MOpcode::OpAnd |
        // MOpcode::OpOr |
        // MOpcode::OpXor => {
        // for op in &operands {
        // let v = self.resolve_and_mark_subtree(ssa, op);
        // vtys.push(v);
        //
        //

        // Second operand can never be ref
        // MOpcode::OpSub |
        // MOpcode::OpMul |
        // MOpcode::OpLsl |
        // Opcode::OpRor |
        // Opcode::OpRol |
        // MOpcode::OpLsr  => {
        // self.mark_subtree_as_scalar(ssa, &operands[1]);
        // let v = self.resolve_and_mark_subtree(ssa, &operands[0]);
        // vtys.push(v);
        // ,

        // / Unary operand that should copy current nodes's status
        // MOpcode::OpNarrow(u16) |
        // MOpcode::OpZeroExt(u16) |
        // MOpcode::OpSignExt(u16)  => {
        // let v = self.resolve_and_mark_subtree(ssa, &operands[0]);
        // vtys.push(v);
        // ,

        // / Special cased to do a section lookup
        // MOpcode::OpConst(u64)  => {
        // / TODO. XXX.
        // vtys.push(ValueType::Scalar);
        // ,

        // / Result can never be ref
        // MOpcode::OpDiv |
        // MOpcode::OpMod |
        // MOpcode::OpNot |
        // MOpcode::OpGt |
        // MOpcode::OpLt => {
        // for op in &operands {
        // self.mark_subtree_as_scalar(ssa, op);
        //
        // vtys.push(ValueType::Scalar);
        // ,
        //

        // / Check if exactly one subtree resolved to be a reference.
        // / TODO.
        // self.compute_result_arr(&vtys)
        // ,
        //
    }

    fn mark_subtree_as_scalar(&self, ssa: &mut SSAStorage, subtree: &NodeIndex) {
        for op in ssa.operands_of(*subtree) {
            if let Some(ref mut nd) = ssa.g.node_weight_mut(op) {
                if let Some(ref mut vi) = nd.get_valueinfo_mut() {
                    vi.mark_as_scalar();
                }
            }
            self.mark_subtree_as_scalar(ssa, &op);
        }
    }

    fn value_type(&self, ssa: &SSAStorage, ni: NodeIndex) -> ValueType {
        *ssa.g[ni].get_valueinfo().expect("").value_type()
    }

    fn mark_node(&self, ssa: &mut SSAStorage, ni: NodeIndex, ty: ValueType) {
        if let Some(ref mut nd) = ssa.g.node_weight_mut(ni) {
            if let Some(vi) = nd.get_valueinfo_mut() {
                match ty {
                    ValueType::Invalid => vi.mark_as_invalid(),
                    ValueType::Reference => vi.mark_as_reference(),
                    ValueType::Scalar => vi.mark_as_scalar(),
                    ValueType::Unresolved => {}
                }
            }
        }
    }

    // Start from sources where this information can originate.
    // For marking nodes as references, the source of this information
    // has to be one of:
    //   - Address to read/write (memory operations)
    //   - Target of a indirect CF-transfer (call/jump).
    //   - Arguments to current function
    //   - Arguments to call
    //   - Return from functions
    //   - Access to stack using rsp/rbp
    fn mark_trivial_references(&self, ssa: &mut SSAStorage) -> VecDeque<NodeIndex> {
        let mut wl = VecDeque::new();
        for block in ssa.blocks() {
            for e in ssa.exprs_in(block) {
                // Address to read/write memory must be a reference
                if let Some(opc) = ssa.opcode(e) {
                    match opc {
                        MOpcode::OpStore | MOpcode::OpLoad => {
                            let operands = ssa.operands_of(e);
                            self.mark_node(ssa, operands[1], ValueType::Reference);
                            wl.push_back(operands[1]);
                        }

                        MOpcode::OpCall => {
                            // Target of call should be an address
                            let operands = ssa.operands_of(e);
                            self.mark_node(ssa, operands[0], ValueType::Reference);
                            wl.push_back(operands[0]);

                            // Check the arguments to call.
                            // TODO.


                            // Check the return value for a reference
                            // TODO
                        }

                        _ => {}
                    }
                }

                // For now, only rsp is considered to contain a reference.
                // Ideally, (For x86) rbp gets marked as a reference automatically due to the
                // function prologue setting rbp to be equal to rsp.
                if ssa.registers(e).contains(&"rsp".to_owned()) {
                    self.mark_node(ssa, e, ValueType::Reference);
                    wl.push_back(e);
                }

            }
        }

        wl
    }

    pub fn resolve_refs(&mut self, rfn: &mut RadecoFunction) {
        //let ssa = rfn;
        //let mut wl: VecDeque<NodeIndex> = VecDeque::new();
        //let seen: HashSet<NodeIndex> = HashSet::new();

        //// Stores the indices to nodes that are marked as references,
        //// but have none of its subtrees marked as references yet.
        //let mut unresolved = HashSet::new();

        //// First mark all the trivial references and add them to a worklist.
        //wl = self.mark_trivial_references(ssa);
        //unresolved = wl.iter().cloned().collect();

        //while !wl.is_empty() {
            //let current_ni = wl.pop_front().expect("This cannot be `None`");

            //// Now we need to look at which opearand/subtree is a reference.
            //// If we can identify the subtree that is a reference, we mark the
            //// others as non-reference (scalar), as an operation cannot
            //// have more than one reference.

            //let operands = ssa.operands_of(current_ni);
            //let operands_vt = operands.iter().map(|&op| {
                //ssa.g[op].get_valueinfo().map(|x| x.value_type()).unwrap_or(&ValueType::Unresolved)
            //});

            //match operands_vt.fold(ValueType::Unresolved,
                                   //|acc, &x| self.compute_result(&[acc, x])) {
                //// Best case! Nothing to do. Mark the ones that are not references
                //// as scalar, and we're done.
                //ValueType::Reference => (),

                //// Need to resolve and decide which tree is a reference
                //ValueType::Unresolved => (),

                //// This subtree, i.e. one indexed at current_ni is not a reference,
                //// Propagate this information upwards, this might help decide
                //// the other unresolved pieces.
                //ValueType::Scalar => (),

                //// Throw a warning. Situation is not consistent with our assumptions
                //ValueType::Invalid => (),
            //}
        //}


        // while !wl.is_empty() {
        // let current = wl.pop_front().expect("Cannot be `None`");
        // let nd  = ssa.g[current].clone();

        // match nd {
        // NodeData::Op(ref opc, ref vi) => {
        // match opc {
        // MOpcode::OpStore | &MOpcode::OpLoad => {
        // let addr = ssa.operands_of(current)[1];
        // if let Some(ref mut ind) = ssa.g.node_weight_mut(addr) {
        // if let Some(ref mut vi) = ind.get_valueinfo_mut() {
        // vi.mark_as_reference();
        //
        //
        // / Push in the next use of memory, if this is a store.
        // if opc == &MOpcode::OpStore {
        // let uses = ssa.uses_of(current);
        // wl.extend(&uses);
        //
        // wl.push_back(addr);
        //

        //&MOpcode::OpCall => unimplemented!(),


        //_ => continue,
        //_
        //_
        //_ NodeData::Phi(ref vi, ref c) => unimplemented!(),
        //_ NodeData::Comment(ref vi, ref c) => unimplemented!(),
        //_ => continue,
        //_
        //_
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn mark_refs_1() {}

    #[test]
    fn test_constraint_solver() {
        let mut cs = ConstraintSet::<u64>::default();

    }
}
