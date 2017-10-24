//! Implements Constaint and ConstraintSet
//!
//! NOTE: Current implementation is limited to work with reference marking.
//! However, this principle is quite general and can be extended as needed later on.
//! This needs a bit more engineering (generics) to make it work for the general case.

// This is what the current implementation solves. Extend to any generic lattice.
// T_a = C
//     = R
//     = op1 U op2
//
//   C = op1 U op2 => op1 = C /\ op2 = C
//   R = op1 U op2 => (op1 = R /\ op2 = C) \/ (op1 = C /\ op2 = R)
//

use std::fmt::Debug;
use std::collections::VecDeque;

use middle::ssa::ssa_traits::ValueType;

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
        match *constraint {
            Constraint::Equality(ni, box ref cs) => {
                match *cs {
                    Constraint::Value(vt) => {
                        self.bindings[ni].1 = vt;
                        true
                    }
                    Constraint::Union(op1, op2) => {
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

#[cfg(test)]
mod test {
    use middle::ssa::ssa_traits::ValueType;
    use super::*;

    #[test]
    fn scalar_union_reference_is_reference() {
        let mut cs = ConstraintSet::<u64>::default();
        cs.bind(&[0, 1, 2]);

        cs.add_constraint(Constraint::Equality(1, Box::new(Constraint::Value(ValueType::Scalar))));
        cs.add_constraint(Constraint::Equality(2, Box::new(Constraint::Value(ValueType::Reference))));
        cs.add_constraint(Constraint::Equality(0, Box::new(Constraint::Union(1, 2))));
        cs.solve();

        assert_eq!(cs.bindings[0].1, ValueType::Reference);
    }

    #[test]
    fn reference_union_reference_is_invalid() {
        let mut cs = ConstraintSet::<u64>::default();
        cs.bind(&[0, 1, 2]);

        cs.add_constraint(Constraint::Equality(1, Box::new(Constraint::Value(ValueType::Reference))));
        cs.add_constraint(Constraint::Equality(2, Box::new(Constraint::Value(ValueType::Reference))));
        cs.add_constraint(Constraint::Equality(0, Box::new(Constraint::Union(1, 2))));
        cs.solve();

        assert_eq!(cs.bindings[0].1, ValueType::Invalid);
    }

    #[test]
    fn know_reference_and_scalar_infer_reference() {
        let mut cs = ConstraintSet::<u64>::default();
        cs.bind(&[0, 1, 2]);

        cs.add_constraint(Constraint::Equality(0, Box::new(Constraint::Value(ValueType::Reference))));
        cs.add_constraint(Constraint::Equality(2, Box::new(Constraint::Value(ValueType::Scalar))));
        cs.add_constraint(Constraint::Equality(0, Box::new(Constraint::Union(1, 2))));
        cs.solve();

        assert_eq!(cs.bindings[1].1, ValueType::Reference);
    }

    #[test]
    fn know_reference_and_reference_infer_scalar() {
        let mut cs = ConstraintSet::<u64>::default();
        cs.bind(&[0, 1, 2]);

        cs.add_constraint(Constraint::Equality(0, Box::new(Constraint::Value(ValueType::Reference))));
        cs.add_constraint(Constraint::Equality(2, Box::new(Constraint::Value(ValueType::Reference))));
        cs.add_constraint(Constraint::Equality(0, Box::new(Constraint::Union(1, 2))));
        cs.solve();

        assert_eq!(cs.bindings[1].1, ValueType::Scalar);
    }

    #[test]
    fn know_scalar_infer_scalar_scalar() {
        let mut cs = ConstraintSet::<u64>::default();
        cs.bind(&[0, 1, 2]);

        cs.add_constraint(Constraint::Equality(0, Box::new(Constraint::Value(ValueType::Scalar))));
        cs.add_constraint(Constraint::Equality(0, Box::new(Constraint::Union(1, 2))));
        cs.solve();

        assert_eq!(cs.bindings[1].1, ValueType::Scalar);
        assert_eq!(cs.bindings[2].1, ValueType::Scalar);
    }

    #[test]
    fn disjoint_tree_infer_references() {
        let mut cs = ConstraintSet::<u64>::default();
        cs.bind(&[0, 1, 2, 3, 4, 5, 6, 7, 8]);

        cs.add_constraint(Constraint::Equality(4, Box::new(Constraint::Value(ValueType::Reference))));
        cs.add_constraint(Constraint::Equality(0, Box::new(Constraint::Value(ValueType::Reference))));

        cs.add_constraint(Constraint::Equality(7, Box::new(Constraint::Union(4, 8))));
        cs.add_constraint(Constraint::Equality(0, Box::new(Constraint::Union(1, 2))));
        cs.add_constraint(Constraint::Equality(1, Box::new(Constraint::Union(3, 4))));
        cs.add_constraint(Constraint::Equality(2, Box::new(Constraint::Union(5, 6))));

        cs.solve();

        assert_eq!(cs.bindings[7].1, ValueType::Reference);
        assert_eq!(cs.bindings[8].1, ValueType::Scalar);
    }
}
