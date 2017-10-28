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


use middle::ssa::ssa_traits::ValueType;
use std::collections::{HashMap, VecDeque};
use std::collections::hash_map;
use std::fmt::{Debug, Display, Formatter, Error};
use std::hash::Hash;
use std::slice;

#[derive(Debug, Clone)]
pub enum Constraint<I: Debug + Clone + Copy> {
    Union(I, I),
    // Solve eq. Equation of form, I = Constraint<I>
    Equality(I, Box<Constraint<I>>),
    // Equivalence. All elements in Vec<I> have the same value.
    // Solve equivalence
    AssertEquivalence(Vec<I>),
    // Solve assert
    Assertion(Box<Constraint<I>>),
    Or(Box<Constraint<I>>, Box<Constraint<I>>),
    And(Box<Constraint<I>>, Box<Constraint<I>>),
    // No solve
    Value(ValueType),
}

#[derive(Default, Debug)]
pub struct ConstraintSet<I: Eq + Debug + Hash + Copy> {
    bindings: HashMap<I, ValueType>,
    set: VecDeque<Constraint<I>>,
}

impl<T: Clone + Debug + Hash + Eq + Copy> ConstraintSet<T> {
    pub fn add_constraint(&mut self, c: Constraint<T>) {
        self.set.push_back(c);
    }

    pub fn add_union(&mut self, lhs: T, ops: &[T]) {
        let n = ops.len();

        let iter_limit = if n > 2 {
            n - 2
        } else {
            0
        };

        for i in (0..iter_limit) {
            self.add_constraint(Constraint::Equality(lhs,
                                    Box::new(Constraint::Union(ops[i],
                                      ops[i + 1]))));
        }
    }

    pub fn add_eq(&mut self, lhs: T, vt: ValueType) {
        self.add_constraint(Constraint::Equality(lhs, Box::new(Constraint::Value(vt))));
    }

    pub fn add_equivalence_assertion(&mut self, eq_set: &[T]) {
        self.add_constraint(Constraint::AssertEquivalence(Vec::from(eq_set)));
    }

    // Retrive binding value for `bind` or insert default if none exist
    fn bvalue(&mut self, bind: T) -> ValueType {
        *self.bindings.entry(bind).or_insert(ValueType::Unresolved)
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
                Constraint::AssertEquivalence(_) => self.solve_equivalence(&constraint),
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

    fn solve_equivalence(&mut self, constraint: &Constraint<T>) -> bool {
        if let &Constraint::AssertEquivalence(ref list) = constraint {
            if let Some(found) = list.iter().find(|&var| {
                // Check if any var has a known value
                self.bvalue(*var) != ValueType::Unresolved
            }) {
                let vt = self.bvalue(*found);
                for var in list {
                    self.bindings.insert(*var, vt);
                }
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn solve_eq(&mut self, constraint: &Constraint<T>) -> bool {
        match *constraint {
            Constraint::Equality(ni, box ref cs) => {
                match *cs {
                    Constraint::Value(vt) => {
                        self.bindings.insert(ni, vt);
                        true
                    }
                    Constraint::Union(op1, op2) => {
                        match self.bvalue(ni) {
                            ValueType::Scalar => {
                                // Both, op1 and op2, are scalars
                                self.bindings.insert(op1, ValueType::Scalar);
                                self.bindings.insert(op2, ValueType::Scalar);
                                true
                            }
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
                            }
                            ValueType::Unresolved => {
                                // Look at the Union
                                let op1_vt = self.bvalue(op1);
                                let op2_vt = self.bvalue(op2);
                                let (vt, retval) = match (op1_vt, op2_vt) {
                                    (ValueType::Invalid, _) |
                                    (_, ValueType::Invalid) |
                                    (ValueType::Reference, ValueType::Reference) => {
                                        (ValueType::Invalid, true)
                                    }

                                    (ValueType::Reference, _) => {
                                        self.bindings.insert(op2, ValueType::Scalar);
                                        (ValueType::Reference, true)
                                    }

                                    (_, ValueType::Reference) => {
                                        self.bindings.insert(op1, ValueType::Scalar);
                                        (ValueType::Reference, true)
                                    }

                                    (ValueType::Scalar, ValueType::Scalar) => {
                                        (ValueType::Scalar, true)
                                    }
                                    (_, _) => (ValueType::Unresolved, false),
                                };
                                self.bindings.insert(ni, vt);
                                retval
                            }
                            _ => false,
                        }
                    }
                    _ => false,
                }
            }
            _ => false,
        }
    }

    fn solve_assert(&mut self, constraint: &Constraint<T>) -> bool {
        // Will be of the form, Assertion(<inner>)
        // inner will be of the form Or(<c1>, <c2>)
        if let &Constraint::Assertion(box Constraint::Or(box ref c1, box ref c2)) = constraint {
            // c1, c2 will be of the form And(Eq(ci1), Eq(ci2))
            [c1, c2].iter().any(|&ci| {
                if let Constraint::And(box Constraint::Equality(op1,
                                                                box Constraint::Value(vt1)),
                                       box Constraint::Equality(op2,
                                                                box Constraint::Value(vt2))) = *ci {
                    if self.bvalue(op1) == vt1 {
                        self.bindings.insert(op2, vt2);
                        true
                    } else if self.bvalue(op2) == vt2 {
                        self.bindings.insert(op1, vt1);
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

    pub fn iter_bindings<'a>(&'a self) -> hash_map::Iter<'a, T, ValueType> {
        self.bindings.iter()
    }
}

impl<T: Debug + Clone + Copy> Display for Constraint<T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let s = match *self {
            Constraint::Union(op1, op2) => { format!("{:?} U {:?}", op1, op2) },
            Constraint::Equality(op1, box ref op2) => { format!("{:?} = {}", op1, op2) },
            Constraint::AssertEquivalence(ref ops) => { ops.iter().fold("Assert ".to_owned(), |mut acc, s| { acc.push_str(&format!("{:?} = ", s)); acc }) },
            Constraint::Assertion(box ref op) => { format!("Assert {}", op) },
            Constraint::Or(box ref op1, box ref op2) => { format!("{} \\/ {}", op1, op2) },
            Constraint::And(box ref op1, box ref op2) => { format!("{} /\\ {}", op1, op2) },
            Constraint::Value(ref vt) => { format!("{:?}", vt) },
        };
        write!(f, "{}", s)
    }
}

impl<T: Clone + Debug + Hash + Eq + Copy> Display for ConstraintSet<T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        // First Bindings information.
        let mut display_str = String::new();
        for (bind, bval) in &self.bindings {
            display_str = format!("{}{:?}: {:?}\n", display_str, bind, bval);
        }

        display_str.push_str("-----------------------\nConstraints:\n");

        for constraint in &self.set {
            display_str.push_str(&constraint.to_string());
            display_str.push_str("\n");
        }

        write!(f, "{}", display_str)
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

        assert_eq!(cs.bindings[&0], ValueType::Reference);
    }

    #[test]
    fn reference_union_reference_is_invalid() {
        let mut cs = ConstraintSet::<u64>::default();
        cs.bind(&[0, 1, 2]);

        cs.add_constraint(Constraint::Equality(1, Box::new(Constraint::Value(ValueType::Reference))));
        cs.add_constraint(Constraint::Equality(2, Box::new(Constraint::Value(ValueType::Reference))));
        cs.add_constraint(Constraint::Equality(0, Box::new(Constraint::Union(1, 2))));
        cs.solve();

        assert_eq!(cs.bindings[&0], ValueType::Invalid);
    }

    #[test]
    fn know_reference_and_scalar_infer_reference() {
        let mut cs = ConstraintSet::<u64>::default();
        cs.bind(&[0, 1, 2]);

        cs.add_constraint(Constraint::Equality(0, Box::new(Constraint::Value(ValueType::Reference))));
        cs.add_constraint(Constraint::Equality(2, Box::new(Constraint::Value(ValueType::Scalar))));
        cs.add_constraint(Constraint::Equality(0, Box::new(Constraint::Union(1, 2))));
        cs.solve();

        assert_eq!(cs.bindings[&1], ValueType::Reference);
    }

    #[test]
    fn know_reference_and_reference_infer_scalar() {
        let mut cs = ConstraintSet::<u64>::default();
        cs.bind(&[0, 1, 2]);

        cs.add_constraint(Constraint::Equality(0, Box::new(Constraint::Value(ValueType::Reference))));
        cs.add_constraint(Constraint::Equality(2, Box::new(Constraint::Value(ValueType::Reference))));
        cs.add_constraint(Constraint::Equality(0, Box::new(Constraint::Union(1, 2))));
        cs.solve();

        assert_eq!(cs.bindings[&1], ValueType::Scalar);
    }

    #[test]
    fn know_scalar_infer_scalar_scalar() {
        let mut cs = ConstraintSet::<u64>::default();
        cs.bind(&[0, 1, 2]);

        cs.add_constraint(Constraint::Equality(0, Box::new(Constraint::Value(ValueType::Scalar))));
        cs.add_constraint(Constraint::Equality(0, Box::new(Constraint::Union(1, 2))));
        cs.solve();

        assert_eq!(cs.bindings[&1], ValueType::Scalar);
        assert_eq!(cs.bindings[&2], ValueType::Scalar);
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

        assert_eq!(cs.bindings[&7], ValueType::Reference);
        assert_eq!(cs.bindings[&8], ValueType::Scalar);
    }
}
