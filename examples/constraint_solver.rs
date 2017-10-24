// Examples to illustrate project loading

extern crate radeco_lib;

use radeco_lib::analysis::mark_refs::{ConstraintSet, Constraint};
use radeco_lib::middle::ssa::ssa_traits::ValueType;

fn main() {
    {
        let mut cs = ConstraintSet::<u64>::default();
        cs.bind(&[0, 1, 2, 3, 4, 5, 6, 7, 8]);

        cs.add_constraint(Constraint::Equality(4, Box::new(Constraint::Value(ValueType::Reference))));
        cs.add_constraint(Constraint::Equality(0, Box::new(Constraint::Value(ValueType::Reference))));

        cs.add_constraint(Constraint::Equality(7, Box::new(Constraint::Union(4, 8))));
        cs.add_constraint(Constraint::Equality(0, Box::new(Constraint::Union(1, 2))));
        cs.add_constraint(Constraint::Equality(1, Box::new(Constraint::Union(3, 4))));
        cs.add_constraint(Constraint::Equality(2, Box::new(Constraint::Union(5, 6))));

        cs.solve();
        println!("{:?}", cs);
    }
}
