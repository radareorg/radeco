//! This module contains common x86_64 idioms as grep and replace patterns
//! which maybe used for further reduction of the SSA form.

use analysis::matcher::gmatch;
use middle::ssa::ssa_traits::{SSAMod, SSAWalk, SSA};

const PATTERNS: &'static [(&'static str, &'static str)] = &[
    ("(OpXor %1, %1)", "#x0"),
    ("(OpXor %1, #x0)", "%1"),
    ("(OpAnd %1, #x1)", "%1"),
    ("(OpAnd %1, #x0)", "#x0"),
    ("(OpOr %1, #x0)", "%1"),
    ("(OpOr %1, #x1)", "#x1"),
    ("(OpSub %1, %1)", "#x0"),
    // Reduce x86 comparisons using flags to >, <, <=, >=, ==

];

pub fn replace<I, S>(ssa: &mut S)
where I: Iterator<Item=S::ValueRef>,
      S: SSA + SSAMod + SSAWalk<I> {
    for pat in PATTERNS {
        grep_and_replace!(ssa, pat.0 => pat.1)
    }
}
