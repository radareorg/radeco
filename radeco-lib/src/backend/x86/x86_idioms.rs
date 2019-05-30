//! This module contains common x86_64 idioms as grep and replace patterns
//! which maybe used for further reduction of the SSA form.
//!
//! NOTE: This module is not stable.

use crate::analysis::matcher::gmatch;
use crate::middle::ssa::ssa_traits::{SSAMod, SSAWalk, SSA};

#[allow(dead_code)]
mod patterns {
    pub const OF: &'static str = "(OpNarrow1 (OpEq (OpAnd (OpLsr (OpAnd (OpXor (OpNot %2), %3), \
                                  (OpXor %1, %2)), #x3f), #x1), #x1))";
    pub const PF: &'static str = "(OpNarrow1 (OpAnd (OpMod (OpAnd (OpMul (OpAnd %1, #xff), \
                                  #x101010101010101), #x8040201008040201), #x1ff), #x1))";
    pub const SF: &'static str = "(OpNarrow1 (OpLsr (OpSub %2, %3), (OpSub #x40, #x1)))";
    pub const SF_32: &'static str = "(OpNarrow1 (OpLsr (OpSub %2, %3), (OpSub #x20, #x1)))";
    pub const CF: &'static str = "(OpNarrow1 (OpGt %2, %1))";
    pub const ZF: &'static str =
        "(OpNarrow1 (OpXor #x1, (OpAnd (OpSub %2, %3), #xffffffffffffffff)))";
    pub const ZF_32: &'static str = "(OpNarrow1 (OpXor #x1, (OpAnd (OpSub %2, %3), #xffffffff)))";
    pub const BF: &'static str = "(OpNarrow1 (OpLt %2, %1))";

    pub const PATTERNS: &'static [(&'static str, &'static str)] = &[
        ("(OpXor %1, %1)", "#x0"),
        ("(OpXor %1, #x0)", "%1"),
        ("(OpAnd %1, #x1)", "%1"),
        ("(OpAnd %1, #x0)", "#x0"),
        ("(OpOr %1, #x0)", "%1"),
        ("(OpOr %1, #x1)", "#x1"),
        ("(OpSub %1, %1)", "#x0"),
    ];

    lazy_static! {
        pub static ref COMPARE_PATTERNS: Vec<(String, &'static str)> = {
            let mut v = Vec::new();
            // LE -  of,sf,^,zf,|
            v.push((format!("(OpOr {}, (OpXor {}, (OpNarrow1 #x0)))", ZF, SF), "(OpLt %2, %3)"));
            v.push((format!("(OpOr {}, (OpXor {}, #x0))", ZF_32, SF_32), "(OpLt %2, %3)"));
            v.push((format!("{}", ZF), "(OpEq %2, %3)"));
            v
        };
    }
}

pub fn replace<I, S>(ssa: &mut S)
where
    I: Iterator<Item = S::ValueRef>,
    S: SSA + SSAMod + SSAWalk<I>,
{
    for pat in patterns::PATTERNS {
        grep_and_replace!(ssa, pat.0 => pat.1)
    }
    for &(ref find, replace) in patterns::COMPARE_PATTERNS.iter() {
        grep_and_replace!(ssa, find => replace)
    }
}
