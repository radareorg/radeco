use super::CombinableOpConstInfo as COCI;
use super::CombinableOpInfo as COI;
use crate::middle::ir::MOpcode::*;

macro_rules! gen_rules {
    (
        . -> $sub_opinfo:ident -> $cur_opinfo:ident
        {
            $( $lhs:tt => $rhs:tt ; )*
        }
    ) => {
        match ((0, $sub_opinfo), $cur_opinfo) {
            $( gen_rules!(@genlhs $lhs) => gen_rules!(@genrhs $rhs) , )*
            _ => None,
        }
    };

    (@genlhs .)
        => {_};
    (@genlhs ($sub:tt $op:tt $c:ident))
        => { (gen_rules!(@genlhs $sub), &COI(gen_rules!(@opcode $op), COCI::Right($c))) };
    (@genlhs ($c:ident $op:tt $sub:tt))
        => { (gen_rules!(@genlhs $sub), &COI(gen_rules!(@opcode $op), COCI::Left($c))) };

    (@genrhs (. $op:tt $sub:tt))
        => { Some(COI(gen_rules!(@opcode $op), COCI::Right(gen_rules!(@geneval $sub)))) };
    (@genrhs ($sub:tt $op:tt .))
        => { Some(COI(gen_rules!(@opcode $op), COCI::Left(gen_rules!(@geneval $sub)))) };

    (@geneval ($l:ident $op:tt $r:ident))
        => { gen_rules!(@opcode $op).eval_binop($l, $r).unwrap() };

    (@opcode +) => (OpAdd);
    (@opcode -) => (OpSub);
    (@opcode &) => (OpAnd);
    (@opcode |) => (OpOr);
    (@opcode ^) => (OpXor);
}

pub(super) fn combine_opinfo(cur_opinfo: &COI, sub_opinfo: &COI) -> Option<COI> {
    // try to keep put the const on the left like `ssasort` does
    gen_rules! {
        . -> sub_opinfo -> cur_opinfo
        {
            // add/add
            ((.+a)+b) => ((a+b)+.);
            ((a+.)+b) => ((a+b)+.);
            (b+(.+a)) => ((a+b)+.);
            (b+(a+.)) => ((a+b)+.);
            // add/sub
            ((.+a)-b) => ((a-b)+.);
            ((a+.)-b) => ((a-b)+.);
            (b-(.+a)) => ((b-a)-.);
            (b-(a+.)) => ((b-a)-.);
            // sub/add
            ((.-a)+b) => ((b-a)+.);
            ((a-.)+b) => ((b+a)-.);
            (b+(.-a)) => ((b-a)+.);
            (b+(a-.)) => ((b+a)-.);
            // sub/sub
            ((.-a)-b) => (.-(a+b));
            ((a-.)-b) => ((a-b)-.);
            (b-(.-a)) => ((b+a)-.);
            (b-(a-.)) => ((b-a)+.);
            // and/and
            ((.&a)&b) => ((a&b)&.);
            ((a&.)&b) => ((a&b)&.);
            (b&(.&a)) => ((a&b)&.);
            (b&(a&.)) => ((a&b)&.);
            // or/or
            ((.|a)|b) => ((a|b)|.);
            ((a|.)|b) => ((a|b)|.);
            (b|(.|a)) => ((a|b)|.);
            (b|(a|.)) => ((a|b)|.);
            // xor/xor
            ((.^a)^b) => ((a^b)^.);
            ((a^.)^b) => ((a^b)^.);
            (b^(.^a)) => ((a^b)^.);
            (b^(a^.)) => ((a^b)^.);
        }
    }
}
