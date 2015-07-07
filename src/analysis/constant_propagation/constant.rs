//! Module that implements constant propagation on the SSA IR.
//!
//! This module implements 'Sparse Conditional Constant' algorithm to
//! perform the propagation. For more details, please refer:
//!    * https://www.cs.utexas.edu/~lin/cs380c/wegman.pdf.
//!

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
