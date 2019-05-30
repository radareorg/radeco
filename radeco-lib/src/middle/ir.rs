// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Enumerations and structs to represent instructions.
//! Shared by the SSA representation and the intermediate representation of the
//! ESIL parser.

use std::borrow::Cow;
use std::fmt;

pub type Address = u64;

/// Used to describe the width of an operand/operation in a SSA Node.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum WidthSpec {
    /// Takes whatever width the other operation takes
    Adaptive,
    Known(u16),
    Unknown,
}

impl WidthSpec {
    pub fn get_width(&self) -> Option<u16> {
        match self {
            &WidthSpec::Known(ref w) => Some(*w),
            _ => None,
        }
    }

    pub fn new_known(w: u16) -> WidthSpec {
        assert!(&[0, 1, 2, 4, 8, 16, 32, 64, 128].contains(&w));
        WidthSpec::Known(w)
    }
}

impl From<u16> for WidthSpec {
    fn from(other: u16) -> WidthSpec {
        if other < u16::max_value() {
            assert!(&[0, 1, 2, 4, 8, 16, 32, 64, 128].contains(&other));
            WidthSpec::Known(other)
        } else {
            WidthSpec::Unknown
        }
    }
}

#[derive(Clone, Default, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
// New address struct
pub struct MAddress {
    pub address: u64,
    pub offset: u64,
}

impl MAddress {
    pub fn new(address: u64, offset: u64) -> MAddress {
        MAddress {
            address: address,
            offset: offset,
        }
    }

    pub fn invalid_address() -> MAddress {
        MAddress {
            address: u64::max_value(),
            offset: u64::max_value(),
        }
    }
}

impl fmt::UpperHex for MAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#08X}.{:04X}", self.address, self.offset)
    }
}

impl fmt::Display for MAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:X}", self)
    }
}

impl From<u64> for MAddress {
    fn from(other: u64) -> MAddress {
        MAddress {
            address: other,
            offset: 0,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum MArity {
    Binary,
    Ternary,
    Unary,
    Zero,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MOpcode {
    OpAdd,
    OpAnd,
    OpCJmp,
    OpCall,
    OpConst(u64),
    OpCustom(String),
    OpDiv,
    OpEq,
    OpGt,
    // If - Then - Else
    OpITE,
    OpIf,
    OpInvalid,
    OpJmp,
    OpLoad,
    OpLsl,
    OpLsr,
    OpLt,
    OpMod,
    OpMov,
    OpMul,
    OpNarrow(u16),
    OpNop,
    OpNot,
    OpOr,
    // Rotate Shift Left
    OpRol,
    // Rotate Shift Right
    OpRor,
    // Sign Extend to width
    OpSignExt(u16),
    OpStore,
    OpSub,
    OpXor,
    // Zero Extend to width
    OpZeroExt(u16),
}

impl MOpcode {
    pub fn is_binary(&self) -> bool {
        self.arity() == MArity::Binary
    }

    pub fn is_unary(&self) -> bool {
        self.arity() == MArity::Unary
    }

    pub fn arity(&self) -> MArity {
        self.info().1
    }

    pub fn to_string(&self) -> Cow<str> {
        self.info().0
    }

    pub fn is_commutative(&self) -> bool {
        match *self {
            MOpcode::OpAdd
            | MOpcode::OpMul
            | MOpcode::OpAnd
            | MOpcode::OpOr
            | MOpcode::OpXor
            | MOpcode::OpEq => true,
            _ => false,
        }
    }

    pub fn has_sideeffects(&self) -> bool {
        match *self {
            MOpcode::OpStore
            | MOpcode::OpJmp
            | MOpcode::OpCJmp
            | MOpcode::OpCall
            | MOpcode::OpITE => true,
            _ => false,
        }
    }

    pub fn allowed_in_ssa(&self) -> bool {
        match *self {
            MOpcode::OpCJmp
            | MOpcode::OpITE
            | MOpcode::OpInvalid
            | MOpcode::OpJmp
            | MOpcode::OpNop => false,
            _ => true,
        }
    }

    fn info(&self) -> (Cow<str>, MArity) {
        match *self {
            MOpcode::OpAdd => (Cow::from("OpAdd"), MArity::Binary),
            MOpcode::OpAnd => (Cow::from("OpAnd"), MArity::Binary),
            MOpcode::OpCJmp => (Cow::from("OpJmpIf"), MArity::Binary),
            MOpcode::OpCall => (Cow::from("OpCall"), MArity::Unary),
            MOpcode::OpConst(c) => (Cow::from(format!("OpCost({})", c)), MArity::Zero),
            MOpcode::OpCustom(ref s) => (Cow::from(format!("OpCustom({})", s)), MArity::Zero),
            MOpcode::OpDiv => (Cow::from("OpDiv"), MArity::Binary),
            MOpcode::OpEq => (Cow::from("OpEq"), MArity::Binary),
            MOpcode::OpGt => (Cow::from("OpGt"), MArity::Binary),
            MOpcode::OpITE => (Cow::from("OpITE"), MArity::Ternary),
            MOpcode::OpIf => (Cow::from("OpIf"), MArity::Unary),
            MOpcode::OpInvalid => (Cow::from("OpInvalid"), MArity::Zero),
            MOpcode::OpJmp => (Cow::from("OpJmp"), MArity::Unary),
            MOpcode::OpLoad => (Cow::from("OpLoad"), MArity::Binary),
            MOpcode::OpLsl => (Cow::from("OpLsl"), MArity::Binary),
            MOpcode::OpLsr => (Cow::from("OpLsr"), MArity::Binary),
            MOpcode::OpLt => (Cow::from("OpLt"), MArity::Binary),
            MOpcode::OpMod => (Cow::from("OpMod"), MArity::Binary),
            MOpcode::OpMov => (Cow::from("OpMov"), MArity::Unary),
            MOpcode::OpMul => (Cow::from("OpMul"), MArity::Binary),
            MOpcode::OpNarrow(_) => (Cow::from("OpNarrow"), MArity::Unary),
            MOpcode::OpNop => (Cow::from("OpNop"), MArity::Zero),
            MOpcode::OpNot => (Cow::from("OpNot"), MArity::Unary),
            MOpcode::OpOr => (Cow::from("OpOr"), MArity::Binary),
            MOpcode::OpRol => (Cow::from("OpRol"), MArity::Binary),
            MOpcode::OpRor => (Cow::from("OpRor"), MArity::Binary),
            MOpcode::OpSignExt(_) => (Cow::from("OpSignExt"), MArity::Unary),
            MOpcode::OpStore => (Cow::from("OpStore"), MArity::Ternary),
            MOpcode::OpSub => (Cow::from("OpSub"), MArity::Binary),
            MOpcode::OpXor => (Cow::from("OpXor"), MArity::Binary),
            MOpcode::OpZeroExt(_) => (Cow::from("OpZeroExt"), MArity::Unary),
        }
    }

    pub fn idx(&self) -> u16 {
        match *self {
            MOpcode::OpAdd => 0,
            MOpcode::OpAnd => 1,
            MOpcode::OpCJmp => 2,
            MOpcode::OpCall => 3,
            MOpcode::OpConst(_) => 4,
            MOpcode::OpCustom(_) => 5,
            MOpcode::OpDiv => 6,
            MOpcode::OpEq => 7,
            MOpcode::OpGt => 8,
            MOpcode::OpITE => 9,
            MOpcode::OpIf => 10,
            MOpcode::OpInvalid => 11,
            MOpcode::OpJmp => 12,
            MOpcode::OpLoad => 13,
            MOpcode::OpLsl => 14,
            MOpcode::OpLsr => 15,
            MOpcode::OpLt => 16,
            MOpcode::OpMod => 17,
            MOpcode::OpMov => 18,
            MOpcode::OpMul => 19,
            MOpcode::OpNarrow(_) => 20,
            MOpcode::OpNop => 21,
            MOpcode::OpNot => 22,
            MOpcode::OpOr => 23,
            MOpcode::OpRol => 24,
            MOpcode::OpRor => 25,
            MOpcode::OpSignExt(_) => 26,
            MOpcode::OpStore => 27,
            MOpcode::OpSub => 28,
            MOpcode::OpXor => 29,
            MOpcode::OpZeroExt(_) => 30,
        }
    }

    pub fn eval_binop(&self, lhs: u64, rhs: u64) -> Option<u64> {
        use self::MOpcode::*;
        use std::num::Wrapping;

        let lhs = Wrapping(lhs);
        let rhs = Wrapping(rhs);
        Some(match self {
            OpAdd => (lhs + rhs).0,
            OpSub => (lhs - rhs).0,
            OpMul => (lhs * rhs).0,
            OpDiv => (lhs / rhs).0,
            OpMod => (lhs % rhs).0,
            OpAnd => (lhs & rhs).0,
            OpOr => (lhs | rhs).0,
            OpXor => (lhs ^ rhs).0,
            OpEq => (lhs == rhs) as u64,
            OpGt => (lhs > rhs) as u64,
            OpLt => (lhs < rhs) as u64,
            OpLsl => (lhs << (rhs.0 as usize)).0,
            OpLsr => (lhs >> (rhs.0 as usize)).0,
            OpRol => lhs.0.rotate_left(rhs.0 as u32),
            OpRor => lhs.0.rotate_right(rhs.0 as u32),
            _ => return None,
        })
    }

    pub fn eval_unop(&self, val: u64) -> Option<u64> {
        use self::MOpcode::*;

        Some(match self {
            OpNot => !val,
            _ => return None,
        })
    }
}

impl fmt::Display for MOpcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}
