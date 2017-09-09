// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Enumerations and structs to represent instructions.
//! Shared by the SSA representation and the intermediate representation of the
//! ESIL parser.

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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
        write!(f, "{:#08X}.{:04}", self.address, self.offset)
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
    Zero,
    Unary,
    Binary,
    Ternary,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum MOpcode {
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
    OpMod,
    OpAnd,
    OpOr,
    OpXor,
    OpNot,
    OpEq,
    OpCmp,
    OpGt,
    OpLt,
    OpLsl,
    OpLsr,
    OpIf,
    OpJmp,
    OpCJmp,
    OpCall,
    OpLoad,
    OpStore,
    OpNarrow(u16),
    OpWiden(u16),
    OpConst(u64),
    OpNop,
    OpInvalid,
    // If - Then - Else
    OpITE,
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

    pub fn to_string(&self) -> String {
        self.info().0
    }

    pub fn is_commutative(&self) -> bool {
        match *self {
            MOpcode::OpAdd |
            MOpcode::OpMul |
            MOpcode::OpAnd |
            MOpcode::OpOr |
            MOpcode::OpXor |
            MOpcode::OpCmp |
            MOpcode::OpGt |
            MOpcode::OpLt => true,
            _ => false,
        }
    }

    pub fn has_sideeffects(&self) -> bool {
        match *self {
            MOpcode::OpStore |
            MOpcode::OpJmp |
            MOpcode::OpCJmp |
            MOpcode::OpCall |
            MOpcode::OpITE => true,
            _ => false,
        }
    }

    pub fn allowed_in_ssa(&self) -> bool {
        match *self {
            MOpcode::OpJmp |
            MOpcode::OpCJmp |
            MOpcode::OpNop |
            MOpcode::OpITE |
            MOpcode::OpInvalid => false,
            _ => true,
        }
    }

    fn info(&self) -> (String, MArity) {
        match *self {
            MOpcode::OpAdd => ("OpAdd".to_owned(), MArity::Binary),
            MOpcode::OpSub => ("OpSub".to_owned(), MArity::Binary),
            MOpcode::OpMul => ("OpMul".to_owned(), MArity::Binary),
            MOpcode::OpDiv => ("OpDiv".to_owned(), MArity::Binary),
            MOpcode::OpMod => ("OpMod".to_owned(), MArity::Binary),
            MOpcode::OpAnd => ("OpAnd".to_owned(), MArity::Binary),
            MOpcode::OpOr => ("OpOr".to_owned(), MArity::Binary),
            MOpcode::OpXor => ("OpXor".to_owned(), MArity::Binary),
            MOpcode::OpNot => ("OpNot".to_owned(), MArity::Unary),
            MOpcode::OpEq => ("OpEq".to_owned(), MArity::Binary),
            MOpcode::OpCmp => ("OpCmp".to_owned(), MArity::Binary),
            MOpcode::OpGt => ("OpGt".to_owned(), MArity::Binary),
            MOpcode::OpLt => ("OpLt".to_owned(), MArity::Binary),
            MOpcode::OpLsl => ("OpLsl".to_owned(), MArity::Binary),
            MOpcode::OpLsr => ("OpLsr".to_owned(), MArity::Binary),
            MOpcode::OpIf => ("OpIf".to_owned(), MArity::Unary),
            MOpcode::OpLoad => ("OpLoad".to_owned(), MArity::Binary),
            MOpcode::OpStore => ("OpStore".to_owned(), MArity::Binary),
            MOpcode::OpNarrow(_) => ("OpNarrow".to_owned(), MArity::Unary),
            MOpcode::OpWiden(_) => ("OpWiden".to_owned(), MArity::Unary),
            MOpcode::OpJmp => ("OpJmp".to_owned(), MArity::Unary),
            MOpcode::OpCJmp => ("OpJmpIf".to_owned(), MArity::Binary),
            MOpcode::OpCall => ("OpCall".to_owned(), MArity::Unary),
            MOpcode::OpConst(c) => (format!("OpCost({})", c), MArity::Zero),
            MOpcode::OpNop => ("OpNop".to_owned(), MArity::Zero),
            MOpcode::OpInvalid => ("OpInvalid".to_owned(), MArity::Zero),
            MOpcode::OpITE => ("ITE".to_owned(), MArity::Ternary),
        }
    }
}

impl fmt::Display for MOpcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}
