// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Enumerations and structs to represent instructions.
//! Shared by the SSA representation and the intermediate representation of the
//! ESIL parser.

use std::default::Default;
use std::fmt;

pub type Address = u64;
pub type WidthSpec = u16;

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
    OpNarrow(WidthSpec),
    OpWiden(WidthSpec),
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

    pub fn has_sideeffects(&self) -> bool {
        match *self {
            MOpcode::OpStore => true,
            MOpcode::OpJmp => true,
            MOpcode::OpCJmp => true,
            MOpcode::OpCall => true,
            MOpcode::OpITE => true,
            _ => false,
        }
    }

    pub fn allowed_in_ssa(&self) -> bool {
        match *self {
            MOpcode::OpJmp => false,
            MOpcode::OpCJmp => false,
            MOpcode::OpNop => false,
            MOpcode::OpITE => false,
            MOpcode::OpInvalid => false,
            _ => true,
        }
    }

    fn info(&self) -> (String, MArity) {
        let (op, arity) = match *self {
            MOpcode::OpAdd => ("+", MArity::Binary),
            MOpcode::OpSub => ("-", MArity::Binary),
            MOpcode::OpMul => ("*", MArity::Binary),
            MOpcode::OpDiv => ("/", MArity::Binary),
            MOpcode::OpMod => ("%", MArity::Binary),
            MOpcode::OpAnd => ("&", MArity::Binary),
            MOpcode::OpOr => ("|", MArity::Binary),
            MOpcode::OpXor => ("^", MArity::Binary),
            MOpcode::OpNot => ("!", MArity::Unary),
            MOpcode::OpEq => ("=", MArity::Binary),
            MOpcode::OpCmp => ("==", MArity::Binary),
            MOpcode::OpGt => (">", MArity::Binary),
            MOpcode::OpLt => ("<", MArity::Binary),
            MOpcode::OpLsl => ("<<", MArity::Binary),
            MOpcode::OpLsr => (">>", MArity::Binary),
            MOpcode::OpIf => ("if", MArity::Unary),
            MOpcode::OpLoad => ("load", MArity::Unary),
            MOpcode::OpStore => ("store", MArity::Binary),
            MOpcode::OpNarrow(_) => ("narrow", MArity::Unary),
            MOpcode::OpWiden(_) => ("widen", MArity::Unary),
            MOpcode::OpJmp => ("jmp", MArity::Unary),
            MOpcode::OpCJmp => ("jmp if", MArity::Binary),
            MOpcode::OpCall => ("call", MArity::Unary),
            MOpcode::OpConst(_) => ("const", MArity::Zero),
            MOpcode::OpNop => ("nop", MArity::Zero),
            MOpcode::OpInvalid => ("invalid", MArity::Zero),
            MOpcode::OpITE => ("ITE", MArity::Ternary),
        };
        (String::from(op), arity)
    }
}

impl fmt::Display for MOpcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}
