// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Module to implement the display traits for the IR.

use super::ir::{MInst, MOpcode, MVal, MValType};
use std::fmt;

impl fmt::Display for MOpcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl fmt::Display for MVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s: String = match self.val_type {
            MValType::EsilCur => {
                format!("esilcur")
            }
            MValType::EsilOld => {
                format!("esilold")
            }
            MValType::Lastsz => {
                format!("lastsz")
            }
            _ => {
                if let Some(v) = self.as_literal {
                    format!("{}[:{}]", v, self.size)
                } else {
                    format!("{}[:{}]", self.name, self.size)
                }
            }
        };
        f.pad_integral(true, "", &s)
    }
}

impl fmt::Display for MInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s: String = match self.opcode {
            MOpcode::OpNot => format!("{} = {}{}", self.dst, self.opcode, self.operand_1),
            MOpcode::OpEq => format!("{} = {}", self.dst, self.operand_1),
            MOpcode::OpInc => format!("{} = {} + 1", self.dst, self.operand_1),
            MOpcode::OpDec => format!("{} = {} - 1", self.dst, self.operand_1),
            MOpcode::OpIf => format!("if ({}) {{", self.operand_1),
            MOpcode::OpLoad => format!("{} = {}({})", self.dst, self.opcode, self.operand_1),
            MOpcode::OpNarrow(w) =>
                format!("{} = {}({}, {})", self.dst, self.opcode, self.operand_1, w),
            MOpcode::OpWiden(w) =>
                format!("{} = {}({}, {})", self.dst, self.opcode, self.operand_1, w),
            MOpcode::OpNop => format!("{}", self.opcode),
            MOpcode::OpJmp => format!("{} {}", self.opcode, self.operand_1),
            MOpcode::OpCJmp => format!("{} {}, {}", self.opcode, self.operand_1, self.operand_2),
            MOpcode::OpCall => format!("{} {}", self.opcode, self.operand_1),
            MOpcode::OpCl => format!("{}", self.opcode),
            MOpcode::OpConst(v) => format!("let {} = {}", self.operand_1.name, v),
            _ => format!("{} = {} {} {}",
                         self.dst,
                         self.operand_1,
                         self.opcode,
                         self.operand_2),
        };
        f.pad_integral(true, "", &s)
    }
}
