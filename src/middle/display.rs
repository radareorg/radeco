//! Module to implement the display traits for the IR.

use super::ir::{MInst, MOpcode, MVal};
use std::fmt;

impl fmt::Display for MOpcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl fmt::Display for MVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s: String = match self.val_type {
            //MValType::Temporary => format!(...)
            _                  => format!("{}[:{}]", self.name, self.size),
        };
        f.pad_integral(true, "", &s)
    }
}

impl fmt::Display for MInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s: String = match self.opcode {
            MOpcode::OpNot       => format!("{} = {}{}", self.dst, self.opcode, self.operand_1),
            MOpcode::OpEq        => format!("{} = {}", self.dst, self.operand_1),
            MOpcode::OpInc       => format!("{} = {} + 1", self.dst, self.operand_1),
            MOpcode::OpDec       => format!("{} = {} - 1", self.dst, self.operand_1),
            MOpcode::OpIf        => format!("if ({}) {{", self.operand_1),
            MOpcode::OpLoad      => format!("{} = {}({})", self.dst, self.opcode, self.operand_1),
            MOpcode::OpNarrow(w) => format!("{} = {}({}, {})", self.dst, self.opcode, self.operand_1, w),
            MOpcode::OpWiden(w)  => format!("{} = {}({}, {})", self.dst, self.opcode, self.operand_1, w),
            MOpcode::OpNop       => format!("{}", self.opcode),
            MOpcode::OpJmp       => format!("{} {}", self.opcode, self.operand_1),
            MOpcode::OpCJmp      => format!("{} {}, {}", self.opcode, self.operand_1, self.operand_2),
            MOpcode::OpCall      => format!("{} {}", self.opcode, self.operand_1),
            MOpcode::OpCl        => format!("{}", self.opcode),
            //MOpcode::OpSetFl(ref f) => format!("{} = {}({}, {})", self.dst, self.opcode, self.operand_1, f),
            _                    => format!("{} = {} {} {}", self.dst, self.operand_1, self.opcode, self.operand_2),
        };
        f.pad_integral(true, "", &s)
    }
}
