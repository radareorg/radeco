//! Module to implement the sipay traits for the IR.

use super::ir::{Instruction, Opcode, Value, Location};
use std::fmt;

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_operator().op)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s: String = match self.location {
            Location::Constant => format!("{}", self.name),
            _ => format!("{}[:{}]", self.name, self.size),
        };
        f.pad_integral(true, "", &s)
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s: String = match self.opcode {
            Opcode::OpNot => format!("{} = {}{}", self.dst, self.opcode, self.operand_1),
            Opcode::OpEq => format!("{} = {}", self.dst, self.operand_1),
            Opcode::OpInc => format!("{} = {} + 1", self.dst, self.operand_1),
            Opcode::OpDec => format!("{} = {} - 1", self.dst, self.operand_1),
            Opcode::OpIf => format!("if ({}) {{", self.operand_1),
            Opcode::OpRef => format!("{} = {}({})", self.dst, self.opcode, self.operand_1),
            Opcode::OpNarrow => format!("{} = {}({}, {})", self.dst, self.opcode, self.operand_1, self.operand_2),
            Opcode::OpWiden => format!("{} = {}({}, {})", self.dst, self.opcode, self.operand_1, self.operand_2),
            Opcode::OpNop => format!("{}", self.opcode),
            Opcode::OpJmp => format!("{} {}", self.opcode, self.operand_1),
            Opcode::OpCJmp => format!("{} {}, {}", self.opcode, self.operand_1, self.operand_2),
            Opcode::OpCl => format!("{}", self.opcode),
            _ => format!("{} = {} {} {}", self.dst, self.operand_1, self.opcode, self.operand_2),
        };
        f.pad_integral(true, "", &s)
    }
}
