//! Module to contain the IR.

pub type Address = u64;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum MArity {
    Zero,
    Unary,
    Binary,
    Ternary,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum MValType {
    Memory,
    Register,
    Constant,
    Temporary,
    Unknown,
    Null,
}

#[derive(Debug, Copy, Clone)]
pub struct MOperator<'a> {
    pub op: &'a str,
    pub arity: MArity,
}

impl<'a> MOperator<'a> {
    pub fn new(op: &'a str, n: MArity) -> MOperator<'a> {
        MOperator { op: op, arity: n }
    }

    pub fn nop() -> MOperator<'a> {
        MOperator { op: "nop", arity: MArity::Zero }
    }
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
    OpLteq,
    OpGteq,
    OpLsl,
    OpLsr,
    OpIf,
    OpJmp,  // Unconditional Jmp.
    OpCJmp, // Conditional Jmp.
    OpCall,
    OpRef,
    OpNarrow,
    OpWiden,
    OpNop,
    OpInvalid,
    // Composite MOpcodes:
    OpInc,
    OpDec,
    OpCl, // '}'
}

impl<'a> MOpcode {
    pub fn to_operator(&self) -> MOperator<'a> {
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
            MOpcode::OpLteq => ("<=", MArity::Binary),
            MOpcode::OpGteq => (">=", MArity::Binary),
            MOpcode::OpLsl => ("<<", MArity::Binary),
            MOpcode::OpLsr => (">>", MArity::Binary),
            MOpcode::OpInc => ("++", MArity::Unary),
            MOpcode::OpDec => ("--", MArity::Unary),
            MOpcode::OpIf => ("if", MArity::Unary),
            MOpcode::OpRef => ("ref", MArity::Unary),
            MOpcode::OpNarrow => ("narrow", MArity::Binary),
            MOpcode::OpWiden => ("widen", MArity::Binary),
            MOpcode::OpNop => ("nop", MArity::Zero),
            MOpcode::OpInvalid => ("invalid", MArity::Zero),
            MOpcode::OpJmp => ("jmp", MArity::Unary),
            MOpcode::OpCJmp => ("jmp if", MArity::Binary),
            MOpcode::OpCall => ("call", MArity::Unary),
            MOpcode::OpCl => ("}", MArity::Zero),
        };
        MOperator::new(op, arity).clone()
    }
}

#[derive(Debug, Clone, Default)]
pub struct MRegInfo {
    pub reg_type: String,
    pub reg: String,
    pub size: u8,
    pub alias: String,
    pub offset: u64,
}

#[derive(Debug, Clone)]
pub struct MVal {
    pub name: String,
    pub size: u8,
    pub location: MValType,
    pub value: i64,
    pub reg_info: Option<MRegInfo>,
    pub typeset: u32,
}

impl MRegInfo {
    pub fn new() -> MRegInfo {
        let def: MRegInfo = Default::default();
        def
    }
}

impl MVal {
    pub fn new(name: String, size: u8, location: MValType, value: i64, typeset: u32, reg_info: Option<MRegInfo>) -> MVal {
        MVal {
            name: name.clone(),
            size: size,
            location: location,
            value: value,
            typeset: typeset,
            reg_info: reg_info,
        }
    }

    pub fn null() -> MVal {
        MVal::new("".to_string(), 0, MValType::Null, 0, 0, None)
    }

    pub fn tmp(i: u64, size: u8) -> MVal {
        MVal::new(format!("tmp_{:x}", i), size, MValType::Temporary, 0, 0, None)
    }

    pub fn constant(i: i64) -> MVal {
        MVal::new(i.to_string(), 64, MValType::Constant, i, 0, None)
    }
}



#[derive(Debug, Clone)]
pub struct MInst {
    pub addr: Address,
    pub opcode: MOpcode,
    pub dst: MVal,
    pub operand_1: MVal,
    pub operand_2: MVal,
}

impl<'a> MInst {
    pub fn new(opcode: MOpcode, dst: MVal, op1: MVal, op2: MVal, _addr: Option<Address>) -> MInst {
        let addr = match _addr {
            Some(s) => s,
            None => 0,
        };

        MInst {
            addr: addr,
            opcode: opcode,
            dst: dst,
            operand_1: op1,
            operand_2: op2,
        }
    }
}
