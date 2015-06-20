//! Module to contain the IR.

pub type Address = u64;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Arity {
    Zero,
    Unary,
    Binary,
    Ternary,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Location {
    Memory,
    Register,
    Constant,
    Temporary,
    Unknown,
    Null,
}

#[derive(Debug, Copy, Clone)]
pub struct Operator<'a> {
    pub op: &'a str,
    pub arity: Arity,
}

impl<'a> Operator<'a> {
    pub fn new(op: &'a str, n: Arity) -> Operator<'a> {
        Operator { op: op, arity: n }
    }

    pub fn nop() -> Operator<'a> {
        Operator { op: "nop", arity: Arity::Zero }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Opcode {
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
    OpRef,
    OpNarrow,
    OpWiden,
    OpNop,
    OpInvalid,
    // Composite Opcodes:
    OpInc,
    OpDec,
    OpCl, // '}'
}

impl<'a> Opcode {
    pub fn to_operator(&self) -> Operator<'a> {
        let (op, arity) = match *self {
            Opcode::OpAdd => ("+", Arity::Binary),
            Opcode::OpSub => ("-", Arity::Binary),
            Opcode::OpMul => ("*", Arity::Binary),
            Opcode::OpDiv => ("/", Arity::Binary),
            Opcode::OpMod => ("%", Arity::Binary),
            Opcode::OpAnd => ("&", Arity::Binary),
            Opcode::OpOr => ("|", Arity::Binary),
            Opcode::OpXor => ("^", Arity::Binary),
            Opcode::OpNot => ("!", Arity::Unary),
            Opcode::OpEq => ("=", Arity::Binary),
            Opcode::OpCmp => ("==", Arity::Binary),
            Opcode::OpGt => (">", Arity::Binary),
            Opcode::OpLt => ("<", Arity::Binary),
            Opcode::OpLteq => ("<=", Arity::Binary),
            Opcode::OpGteq => (">=", Arity::Binary),
            Opcode::OpLsl => ("<<", Arity::Binary),
            Opcode::OpLsr => (">>", Arity::Binary),
            Opcode::OpInc => ("++", Arity::Unary),
            Opcode::OpDec => ("--", Arity::Unary),
            Opcode::OpIf => ("if", Arity::Unary),
            Opcode::OpRef => ("ref", Arity::Unary),
            Opcode::OpNarrow => ("narrow", Arity::Binary),
            Opcode::OpWiden => ("widen", Arity::Binary),
            Opcode::OpNop => ("nop", Arity::Zero),
            Opcode::OpInvalid => ("invalid", Arity::Zero),
            Opcode::OpJmp => ("jmp", Arity::Unary),
            Opcode::OpCJmp => ("jmp if", Arity::Binary),
            Opcode::OpCl => ("}", Arity::Zero),
        };
        Operator::new(op, arity).clone()
    }
}

#[derive(Debug, Clone)]
pub struct Value {
    pub name: String,
    pub size: u8,
    pub location: Location,
    pub value: i64,
    // TODO: Convert from u32 to TypeSet.
    // Every value can be considered in terms of typesets rather than fixed
    // types which can then be narrowed down based on the analysis.
    // TypeSet can be implemented simply as a bit-vector.
    typeset: u32,
}

impl Value {
    pub fn new(name: String, size: u8, location: Location, value: i64, typeset: u32) -> Value {
        Value {
            name: name.clone(),
            size: size,
            location: location,
            value: value,
            typeset: typeset,
        }
    }

    pub fn null() -> Value {
        Value::new("".to_string(), 0, Location::Null, 0, 0)
    }

    pub fn tmp(i: u64, size: u8) -> Value {
        Value::new(format!("tmp_{:x}", i), size, Location::Temporary, 0, 0)
    }

    pub fn constant(i: i64) -> Value {
        Value::new(i.to_string(), 64, Location::Constant, i, 0)
    }
}



#[derive(Debug, Clone)]
pub struct Instruction {
    pub addr: Address,
    pub opcode: Opcode,
    pub dst: Value,
    pub operand_1: Value,
    pub operand_2: Value,
}

impl<'a> Instruction {
    pub fn new(opcode: Opcode, dst: Value, op1: Value, op2: Value, _addr: Option<Address>) -> Instruction {
        let addr = match _addr {
            Some(s) => s,
            None => 0,
        };

        Instruction {
            addr: addr,
            opcode: opcode,
            dst: dst,
            operand_1: op1,
            operand_2: op2,
        }
    }
}
