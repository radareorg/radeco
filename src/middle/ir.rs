//! Module to contain the IR.

pub type Address = u64;

#[derive(Debug, Clone, Default)]
pub struct MAddr {
    pub val:      u64,
    pub comments: Vec<String>,
    pub flags:    Vec<String>,
}

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
    Internal,
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
    OpJmp,
    OpCJmp,
    OpCall,
    OpRef,
    OpNarrow,
    OpWiden,
    OpNop,
    OpInvalid,
    OpInc,
    OpDec,
    OpCl,    // '}'
    OpSetFl, // pseudo-op to set flags.
}

#[derive(Debug, Clone, Default)]
pub struct MRegInfo {
    pub reg_type: String,
    pub reg:      String,
    pub size:     u8,
    pub alias:    String,
    pub offset:   u64,
}

#[derive(Debug, Clone)]
pub struct MVal {
    pub name:     String,
    pub size:     u8,
    pub val_type: MValType,
    pub value:    i64,
    pub reg_info: Option<MRegInfo>,
    pub typeset:  u32,
}

// Minor: Change MInst to take Option<MVal> instead. This will allow us to eliminate MVal::null and
// check for `None` instead.
#[derive(Debug, Clone)]
pub struct MInst {
    pub addr:      MAddr,
    pub opcode:    MOpcode,
    pub dst:       MVal,
    pub operand_1: MVal,
    pub operand_2: MVal,
}

impl MOpcode {
    pub fn arity(&self) -> MArity {
        self.info().1
    }

    pub fn to_string(&self) -> String {
        self.info().0
    }

    fn info(&self) -> (String, MArity) {
        let (op, arity) = match *self {
            MOpcode::OpAdd     => ("+", MArity::Binary),
            MOpcode::OpSub     => ("-", MArity::Binary),
            MOpcode::OpMul     => ("*", MArity::Binary),
            MOpcode::OpDiv     => ("/", MArity::Binary),
            MOpcode::OpMod     => ("%", MArity::Binary),
            MOpcode::OpAnd     => ("&", MArity::Binary),
            MOpcode::OpOr      => ("|", MArity::Binary),
            MOpcode::OpXor     => ("^", MArity::Binary),
            MOpcode::OpNot     => ("!", MArity::Unary),
            MOpcode::OpEq      => ("=", MArity::Binary),
            MOpcode::OpCmp     => ("==", MArity::Binary),
            MOpcode::OpGt      => (">", MArity::Binary),
            MOpcode::OpLt      => ("<", MArity::Binary),
            MOpcode::OpLteq    => ("<=", MArity::Binary),
            MOpcode::OpGteq    => (">=", MArity::Binary),
            MOpcode::OpLsl     => ("<<", MArity::Binary),
            MOpcode::OpLsr     => (">>", MArity::Binary),
            MOpcode::OpInc     => ("++", MArity::Unary),
            MOpcode::OpDec     => ("--", MArity::Unary),
            MOpcode::OpIf      => ("if", MArity::Unary),
            MOpcode::OpRef     => ("ref", MArity::Unary),
            MOpcode::OpNarrow  => ("narrow", MArity::Binary),
            MOpcode::OpWiden   => ("widen", MArity::Binary),
            MOpcode::OpNop     => ("nop", MArity::Zero),
            MOpcode::OpInvalid => ("invalid", MArity::Zero),
            MOpcode::OpJmp     => ("jmp", MArity::Unary),
            MOpcode::OpCJmp    => ("jmp if", MArity::Binary),
            MOpcode::OpCall    => ("call", MArity::Unary),
            MOpcode::OpCl      => ("}", MArity::Zero),
            MOpcode::OpSetFl   => ("expand_flag", MArity::Binary),
        };
        (String::from(op), arity)
    }
}

impl MRegInfo {
    pub fn new() -> MRegInfo {
        let def: MRegInfo = Default::default();
        def
    }
}

impl MVal {
    pub fn new(name: String, size: u8, val_type: MValType, value: i64, typeset: u32, reg_info: Option<MRegInfo>) -> MVal {
        MVal {
            name:     name.clone(),
            size:     size,
            val_type: val_type,
            value:    value,
            typeset:  typeset,
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

impl MInst {
    pub fn new(opcode: MOpcode, dst: MVal, op1: MVal, op2: MVal, _addr: Option<MAddr>) -> MInst {
        let addr = _addr.unwrap_or_default();
        MInst {
            addr:      addr,
            opcode:    opcode,
            dst:       dst,
            operand_1: op1,
            operand_2: op2,
        }
    }
}

impl MAddr {
    pub fn new(addr: u64) -> MAddr {
        MAddr {
            val:      addr,
            comments: Vec::new(),
            flags:    Vec::new(),
        }
    }
}
