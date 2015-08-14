//! Enumerations and structs to represent instructions.
//! Shared by the SSA representation and the intermediate representation of the ESIL parser.

use std::default::Default;

pub type Address = u64;
pub type WidthSpec = u16;

#[derive(Debug, Clone, Default)]
pub struct MAddr {
	// maybe store section id and offset instead
	pub val: u64,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum MArity {
	Zero,
	Unary,
	Binary,
	Ternary, // Unused for now. Maybe remove later?
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum MValType {
	Register,
	Temporary,
	Internal,
	EsilCur,
	EsilOld,
	Lastsz,
	Unknown,
	Null,
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
	OpLoad,
	OpStore,
	OpNarrow(WidthSpec),
	OpWiden(WidthSpec),
	OpConst(u64),
	OpNop,
	OpInvalid,
	OpInc,
	OpDec,
	OpCl,    // '}'
}

#[derive(Debug, Clone, Default)]
pub struct MRegInfo {
	pub reg_type: String,
	pub reg:      String,
	pub size:     usize,
	pub alias:    String,
	pub offset:   usize,
}

#[derive(Debug, Clone)]
pub struct MVal {
	pub name:       String,
	pub size:       WidthSpec,
	pub val_type:   MValType,
	pub reg_info:   Option<MRegInfo>,
	pub typeset:    u32,
	pub as_literal: Option<u64>,
}

// Default for MVal is null.
impl Default for MVal {
	fn default() -> Self {
		MVal::new("".to_string(), 0, MValType::Null, 0, None)
	}
}

// Minor: Change MInst to take Option<MVal> instead. This will allow us to eliminate MVal::null and
// check for `None` instead.
#[derive(Debug, Clone)]
pub struct MInst {
	pub addr:         MAddr,
	pub opcode:       MOpcode,
	pub dst:          MVal,
	pub operand_1:    MVal,
	pub operand_2:    MVal,
	pub update_flags: bool,
}

impl MOpcode {
	pub fn to_inst(&self, dst: MVal, op1: MVal, op2: MVal, addr: Option<MAddr>) -> MInst {
		MInst::new(*self, dst, op1, op2, addr)
	}

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
			MOpcode::OpJmp   => true,
			MOpcode::OpCJmp  => true,
			MOpcode::OpCall  => true,
			_ => false,
		}
	}

	pub fn allowed_in_ssa(&self) -> bool {
		match *self {
			MOpcode::OpJmp     => false,
			MOpcode::OpCJmp    => false,
			MOpcode::OpNop     => false,
			MOpcode::OpInvalid => false,
			MOpcode::OpInc     => false,
			MOpcode::OpDec     => false,
			MOpcode::OpCl      => false,
			_ => true,
		}
	}

	fn info(&self) -> (String, MArity) {
		let (op, arity) = match *self {
			MOpcode::OpAdd        => ("+", MArity::Binary),
			MOpcode::OpSub        => ("-", MArity::Binary),
			MOpcode::OpMul        => ("*", MArity::Binary),
			MOpcode::OpDiv        => ("/", MArity::Binary),
			MOpcode::OpMod        => ("%", MArity::Binary),
			MOpcode::OpAnd        => ("&", MArity::Binary),
			MOpcode::OpOr         => ("|", MArity::Binary),
			MOpcode::OpXor        => ("^", MArity::Binary),
			MOpcode::OpNot        => ("!", MArity::Unary),
			MOpcode::OpEq         => ("=", MArity::Binary),
			MOpcode::OpCmp        => ("==", MArity::Binary),
			MOpcode::OpGt         => (">", MArity::Binary),
			MOpcode::OpLt         => ("<", MArity::Binary),
			MOpcode::OpLteq       => ("<=", MArity::Binary),
			MOpcode::OpGteq       => (">=", MArity::Binary),
			MOpcode::OpLsl        => ("<<", MArity::Binary),
			MOpcode::OpLsr        => (">>", MArity::Binary),
			MOpcode::OpIf         => ("if", MArity::Unary),
			MOpcode::OpLoad       => ("load", MArity::Unary),
			MOpcode::OpStore      => ("store", MArity::Binary),
			MOpcode::OpNarrow(_)  => ("narrow", MArity::Unary),
			MOpcode::OpWiden(_)   => ("widen", MArity::Unary),
			MOpcode::OpJmp        => ("jmp", MArity::Unary),
			MOpcode::OpCJmp       => ("jmp if", MArity::Binary),
			MOpcode::OpCall       => ("call", MArity::Unary),
			MOpcode::OpConst(_)   => ("const", MArity::Zero),
			MOpcode::OpNop        => ("nop", MArity::Zero),
			MOpcode::OpInvalid    => ("invalid", MArity::Zero),
			MOpcode::OpInc        => ("++", MArity::Unary),
			MOpcode::OpDec        => ("--", MArity::Unary),
			MOpcode::OpCl         => ("}", MArity::Zero),
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
	pub fn new(name: String, size: WidthSpec, val_type: MValType, typeset: u32, reg_info: Option<MRegInfo>) -> MVal {
		MVal {
			name:       name.clone(),
			size:       size,
			val_type:   val_type,
			typeset:    typeset,
			reg_info:   reg_info,
			as_literal: None
		}
	}

	pub fn null() -> MVal {
		MVal::new("".to_string(), 0, MValType::Null, 0, None)
	}

	pub fn tmp(i: u64, size: WidthSpec) -> MVal {
		MVal::new(format!("tmp_{:x}", i), size, MValType::Temporary, 0, None)
	}

	pub fn esilcur() -> MVal {
		MVal::new("".to_string(), 64, MValType::EsilCur, 0, None)
	}

	pub fn esilold() -> MVal {
		MVal::new("".to_string(), 64, MValType::EsilOld, 0, None)
	}

	pub fn esillastsz() -> MVal {
		MVal::new("".to_string(), 64, MValType::Lastsz, 0, None)
	}
}

impl MInst {
	pub fn new(opcode: MOpcode, dst: MVal, op1: MVal, op2: MVal, _addr: Option<MAddr>) -> MInst {
		let addr = _addr.unwrap_or_default();
		MInst {
			addr:         addr,
			opcode:       opcode,
			dst:          dst,
			operand_1:    op1,
			operand_2:    op2,
			update_flags: false,
		}
	}
}

impl MAddr {
	pub fn new(addr: u64) -> MAddr {
		MAddr { val: addr }
	}
}
