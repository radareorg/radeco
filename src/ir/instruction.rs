use std::mem;
use super::traits;

pub enum FieldSpec {
	Register
}

pub enum UnaryArith {
	Neg,
	Not,
	SignExtend,
	ZeroExtend,
	Truncate
}

pub enum BinaryArith {
	Sub,
	Div
}

pub enum NaryArith {
	Add,
	Mul
}

pub type BitCount = u8;
pub type Const = u64;

#[derive(Clone, Copy, Debug)]
pub enum ValueType {
	Void,
	MachineState,
	Bits(BitCount)
}

pub enum InstructionType {
	Nop,
	Phi(ValueType),
	Select(ValueType),
	ConstBits(Const),

	Unary(BitCount, UnaryArith),
	Binary(BitCount, BinaryArith),
//	Extension(BitCount, [Opnd; 2]),

	Extract(FieldSpec),
	Inject(FieldSpec)
}

impl traits::InstructionType for InstructionType {
	type PhiType = ValueType;
	fn make_phi(vt: ValueType) -> InstructionType {
		InstructionType::Phi(vt)
	}
	fn is_phi(&self) -> bool {
		if let &InstructionType::Phi(_) = self { true } else { false }
	}
}

pub fn exprtype(nd: &InstructionType) -> ValueType {
	match *nd {
		InstructionType::Nop                   => ValueType::Void,
		InstructionType::Phi(valueType)        => valueType,
		InstructionType::Select(valueType)     => valueType,
		InstructionType::ConstBits(width)      => ValueType::Bits(mem::size_of::<Const>() as BitCount),

		InstructionType::Unary(width, ref op)  => ValueType::Bits(width),
		InstructionType::Binary(width, ref op) => ValueType::Bits(width),
		//InstructionType::Nary(width, op)   => ValueType::Bits(width),

		InstructionType::Extract(_)            => ValueType::Bits(32),
		InstructionType::Inject(_)             => ValueType::MachineState
	}
}
