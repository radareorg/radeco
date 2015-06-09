use super::KnowsIndexType;
use std::mem;

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

pub enum Instruction<Ref> {
	Nop,
	Phi(ValueType), //Box<FnMut(&BasicBlock) -> Ref>),
	Select(ValueType, Ref),
	ConstBits(Const),

	Unary(BitCount, UnaryArith, [Ref; 1]),
	Binary(BitCount, BinaryArith, [Ref; 2]),
//	Extension(BitCount, [Ref; 2]),

	Extract(FieldSpec, [Ref; 1]),
	Inject(FieldSpec, [Ref; 2])
}

impl<Ref: KnowsIndexType> KnowsIndexType for Instruction<Ref> {
	type I = Ref::I;
}

pub fn exprtype<Ref>(nd: &Instruction<Ref>) -> ValueType {
	match *nd {
		Instruction::Nop                      => ValueType::Void,
		Instruction::Phi(valueType)           => valueType,
		Instruction::Select(valueType, _)     => valueType,
		Instruction::ConstBits(width)         => ValueType::Bits(mem::size_of::<Const>() as BitCount),

		Instruction::Unary(width, ref op, _)  => ValueType::Bits(width),
		Instruction::Binary(width, ref op, _) => ValueType::Bits(width),
		//Instruction::Nary(width, op)   => ValueType::Bits(width),

		Instruction::Extract(_, _)            => ValueType::Bits(32),
		Instruction::Inject(_, _)             => ValueType::MachineState
	}
}
