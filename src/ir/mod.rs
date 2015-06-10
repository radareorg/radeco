extern crate petgraph;

pub mod instruction;
use self::instruction::InstructionType;

mod v1 {
	pub mod graph;
}

// mod v2 {
// 	pub mod basicblock;
// 	pub mod graph;
// 	pub mod index;
// }

trait Graph {
	type BasicBlock;
	type Instruction;

	fn blockbylabel(&self, label: &str) -> BasicBlock;
	fn instrbylabel(&self, label: &str) -> Instruction;

	fn newblock(&mut self) -> BasicBlock;
}

trait BasicBlock<'a> {
	type Instruction;

	fn iter_phis(&self) -> Box<Iterator<Item=Instruction<'a>>>;
	fn iter_instr(&self) -> Box<Iterator<Item=Instruction<'a>>>;
	fn iter_succ(&self) -> Box<Iterator<Item=Self<'a>>>;
	fn iter_pred(&self) -> Box<Iterator<Item=Self<'a>>>;

	fn add(&mut self, i: InstructionType) -> Instruction;
}

trait Instruction<'a> {
	type InstructionX;
	type BasicBlock;

	fn get_type(&self) -> InstructionType;

	fn as_phi() -> Option<InstructionX<BasicBlock>>;
	fn as_nrm() -> Option<InstructionX<u8>>;

	fn iter_users(&self) -> Box<Iterator<Item=Self<'a>>>;
}

trait InstructionX<'a, X> {
	type Instruction;
	type BasicBlock;

	fn get_opnd(&self, X) -> Instruction<'a>;
	fn set_opnd(&self, X, &Instruction<'a>);

	fn iter_opnd(&self) -> Box<Iterator<Item=(Instruction<'a>, X)>>;
	fn iter_users(&self) -> Box<Iterator<Item=(Instruction<'a>, X)>>;
}
