pub mod instruction;
pub mod graph;
mod traits;

use self::instruction::InstructionType;

trait Graph {
	fn blockbylabel<'a>(&'a self, label: &str) -> BasicBlock<'a>;
	fn instrbylabel<'a>(&'a self, label: &str) -> Instruction<'a>;

	fn newblock(&mut self) -> BasicBlock;
}

trait BasicBlock<'a> {
	fn iter_phis(&self) -> Box<Iterator<Item=Instruction<'a>>>;
	fn iter_instr(&self) -> Box<Iterator<Item=Instruction<'a>>>;
	fn iter_succ(&self) -> Box<Iterator<Item=Self>>;
	fn iter_pred(&self) -> Box<Iterator<Item=Self>>;

	fn add(&mut self, i: InstructionType) -> Instruction;
}

trait Instruction<'a> {
	fn get_type(&self) -> InstructionType;

	fn as_phi(&self) -> Option<InstructionX<BasicBlock<'a>>>;
	fn as_nrm(&self) -> Option<InstructionX<u8>>;

	fn iter_users(&self) -> Box<Iterator<Item=Self>>;
}

trait InstructionX<'a, X> {
	fn get_opnd(&self, X) -> Instruction<'a>;
	fn set_opnd(&self, X, &Instruction<'a>);

	fn iter_opnd(&self) -> Box<Iterator<Item=(Instruction<'a>, X)>>;
	fn iter_users(&self) -> Box<Iterator<Item=(Instruction<'a>, X)>>;
}
