extern crate petgraph;

pub mod instruction;
use self::instruction::InstructionType;

mod v1 {
	// pub mod basicblock;
	// pub mod graph;
}

mod v2 {
	pub mod basicblock;
	pub mod graph;
	pub mod index;
}

trait Graph {
	fn blockbylabel(&self, label: &str) -> BasicBlock;
	fn instrbylabel(&self, label: &str) -> Instruction;
}

trait BasicBlock {
	fn iter_phis();
	fn iter_instr();
	fn iter_succ();
	fn iter_pred();
}

trait Instruction<'a> {
	fn get_type(&self) -> InstructionType;
	fn get_opnd(&self, u8) -> Instruction<'a>;
	fn set_opnd(&self, u8, &Instruction<'a>);
}
