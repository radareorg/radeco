use super::graph::NodeRef;
use super::index::{InnerIndexType, KnowsIndexType};

enum IndexKind {
	External,
	Phi,
	Inner
}

pub struct PhiIndex<I>(I);

#[allow(dead_code)]
pub struct BasicBlock<Instr: KnowsIndexType> {
	num_ext: Instr::I,
	num_phi: Instr::I,
	instr: Vec<Instr>
}

trait PhiInputProvider {
	fn provide_for<Instr: KnowsIndexType>(src: &BasicBlock<Instr>) -> NodeRef<Instr::I>;
}

impl<Instr: KnowsIndexType> BasicBlock<Instr>
{
	pub fn new() -> Self {
		BasicBlock::<Instr> {
			num_phi: Instr::I::zero(),
			num_ext: Instr::I::zero(),
			instr: Vec::new()
		}
	}
	pub fn phiindex(&self, i: Instr::I) -> PhiIndex<Instr::I> {
		PhiIndex(i+self.num_phi)
	}
	// pub fn phi(&mut self, input_provider: Box<PhiInputProvider>) {
	// }
	// pub fn add(&mut self, opc: Opcode, args: &[OpRef]) {
	// }
	// pub fn flow_into(&mut self, target: &BasicBlock or Node<>) {
	// }
}
