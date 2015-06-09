use super::graph::NodeRef;
use super::index::InnerIndexType;

enum IndexKind {
	External,
	Phi,
	Inner
}

pub struct PhiIndex<I>(I);

#[allow(dead_code)]
pub struct BasicBlock<I: InnerIndexType, Instr> {
	num_ext: I,
	num_phi: I,
	instr: Vec<Instr>
}

trait PhiInputProvider<I> {
	fn provide_for<Instr>(src: &BasicBlock<I, Instr>) -> NodeRef<I>;
}

impl<I: InnerIndexType, Instr> BasicBlock<I, Instr>
{
	pub fn new() -> Self {
		BasicBlock::<I, Instr> {
			num_phi: I::zero(),
			num_ext: I::zero(),
			instr: Vec::new()
		}
	}
	pub fn phiindex(&self, i: I) -> PhiIndex<I> {
		PhiIndex(i+self.num_phi)
	}
	// pub fn phi(&mut self, input_provider: Box<PhiInputProvider>) {
	// }
	// pub fn add(&mut self, opc: Opcode, args: &[OpRef]) {
	// }
	// pub fn flow_into(&mut self, target: &BasicBlock or Node<>) {
	// }
}
