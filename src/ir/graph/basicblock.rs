use super::NodeRef;
use super::inner::{InnerGraph, InnerEdgeLight, InnerIndexType};

pub struct PhiIndex<I>(I);

pub struct BasicBlock<I: InnerIndexType, Instr> {
	num_ext: I,
	num_phi: I,
	graph: InnerGraph<Instr, InnerEdgeLight<I>>
}


impl<I: InnerIndexType, Instr> BasicBlock<I, Instr>
{
	pub fn new() -> Self {
		BasicBlock::<I, Instr> {
			num_phi: I::zero(),
			num_ext: I::zero(),
			graph: InnerGraph::<Instr, InnerEdgeLight<I>>::new()
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
