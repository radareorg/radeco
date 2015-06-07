use util::grid::Grid;

struct NodeRef; // stub
struct Use; // stub
struct Instr; // stub

#[allow(dead_code)]
struct Usable<T> {
	t: T,
	uses: Vec<Use>
}

#[allow(dead_code)]
pub struct BasicBlock<'a> {
	// TODO: Replace <'a> refs by something that integrates with our graph libs
	phigrid: Grid<&'a BasicBlock<'a>, Phi, NodeRef>,
	instr: Vec<Usable<Instr>>
}

pub type PhiInputProvider = Box<FnMut(&BasicBlock) -> NodeRef>;

pub struct Phi {
	input_provider: PhiInputProvider
}

fn phiprovide(bb: &mut &BasicBlock, phi: &mut Phi) -> NodeRef {
	let nr = (&mut *phi.input_provider)(bb);
	//assert!(nr); make sure nr is actully in bb
	nr
}

impl<'a> BasicBlock<'a> {
	pub fn new() -> Self {
		BasicBlock { phigrid: Grid::new(Box::new(phiprovide)), instr: Vec::new() }
	}
	pub fn phi(&mut self, input_provider: PhiInputProvider) {
		self.phigrid.push_row(Phi {input_provider: input_provider});
	}
	// pub fn add(&mut self, opc: Opcode, args: &[OpRef]) {
	// 	self.instr.push()
	// }
	//pub fn flow_into(&mut self, )
}
