use ir;
use ir::petgraph::graph::Graph as PetGraph;
use ir::instruction::InstructionType;

use std::cell::RefCell;
use std::marker::PhantomData;

struct PNode;
struct PEdge;

struct GraphV1 {
	petgraph: RefCell<PetGraph<PNode, PEdge>>
}

struct BasicBlockV1<'a> {
	graph: &'a GraphV1,
	index: usize // TODO: find correct int type
}

struct InstructionV1<'a> {
	cell: &'a GraphV1,
	index: usize // TODO: find correct int type
}

struct InstructionXV1<'a> {
	instr: &'a InstructionV1<'a>
}

struct EmptyIterator<T> {
	_marker: PhantomData<T>
}

impl ir::Graph for GraphV1 {
	type BasicBlock = BasicBlockV1<'a>;

	fn blockbylabel<'a>(&'a self, label: &str) -> BasicBlockV1<'a> {
		//
	}

	fn instrbylabel<'a>(&'a self, label: &str) -> InstructionV1<'a> {
		//
	}


	fn newblock(&mut self) -> BasicBlockV1 {
		//
	}
}

impl<'a, 'b> ir::BasicBlock<'a> for BasicBlockV1<'b> {
	fn iter_phis(&self) -> Box<Iterator<Item=InstructionV1<'a>>> {
		let t: EmptyIterator<InstructionV1<'a>>;
		Box::new(t)
	}

	fn iter_instr(&self) -> Box<Iterator<Item=InstructionV1<'a>>> {
		//
	}

	fn iter_succ(&self) -> Box<Iterator<Item=BasicBlockV1<'a>>> {
		//
	}

	fn iter_pred(&self) -> Box<Iterator<Item=BasicBlockV1<'a>>> {
		//
	}


	fn add(&mut self, i: InstructionType) -> InstructionV1 {
		//
	}
}

impl<'a, 'b> ir::Instruction<'a> for InstructionV1<'b> {
	fn get_type(&self) -> InstructionType {
		//
	}


	fn as_phi() -> Option<InstructionXV1<BasicBlockV1>> {
		//
	}

	fn as_nrm() -> Option<InstructionXV1<u8>> {
		//
	}

	fn iter_users(&self) -> Box<Iterator<Item=InstructionV1<'a>>> {
		//
	}
}


/*impl ir::InstructionX<'a, X> for InstructionXV1<'a, X> {
	fn get_opnd(&self, bb: BasicBlock) -> Instruction<'a> {
		//
	}

	fn set_opnd(&self, bb: BasicBlock, value: &Instruction<'a>) {
		//
	}


	fn iter_opnds(&self) -> Box<Iterator<Item=(Instruction<'a>, X)>> {
		//
	}

	fn iter_users(&self) -> Box<Iterator<Item=(Instruction<'a>, X)>> {
		//
	}
}*/

impl<T> Iterator for EmptyIterator<T> {
	type Item = T;
	fn next(&self) -> Option<T> { Option::None }
}
