//! Implements the SSA construction algorithm described in
//! "Simple and Efficient Construction of Static Single Assignment Form"

use std::collections::HashMap;
use petgraph::graph::NodeIndex;
use frontend::structs::LRegInfo;
use middle::cfg::NodeData as CFGNodeData;
use middle::cfg::EdgeType as CFGEdgeType;
use middle::cfg::{CFG, BasicBlock};
use middle::ssa::{BBInfo, SSA, SSAMod, ValueType};
use middle::ir::{MVal, MOpcode, MValType};
use middle::regfile::SubRegisterFile;
use transform::phiplacement::PhiPlacer;

pub type VarId = usize;

pub struct SSAConstruction<'a, T: SSAMod<BBInfo=BBInfo> + 'a> {
	pub phiplacer: PhiPlacer<'a, T>,
	pub regfile:   SubRegisterFile,
	pub temps:     HashMap<String, T::ValueRef>,
}

impl<'a, T: SSAMod<BBInfo=BBInfo> + 'a> SSAConstruction<'a, T> {
	pub fn new(ssa: &'a mut T, reg_info: &LRegInfo) -> SSAConstruction<'a, T> {
		let mut sc = SSAConstruction {
			phiplacer: PhiPlacer::new(ssa),
			regfile:   SubRegisterFile::new(reg_info),
			temps:     HashMap::new(),
		};
		// make the following a method of regfile?
		sc.phiplacer.add_variables(sc.regfile.whole_registers.clone());
		sc
	}

	pub fn run(&mut self, cfg: &CFG) {
		// TODO: move this out
		let mut blocks = Vec::<T::ActionRef>::new();
		for i in 0..cfg.g.node_count() {
		 	let block = self.phiplacer.ssa.add_block(BBInfo{addr: 99});
			self.phiplacer.incomplete_phis.insert(block, HashMap::new());
			blocks.push(block);

			match cfg.g[NodeIndex::new(i)] {
		 		CFGNodeData::Block(ref srcbb) => {
		 			self.process_block(block, srcbb);
				},
				CFGNodeData::Entry => {
					/*for (ref name, _ /*ref mut vd*/) in &mut self.phiplacer.current_def {
						// TODO: Look up actual name
						// let msg = format!("initial_{}", name);
						// TODO: Add OpComment?
						//vd.insert(block, self.phiplacer.ssa.add_comment(block, &msg));
					}*/
				},
				_ => {}
			}
		}
		for edge in cfg.g.raw_edges() {
			let i = match edge.weight.edge_type {
				CFGEdgeType::True => 1,
				CFGEdgeType::False => 0,
				CFGEdgeType::Unconditional => 0,
			};
			self.phiplacer.ssa.add_control_edge(
				blocks[edge.source().index()],
				blocks[edge.target().index()],
				i);
		}
		for block in blocks {
			self.phiplacer.seal_block(block);
		}
		//self.phiplacer.ssa.stable_indexing = false;
		//self.phiplacer.ssa.cleanup();
	}

	fn process_in(&mut self, block: T::ActionRef, mval: &MVal) -> T::ValueRef {
		match mval.val_type {
			MValType::Register  => self.regfile.read_register(&mut self.phiplacer, 0, block, &mval.name),
			MValType::Temporary => self.temps[&mval.name],
			MValType::Unknown   => self.phiplacer.ssa.invalid_value(), //self.phiplacer.ssa.add_comment(block, &"Unknown".to_string()), // unimplemented!()
			MValType::Internal  => self.phiplacer.ssa.invalid_value(), //self.phiplacer.ssa.add_comment(block, &mval.name), // unimplemented!()
			MValType::Null      => self.phiplacer.ssa.invalid_value(),
		}
	}

	fn process_out(&mut self, block: T::ActionRef, mval: &MVal, value: T::ValueRef) {
		match mval.val_type {
			MValType::Register  => self.regfile.write_register(&mut self.phiplacer, 0, block, &mval.name, value),
			MValType::Temporary => {self.temps.insert(mval.name.clone(), value);},
			MValType::Unknown   => {}, // unimplemented!(),
			MValType::Internal  => {}, // unimplemented!()
			MValType::Null      => {},
		}
	}

	fn process_op(&mut self, block: T::ActionRef, opc: MOpcode, n0: T::ValueRef, n1: T::ValueRef) -> T::ValueRef {
		if opc == MOpcode::OpEq {
			return n0
		}
		let ref mut ssa = self.phiplacer.ssa;

		// TODO: give correct integer type here
		let nn = ssa.add_op(block, opc, ValueType::Integer{width: 64});
		ssa.op_use(nn, 0, n0);
		ssa.op_use(nn, 1, n1);
		return nn
	}

	fn process_block(&mut self, block: T::ActionRef, source: &BasicBlock) {
		for ref instruction in &source.instructions {
			let n0 = self.process_in(block, &instruction.operand_1);
			let n1 = self.process_in(block, &instruction.operand_2);

			if instruction.opcode == MOpcode::OpJmp {
				//TODO
				//self.ssa.g.add_edge(block, n0, SSAEdgeData::DynamicControl(0));
				break;
			}

			if instruction.opcode == MOpcode::OpCJmp {
				self.phiplacer.ssa.mark_selector(block, n0);
				continue;
			}

			let nn = self.process_op(block, instruction.opcode, n0, n1);
			self.process_out(block, &instruction.dst, nn);
		}
	}
}
