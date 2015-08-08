//! Implements the SSA construction algorithm described in
//! "Simple and Efficient Construction of Static Single Assignment Form"

use std::collections::HashMap;
use petgraph::graph::NodeIndex;
use frontend::structs::LRegInfo;
use middle::cfg::NodeData as CFGNodeData;
use middle::cfg::EdgeType as CFGEdgeType;
use middle::cfg::{CFG, BasicBlock};
use middle::ssa::{BBInfo, SSA, SSAMod, ValueType};
use middle::ir::{MVal, MInst, MOpcode, MValType};
use middle::phiplacement::PhiPlacer;
use middle::regfile::SubRegisterFile;
use middle::dce;

pub type VarId = usize;

pub struct SSAConstruction<'a, T> 
where T: Clone + SSAMod<BBInfo=BBInfo> + 'a {
	pub phiplacer: PhiPlacer<'a, T>,
	pub regfile:   SubRegisterFile,
	pub temps:     HashMap<String, T::ValueRef>,
}

impl<'a, T> SSAConstruction<'a, T> 
where T: 'a + Clone +
SSAMod<BBInfo=BBInfo, ValueRef=NodeIndex, ActionRef=NodeIndex> {
	pub fn new(ssa: &'a mut T, reg_info: &LRegInfo) -> SSAConstruction<'a, T> {
		let mut sc = SSAConstruction {
			phiplacer: PhiPlacer::new(ssa),
			regfile:   SubRegisterFile::new(reg_info),
			temps:     HashMap::new(),
		};
		sc.phiplacer.add_variables(vec![
								   ValueType::Integer{width: 64}, // cur
								   ValueType::Integer{width: 64}  // old
		]);
		// make the following a method of regfile?
		sc.phiplacer.add_variables(sc.regfile.whole_registers.clone());
		sc
	}

	pub fn run(&mut self, cfg: &CFG) {
		let mut blocks = Vec::<T::ActionRef>::with_capacity(cfg.g.node_count());
		let bb_iter = cfg.bbs.iter();

		for _ in 0..cfg.g.node_count() {
			blocks.push(self.phiplacer.ssa.invalid_action());
		}

		{
			// Insert the entry and exit blocks for the ssa.
			let block = self.phiplacer.add_block(BBInfo { addr: 0 });
			self.phiplacer.ssa.mark_start_node(&block);
			let zero = self.phiplacer.ssa.add_const(block, 0);
			self.phiplacer.write_variable(block, 0, zero); // cur = 0
			self.phiplacer.write_variable(block, 1, zero); // old = 0

			for (i, name) in self.regfile.whole_names.iter().enumerate() {
				let argnode = self.phiplacer.ssa.add_comment(block, name.clone());
				self.phiplacer.write_variable(block, i+1, argnode);
			}

			blocks[cfg.entry.index()] = block;

			let block = self.phiplacer.add_block(BBInfo { addr: 0 });
			self.phiplacer.ssa.mark_exit_node(&block);
			blocks[cfg.exit.index()] = block;
			self.phiplacer.sync_register_state(block);
		}

		for (addr, i) in bb_iter {
			let block = self.phiplacer.add_block(BBInfo { addr: *addr });
			blocks[i.index()] = block;
			match cfg.g[*i] {
				CFGNodeData::Block(ref srcbb) => {
					self.process_block(block, srcbb);
				}
				_ => unreachable!(),
			}
		}

		for edge in cfg.g.raw_edges() {
			let i = match edge.weight.edge_type {
				CFGEdgeType::False => 0,
				CFGEdgeType::True => 1,
				CFGEdgeType::Unconditional => 2,
			};
			self.phiplacer.ssa.add_control_edge(
				blocks[edge.source().index()],
				blocks[edge.target().index()],
				i);
		}
		for &block in &blocks {
			self.phiplacer.seal_block(block);
		}
		//self.phiplacer.ssa.stable_indexing = false;
		//self.phiplacer.ssa.cleanup();

		{
			dce::collect(self.phiplacer.ssa);
		}
		//dce::collect(self.phiplacer.ssa, &[exit_regstate]);
		//self.phiplacer.ssa.cleanup();
	}

	fn process_in_flag(&mut self, block: T::ActionRef, _mval: &MVal) -> T::ValueRef {
		// this would be neccesary if parser didn't use EsilCur/EsilOld
		let _cur = self.phiplacer.read_variable(block, 0);
		let _old = self.phiplacer.read_variable(block, 1);
		//unimplemented!();
		self.phiplacer.ssa.invalid_value()
	}

	fn process_in(&mut self, block: T::ActionRef, mval: &MVal) -> T::ValueRef {
		match mval.val_type {
			MValType::Register  => self.regfile.read_register(&mut self.phiplacer, 2, block, &mval.name),
			MValType::Temporary => self.temps[&mval.name],
			MValType::Internal  => self.process_in_flag(block, mval),
			MValType::EsilCur   => self.phiplacer.read_variable(block, 0),
			MValType::EsilOld   => self.phiplacer.read_variable(block, 1),
			MValType::Unknown   => self.phiplacer.ssa.invalid_value(),
			                       //self.phiplacer.ssa.add_comment(block, &"Unknown".to_string()), // unimplemented!()
			MValType::Null      => self.phiplacer.ssa.invalid_value(),
		}
	}

	fn process_out(&mut self, block: T::ActionRef, mval: &MVal, value: T::ValueRef) {
		match mval.val_type { 
			MValType::Register  => self.regfile.write_register(&mut self.phiplacer, 2, block, &mval.name, value),
			MValType::Temporary => {self.temps.insert(mval.name.clone(), value);},
			MValType::Null      => {},
			_                   => panic!(),
		}
	}

	fn process_op(&mut self, block: T::ActionRef, inst: &MInst, n0: T::ValueRef, n1: T::ValueRef) -> T::ValueRef {
		if inst.opcode == MOpcode::OpEq {
			return n0
		}

		let dsttype = match inst.dst.val_type {
			MValType::Null => ValueType::Integer{width: 0}, // there is no ValueType::None?
			_              => ValueType::Integer{width: inst.dst.size},
		};


		/*
		// TODO: When developing a ssa check pass, reuse this maybe
		let width = match inst.opcode {
		MOpcode::OpNarrow(w)
		| MOpcode::OpWiden(w) => { w },
		MOpcode::OpCmp => { 1 },
		_ => { 
		let extract = |x: NodeData| -> Option<u8> {
		if let NodeData::Op(_, ValueType::Integer { width: w }) = x {
		Some(w)
		} else {
		None
		}
		};
		let w1 = self.phiplacer.ssa.safe_get_node_data(&n0)
		.map(&extract)
		.unwrap_or(None);

		let w2 = self.phiplacer.ssa.safe_get_node_data(&n1)
		.map(&extract)
		.unwrap_or(None);

		if w1 == None && w2 == None {
		// TODO: Replace by default value.
		64
		} else if w1 == None {
		w2.unwrap()
		} else if w2 == None {
		w1.unwrap()
		} else {
		let w1 = w1.unwrap();
		let w2 = w2.unwrap();
		// Check the width of the two operands.
		assert!(w1 == w2);
		w1
		}
		},
		};*/

		let nn = {
			let ref mut ssa = self.phiplacer.ssa;
			let nn = ssa.add_op(block, inst.opcode, dsttype);
			ssa.op_use(nn, 0, n0);
			ssa.op_use(nn, 1, n1);
			nn
		};

		if inst.update_flags {
			let old = self.phiplacer.read_variable(block, 0);
			self.phiplacer.write_variable(block, 1, old);
			self.phiplacer.write_variable(block, 0, nn);
		}

		return nn
	}

	fn process_block(&mut self, block: T::ActionRef, source: &BasicBlock) {
		let mut machinestate = self.phiplacer.ssa.to_value(block);
		for ref instruction in &source.instructions {
			let n0 = self.process_in(block, &instruction.operand_1);
			let n1 = self.process_in(block, &instruction.operand_2);

			if instruction.opcode == MOpcode::OpJmp {
				// TODO: In case of static jumps, this is trivial and does not need a selector.
				// In case of dynamic jump, the jump targets have to be determined.
				//self.ssa.g.add_edge(block, n0, SSAEdgeData::DynamicControl(0));
				break;
			}

			if instruction.opcode == MOpcode::OpCJmp {
				self.phiplacer.ssa.mark_selector(n0, block);
				continue;
			}

			let nn = self.process_op(block, instruction, n0, n1);

			if instruction.opcode == MOpcode::OpLoad {
				self.phiplacer.ssa.op_use(nn, 3, machinestate);
			}
			if instruction.opcode == MOpcode::OpStore {
				self.phiplacer.ssa.op_use(nn, 3, machinestate);
				machinestate = nn;
			}

			self.process_out(block, &instruction.dst, nn);
		}
	}
}
