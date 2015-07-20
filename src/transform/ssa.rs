//! Implements the SSA construction algorithm described in
//! "Simple and Efficient Construction of Static Single Assignment Form"

use std::collections::{HashSet, HashMap};
use petgraph::graph::NodeIndex;
use frontend::structs::LRegInfo;
use middle::cfg::NodeData as CFGNodeData;
use middle::cfg::EdgeType as CFGEdgeType;
use middle::cfg::{CFG, BasicBlock};
use middle::ssa::{BBInfo, SSA, SSAMod, NodeData, ValueType};
use middle::ir::{MVal, MOpcode, MValType};

pub type VarId = String; // consider transitioning to &str

pub struct SSAConstruction<'a, T: SSAMod<BBInfo=BBInfo> + 'a> {
	pub ssa:          &'a mut T,
	variables:        Vec<VarId>, // assume consequtive integers?
	sealed_blocks:    HashSet<T::ActionRef>, // replace with bitfield
	current_def:      HashMap<VarId, HashMap<T::ActionRef, T::ValueRef>>,
	incomplete_phis:  HashMap<T::ActionRef, HashMap<VarId, T::ValueRef>>,
	global_variables: HashMap<VarId, T::ValueRef>
}

impl<'a, T: SSAMod<BBInfo=BBInfo> + 'a> SSAConstruction<'a, T> {
	pub fn new(ssa: &'a mut T, reg_info: &LRegInfo) -> SSAConstruction<'a, T> {
		let mut s = SSAConstruction {
			ssa:              ssa,
			variables:        Vec::new(),
			sealed_blocks:    HashSet::new(),
			current_def:      HashMap::new(),
			incomplete_phis:  HashMap::new(),
			global_variables: HashMap::new()
		};
		for reg in &reg_info.reg_info {
			s.variables.push(reg.name.clone());
		}
		for var in &s.variables {
			s.current_def.insert(var.clone(), HashMap::new());
		}
		return s
	}

	pub fn write_variable(&mut self, block: T::ActionRef, variable: VarId, value: T::ValueRef) {
		if let Option::Some(vd) = self.current_def.get_mut(&variable) {
			vd.insert(block, value);
		} else {
			self.global_variables.insert(variable, value);
		}
	}

	pub fn read_variable(&mut self, block: T::ActionRef, variable: VarId) -> T::ValueRef {
		let mut n = match {
			if let Option::Some(vd) = self.current_def.get(&variable) {
				vd.get(&block)
			} else {
				self.global_variables.get(&variable)
			}
		}.map(|r|*r) {
			Option::Some(r) => r,
			Option::None => self.read_variable_recursive(variable, block)
		};
		n = self.ssa.refresh(n);
		return n
	}

	pub fn add_block(&mut self, info: BBInfo) -> T::ActionRef {
		let block = self.ssa.add_block(info);
		self.incomplete_phis.insert(block, HashMap::new());
		block
	}

	pub fn seal_block(&mut self, block: T::ActionRef) {
		let inc = self.incomplete_phis[&block].clone(); // TODO: remove clone

		for (variable, node) in inc {
			self.add_phi_operands(block, variable.clone(), node.clone());
		}
		self.sealed_blocks.insert(block);
	}

	pub fn run(&mut self, cfg: &CFG) {
		let mut blocks = Vec::<T::ActionRef>::new();
		for i in 0..cfg.g.node_count() {
		 	let block = self.ssa.add_block(BBInfo{addr: 99});
			self.incomplete_phis.insert(block, HashMap::new());
			blocks.push(block);

			match cfg.g[NodeIndex::new(i)] {
		 		CFGNodeData::Block(ref srcbb) => {
		 			self.process_block(block, srcbb);
				},
				CFGNodeData::Entry => {
					for (ref name, ref mut vd) in &mut self.current_def {
						let mut msg = "initial_".to_string();
						msg.push_str(name);
						// TODO: Add OpComment?
						//vd.insert(block, self.ssa.add_comment(block, &msg));
					}
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
			self.ssa.add_control_edge(
				blocks[edge.source().index()],
				blocks[edge.target().index()],
				i);
		}
		for block in blocks {
			self.seal_block(block);
		}
		//self.ssa.stable_indexing = false;
		//self.ssa.cleanup();
	}

	fn process_in(&mut self, block: T::ActionRef, mval: &MVal) -> T::ValueRef {
		match mval.val_type {
			MValType::Register  => self.read_variable(block, mval.name.clone()),
			MValType::Temporary => self.read_variable(block, mval.name.clone()),
			MValType::Unknown   => self.ssa.invalid_value(), //self.ssa.add_comment(block, &"Unknown".to_string()), // unimplemented!()
			MValType::Internal  => self.ssa.invalid_value(), //self.ssa.add_comment(block, &mval.name), // unimplemented!()
			MValType::Null      => self.ssa.invalid_value(),
		}
	}

	fn process_out(&mut self, block: T::ActionRef, mval: &MVal, value: T::ValueRef) {
		match mval.val_type {
			MValType::Register  => self.write_variable(block, mval.name.clone(), value),
			MValType::Temporary => self.write_variable(block, mval.name.clone(), value),
			MValType::Unknown   => {}, // unimplemented!(),
			MValType::Internal  => {}, // unimplemented!()
			MValType::Null      => {},
		}
	}

	fn process_op(&mut self, block: T::ActionRef, opc: MOpcode, n0: T::ValueRef, n1: T::ValueRef) -> T::ValueRef {
		if opc == MOpcode::OpEq {
			return n0
		}
		// TODO: give correct integer type here
		let nn = self.ssa.add_op(block, opc, ValueType::Integer{width: 64});
		self.ssa.op_use(nn, 0, n0);
		self.ssa.op_use(nn, 1, n1);
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
				self.ssa.mark_selector(block, n0);
				continue;
			}

			let nn = self.process_op(block, instruction.opcode, n0, n1);
			self.process_out(block, &instruction.dst, nn);
		}
	}

	fn read_variable_recursive(&mut self, variable: VarId, block: T::ActionRef) -> T::ValueRef {
		let mut val;

		if !self.sealed_blocks.contains(&block) {
			// Incomplete CFG
			// TODO: bring back comments
			// val = self.ssa.add_phi_comment(block, &variable);
			val = self.ssa.add_phi(block);
			let oldval = self.incomplete_phis.get_mut(&block).unwrap().insert(variable.clone(), val);
			assert!(oldval.is_none());
		} else {
			let pred = self.ssa.preds_of(block);
			if pred.len() == 1 {
				// Optimize the common case of one predecessor: No phi needed
				val = self.read_variable(pred[0], variable.clone())
			} else {
				// Break potential cycles with operandless phi
				// TODO: bring back comments
				// val = self.ssa.add_phi_comment(block, &variable);
				val = self.ssa.add_phi(block);
				// TODO: only mark (see paper)
				self.write_variable(block, variable.clone(), val);
				val = self.add_phi_operands(block, variable.clone(), val)
			}
		}
		self.write_variable(block, variable, val);
		return val
	}

	fn add_phi_operands(&mut self, block: T::ActionRef, variable: VarId, phi: T::ValueRef) -> T::ValueRef {
		assert!(block == self.ssa.block_of(&phi));
		// Determine operands from predecessors
		for pred in self.ssa.preds_of(block) {
			let datasource = self.read_variable(pred, variable.clone());
			self.ssa.phi_use(phi, datasource)
		}
		return self.try_remove_trivial_phi(phi)
	}

	fn try_remove_trivial_phi(&mut self, phi: T::ValueRef) -> T::ValueRef {
		let undef = self.ssa.invalid_value();
		let mut same: T::ValueRef = undef; // The phi is unreachable or in the start block
		for op in self.ssa.args_of(phi) {
			if op == same || op == phi {
				continue // Unique value or self−reference
			}
			if same != undef {
				return phi // The phi merges at least two values: not trivial
			}
			same = op
		}

		if same == undef {
			let block = self.ssa.block_of(&phi);
			same = self.ssa.add_undefined(block);
		}

		let users = self.ssa.uses_of(phi);
		self.ssa.replace(phi, same); // Reroute all uses of phi to same and remove phi

		// Try to recursively remove all phi users, which might have become trivial
		for use_ in users {
			if use_ == phi { continue; }
			if let NodeData::Phi(_) = self.ssa.get_node_data(&use_) {
				//println!("After replacing {:?} by {:?}, proceeding to simplify user {:?}",
				//	self.ssa.g[phi],
				//	self.ssa.g[same],
				//	self.ssa.g[use_]);
				self.try_remove_trivial_phi(use_);
				//println!("done");
			}
		}
		return same
	}

	/*fn remove_redundant_phis(&self, phi_functions: Vec<T::ValueRef>) {
		// should phi_functions be `Vec` or something else?
		let sccs = compute_phi_sccs(induced_subgraph(phi_functions));
		for scc in topological_sort(sccs) {
			processSCC(scc)
		}
	}*/

	/*fn processSCC(&self, scc) {
		if len(scc) = 1 { return } // we already handled trivial φ functions

		let inner = HashSet::new();
		let outerOps = HashSet::new();

		for phi in scc:
			isInner = True
			for operand in phi.getOperands() {
				if operand not in scc {
					outerOps.add(operand)
					isInner = False
				}
			}
			if isInner {
				inner.add(phi)
			}

		if len(outerOps) == 1 {
			replaceSCCByValue(scc, outerOps.pop())
		} else if len(outerOps) > 1 {
			remove_redundant_phis(inner)
		}
	}*/
}
