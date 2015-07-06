//! Implements the SSA construction algorithm described in
//! "Simple and Efficient Construction of Static Single Assignment Form"

use petgraph::graph::{DefIndex, NodeIndex};
use frontend::structs::LRegInfo;
use middle::cfg::{CFG, BasicBlock};
use middle::ssa::{SSA, NodeData};
use middle::ir::{MVal, MOpcode, MValType, MRegInfo};
use std::collections::{HashSet, HashMap};

pub type VarId = String; // consider transitioning to &str
pub type Block = NodeIndex;
pub type Node = NodeIndex;

pub struct SSAConstruction<'a> {
	cfg:             &'a CFG,
	ssa:             &'a mut SSA,
	variables:       Vec<VarId>, // assume consequtive integers?
	sealed_blocks:   HashSet<Block>, // replace with bitfield
	current_def:     HashMap<VarId, HashMap<Block, Node>>,
	incomplete_phis: HashMap<Block, HashMap<VarId, Node>>
}

impl<'a> SSAConstruction<'a> {
	pub fn new(ssa: &'a mut SSA, cfg: &'a CFG, reg_info: &LRegInfo) -> SSAConstruction<'a> {
		let mut s = SSAConstruction {
			cfg:             cfg,
			ssa:             ssa,
			variables:       Vec::new(),
			sealed_blocks:   HashSet::new(),
			current_def:     HashMap::new(),
			incomplete_phis: HashMap::new()
		};
		for reg in &reg_info.reg_info {
			s.variables.push(reg.name.clone());
		}
		for var in &s.variables {
			s.current_def.insert(var.clone(), HashMap::new());
		}
		for bbi in 0..cfg.g.node_count() {
			s.incomplete_phis.insert(NodeIndex::new(bbi), HashMap::new());
		}
		return s
	}

	pub fn write_variable(&mut self, variable: VarId, block: Block, value: Node) {
		self.current_def.get_mut(&variable).unwrap().insert(block, value);
	}

	pub fn read_variable(&mut self, variable: VarId, block: Block) -> Node {
		match self.current_def[&variable].get(&block).map(|r|*r) {
			Option::Some(r) => r,
			Option::None => self.read_variable_recursive(variable, block)
		}
	}

	pub fn seal_block(&mut self, block: Block) {
		let inc = self.incomplete_phis[&block].clone(); // TODO: remove clone

		for (variable, node) in inc {
			self.add_phi_operands(variable.clone(), node.clone());
		}
		self.sealed_blocks.insert(block);
	}

	pub fn run(&mut self) {
		unimplemented!();
		// for i in 0..self.cfg.g.node_count() {
		// 	
		// }
	}

	fn process_in(&mut self, block: Block, mval: &MVal) -> Node {
		match mval.val_type {
			MValType::Memory    => unimplemented!(),
			MValType::Register  => self.read_variable(mval.name.clone(), block),
			MValType::Constant  => unimplemented!(),
			MValType::Temporary => unimplemented!(),
			MValType::Unknown   => unimplemented!(),
			MValType::Null      => NodeIndex::end(),
			MValType::Internal  => unimplemented!()
		}
	}

	fn process_out(&mut self, block: Block, mval: &MVal, value: Node) {
		match mval.val_type {
			MValType::Memory    => unimplemented!(),
			MValType::Register  => self.write_variable(mval.name.clone(), block, value),
			MValType::Constant  => panic!(),
			MValType::Temporary => unimplemented!(),
			MValType::Unknown   => unimplemented!(),
			MValType::Null      => {},
			MValType::Internal  => unimplemented!()
		}
	}

	fn process_op(&mut self, block: Block, opc: MOpcode, n1: Node, n2: Node) -> Node {
		let n = self.ssa.add_op(block, opc);
		self.ssa.op_use(n, 0, n1);
		self.ssa.op_use(n, 1, n2);
		return n
	}

	fn process_block(&mut self, block: Block, source: BasicBlock) {
		for ref instruction in &source.instructions {
			// instruction.addr
			// instruction.opcode

			let n1 = self.process_in(block, &instruction.operand_1);
			let n2 = self.process_in(block, &instruction.operand_2);
			let n0 = self.process_op(block, instruction.opcode, n1, n2);
			self.process_out(block, &instruction.dst, n0);

			/*pub struct MVal {
				pub name:     String,
				pub size:     u8,
				pub val_type: MValType,
				pub value:    i64,
				pub reg_info: Option<MRegInfo>,
				pub typeset:  u32,
			}*/
		}
	}

	fn read_variable_recursive(&mut self, variable: VarId, block: Block) -> Node {
		let mut val;

		if !self.sealed_blocks.contains(&block) {
			// Incomplete CFG
			val = self.ssa.add_phi(block);
			let oldval = self.incomplete_phis.get_mut(&block).unwrap().insert(variable.clone(), val);
			assert!(oldval.is_none());
		} else {
			let pred = self.ssa.preds_of(block);
			if pred.len() == 1 {
				// Optimize the common case of one predecessor: No phi needed
				val = self.read_variable(variable.clone(), pred[0])
			} else {
				// Break potential cycles with operandless phi
				val = self.ssa.add_phi(block);
				// TODO: only mark (see paper)
				self.write_variable(variable.clone(), block, val);
				val = self.add_phi_operands(variable.clone(), val)
			}
		}
		self.write_variable(variable, block, val);
		return val
	}

	fn add_phi_operands(&mut self, variable: VarId, phi: Node) -> Node {
		// Determine operands from predecessors
		for pred in self.ssa.preds_of(self.ssa.block_of(phi)) {
			let datasource = self.read_variable(variable.clone(), pred);
			self.ssa.phi_use(phi, datasource)
		}
		return self.try_remove_trivial_phi(phi)
	}

	fn try_remove_trivial_phi(&mut self, phi: Node) -> Node {
		let undef = NodeIndex::end();
		let mut same: Node = undef; // The phi is unreachable or in the start block
		for op in self.ssa.args_of(phi) {
			if op == same || op == phi {
				continue // Unique value or self−reference
			}
			if same != undef {
				return phi // The phi merges at least two values: not trivial
			}
			same = op
		}

		let users = self.ssa.uses_of(phi);
		self.ssa.replace(phi, same); // Reroute all uses of phi to same and remove phi

		// Try to recursively remove all phi users, which might have become trivial
		for use_ in users {
			if use_ == phi { continue; }
			if let NodeData::Phi = self.ssa.g[use_] {
				self.try_remove_trivial_phi(use_);
			}
		}
		return same
	}

	/*fn remove_redundant_phis(&self, phi_functions: Vec<Node>) {
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
