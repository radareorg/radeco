//! Implements the SSA construction algorithm described in
//! "Simple and Efficient Construction of Static Single Assignment Form"

use petgraph::graph::NodeIndex;
use frontend::structs::LRegInfo;
use middle::cfg::NodeData as CFGNodeData;
use middle::cfg::{CFG, BasicBlock};
use middle::ssa::EdgeData as SSAEdgeData;
use middle::ssa::{SSA, NodeData};
use middle::ir::{MVal, MOpcode, MValType};
use std::collections::{HashSet, HashMap};

pub type VarId = String; // consider transitioning to &str
pub type Block = NodeIndex;
pub type Node = NodeIndex;

pub struct SSAConstruction<'a> {
	cfg:              &'a CFG,
	ssa:              &'a mut SSA,
	variables:        Vec<VarId>, // assume consequtive integers?
	sealed_blocks:    HashSet<Block>, // replace with bitfield
	current_def:      HashMap<VarId, HashMap<Block, Node>>,
	incomplete_phis:  HashMap<Block, HashMap<VarId, Node>>,
	global_variables: HashMap<VarId, Node>
}

impl<'a> SSAConstruction<'a> {
	pub fn new(ssa: &'a mut SSA, cfg: &'a CFG, reg_info: &LRegInfo) -> SSAConstruction<'a> {
		let mut s = SSAConstruction {
			cfg:              cfg,
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

	pub fn write_variable(&mut self, block: Block, variable: VarId, value: Node) {
		if let Option::Some(vd) = self.current_def.get_mut(&variable) {
			vd.insert(block, value);
		} else {
			self.global_variables.insert(variable, value);
		}
	}

	pub fn read_variable(&mut self, block: Block, variable: VarId) -> Node {
		match {
			if let Option::Some(vd) = self.current_def.get(&variable) {
				vd.get(&block)
			} else {
				self.global_variables.get(&variable)
			}
		}.map(|r|*r) {
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
		let mut blocks = Vec::<Block>::new();
		for i in 0..self.cfg.g.node_count() {
		 	let block = self.ssa.add_block();
			self.incomplete_phis.insert(block, HashMap::new());
			blocks.push(block);

			match self.cfg.g[NodeIndex::new(i)] {
		 		CFGNodeData::Block(ref srcbb) => {
		 			self.process_block(block, srcbb);
				},
				CFGNodeData::Entry => {
					for (ref name, ref mut vd) in &mut self.current_def {
						let mut msg = "initial_".to_string();
						msg.push_str(name);
						vd.insert(block, self.ssa.add_comment(block, &msg));
					}
				},
				_ => {}
			}
		}
		for edge in self.cfg.g.raw_edges() {
			// TODO more than Control(0)
			self.ssa.g.add_edge(
				blocks[edge.source().index()],
				blocks[edge.target().index()],
				SSAEdgeData::Control(0));
		}
		for block in blocks {
			self.seal_block(block);
		}
	}

	fn process_in(&mut self, block: Block, mval: &MVal) -> Node {
		match mval.val_type {
			MValType::Memory    => unimplemented!(),
			MValType::Register  => self.read_variable(block, mval.name.clone()),
			MValType::Constant  => self.ssa.add_const(block, mval.value as u64),
			MValType::Temporary => self.read_variable(block, mval.name.clone()),
			MValType::Unknown   => unimplemented!(),
			MValType::Null      => NodeIndex::end(),
			MValType::Internal  => self.ssa.add_comment(block, &mval.name), // unimplemented!()
		}
	}

	fn process_out(&mut self, block: Block, mval: &MVal, value: Node) {
		match mval.val_type {
			MValType::Memory    => unimplemented!(),
			MValType::Register  => self.write_variable(block, mval.name.clone(), value),
			MValType::Constant  => panic!(),
			MValType::Temporary => self.write_variable(block, mval.name.clone(), value),
			MValType::Unknown   => {}, // unimplemented!(),
			MValType::Null      => {},
			MValType::Internal  => {}, // unimplemented!()
		}
	}

	fn process_op(&mut self, block: Block, opc: MOpcode, n0: Node, n1: Node) -> Node {
		if opc == MOpcode::OpEq {
			return n0
		}
		let nn = self.ssa.add_op(block, opc);
		self.ssa.op_use(nn, 0, n0);
		self.ssa.op_use(nn, 1, n1);
		return nn
	}

	fn process_block(&mut self, block: Block, source: &BasicBlock) {
		for ref instruction in &source.instructions {
			// instruction.addr
			// instruction.opcode

			let n0 = self.process_in(block, &instruction.operand_1);
			let n1 = self.process_in(block, &instruction.operand_2);
			let nn = self.process_op(block, instruction.opcode, n0, n1);
			self.process_out(block, &instruction.dst, nn);

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
			val = self.ssa.add_phi_comment(block, &variable);
			let oldval = self.incomplete_phis.get_mut(&block).unwrap().insert(variable.clone(), val);
			assert!(oldval.is_none());
		} else {
			let pred = self.ssa.preds_of(block);
			if pred.len() == 1 {
				// Optimize the common case of one predecessor: No phi needed
				val = self.read_variable(pred[0], variable.clone())
			} else {
				// Break potential cycles with operandless phi
				val = self.ssa.add_phi_comment(block, &variable);
				// TODO: only mark (see paper)
				self.write_variable(block, variable.clone(), val);
				val = self.add_phi_operands(variable.clone(), val)
			}
		}
		self.write_variable(block, variable, val);
		return val
	}

	fn add_phi_operands(&mut self, variable: VarId, phi: Node) -> Node {
		// Determine operands from predecessors
		for pred in self.ssa.preds_of(self.ssa.block_of(phi)) {
			let datasource = self.read_variable(pred, variable.clone());
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

		if same == undef {
			// The original algorithm doesn't check for this conditions,
			// so that fact that I have to probably means I got something
			// wrong that leads to this situation
			return phi
		}

		let users = self.ssa.uses_of(phi);
		self.ssa.replace(phi, same); // Reroute all uses of phi to same and remove phi
		//println!("Replacing! {:?} users", users.len());

		// Try to recursively remove all phi users, which might have become trivial
		for use_ in users {
			if use_ == phi { continue; }
			if let NodeData::Phi(_) = self.ssa.g[use_] {
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
