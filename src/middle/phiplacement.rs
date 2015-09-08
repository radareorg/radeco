// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Implements the SSA construction algorithm described in
//! "Simple and Efficient Construction of Static Single Assignment Form"

use std::collections::{HashSet, HashMap};
use super::ssa::{BBInfo, SSA, SSAMod, ValueType};
use super::ssa::ssa_traits::{NodeType, NodeData};

pub type VarId = usize;

pub struct PhiPlacer<'a, T: SSAMod<BBInfo=BBInfo> + 'a> {
	pub ssa:            &'a mut T,
	pub variable_types: Vec<ValueType>,
	sealed_blocks:      HashSet<T::ActionRef>,
	current_def:        Vec<HashMap<T::ActionRef, T::ValueRef>>,
	incomplete_phis:    HashMap<T::ActionRef, HashMap<VarId, T::ValueRef>>,
}

impl<'a, T: SSAMod<BBInfo=BBInfo> + 'a> PhiPlacer<'a, T> {
	pub fn new(ssa: &'a mut T) -> PhiPlacer<'a, T> {
		PhiPlacer {
			ssa:              ssa,
			variable_types:   Vec::new(),
			sealed_blocks:    HashSet::new(),
			current_def:      Vec::new(),
			incomplete_phis:  HashMap::new(),
		}
	}

	pub fn add_variables(&mut self, variable_types: Vec<ValueType>) {
		for _ in &variable_types {
			self.current_def.push(HashMap::new());
		}
		self.variable_types.extend(variable_types);
	}

	pub fn write_variable(&mut self, block: T::ActionRef, variable: VarId, value: T::ValueRef) {
		assert!(!self.sealed_blocks.contains(&block));
		self.write_variable_internal(block, variable, value)
	}

	fn write_variable_internal(&mut self, block: T::ActionRef, variable: VarId, value: T::ValueRef) {
		assert_eq!(self.ssa.get_node_data(&value).unwrap().vt, self.variable_types[variable]);
		self.current_def[variable].insert(block, value);
	}

	pub fn read_variable(&mut self, block: T::ActionRef, variable: VarId) -> T::ValueRef {
		let mut n = match {
			self.current_def[variable].get(&block)
		}.map(|r| *r) {
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

	fn read_variable_recursive(&mut self, variable: VarId, block: T::ActionRef) -> T::ValueRef {
		let valtype = self.variable_types[variable];
		let mut val;

		if !self.sealed_blocks.contains(&block) {
			// Incomplete CFG
			// TODO: bring back comments
			// val = self.ssa.add_phi_comment(block, &variable);
			val = self.ssa.add_phi(block, valtype);
			let oldval = self.incomplete_phis.get_mut(&block).unwrap().insert(variable.clone(), val);
			assert!(oldval.is_none());
		} else {
			let pred = self.ssa.preds_of(block);
			if pred.len() == 1 {
				// Optimize the common case of one predecessor: No phi needed
				val = self.read_variable(pred[0], variable.clone())
			} else {
				// Break potential cycles with operandless phi

				val = self.ssa.add_phi(block, valtype);
				//  = self.ssa.add_phi_comment(block, &variable);

				// dkreuter:
				// The paper suggests marking nodes instead of creating phi nodes.
				// However this turned out to be almost as expensive
				// and added unneccesary complexity to the code.
				// Also, I can't see any marking implemented in libfirms implementation
				// of this algorithm.

				self.write_variable_internal(block, variable.clone(), val);
				val = self.add_phi_operands(block, variable.clone(), val)
			}
		}
		self.write_variable_internal(block, variable, val);
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
			let valtype = self.ssa.get_node_data(&phi).unwrap().vt;
			same = self.ssa.add_undefined(block, valtype);
		}

		let users = self.ssa.uses_of(phi);
		self.ssa.replace(phi, same); // Reroute all uses of phi to same and remove phi

		// Try to recursively remove all phi users, which might have become trivial
		for use_ in users {
			if use_ == phi { continue; }
			if let Ok( NodeData {nt: NodeType::Phi, ..} ) = self.ssa.get_node_data(&use_) {
				//	self.ssa.g[phi],
				//	self.ssa.g[same],
				//	self.ssa.g[use_]);
				self.try_remove_trivial_phi(use_);
			}
		}
		return same
	}

	pub fn add_dynamic(&mut self) -> T::ActionRef {
		let action = self.ssa.add_dynamic();
		self.incomplete_phis.insert(action, HashMap::new());
		self.sync_register_state(action);
		action
	}

	pub fn sync_register_state(&mut self, block: T::ActionRef) {
		let rs = self.ssa.registers_at(&block);
		for var in 0..self.variable_types.len() {
			let val = self.read_variable(block, var);
			self.ssa.op_use(rs, var as u8, val);
		}
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
