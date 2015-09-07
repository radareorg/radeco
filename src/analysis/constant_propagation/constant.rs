// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Module that implements constant propagation on the SSA IR.
//!
//! This module implements 'Sparse Conditional Constant' algorithm to
//! perform the propagation. For more details, please refer:
//!    * https://www.cs.utexas.edu/~lin/cs380c/wegman.pdf.
//!

use std::collections::{HashMap};
use ::middle::ssa::{SSA, SSAMod};
use ::middle::ssa::ssa_traits::NodeType;
use ::middle::ir::{MOpcode, MArity};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ExprVal {
	Top,
	Bottom,
	Const(u64),
}

fn meet(v1: &ExprVal, v2: &ExprVal) -> ExprVal {
	// Any ^ Top    = Any
	// Any ^ Bottom = Bottom
	//   C ^ C      = C      (C = Constant)
	//   C ^ D      = Bottom (C, D = Constant and C != D).

	match *v1 {
		ExprVal::Top    => return *v2,
		ExprVal::Bottom => return ExprVal::Bottom,
		_               => { },
	}

	match *v2 {
		ExprVal::Top    => return *v1,
		ExprVal::Bottom => return ExprVal::Bottom,
		_               => { },
	}

	if *v1 != *v2 {
		return ExprVal::Bottom;
	}

	return *v1;
}

pub struct Analyzer<T: SSAMod + SSA + Clone> {
	ssa_worklist: Vec<T::ValueRef>,
	cfg_worklist: Vec<T::CFEdgeRef>,
	executable: HashMap<T::CFEdgeRef, bool>,
	expr_val: HashMap<T::ValueRef, ExprVal>,
	g: T,
}

impl<T: SSA + SSAMod + Clone> Analyzer<T> {
	pub fn new(g: &mut T) -> Analyzer<T> {
		Analyzer {
			ssa_worklist: Vec::new(),
			cfg_worklist: Vec::new(),
			executable: HashMap::new(),
			expr_val: HashMap::new(),
			g: g.clone(),
		}
	}

	pub fn dump(&self) {
		println!("{:?}", self.expr_val);
	}

	fn visit_phi(&mut self, i: &T::ValueRef) -> ExprVal {
		let operands = self.g.get_operands(i);
		let mut phi_val = self.get_value(i);
		let cur_block = self.g.get_block(i);
		for op in operands.iter() {
			let b = self.g.get_block(&op);
			let edge = self.g.find_edge(&b, &cur_block);
			if !self.is_executable(&edge) { continue; }
			let val = self.get_value(&op);
			phi_val = meet(&phi_val, &val);
		}
		return phi_val;
	}

	fn evaluate_control_flow(&mut self, i: &T::ValueRef) {
		assert!(self.g.is_selector(i));

		let cond_val = self.get_value(i);
		let block = self.g.selects_for(i);
		let true_branch = self.g.true_edge_of(&block);
		let false_branch = self.g.false_edge_of(&block);
		match cond_val {
			ExprVal::Bottom => {
				self.cfg_worklist.push(true_branch);
				self.cfg_worklist.push(false_branch);
			},
			ExprVal::Top => {
				// TODO: Not really sure what to do here.
			},
			ExprVal::Const(cval) => {
				if cval == 0 {
					self.cfg_worklist.push(true_branch);
				} else {
					self.cfg_worklist.push(false_branch);
				}
			},
		}
	}

	fn evaluate_unary_op(&mut self, i: &T::ValueRef, opcode: MOpcode) -> ExprVal {
		let operand = self.g.get_operands(i);
		let operand = if operand.len() > 0 {
			operand[0]
		} else {
			// TODO: panic! or return correct error.
			//panic!("Unary operation has less than one operand!\n");
			return ExprVal::Top;
		};

		let val = self.get_value(&operand);
		let const_val = if let ExprVal::Const(cval)  = val { cval } else { return val; };
		let _val = match opcode {
			MOpcode::OpWiden(_) => { 
				// Nothing to do in case of widen as the value cannot change.
				const_val
			},
			MOpcode::OpNarrow(size) => {
				//Max size is 64. Therefore, we can _never_ narrow to 64.
				assert!(size < 64);
				let mask = (2 << (size + 1)) - 1;
				const_val & mask
			},
			MOpcode::OpNot  => { 
				!const_val as u64
			},
			_ => unreachable!(),
		};

		ExprVal::Const(_val)
	}

	fn evaluate_binary_op(&mut self, i: &T::ValueRef, opcode: MOpcode) -> ExprVal {
		let operands = self.g.get_operands(i)
		                     .iter()
		                     .map(|x| self.get_value(x))
		                     .collect::<Vec<_>>();

		let lhs = operands[0];
		let rhs = operands[1];

		let lhs_val = if let ExprVal::Const(cval) = lhs { cval } else { return lhs; };
		let rhs_val = if let ExprVal::Const(cval) = rhs { cval } else { return rhs; };

		let _val = match opcode {
			MOpcode::OpAdd        => { lhs_val + rhs_val },
			MOpcode::OpSub        => { 
				if lhs_val > rhs_val {
					lhs_val - rhs_val
				} else {
					rhs_val - lhs_val
				}
			},
			MOpcode::OpMul        => { lhs_val * rhs_val },
			MOpcode::OpDiv        => { lhs_val / rhs_val },
			MOpcode::OpMod        => { lhs_val % rhs_val },
			MOpcode::OpAnd        => { lhs_val & rhs_val },
			MOpcode::OpOr         => { lhs_val | rhs_val },
			MOpcode::OpXor        => { lhs_val ^ rhs_val },
			MOpcode::OpCmp        => { (lhs_val == rhs_val) as u64 },
			MOpcode::OpGt         => { (lhs_val > rhs_val) as u64 },
			MOpcode::OpLt         => { (lhs_val < rhs_val) as u64 },
			MOpcode::OpLteq       => { (lhs_val <= rhs_val) as u64 },
			MOpcode::OpGteq       => { (lhs_val >= rhs_val) as u64 },
			MOpcode::OpLsl        => { lhs_val << rhs_val },
			MOpcode::OpLsr        => { lhs_val >> rhs_val },
			_ => unreachable!(),
		};

		ExprVal::Const(_val)
	}

	fn visit_expression(&mut self, i: &T::ValueRef) -> ExprVal {
		let expr = self.g.get_node_data(i).unwrap(); //"visit_expression() received invalid valueref");
		let opcode = if let NodeType::Op(_opcode) = expr.nt {
			_opcode.clone()
		} else { 
			panic!("Found something other than an expression!");
		};

		if let MOpcode::OpConst(v) = opcode {
			return ExprVal::Const(v as u64);
		}

		let val = match opcode.arity() {
			MArity::Unary => self.evaluate_unary_op(i, opcode),
			MArity::Binary => self.evaluate_binary_op(i, opcode),
			_ => unimplemented!(),
		};

		// If expression is a `Selector` it means that it's value can affect the control flow.
		// Hence evaluate the control flow to add edges to the cfgwl.
		// TODO: Handle the case where the selector of a block may belong to a different block.
		if self.g.is_selector(i) {
			self.evaluate_control_flow(i);
		}

		return val;
	}

	pub fn analyze(&mut self) {
		
		{
			let start_node = self.g.start_node();
			let edges = self.g.edges_of(&start_node);
			for next in edges.iter() {
				self.mark_executable(next);
				self.cfgwl_push(next);
			}
		}

		while self.ssa_worklist.len() > 0 || self.cfg_worklist.len() > 0 {
			while let Some(edge) = self.cfg_worklist.pop() {
				let block = self.g.target_of(&edge);
				self.mark_executable(&edge);
				let phis = self.g.get_phis(&block);
				for phi in phis.iter() {
					let v = self.visit_phi(phi);
					self.set_value(phi, v);
				}

				let next_edge = self.g.next_edge_of(&block);
				if next_edge != self.g.invalid_edge() {
					self.cfgwl_push(&next_edge);
					self.mark_executable(&next_edge);
				}

				for expr in self.g.exprs_in(&block) {
					let val = self.visit_expression(&expr);
					self.set_value(&expr, val);
					for _use in self.g.get_uses(&expr) {
						self.ssawl_push(&_use);
					}
				}
			}
			while let Some(e) = self.ssa_worklist.pop() {
				let t = self.visit_expression(&e);
				if t !=  self.get_value(&e) {
					self.set_value(&e, t);
					for _use in self.g.get_uses(&e).iter() {
						self.ssawl_push(_use);
					}
				}
			}
		}
	}

	pub fn emit_ssa(&mut self) -> T {
		for (k, v) in self.expr_val.iter() {
			if let ExprVal::Const(val) = *v {
				let block = self.g.block_of(k);
				let newnode = self.g.add_const(block, val);
				self.g.replace(*k, newnode);
			}
		}
		let blocks = self.g.blocks();
		let mut remove_edges = Vec::<T::CFEdgeRef>::new();
		let mut remove_blocks = Vec::<T::ActionRef>::new();
		for block in blocks.iter() {
			let edges = self.g.edges_of(block);
			for edge in edges.iter() {
				if !self.is_executable(edge) {
					remove_edges.push(*edge);
				}
			}
			// TODO: Make this automatic in dce.
			if !self.is_block_executable(block) {
				remove_blocks.push(*block);
			}
		}
		for edge in remove_edges.iter() {
			self.g.remove_edge(edge);
		}
		for block in remove_blocks.iter() {
			self.g.remove_block(*block);
		}
		self.g.clone()
	}

	///////////////////////////////////////////////////////////////////////////
	//// Helper functions.
	///////////////////////////////////////////////////////////////////////////

	fn is_executable(&mut self, i: &T::CFEdgeRef) -> bool {
		*(self.executable.entry(*i).or_insert(false))
	}

	fn mark_executable(&mut self, i: &T::CFEdgeRef) {
		let n = self.executable.entry(*i).or_insert(false);
		*n = true;
	}

	// Determines the Initial value
	fn init_val(&self, i: &T::ValueRef) -> ExprVal {
		let node_data = self.g.get_node_data(i).unwrap();
		match node_data.nt {
			NodeType::Op(MOpcode::OpConst(v)) => ExprVal::Const(v),
			NodeType::Undefined               => ExprVal::Bottom,
			_                                 => ExprVal::Top,
		}
	}

	fn get_value(&mut self, i: &T::ValueRef) -> ExprVal {
		if self.expr_val.contains_key(i) {
			return self.expr_val[i];
		}

		let v = self.init_val(i);
		self.expr_val.insert(*i, v);
		v
	}

	fn set_value(&mut self, i: &T::ValueRef, v: ExprVal) {
		let n = self.expr_val.entry(*i).or_insert(ExprVal::Top);
		*n = v;
	}

	fn is_block_executable(&mut self, i: &T::ActionRef) -> bool {
		// start_node is always reachable.
		if *i == self.g.start_node() { return true; }
		let incoming = self.g.incoming_edges(i);
		for edge in incoming.iter() {
			if self.is_executable(edge) { return true; }
		}
		return false;
	}

	fn ssawl_push(&mut self, i: &T::ValueRef) {
		if !self.g.is_expr(i) { return; }
		let owner_block = self.g.get_block(&i);
		if self.is_block_executable(&owner_block) {
			self.ssa_worklist.push(*i);
		}
	}

	fn cfgwl_push(&mut self, i: &T::CFEdgeRef) {
		self.cfg_worklist.push(*i);
	}
}

#[cfg(test)]
mod test {
	use super::{ExprVal, meet};

	#[test]
	fn test_meet() {
		let t = ExprVal::Top;
		let b = ExprVal::Bottom;
		let c1 = ExprVal::Const(1);
		let c2 = ExprVal::Const(2);

		assert_eq!(meet(&t,  &t) , t);
		assert_eq!(meet(&t,  &b) , b);
		assert_eq!(meet(&t,  &c1), c1);
		assert_eq!(meet(&t,  &c2), c2);
		assert_eq!(meet(&c1, &b) , b);
		assert_eq!(meet(&c2, &c1), b);
		assert_eq!(meet(&c1, &c1), c1);
	}
}
