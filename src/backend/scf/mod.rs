// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

/// Data structures and algorithms for control flow restructuring.
// SCF = Structured control flow

use std::collections::BTreeMap;

pub type AST_ = Box<AST>;

pub type RUnit = u32;
pub type RCond = u32;
pub type RExpr = u32;

pub enum LoopType {
	While,
	DoWhile,
	ForEver,
}

pub enum AST {
	Unit(RUnit),

	Seq(Vec<AST_>),
	Cond(RCond, AST_, AST_),
	Loop(LoopType, RCond, AST_),
	Switch(RExpr, BTreeMap<u64, AST_>, AST_),
}
