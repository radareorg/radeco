// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

use std::{error, fmt};
use super::ssa_traits::SSA;
use super::ssastorage::SSAStorage;
use std::fmt::Debug;

#[derive(Debug)]
pub enum SSAErr<T: SSA + Debug> {
    InvalidBlock(T::ActionRef),
    InvalidType(String),
    InvalidTarget(T::ActionRef, T::CFEdgeRef, T::ActionRef),
    InvalidControl(T::ActionRef, T::CFEdgeRef),
    WrongNumOperands(T::ValueRef, usize, usize),
    WrongNumEdges(T::ActionRef, usize, usize),
    NoSelector(T::ActionRef),
    UnexpectedSelector(T::ActionRef, T::ValueRef),
    UnreachableBlock(T::ActionRef),
    InvalidExpr(T::ValueRef),
    IncompatibleWidth(T::ValueRef, u16, u16),
}

impl fmt::Display for SSAErr<SSAStorage> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let err = match *self {
            SSAErr::InvalidBlock(ni) => {
                format!("Found Block with index: {:?}.", ni)
            }
            SSAErr::InvalidType(ref e) => {
                format!("Expected type: {}.", e)
            }
            SSAErr::InvalidControl(bi, ei) => {
                format!("Block {:?} has invalid outgoing edge: {:?}", bi, ei)
            }
            SSAErr::WrongNumOperands(n, e, f) => {
                format!("{:?} expected {} number of operands, found: {}", n, e, f)
            }
            SSAErr::InvalidTarget(bi, ei, ti) => {
                format!("Block {:?} has Edge {:?} with invalid target {:?}",
                        bi,
                        ei,
                        ti)
            }
            SSAErr::WrongNumEdges(ni, e, f) => {
                format!("Block {:?} expects {} edge(s), found: {}", ni, e, f)
            }
            SSAErr::NoSelector(bi) => {
                format!("Block {:?} expects a selector. None found.", bi)
            }
            SSAErr::UnexpectedSelector(bi, ni) => {
                format!("Block {:?} expected no selector, found: {:?}", bi, ni)
            }
            SSAErr::UnreachableBlock(bi) => {
                format!("Unreachable block found: {:?}", bi)
            }
            SSAErr::InvalidExpr(ni) => {
                format!("Found an invalid expression: {:?}", ni)
            }
            SSAErr::IncompatibleWidth(ni, e, f) => {
                format!("{:?} expected with to be {}, found width: {}", ni, e, f)
            }
        };
        write!(f, "{}.", err)
    }
}

impl error::Error for SSAErr<SSAStorage> {
    fn description(&self) -> &str {
        ""
    }

    fn cause(&self) -> Option<&error::Error> {
        None
    }
}
