// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Module that implements trait to abstract set of integers.
//!
//! As for Gogul Balakrishnan's Ph.D. Thesis, the abstract set of integers is 
//! strided interval.
//! Please refer here:
//!     * https://research.cs.wisc.edu/wpis/papers/balakrishnan_thesis.pdf
//!
//! As for "BDD-based Value Analysis for X86 Executables", the abstract set of
//! integers is BDD.
//! Please refer here:
//!     * https://tubdok.tub.tuhh.de/bitstream/11420/1510/1/dis.pdf
//!


use std::ops::{Neg, Add, Sub, Div, Rem, Mul};
use std::ops::{BitAnd, BitOr, BitXor, Not, Shl, Shr};

// XXX: Try to support 128-bit registers in the future
// type inum = i128; <-- this one do influence performance
// const full_bits: u8 = 128;
//
// All numbers (except k) in AbstractSet will be stored as inum 
pub type inum = i64;
// All members (except k) in AbstractSet will be hold in _bits bits
pub const _bits: u8 = 64;

pub trait AbstractSet: Neg + Add + Sub + Div + Rem + Mul +
                        BitAnd + BitOr + BitXor + Not + Shl + Shr +
                        From<inum> + Default
{
    // Returns true if the AbstractSet `self` contains `other`, false otherwise.
    fn contains(self, other: Self) -> bool {
        unimplemented!();
    }

    // Returns the meet (intersection) of AbstractSet `self` and `other`.
    fn meets(self, other: Self) -> Self {
        unimplemented!();
    }

    // Returns the join (union) of AbstractSet `self` and `other`.
    fn joins(self, other: Self) -> Self {
        unimplemented!();
    }

    // Returns the AbstractSet obtained by widening `self` with respect to `other`
    fn widens(self, other: Self) -> Self {
        unimplemented!();
    }

    // Returns the AbstractSet obtained by remoing lower bound for `self`
    fn removes_lower_bound(self) -> Self {
        unimplemented!()
    }

    // Returns the AbstractSet obtained by remoing upper bound for `self`
    fn removes_upper_bound(self) -> Self {
        unimplemented!()
    }
}