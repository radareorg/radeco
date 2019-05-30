// Copyright (c) 2018, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Module that implements trait to abstract set of integers.
//! This Trait allows us to use other abstract domains for integer set, not
//! only strided interval.
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

use std::fmt::Debug;
use std::hash::Hash;
use std::ops::{Add, Div, Mul, Neg, Rem, Sub};
use std::ops::{BitAnd, BitOr, BitXor, Not, Shl, Shr};

// XXX: Try to support 128-bit registers in the future
// type Inum = i128; <-- this one do influence performance
// const full_bits: u8 = 128;

/// All numbers (except k) in AbstractSet will be stored as `Inum` type
pub type Inum = i64;
/// Unsigned type according to Inum
pub type Unum = u64;
/// All members (except k) in AbstractSet will be hold in `_BITS` bits
pub const _BITS: u8 = 64;

/// Trait used to indicate this is a container which contains something
pub trait Container<T: Clone> {
    // Returns true if the Container `self` contains `object`, false otherwise.
    fn contains(&self, _object: &T) -> bool {
        unimplemented!();
    }
}

/// Type of abstract set.
/// According to demand, users could choose which kind of abstract set they wanna
/// use in their VSA.
pub enum AbstractSetType {
    StridedInterval,
    BDD,
    Polynomial,
}

pub trait AbstractSet:
    Copy
    + Clone
    + Debug
    + PartialEq
    + Eq
    + Hash
    + Neg
    + Add
    + Sub
    + Div
    + Rem
    + Mul
    + BitAnd
    + BitOr
    + BitXor
    + Not
    + Shl
    + Shr
    + Container<Inum>
    + Container<Self>
    + Container<Vec<Inum>>
    + From<Inum>
    + From<(u8, Inum)>
    + Default
{
    /// Returns the meet (intersection) of AbstractSet `self` and `other`.
    fn meet(&self, _other: &Self) -> Self {
        unimplemented!();
    }

    /// Returns the join (union) of AbstractSet `self` and `other`.
    fn join(&self, _other: &Self) -> Self {
        unimplemented!();
    }

    /// Returns the AbstractSet obtained by widening `self` with respect to `other`
    fn widen(&self, _other: &Self) -> Self {
        unimplemented!();
    }

    /// Returns the AbstractSet obtained by remoing lower bound for `self`
    fn remove_lower_bound(&self) -> Self {
        unimplemented!()
    }

    /// Returns the AbstractSet obtained by setting lower bound as x for `self`
    fn set_lower_bound(&self, _x: Inum) -> Self {
        unimplemented!()
    }

    /// Returns the AbstractSet obtained by remoing upper bound for `self`
    fn remove_upper_bound(&self) -> Self {
        unimplemented!()
    }

    /// Returns the AbstractSet obtained by setting upper bound as x for `self`
    fn set_upper_bound(&self, _x: Inum) -> Self {
        unimplemented!()
    }

    /// Returns the AbstractSet narrowed into a new k-bit-filed, for OpNarrow.
    fn narrow(&self, _k: u8) -> Self {
        unimplemented!();
    }

    /// Returns the AbstractSet sign extended into a new k-bit-filed, for OpSignExt.
    fn sign_extend(&self, _k: u8) -> Self {
        unimplemented!();
    }

    /// Returns the AbstractSet zero extended into a new k-bit-filed, for OpZeroExt.
    fn zero_extend(&self, _k: u8) -> Self {
        unimplemented!();
    }

    /// Returns Some(cons) if the AbstractSet only contains one constant, None otherwise.
    fn constant(&self) -> Option<Inum> {
        unimplemented!()
    }

    /// Returns capacity of AbstractSet.
    fn capacity(&self) -> Inum {
        unimplemented!()
    }

    /// Returns whether the AbstractSet is empty.
    fn is_empty(&self) -> bool {
        unimplemented!()
    }
}
