// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! This module offers traits for valuesets and implementations for valuesets
//! of 64 bit integers
//! using different compact representations, including methods for intersection
//! and union.
//! (Currently unused)

use std::fmt::Debug;
use std::ops::{BitAnd, BitOr};

#[cfg(test)]
mod test;

mod math;

mod knownbits;
mod uintrange;
mod sintrange;
mod uintmultiple;
mod sintmultiple;
mod strided_interval;
pub mod mem_structs;
pub mod analyzer_wysinwyx;

/// Value set of u64 integers with certain bits set/cleared.
#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
pub struct KnownBits {
    /// Bits that are cleared
    pub zerobits: u64,
    /// Bits that are set
    pub onebits: u64,
}

/// Value set of unsigned values that satisfy value % modulus = residue.
#[derive(Clone, Copy, Debug)]
pub struct UIntMultiple {
    pub modulus: u64,
    pub residue: u64,
}

/// Value set of unsigned values between min and max.
#[derive(Clone, Copy, Debug)]
pub struct UIntRange {
    pub min: u64,
    pub max: u64,
}

/// Value set of signed values between min and max.
#[derive(Clone, Copy, Debug)]
pub struct SIntRange {
    pub min: i64,
    pub max: i64,
}

/// Strided Interval (on signd ints) s[l, u] = l <= i <= u, i mod s + offset
/// In essence the 4-tuple used in
/// "Analyzing Memory Accesses in x86 Executables"
/// by Gogul Balakrishnan and Thomas Reps
// TODO write test cases, check logic
// make enum to represent unknown values
#[derive(Clone, Copy, Debug)]
pub struct StridedInterval {
    ui_mult : UIntMultiple,
    si_range: SIntRange,
}

/// A Strided Interval without undefined value.
#[derive(Clone, Copy, Debug)]
pub struct StridedInterval_n {
    stride: u64,
    min:    i64,
    max:    i64,
}

/// A Strided Interval with undefined value.
#[derive(Clone, Copy, Debug)]
pub enum StridedInterval_u {
    Undefined,
    StridedInterval(StridedInterval_n),
    //StridedInterval {
    //    strided_interval: StridedInterval_n,
    //}
}

trait AbstractValue {
    fn from_const(n: i64) -> Self;
    //fn new(stride: u64, min: i64, max: i64) -> Self;
    fn new() -> Self;
    fn is_subset_of(self, other: Self) -> bool;
    fn meet(self, other: Self) -> Self;
    fn join(self, other: Self) -> Self;
    //fn widen(self) -> Self;
}

const EMPTY_UINTMULTIPLE: UIntMultiple = UIntMultiple {
    modulus: 0,
    residue: 0,
};
const EMPTY_UINTRANGE: UIntRange = UIntRange {
    min: 0xffffffffffffffff,
    max: 0x0000000000000000,
};
const EMPTY_SINTRANGE: SIntRange = SIntRange {
    min: 0x7fffffffffffffff,
    max: -0x8000000000000000,
};

/// A value set. Some implementations introduce false positives when intersected or unified with each other.
pub trait ValueSet<T>: Debug /*+ BitAnd<Self> + BitOr<Self>*/ {
    fn contains(&self, value: T) -> bool;
}

/// A value set over a totally ordered domain with methods to query the next greater or smaller value
pub trait ScannableSet<T: Ord>: ValueSet<T> {
    /// Return Some(x) where x ≥ value and x ∈ self
    fn scan_up(&self, value: T) -> Option<T>;
    /// Return Some(x) where x ≤ value and x ∈ self
    fn scan_dn(&self, value: T) -> Option<T>;
}

// implements binary operators "&T op U", "T op &U", "T op U"
macro_rules! forward_ref_binop {
    (impl $imp:ident, $method:ident for $t:ty, $u:ty) => {
        impl<'a> $imp<$u> for &'a $t {
            type Output = <&'static $t as $imp<&'static $u>>::Output;

            #[inline]
            fn $method(self, other: $u) -> <$t as $imp<$u>>::Output {
                $imp::$method(self, &other)
            }
        }

        impl<'a> $imp<&'a $u> for $t {
            type Output = <&'static $t as $imp<&'static $u>>::Output;

            #[inline]
            fn $method(self, other: &'a $u) -> <$t as $imp<$u>>::Output {
                $imp::$method(&self, other)
            }
        }

        impl $imp for $t {
            type Output = <&'static $t as $imp<&'static $u>>::Output;

            #[inline]
            fn $method(self, other: $u) -> <$t as $imp<$u>>::Output {
                $imp::$method(&self, &other)
            }
        }
    }
}

macro_rules! bitand_impl {
    ($($t:ty)*) => ($(
        forward_ref_binop! { impl BitAnd, bitand for $t, $t }
    )*)
}

bitand_impl! { KnownBits UIntMultiple UIntRange SIntRange StridedInterval}

macro_rules! bitor_impl {
    ($($t:ty)*) => ($(
        forward_ref_binop! { impl BitOr, bitor for $t, $t }
    )*)
}

bitor_impl! { KnownBits UIntMultiple UIntRange SIntRange StridedInterval}
