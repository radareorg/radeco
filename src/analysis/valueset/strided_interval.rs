// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

use super::{KnownBits, SIntRange, UIntMultiple, ScannableSet, StridedInterval, UIntRange, ValueSet};
use super::{StridedInterval_n, StridedInterval_u, AbstractValue};
use super::{EMPTY_SINTRANGE, EMPTY_UINTMULTIPLE, EMPTY_UINTRANGE};
use std;
use std::ops::{Add, Sub, Mul, Div};
use std::fmt;
use std::cmp::{max, min};
use std::cmp::PartialEq;
use std::ops::{BitAnd, BitOr};
use super::math::{gcd_lcm, multiplicative_inverse, tzmsk};

const U64MIN: u64 = std::u64::MIN;  // 0x0000000000000000;
const U64MAX: u64 = std::u64::MAX;  // 0xffffffffffffffff;
const S64MIN: i64 = std::i64::MIN;  // -0x8000000000000000;
const S64MAX: i64 = std::i64::MAX;  // 0x7fffffffffffffff;

impl ValueSet<u64> for StridedInterval {
    fn contains(&self, value: u64) -> bool {
        //if self.modulus == 0 {
        //    return false;
        //}
        //value % self.modulus == self.residue
        if !self.ui_mult.contains(value) {
            return false;
        }
        self.si_range.contains(value)
    }
}

impl ScannableSet<u64> for StridedInterval {
    fn scan_up(&self, n: u64) -> Option<u64> {
        //let io = self.modulus - self.residue;
        //if n > U64MAX - io {
        //    return Option::None;
        //}
        //let t = (n + io) % self.modulus;
        //Option::Some(if t == 0 {
        //    n
        //} else {
        //    n + (self.modulus - t)
        //})

        //TODO correct?
        if let Some(su) = self.ui_mult.scan_up(n) {
            if self.si_range.contains(su) {
                return Some(su);
            } else {
                if let Some(su) = self.si_range.scan_up(n) {
                    if self.ui_mult.contains(su) {
                        return Some(su);
                    } else {
                        if let Some(su) = self.si_range.scan_up(su) {
                            if self.ui_mult.contains(su) {
                                return Some(su);
                            }
                        };
                    }
                };
            }
        };
        None
    }
    fn scan_dn(&self, n: u64) -> Option<u64> {
        //if n < self.residue {
        //    return Option::None;
        //}
        //Option::Some(n - (n - self.residue) % self.modulus)

        //TODO correct?
        if let Some(su) = self.ui_mult.scan_dn(n) {
            if self.si_range.contains(su) {
                return Some(su);
            } else {
                if let Some(su) = self.si_range.scan_dn(n) {
                    if self.ui_mult.contains(su) {
                        return Some(su);
                    } else {
                        if let Some(su) = self.si_range.scan_dn(su) {
                            if self.ui_mult.contains(su) {
                                return Some(su);
                            }
                        };
                    }
                };
            }
        };
        None
    }
}

impl StridedInterval_n {
    fn from (stride: u64, min: i64, max:i64) -> StridedInterval_n {
        StridedInterval_n {
                stride: 0,
                min:    min,
                max:    max,
        }
    }
    //TODO get rid of this entirely
    pub fn as_const(self) -> i64 {
        self.min
    }
}

impl StridedInterval_u {
    fn from(stride: u64, min: i64, max:i64) -> StridedInterval_u {
        StridedInterval_u::StridedInterval(
            StridedInterval_n::from(stride, min, max)
            )
    }
    //TODO get rid of this entirely
    pub fn as_const(self) -> i64 {
        match self {
            StridedInterval_u::Undefined => 0,
            StridedInterval_u::StridedInterval(si) => si.as_const(),
        }
    }
}

impl AbstractValue for StridedInterval_n {
    fn from_const(n: i64) -> Self {
        StridedInterval_n::from(0, n, n)
    }
    //fn new(stride: u64, min: i64, max: i64) -> Self;
    /// Will this ever be implemented?
    fn new() -> Self {
        StridedInterval_n::from(0, 0, 0)
    }
    fn is_subset_of(self, other: Self) -> bool {
        unimplemented!();
    }
    fn meet(self, other: Self) -> Self {
        unimplemented!();
    }
    fn join(self, other: Self) -> Self {
        let min = min(self.min, other.min);
        let max = max(self.max, other.max);
        // note: it is important to get the difference between elements
        //       of _different_ strided intervals:
        let diff = (self.min - other.min).abs() as u64;
        let (gcd1, lcm1) = gcd_lcm(self.stride, other.stride);
        let (gcd2, lcm2) = gcd_lcm(gcd1, diff);
        let stride_new = gcd2;
        StridedInterval_n::from (stride_new, min, max)
    }
    //fn widen(self) -> Self;
}

impl AbstractValue for StridedInterval_u {
    fn from_const(n: i64) -> Self {
        StridedInterval_u::StridedInterval(StridedInterval_n::from_const(n))
    }
    //fn new(stride: u64, min: i64, max: i64) -> Self;
    fn new() -> Self {
        StridedInterval_u::Undefined
    }
    fn is_subset_of(self, other: Self) -> bool {
        unimplemented!();
    }
    fn meet(self, other: Self) -> Self {
        unimplemented!();
    }
    fn join(self, other: Self) -> Self {
        match (self, other) {
            (StridedInterval_u::StridedInterval(self_si),
             StridedInterval_u::StridedInterval(other_si))
                => StridedInterval_u::StridedInterval(self_si.join(other_si)),
            _   => StridedInterval_u::Undefined,
        }
    }
    //fn widen(self) -> Self;
}

impl StridedInterval {
    pub fn new_const(n: i64) -> StridedInterval {
        StridedInterval {
            ui_mult : UIntMultiple {
                modulus: 0,
                residue: 0, // basically unused
            },
            si_range: SIntRange {
                min: n,
                max: n,
            },
        }
    }

    pub fn new(stride: u64, lower: i64, upper: i64) -> StridedInterval {
        StridedInterval {
            ui_mult : UIntMultiple {
                modulus: stride,
                residue: 0, // basically unused
            },
            si_range: SIntRange {
                min: lower,
                max: upper,
            },
        }
    }

    pub fn is_subset_of() {}
    pub fn meet() {}
    pub fn join(self, si: StridedInterval) -> StridedInterval {
        let min = min(self.si_range.min, si.si_range.min);
        let max = max(self.si_range.max, si.si_range.max);
        // note: it is important to get the difference between elements
        //       of _different_ strided intervals:
        let diff = (self.si_range.min - si.si_range.min).abs() as u64;
        let (gcd1, lcm1) = gcd_lcm(self.ui_mult.modulus,
                                     si.ui_mult.modulus);
        let (gcd2, lcm2) = gcd_lcm(gcd1, diff);
        let mod_new = gcd2;
        StridedInterval::new(mod_new, min, max)
    }
    pub fn widen() {}
    pub fn adjust_const() {}
    pub fn adjust_const_add(self, si: StridedInterval) -> StridedInterval {
        StridedInterval {
            ui_mult: UIntMultiple {
                modulus: si.ui_mult.modulus,
                residue: 0,
            },
            si_range: SIntRange {
                min: self.si_range.min + si.si_range.min,
                max: self.si_range.max + si.si_range.max,
            },
        }
    }
    pub fn adjust_const_sub(self, si: StridedInterval) -> StridedInterval {
        StridedInterval {
            ui_mult: UIntMultiple {
                modulus: si.ui_mult.modulus,
                residue: 0,
            },
            si_range: SIntRange {
                min: self.si_range.min - si.si_range.min,
                max: self.si_range.max - si.si_range.max,
            },
        }
    }
    pub fn adjust_const_mul(self, si: StridedInterval) -> StridedInterval {
        StridedInterval {
            ui_mult: UIntMultiple {
                modulus: self.ui_mult.modulus * si.ui_mult.modulus, // correct?
                residue: 0,
            },
            si_range: SIntRange {
                min: self.si_range.min * si.si_range.min,
                max: self.si_range.max * si.si_range.max,
            },
        }
    }
    pub fn adjust_const_div(self, si: StridedInterval) -> StridedInterval {
        StridedInterval {
            ui_mult: UIntMultiple {
                modulus: self.ui_mult.modulus / si.ui_mult.modulus, // correct?
                residue: 0,
            },
            si_range: SIntRange {
                min: self.si_range.min / si.si_range.min,
                max: self.si_range.max / si.si_range.max,
            },
        }
    }
    pub fn remove_lower_bounds() {}
    pub fn remove_upper_bounds() {}

    pub fn print(self) {
        println!("\tmod: {}", self.ui_mult.modulus);
        println!("\tmin: {}", self.si_range.min);
        println!("\tmax: {}", self.si_range.max);
    }
    pub fn is_const(self) -> bool {
        if (self.ui_mult.modulus == 0 as u64) &
            (self.si_range.min == self.si_range.max) {true}
        else {false}
    }
    pub fn as_const(self) -> i64 {
        self.si_range.min
    }
    pub fn concretize(self) -> Vec<i64> {
        Vec::new()
    }
//    pub fn as_knownbits(&self) -> KnownBits {
//        let z = tzmsk(self.modulus);
//        KnownBits {
//            zerobits: z & !self.residue,
//            onebits: z & self.residue,
//        }
//    }
//    // dkreuter: it seems pointless to convert a multiple constraint to a range
//    pub fn as_urange(&self) -> UIntRange {
//        if self.modulus == 0 {
//            return EMPTY_UINTRANGE;
//        }
//        UIntRange {
//            min: self.scan_up(U64MIN).unwrap_or(U64MAX),
//            max: self.scan_dn(U64MAX).unwrap_or(U64MIN),
//        }
//    }
//    pub fn as_srange(&self) -> SIntRange {
//        if self.modulus == 0 {
//            return EMPTY_SINTRANGE;
//        }
//        SIntRange {
//            min: self.scan_up(S64MIN as u64)
//                     .or_else(|| self.scan_up(U64MIN))
//                     .unwrap_or(S64MIN as u64) as i64,
//            max: self.scan_dn(S64MAX as u64)
//                     .or_else(|| self.scan_dn(U64MAX))
//                     .unwrap_or(S64MAX as u64) as i64,
//        }
//    }
}

impl<'a, 'b> BitAnd<&'a StridedInterval> for &'b StridedInterval {
    type Output = StridedInterval;

    fn bitand(self, rhs: &StridedInterval) -> StridedInterval {
        StridedInterval {
            ui_mult: self.ui_mult.bitand (rhs.ui_mult),
            si_range: self.si_range.bitand (rhs.si_range),
        }
    }
}

impl<'a, 'b> BitOr<&'a StridedInterval> for &'b StridedInterval {
    type Output = StridedInterval;

    fn bitor(self, rhs: &StridedInterval) -> StridedInterval {
        StridedInterval {
            ui_mult: self.ui_mult.bitor (rhs.ui_mult),
            si_range: self.si_range.bitor (rhs.si_range),
        }
    }
}

impl fmt::Display for StridedInterval {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}[{}, {}]",
               self.ui_mult.modulus,
               self.si_range.min,
               self.si_range.max)
    }
}

impl fmt::Display for StridedInterval_n {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}[{}, {}]",
               self.stride,
               self.min,
               self.max)
    }
}

impl fmt::Display for StridedInterval_u {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            StridedInterval_u::Undefined => write!(f, "s[l, h]"),
            StridedInterval_u::StridedInterval(si) => si.fmt(f),
        }
    }
}

impl PartialEq for StridedInterval {
    fn eq(&self, other: &StridedInterval) -> bool {
        (self.ui_mult.modulus == other.ui_mult.modulus) &
        (self.ui_mult.residue == other.ui_mult.residue) &
        (self.si_range.min    == other.si_range.min) &
        (self.si_range.max    == other.si_range.max)
    }
}

impl PartialEq for StridedInterval_n {
    fn eq(&self, other: &StridedInterval_n) -> bool {
        (self.stride == other.stride) &
        (self.min    == other.min) &
        (self.max    == other.max)
    }
}

impl Add for StridedInterval_n {
    type Output = StridedInterval_n;

    fn add(self, other: StridedInterval_n) -> StridedInterval_n {
        let (gcd, lcm) = gcd_lcm(self.stride, other.stride);
        StridedInterval_n::from(gcd,
                                self.min + other.min,
                                self.max + other.max,)
    }
}

impl Sub for StridedInterval_n {
    type Output = StridedInterval_n;

    fn sub(self, other: StridedInterval_n) -> StridedInterval_n {
        let (gcd, lcm) = gcd_lcm(self.stride, other.stride);
        StridedInterval_n::from(gcd,
                                self.min - other.max,
                                self.max - other.min,)
    }
}

impl Mul for StridedInterval_n {
    type Output = StridedInterval_n;

    fn mul(self, other: StridedInterval_n) -> StridedInterval_n {
        let t1 = self.min.abs() as u64 * other.stride;
        let t2 = other.min.abs() as u64 * self.stride;
        let (gcd, lcm) = gcd_lcm(t1, t2);
        StridedInterval_n::from(gcd,
                                self.min * other.min,
                                self.max * other.max,)
    }
}

impl Div for StridedInterval_n {
    type Output = StridedInterval_n;

    fn div(self, other: StridedInterval_n) -> StridedInterval_n {
        let t1 = other.stride / self.min.abs() as u64;
        let t2 = self.stride / other.min.abs() as u64;
        let (gcd, lcm) = gcd_lcm(t1, t2);
        StridedInterval_n::from(gcd,
                                self.min / other.max,
                                self.max / other.min,)
    }
}

impl Add for StridedInterval_u {
    type Output = StridedInterval_u;

    fn add(self, other: StridedInterval_u) -> StridedInterval_u {
        match (self, other) {
            (StridedInterval_u::StridedInterval(self_si),
             StridedInterval_u::StridedInterval(other_si))
                => StridedInterval_u::StridedInterval(self_si + other_si),
            _   => StridedInterval_u::Undefined,
        }
    }
}

impl Sub for StridedInterval_u {
    type Output = StridedInterval_u;

    fn sub(self, other: StridedInterval_u) -> StridedInterval_u {
        match (self, other) {
            (StridedInterval_u::StridedInterval(self_si),
             StridedInterval_u::StridedInterval(other_si))
                => StridedInterval_u::StridedInterval(self_si - other_si),
            _   => StridedInterval_u::Undefined,
        }
    }
}

impl Mul for StridedInterval_u {
    type Output = StridedInterval_u;

    fn mul(self, other: StridedInterval_u) -> StridedInterval_u {
        match (self, other) {
            (StridedInterval_u::StridedInterval(self_si),
             StridedInterval_u::StridedInterval(other_si))
                => StridedInterval_u::StridedInterval(self_si * other_si),
            _   => StridedInterval_u::Undefined,
        }
    }
}

impl Div for StridedInterval_u {
    type Output = StridedInterval_u;

    fn div(self, other: StridedInterval_u) -> StridedInterval_u {
        match (self, other) {
            (StridedInterval_u::StridedInterval(self_si),
             StridedInterval_u::StridedInterval(other_si))
                => StridedInterval_u::StridedInterval(self_si / other_si),
            _   => StridedInterval_u::Undefined,
        }
    }
}


#[cfg(test)]
mod strided_interval {
    //use analysis::valueset::strided_interval::StridedInterval;
    use analysis::valueset::StridedInterval;

    #[test]
    fn si_join_const_const_same () {
        let c1 = StridedInterval::new_const(42);
        let c2 = StridedInterval::new_const(42);
        let res = c1.join(c2);
        assert_eq!(res, StridedInterval::new_const(42));
    }
    #[test]
    fn si_join_const_const_diff () {
        let c1 = StridedInterval::new_const(31337);
        let c2 = StridedInterval::new_const(42);
        let res = c1.join(c2);
        assert_eq!(res, StridedInterval::new(31295, 42, 31337));
    }
    #[test]
    fn si_join_si_const_unaligned () {
        let si = StridedInterval::new(6, 12, 42);
        let c  = StridedInterval::new_const(5);
        let res = si.join(c);
        assert_eq!(res, StridedInterval::new(1, 5, 42));
    }
    #[test]
    fn si_join_si_const_unaligned_II () {
        let si = StridedInterval::new(6, 12, 42);
        let c  = StridedInterval::new_const(15);
        let res = si.join(c);
        assert_eq!(res, StridedInterval::new(3, 12, 42));
    }
    #[test]
    fn si_join_si_const_aligned_inside () {
        let si = StridedInterval::new(6, 12, 42);
        let c  = StridedInterval::new_const(18);
        let res = si.join(c);
        assert_eq!(res, StridedInterval::new(6, 12, 42));
    }
    #[test]
    fn si_join_si_const_aligned_outside () {
        let si = StridedInterval::new(6, 12, 42);
        let c  = StridedInterval::new_const(48);
        let res = si.join(c);
        assert_eq!(res, StridedInterval::new(6, 12, 48));
    }
    #[test]
    fn si_join_si_si_same_mod () {
        let si1 = StridedInterval::new(6, 12, 42);
        let si2 = StridedInterval::new(6, -6, 6);
        let res = si1.join(si2);
        assert_eq!(res, StridedInterval::new(6, -6, 42));
    }
    #[test]
    fn si_join_si_si_diff_mod () {
        let si1 = StridedInterval::new(6, 12, 42);
        let si2 = StridedInterval::new(9, 15, 24);
        let res = si1.join(si2);
        assert_eq!(res, StridedInterval::new(3, 12, 42));
    }
    #[test]
    fn si_join_si_si_subset () {
        let si1 = StridedInterval::new(6, 12, 42);
        let si2 = StridedInterval::new(12, 24, 36);
        let res = si1.join(si2);
        assert_eq!(res, StridedInterval::new(6, 12, 42));
    }
}

#[cfg(test)]
mod strided_interval_n {
    use analysis::valueset::{StridedInterval_n,AbstractValue};

    #[test]
    fn si_join_const_const_same () {
        let c1 = StridedInterval_n::from_const(42);
        let c2 = StridedInterval_n::from_const(42);
        let res = c1.join(c2);
        assert_eq!(res, StridedInterval_n::from_const(42));
    }
    #[test]
    fn si_join_const_const_diff () {
        let c1 = StridedInterval_n::from_const(31337);
        let c2 = StridedInterval_n::from_const(42);
        let res = c1.join(c2);
        assert_eq!(res, StridedInterval_n::from(31295, 42, 31337));
    }
    #[test]
    fn si_join_const_const_zero () {
        let c1 = StridedInterval_n::from_const(0);
        let c2 = StridedInterval_n::from_const(42);
        let res = c1.join(c2);
        assert_eq!(res, StridedInterval_n::from(42, 0, 42));
    }
    #[test]
    fn si_join_const_zero_const () {
        let c1 = StridedInterval_n::from_const(42);
        let c2 = StridedInterval_n::from_const(0);
        let res = c1.join(c2);
        assert_eq!(res, StridedInterval_n::from(42, 0, 42));
    }
    #[test]
    fn si_join_si_const_unaligned () {
        let si = StridedInterval_n::from(6, 12, 42);
        let c  = StridedInterval_n::from_const(5);
        let res = si.join(c);
        assert_eq!(res, StridedInterval_n::from(1, 5, 42));
    }
    #[test]
    fn si_join_si_const_unaligned_II () {
        let si = StridedInterval_n::from(6, 12, 42);
        let c  = StridedInterval_n::from_const(15);
        let res = si.join(c);
        assert_eq!(res, StridedInterval_n::from(3, 12, 42));
    }
    #[test]
    fn si_join_si_const_aligned_inside () {
        let si = StridedInterval_n::from(6, 12, 42);
        let c  = StridedInterval_n::from_const(18);
        let res = si.join(c);
        assert_eq!(res, StridedInterval_n::from(6, 12, 42));
    }
    #[test]
    fn si_join_si_const_aligned_outside () {
        let si = StridedInterval_n::from(6, 12, 42);
        let c  = StridedInterval_n::from_const(48);
        let res = si.join(c);
        assert_eq!(res, StridedInterval_n::from(6, 12, 48));
    }
    #[test]
    fn si_join_si_si_same_mod () {
        let si1 = StridedInterval_n::from(6, 12, 42);
        let si2 = StridedInterval_n::from(6, -6, 6);
        let res = si1.join(si2);
        assert_eq!(res, StridedInterval_n::from(6, -6, 42));
    }
    #[test]
    fn si_join_si_si_diff_mod () {
        let si1 = StridedInterval_n::from(6, 12, 42);
        let si2 = StridedInterval_n::from(9, 15, 24);
        let res = si1.join(si2);
        assert_eq!(res, StridedInterval_n::from(3, 12, 42));
    }
    #[test]
    fn si_join_si_si_subset () {
        let si1 = StridedInterval_n::from(6, 12, 42);
        let si2 = StridedInterval_n::from(12, 24, 36);
        let res = si1.join(si2);
        assert_eq!(res, StridedInterval_n::from(6, 12, 42));
    }
}

#[cfg(test)]
mod strided_interval_u {
    use analysis::valueset::{StridedInterval_n,AbstractValue};

    #[test]
    fn si_add_const_const_same () {
        let c1 = StridedInterval_n::from_const(42);
        let c2 = StridedInterval_n::from_const(42);
        assert_eq!(c1 + c2, StridedInterval_n::from_const(84));
    }
    //  4 6 8
    // +3 5 7
    // ------
    // 7 9 11 13 15
    #[test]
    fn si_add_equal_stride () {
        let c1 = StridedInterval_n::from(2, 4, 8);
        let c2 = StridedInterval_n::from(2, 3, 7);
        assert_eq!(c1 + c2, StridedInterval_n::from(2, 7, 15));
    }
    //  4 6 8
    // +3 6 9
    // ------
    // 7 9 10 11 12 13 14 15 17
    #[test]
    fn si_add_diff_stride () {
        let c1 = StridedInterval_n::from(2, 4, 8);
        let c2 = StridedInterval_n::from(3, 3, 9);
        assert_eq!(c1 + c2, StridedInterval_n::from(1, 7, 17));
    }
    #[test]
    fn si_sub_const_const_same () {
        let c1 = StridedInterval_n::from_const(42);
        let c2 = StridedInterval_n::from_const(42);
        assert_eq!(c1 - c2, StridedInterval_n::from_const(0));
    }
    //  8 10 12
    // -1  3  5
    // --------
    // 3 5 7 9 11
    #[test]
    fn si_sub_equal_stride () {
        let c1 = StridedInterval_n::from(2, 8, 12);
        let c2 = StridedInterval_n::from(2, 1, 5);
        assert_eq!(c1 - c2, StridedInterval_n::from(2, 3, 11));
    }
    //  8 10 12
    // -1  4  7
    // --------
    // 1 3 4 5 6 7 8 9 11
    #[test]
    fn si_sub_diff_stride () {
        let c1 = StridedInterval_n::from(2, 8, 12);
        let c2 = StridedInterval_n::from(3, 1, 7);
        assert_eq!(c1 - c2, StridedInterval_n::from(1, 1, 11));
    }
    //  3 5 7
    // *2
    // --------
    // 6 10 14  4[6, 14]
    #[test]
    fn si_mul_zero_stride_1 () {
        let c1 = StridedInterval_n::from(2, 3, 7);
        let c2 = StridedInterval_n::from_const(2);
        assert_eq!(c1 * c2, StridedInterval_n::from(4, 6, 14));
    }
    //  3 5 7
    // *3
    // --------
    // 9 15 21  6[9, 21]
    #[test]
    fn si_mul_zero_stride_2 () {
        let c1 = StridedInterval_n::from(2, 3, 7);
        let c2 = StridedInterval_n::from_const(3);
        assert_eq!(c1 * c2, StridedInterval_n::from(6, 9, 21));
    }
    //  3 5 7
    // *2 4 6
    // --------
    // 6 10 12 14 18 20 28 30 42  2[6, 42]
    #[test]
    fn si_mul_equal_stride() {
        let c1 = StridedInterval_n::from(2, 3, 7);
        let c2 = StridedInterval_n::from(2, 2, 6);
        assert_eq!(c1 * c2, StridedInterval_n::from(2, 6, 42));
    }
    //  1 3 5
    // *2 3 4
    // --------
    // 2 3 4 6 9 10 12 15 20  1[2, 20]
    #[test]
    fn si_mul_diff_stride_1() {
        let c1 = StridedInterval_n::from(2, 1, 5);
        let c2 = StridedInterval_n::from(1, 2, 4);
        assert_eq!(c1 * c2, StridedInterval_n::from(1, 2, 20));
    }
    //  2 4 6
    // *2 5 8
    // --------
    // 4 8 10 12 16 20 30 32 48  2[4, 48]
    #[test]
    fn si_mul_diff_stride_2() {
        let c1 = StridedInterval_n::from(2, 2, 6);
        let c2 = StridedInterval_n::from(3, 2, 8);
        assert_eq!(c1 * c2, StridedInterval_n::from(2, 4, 48));
    }
    //  2 4 6
    // *3 5 7
    // --------
    // 6 10 12 14 18 20 28 30 42  2[6, 42]
    #[test]
    fn si_mul_equal_stride_same_offs() {
        let c1 = StridedInterval_n::from(2, 2, 6);
        let c2 = StridedInterval_n::from(2, 3, 7);
        assert_eq!(c1 * c2, StridedInterval_n::from(2, 6, 42));
    }
    //  2 4 6
    // /2
    // --------
    // 1 2 3  1[1, 3]
    #[test]
    fn si_div_zero_stride_1() {
        let c1 = StridedInterval_n::from(2, 2, 6);
        let c2 = StridedInterval_n::from_const(2);
        assert_eq!(c1 / c2, StridedInterval_n::from(1, 1, 3));
    }
    //  6 12 18
    // /2
    // --------
    // 3 6 9  3[3, 9]
    #[test]
    fn si_div_zero_stride_2() {
        let c1 = StridedInterval_n::from(6, 6, 18);
        let c2 = StridedInterval_n::from_const(2);
        assert_eq!(c1 / c2, StridedInterval_n::from(3, 3, 9));
    }
    //  6 12 18
    // /2  3
    // --------
    // 2 3 4 6 9  1[2, 9]
    #[test]
    fn si_div_general_1() {
        let c1 = StridedInterval_n::from(6, 6, 18);
        let c2 = StridedInterval_n::from(1, 2, 3);
        assert_eq!(c1 / c2, StridedInterval_n::from(1, 2, 9));
    }
    //  6 12 18
    // /2  6
    // --------
    // 1 2 3 6 9  1[1, 9]
    #[test]
    fn si_div_general_2() {
        let c1 = StridedInterval_n::from(6, 6, 18);
        let c2 = StridedInterval_n::from(4, 2, 6);
        assert_eq!(c1 / c2, StridedInterval_n::from(1, 1, 9));
    }
    //  12 24 36
    // /2  6
    // --------
    // 2 4 6 12 18  2[2, 18]
    #[test]
    fn si_div_general_3() {
        let c1 = StridedInterval_n::from(12, 12, 36);
        let c2 = StridedInterval_n::from(4, 2, 6);
        assert_eq!(c1 / c2, StridedInterval_n::from(2, 2, 18));
    }
    //  12 24 36
    // /2  3
    // --------
    // 4 6 8 12 18  2[4, 18]
    #[test]
    fn si_div_general_4() {
        let c1 = StridedInterval_n::from(12, 12, 36);
        let c2 = StridedInterval_n::from(1, 2, 3);
        assert_eq!(c1 / c2, StridedInterval_n::from(2, 4, 18));
    }
    //  6 18 30
    // /2  6
    // --------
    // 1 3 5 9 15  2[1, 15]
    #[test]
    fn si_div_general_5() {
        let c1 = StridedInterval_n::from(12, 6, 30);
        let c2 = StridedInterval_n::from(4, 2, 6);
        assert_eq!(c1 / c2, StridedInterval_n::from(2, 1, 15));
    }
}
