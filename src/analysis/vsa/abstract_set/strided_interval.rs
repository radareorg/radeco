// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Module that implements math operation on the strided interval.
//!
//! More information are available from Gogul Balakrishnan's Ph.D. Thesis.
//! Please refer here:
//!     * https://research.cs.wisc.edu/wpis/papers/balakrishnan_thesis.pdf
//!
//! Thanks to angr/claripy, which also gives more ideas.
//! Please refer here:
//!     * https://github.com/angr/claripy/blob/master/claripy/vsa/StridedInterval.py

use std::fmt;
use std::ops::{Neg, Add, Sub, Div, Rem, Mul};
use std::ops::{BitAnd, BitOr, BitXor, Not, Shl, Shr};

use super::abstract_set::{inum, _bits};
use super::abstract_set::AbstractSet;

// Avoid integet overflow when k equls to _bits
macro_rules! min_in_k_bits {
    ($k:expr) => {
        ((inum::min_value()) >> (_bits - ($k)))
    }
}

macro_rules! max_in_k_bits {
    ($k:expr) => {
        ((inum::max_value()) >> (_bits - ($k)))
    }
}

// Because rust does not have logical shift right for signed number,
// This is a hack to generate k-bits mask even k equals to _bits.
macro_rules! mask_in_k_bits {
    ($k:expr) => {
       (((!min_in_k_bits!($k)) << 1) ^ 1) 
    }
}

// Returns the corresponding number of n in k bits
// e.g. 
//      0xf0001 in 16 bits --> 1
//      0xf8000 in 16 bits --> -32768
// XXX: Watch out for overflow when k equals to _bits
// This is a hack
macro_rules! n_in_k_bits {
    ($n:expr, $k:expr) => {
        // Both branches return the same value, but we need a check for overflow.
        if (max_in_k_bits!($k) < ($n) as inum) || (min_in_k_bits!($k) > ($n) as inum) {
            radeco_err!("{:?} cannot be hold by {:?} bits", $n, $k);
            ((((((($n) as inum) & mask_in_k_bits!($k)) << 
                (_bits - ($k))) >> 
                (_bits - 1)) & 
                (!mask_in_k_bits!($k))) | 
                ((($n) as inum) & mask_in_k_bits!($k)))
        } else {
            (($n) as inum)
        }
    };
}

macro_rules! check_overflow {
    ($res:expr, $k:expr) => {
        if let Some(_res) = ($res) {
            if (_res < min_in_k_bits!($k)) || (_res > max_in_k_bits!($k)) {
                true
            } else {
                false
            }
        } else {
            true
        }
    }
}


// Euclid's two-thousand-year-old algorithm for finding the greatest common
// divisor.
fn gcd(x: inum, y: inum) -> inum {
    let mut x = x;
    let mut y = y;
    while y != 0 {
        let t = y;
        y = x % y;
        x = t;
    }
    x
}


// A k-bit strided interval s[lb, ub].
//
// s[lb, ub] := {i| lb <= i <= ub, (i - lb) % s = 0}
// Without loss of generality, we will assume that all strided intervals are 
// reduced (i.e., upper bounds are tight, and whenever the upper bound equals 
// the lower bound the stride is 0)
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct StridedInterval {
    // Used for indicate radix of strided interval (k-bits)
    pub k: u8,
    // lower bound and upper bound of strided interval
    pub lb: inum,
    pub ub: inum,
    // stride of strided interval
    pub s: inum,
}


// Utility functions go here

impl fmt::LowerHex for StridedInterval {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-bits: {:#x}[{:#x}, {:#x}]", 
               self.k, 
               self.s,
               self.lb,
               self.ub)
    }
}

impl fmt::UpperHex for StridedInterval {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-bits: {:#X}[{:#X}, {:#X}]", 
               self.k, 
               self.s,
               self.lb,
               self.ub)
    }
}

impl fmt::Display for StridedInterval {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-bits: {}[{}, {}]", 
               self.k, 
               self.s,
               self.lb,
               self.ub)
    }
}

// Basic functions for StridedInterval
impl StridedInterval {
    // Returns a default strided interval in k bits;
    pub fn default_k(k: u8) -> Self {
        let k = if (k > _bits) || (k == 0) {
            _bits
        } else {
            k
        };
        StridedInterval {
            k: k,
            s: 1,
            lb: min_in_k_bits!(k),
            ub: max_in_k_bits!(k),
        }
    }

    pub fn new(k: u8, s: inum, lb: inum, ub: inum) -> Self {
       let mut si = StridedInterval {
           k: k,
           s: s,
           lb: lb,
           ub: ub,
       };
       si.validate();
       si
    }
    
    // Automatically correct variable to generate StridedInterval. 
    pub fn validate(&mut self) {
        // Correct k
        let mut k = if (self.k > _bits) || (self.k == 0) {
            _bits
        } else {
            self.k
        };

        // Correct lb/ub 
        let _lb = n_in_k_bits!(self.lb, k);
        let _ub = n_in_k_bits!(self.ub, k);
        let (mut lb, mut ub) = if _lb > _ub {
            radeco_err!("lb({:?}) is bigger than ub({:?}), exchange these two", 
                        _lb, _ub);
            (_ub, _lb)
        } else {
            (_lb, _ub)
        }; 

        // Correct stride
        let mut s = if let Some(_s) = self.s.checked_abs() {
            _s
        } else {
            radeco_err!("Strided({:?}) should not be MIN in {:?} bits", 
                        s, _bits);
            1
        };
        if (s != n_in_k_bits!(s, k)) || (s > (ub - lb)) {
            radeco_err!("Invalid stride({:?})", s);
            s = 1;
        }

        // Reduce Strided Interval
        if ub == lb {
            s = 0;
        }
        if (s == 0) && (ub != lb) {
            s = 1;
        }
        ub -= (ub - lb) % s;

        self.k = k;
        self.s = s;
        self.lb = lb;
        self.ub = ub;
    }
}


// Operations for StridedInterval go here

impl Default for StridedInterval {
    fn default() -> Self {
        StridedInterval {
            k: _bits,
            s: 1,
            lb: min_in_k_bits!(_bits),
            ub: max_in_k_bits!(_bits),
        }
    }
}

// Generate StridedInterval by a constant
impl From<inum> for StridedInterval {
    fn from(number: inum) -> Self {
        StridedInterval {
            k: _bits,
            s: 0,
            lb: n_in_k_bits!(number, _bits),
            ub: n_in_k_bits!(number, _bits),
        }
    }
}

impl Neg for StridedInterval {
    type Output = Self;

    fn neg(self) -> Self {
        Self::default()
    }
}

impl Add for StridedInterval {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        if self.k != other.k {
            radeco_err!("Addition between two strided intervals with different radices");
            Self::default()
        } else {
            if check_overflow!(self.lb.checked_add(other.lb), self.k) ^
                check_overflow!(self.ub.checked_add(other.ub), self.k) {
                StridedInterval::default_k(self.k)
            } else {
                StridedInterval::new(
                    self.k, 
                    gcd(self.s, other.s), 
                    self.lb.wrapping_add(other.lb),
                    self.ub.wrapping_add(other.ub),
                )
            }
        }
    }
}

impl Sub for StridedInterval {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self::default()
    }
}

impl Mul for StridedInterval {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        Self::default()
    }
}

impl Div for StridedInterval {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        Self::default()
    }
}

impl Rem for StridedInterval {
    type Output = Self;

    fn rem(self, other: Self) -> Self {
        Self::default()
    }
}

// Implement trait AbstractSet for StridedInterval
// impl AbstractSet for StridedInterval {
//}

#[cfg(test)]
mod test {
    use super::*;
    
    #[test]
    fn strided_interval_marco_test() {
        assert_eq!(-32768, min_in_k_bits!(16));
        assert_eq!(32767, max_in_k_bits!(16));
        assert_eq!(-9223372036854775808, min_in_k_bits!(64));
        assert_eq!(9223372036854775807, max_in_k_bits!(64));
        assert_eq!(0xffff, mask_in_k_bits!(16));
        assert_eq!(0xffffffffffffffff, mask_in_k_bits!(64));
        assert_eq!(0xdeadbeef, n_in_k_bits!(0xdeadbeef, 64));
        assert_eq!(1, n_in_k_bits!(0xf0001, 16));
        assert_eq!(-32768, n_in_k_bits!(0xf8000, 16));
        assert_eq!(false, check_overflow!((0 as inum).checked_add(0xdeadbeef), 64));
        assert_eq!(true, check_overflow!(inum::max_value().checked_add(1), 64));
        assert_eq!(true, check_overflow!((100 as inum).checked_add(100), 8));
    }

    #[test]
    fn strided_interval_new_test() {
        assert_eq!(StridedInterval{k: _bits, s: 1, lb: -9223372036854775808, ub: 9223372036854775807}, 
                   StridedInterval::default());
        assert_eq!(StridedInterval{k: _bits, s: 0, lb: 0xdeadbeef, ub: 0xdeadbeef}, 
                   StridedInterval::from(0xdeadbeef));
        assert_eq!(StridedInterval{k: _bits, s: 3, lb: -2, ub: 28}, 
                   StridedInterval::new(0, -3, -2, 29));
    }
}
