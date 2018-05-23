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

// XXX: Try to support 128-bit registers in the future
// type inum = i128; <-- this one do influence performance
// const full_bits: u8 = 128;
//
// All numbers (except k) in StridedInterval will be stored as inum 
type inum = i64;
// All members (except k) in StridedInterval will be hold in _bits bits
const _bits: u8 = 64;

macro_rules! min_in_k_bits {
    ($k:expr) => {
        // Avoid integet overflow when k equls to _bits
        ((inum::min_value()) >> (_bits - ($k)))
    }
}

macro_rules! max_in_k_bits {
    ($k:expr) => {
        ((inum::max_value()) >> (_bits - ($k)))
    }
}

macro_rules! n_in_k_bits {
    ($n:expr, $k:expr) => {
        if (max_in_k_bits!($k) < ($n)) || (min_in_k_bits!($k) > ($n)) {
            radeco_err!("{:?} cannot be hold by {:?} bits", $n, $k);
            // reture max_int if $n is too big, while min_int if is too small.
            (max_in_k_bits!($k) ^ (($n) >> (_bits - 1)))
        } else {
            ($n)
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


// A k-bit strided interval s[lb, ub].
//
// s[lb, ub] := {i| lb <= i <= ub, (i - lb) % s = 0}
// Without loss of generality, we will assume that all strided intervals are 
// reduced (i.e., upper bounds are tight, and whenever the upper bound equals 
// the lower bound the stride is 0)
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct StridedInterval {
    // Used for indicate radix of strided interval (k-bits)
    k: u8,
    // lower bound and upper bound of strided interval
    lb: inum,
    ub: inum,
    // stride of strided interval
    s: inum,
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

impl Default for StridedInterval {
    fn default() -> Self {
        StridedInterval {
            k: _bits,
            s: 0,
            lb: 0,
            ub: 0,
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


// Basic functions for StridedInterval
impl StridedInterval {
    // Automatically correct variable to generate StridedInterval. 
    fn new(k: u8, s: inum, lb: inum, ub: inum) -> Self {
        // Correct k
        let mut k = if (k > _bits) || (k == 0) {
            _bits
        } else {
            k
        };

        // Correct lb/ub 
        let _lb = n_in_k_bits!(lb, k);
        let _ub = n_in_k_bits!(ub, k);
        let (mut lb, mut ub) = if _lb > _ub {
            radeco_err!("lb({:?}) is bigger than ub({:?}), exchange these two", 
                        _lb, _ub);
            (_ub, _lb)
        } else {
            (_lb, _ub)
        }; 

        // Correct stride
        let mut s = if let Some(_s) = s.checked_abs() {
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

       StridedInterval {
           k: k,
           s: s,
           lb: lb,
           ub: ub,
       }
    }
}


#[cfg(test)]
mod test {
    use super::*;
    
    #[test]
    fn strided_interval_marco_test() {
        assert_eq!(-32768, min_in_k_bits!(16));
        assert_eq!(32767, max_in_k_bits!(16));
        assert_eq!(-9223372036854775808, min_in_k_bits!(64));
        assert_eq!(9223372036854775807, max_in_k_bits!(64));
        assert_eq!(0xdeadbeef, n_in_k_bits!(0xdeadbeef, 64));
        assert_eq!(127, n_in_k_bits!(0xdeadbeef, 8));
        assert_eq!(-128, n_in_k_bits!(-0xdeadbeef, 8));
        assert_eq!(false, check_overflow!((0 as inum).checked_add(0xdeadbeef), 64));
        assert_eq!(true, check_overflow!(inum::max_value().checked_add(1), 64));
        assert_eq!(true, check_overflow!((100 as inum).checked_add(100), 8));
    }

    #[test]
    fn strided_interval_new_test() {
        assert_eq!(StridedInterval{k: _bits, s: 0, lb: 0, ub: 0}, 
                   StridedInterval::default());
        assert_eq!(StridedInterval{k: _bits, s: 0, lb: 0xdeadbeef, ub: 0xdeadbeef}, 
                   StridedInterval::from(0xdeadbeef));
        assert_eq!(StridedInterval{k: _bits, s: 3, lb: -2, ub: 28}, 
                   StridedInterval::new(0, -3, -2, 29));
    }
}
