// Copyright (c) 2018, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Module that implements basic math operation on the strided interval.
//!
//! Strided Interval (on k bits' space) goes like:
//!     s[lb, ub] --> {i | lb <= i <= ub && (i - lb) % s == 0}
//!               --> {lb, lb + s, lb + 2s, .., lb + ks, .., ub}
//! The soundness of operations between Strided Intervals sould be like:
//!     For every operaction op (+, - *, /, %, |, &, ^, >>, <<):
//!         {a op b | si1.constants(a) && si2.constants(b)} is a subset of `si1 op si2`
//!
//! More information are available from Gogul Balakrishnan's Ph.D. Thesis.
//! Please refer here:
//!     * https://research.cs.wisc.edu/wpis/papers/balakrishnan_thesis.pdf
//!
//! Thanks to angr/claripy, which also gives more ideas.
//! Please refer here:
//!     * https://github.com/angr/claripy/blob/master/claripy/vsa/StridedInterval.py

use std::cmp;
use std::fmt;
use std::ops::{Add, Div, Mul, Neg, Rem, Sub};
use std::ops::{BitAnd, BitOr, BitXor, Not, Shl, Shr};

use num::bigint::BigInt;

use super::abstract_set::{AbstractSet, Container};
use super::abstract_set::{Inum, Unum, _BITS};

// XXX: Distiguish imul/umul idiv/udiv urem/irem
// XXX: Might fail when k bits is one bit (k == 1)

// Avoid integet overflow when k equls to _BITS
macro_rules! min_in_k_bits {
    ($k:expr) => {
        (Inum::min_value()) >> (_BITS - ($k))
    };
}

macro_rules! max_in_k_bits {
    ($k:expr) => {
        (Inum::max_value()) >> (_BITS - ($k))
    };
}

// Because rust does not have logical shift right for signed number,
// This is a hack to generate k-bits mask even k equals to _BITS.
macro_rules! mask_in_k_bits {
    ($k:expr) => {
        (((!min_in_k_bits!($k)) << 1) ^ 1)
    };
}

// Returns the corresponding number of n in k bits
// e.g.
//      0xf0001 in 16 bits --> 1
//      0xf8001 in 16 bits --> -32767
// XXX: Watch out for overflow when k equals to _BITS
// This is a hack
macro_rules! n_in_k_bits {
    ($n:expr, $k:expr) => {
        // Both branches return the same value, but we need a check for overflow.
        if (max_in_k_bits!($k) < ($n) as Inum) || (min_in_k_bits!($k) > ($n) as Inum) {
            radeco_warn!("{:?} cannot be hold by {:?} bits", $n, $k);
            ((((((($n) as Inum) & mask_in_k_bits!($k)) << (_BITS - ($k))) >> (_BITS - 1))
                & (!mask_in_k_bits!($k)))
                | ((($n) as Inum) & mask_in_k_bits!($k)))
        } else {
            (($n) as Inum)
        }
    };
}

/// Euclid's two-thousand-year-old algorithm for finding the greatest common
/// divisor.
fn gcd(x: Inum, y: Inum) -> Inum {
    let mut x = x;
    let mut y = y;
    while y != 0 {
        let t = y;
        y = x % y;
        x = t;
    }
    x
}

/// Returns (s, y) which make sx + ty = gcd(x, y)
fn exgcd(x: Inum, y: Inum) -> (Inum, Inum) {
    if y == 0 {
        (1, 0)
    } else {
        let (s, t) = exgcd(y, x % y);
        (t, s - (x / y) * t)
    }
}

/// A k-bits strided interval s[lb, ub].
///
/// s[lb, ub] := {i| lb <= i <= ub, (i - lb) % s = 0}
/// Without loss of generality, we will assume that all strided intervals are
/// reduced (i.e., upper bounds are tight, and whenever the upper bound equals
/// the lower bound the stride is 0)
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct StridedInterval {
    // Used for indicate radix of strided interval (k-bits)
    // k == 0 means this StridedInterval is an empty set
    pub k: u8,
    // stride of strided interval
    pub s: Inum,
    // lower bound and upper bound of strided interval
    pub lb: Inum,
    pub ub: Inum,
}

//
// Utility functions go here
//

impl fmt::LowerHex for StridedInterval {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}-bits: {:#x}[{:#x}, {:#x}]",
            self.k, self.s, self.lb, self.ub
        )
    }
}

impl fmt::UpperHex for StridedInterval {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}-bits: {:#X}[{:#X}, {:#X}]",
            self.k, self.s, self.lb, self.ub
        )
    }
}

impl fmt::Display for StridedInterval {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-bits: {}[{}, {}]", self.k, self.s, self.lb, self.ub)
    }
}

//
// Help functions go here
//

/// Returns the period number on k-bits-filed
// Try to get the stable step function mentioned in math div.
//      e.g.
//          {-6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6} / 4
//        = {-2, -2, -1, -1, -1, -1, 0, 0, 0, 0, 1, 1, 1}
//        --> period_nr(-5, 4) = -2
//            period_nr(3, 4) = 0
fn period_nr<T>(x: T, period: T) -> T
where
    T: Clone
        + PartialEq
        + Eq
        + Ord
        + From<Inum>
        + Add<Output = T>
        + Sub<Output = T>
        + Rem<Output = T>
        + Div<Output = T>,
{
    // period must be positive
    assert!(period > T::from(0), "Period must be positive");
    // For period == 8:
    // if x >= 0 || x % 8 == 0:
    //      u = x - x % k_period = i * k_period
    //      e.g. 4 bits: 9 -> 8, 8 -> 8, 0 -> 0, -8 -> -8
    // if x < 0 && x % 8 != 0:
    //      u = x - x % k_period - k_period = i * k_period
    //      e.g. 4 bits: -1 -> -8, -7 -> -8, -9 -> -16
    // in which i is a stable step function
    let _x = x.clone()
        - x.clone() % period.clone()
        - if (x < T::from(0)) && (x.clone() % period.clone() != T::from(0)) {
            period.clone()
        } else {
            T::from(0)
        };

    _x.clone() / period.clone()
}

/// Returns the number of trailing zeroes of x
fn ntz(_x: Inum) -> u8 {
    let x = _x as Unum;
    let mut n: u8 = 0;
    let mut y = (!x) & x.wrapping_add(Unum::max_value());
    while y != 0 {
        n += 1;
        y >>= 1;
    }
    n
}

/// Returns the min value in `unsigned` interval ([a, b] | [c, d])
fn min_or(_a: Inum, _b: Inum, _c: Inum, _d: Inum) -> Inum {
    let (mut a, b, mut c, d) = (_a as Unum, _b as Unum, _c as Unum, _d as Unum);
    let mut temp: Unum;

    let mut m = (1 as Unum) << (_BITS - 1);

    while m != 0 {
        if ((!a) & c & m) != 0 {
            temp = (a | m) & (!m).wrapping_add(1);
            if temp <= b {
                a = temp;
                break;
            }
        } else if (a & (!c) & m) != 0 {
            temp = (c | m) & (!m).wrapping_add(1);
            if temp <= d {
                c = temp;
                break;
            }
        }
        m >>= 1;
    }

    (a | c) as Inum
}

/// Returns the max value in `unsigned` interval ([a, b] | [c, d])
fn max_or(_a: Inum, _b: Inum, _c: Inum, _d: Inum) -> Inum {
    let (a, mut b, c, mut d) = (_a as Unum, _b as Unum, _c as Unum, _d as Unum);
    let mut temp: Unum;

    let mut m = (1 as Unum) << (_BITS - 1);

    while m != 0 {
        if (b & d & m) != 0 {
            temp = (b ^ m) | m.wrapping_add(Unum::max_value());
            if temp >= a {
                b = temp;
                break;
            }
            temp = (d ^ m) | m.wrapping_add(Unum::max_value());
            if temp >= c {
                d = temp;
                break;
            }
        }
        m >>= 1;
    }

    (b | d) as Inum
}

//
// Basic functions for StridedInterval go here
//

impl StridedInterval {
    /// Returns the empty set of StridedInterval.
    pub fn null() -> Self {
        StridedInterval::default_k(0)
    }

    /// Returns a default strided interval in k bits.
    pub fn default_k(k: u8) -> Self {
        if k == 0 {
            StridedInterval {
                k: 0,
                s: 0,
                lb: 0,
                ub: 0,
            }
        } else {
            let k = if k > _BITS { _BITS } else { k };
            StridedInterval {
                k: k,
                s: 1,
                lb: min_in_k_bits!(k),
                ub: max_in_k_bits!(k),
            }
        }
    }

    // XXX: Generate StridedInterval only by new and from
    pub fn new(k: u8, s: Inum, lb: Inum, ub: Inum) -> Self {
        let mut si = StridedInterval {
            k: k,
            s: s,
            lb: lb,
            ub: ub,
        };
        si.validate();
        si
    }

    /// Automatically correct variable to generate StridedInterval.
    // Algorithm's correctness is heavily depended on this function
    pub fn validate(&mut self) {
        // Check whether self is empty
        if self.k == 0 {
            self.s = 0;
            self.lb = 0;
            self.ub = 0;
            return;
        }

        // Correct k
        let k = if self.k > _BITS { _BITS } else { self.k };

        // Correct lb/ub
        let _lb = n_in_k_bits!(self.lb, k);
        let _ub = n_in_k_bits!(self.ub, k);
        let (lb, mut ub) = if _lb > _ub {
            radeco_warn!(
                "lb({:?}) is bigger than ub({:?}), exchange these two",
                _lb,
                _ub
            );
            (_ub, _lb)
        } else {
            (_lb, _ub)
        };

        // Correct stride
        let mut s = if let Some(_s) = self.s.checked_abs() {
            _s
        } else {
            radeco_warn!(
                "Strided({:?}) should not be MIN in {:?} bits",
                self.s,
                _BITS
            );
            1
        };
        if s != n_in_k_bits!(s, k) {
            radeco_warn!("Invalid stride({:?})", s);
            s = 1;
        }

        // Reduce Strided Interval
        if (s == 0) && (ub != lb) {
            s = 1;
        }
        if s != 0 {
            // Avoid overflow
            //  e.g.
            //      ub = Inum::max_value()
            //      lb = Inum::min_value()
            //      s = 1
            ub -= ((ub % s + s) % s - (lb % s + s) % s + s) % s;
            if ub == lb {
                s = 0;
            }
        }

        self.k = k;
        self.s = s;
        self.lb = lb;
        self.ub = ub;
    }
}

//
// Operations for StridedInterval go here
//

impl Default for StridedInterval {
    fn default() -> Self {
        StridedInterval {
            k: _BITS,
            s: 1,
            lb: min_in_k_bits!(_BITS),
            ub: max_in_k_bits!(_BITS),
        }
    }
}

// Generate StridedInterval by a constant
impl From<Inum> for StridedInterval {
    fn from(number: Inum) -> Self {
        StridedInterval {
            k: _BITS,
            s: 0,
            lb: n_in_k_bits!(number, _BITS),
            ub: n_in_k_bits!(number, _BITS),
        }
    }
}

impl From<(u8, Inum)> for StridedInterval {
    fn from(number_k: (u8, Inum)) -> Self {
        let mut k = number_k.0;
        if k == 0 {
            StridedInterval::null()
        } else {
            let n = number_k.1;
            k = if (k > _BITS) || (k == 0) { _BITS } else { k };
            StridedInterval {
                k: k,
                s: 0,
                lb: n_in_k_bits!(n, k),
                ub: n_in_k_bits!(n, k),
            }
        }
    }
}

impl Container<Inum> for StridedInterval {
    fn contains(&self, object: &Inum) -> bool {
        if self.is_empty() {
            false
        } else if (*object < self.lb) || (*object > self.ub) {
            false
        } else if self.s == 0 {
            if self.lb == *object {
                true
            } else {
                false
            }
        } else if (*object % self.s == self.lb % self.s)
            || (*object % self.s == self.lb % self.s + self.s)
        {
            // self.lb % s might be negative
            true
        } else {
            false
        }
    }
}

impl Container<StridedInterval> for StridedInterval {
    fn contains(&self, object: &StridedInterval) -> bool {
        if self.is_empty() && object.is_empty() {
            // both are empty set
            true
        } else if self.is_empty() {
            // self is empty but objuect is not
            false
        } else if object.is_empty() {
            // object is empty but self is not
            true
        } else if object.constant().is_some() {
            self.contains(&object.constant().unwrap_or(0))
        } else if object.s % self.s != 0 {
            false
        } else {
            self.contains(&object.lb) && self.contains(&object.ub)
        }
    }
}

impl Container<Vec<Inum>> for StridedInterval {
    fn contains(&self, object: &Vec<Inum>) -> bool {
        object.iter().fold(true, |acc, x| acc & self.contains(x))
    }
}

impl Neg for StridedInterval {
    type Output = Self;

    fn neg(self) -> Self {
        if self.is_empty() {
            radeco_err!("Empty set cannot be used in arithmetical operation");
            StridedInterval::default()
        } else if self.constant().is_some() {
            // Avoid overflow when k equals to _bits
            StridedInterval::from((self.k, (!self.lb).wrapping_add(1)))
        } else if self.lb == min_in_k_bits!(self.k) {
            // -min_in_k_bits == min_in_k_bits on k-bits-filed.
            StridedInterval::new(self.k, self.s, -self.ub, -(self.lb + self.s))
                .join(&StridedInterval::from((self.k, min_in_k_bits!(self.k))))
        } else {
            StridedInterval::new(self.k, self.s, -self.ub, -self.lb)
        }
    }
}

impl Add for StridedInterval {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        if other.is_empty() || self.is_empty() {
            radeco_err!("Empty set cannot be used in arithmetical operation");
            StridedInterval::default()
        } else if self.k != other.k {
            radeco_err!("Addition between two strided intervals with different radices");
            StridedInterval::default()
        } else if (self.constant().is_some()) && (other.constant().is_some()) {
            // two constants
            let n: Inum = n_in_k_bits!(self.lb.wrapping_add(other.lb), self.k);
            StridedInterval::from((self.k, n))
        } else {
            let lb: Inum = n_in_k_bits!(self.lb.wrapping_add(other.lb), self.k);
            let ub: Inum = n_in_k_bits!(self.ub.wrapping_add(other.ub), self.k);
            // u < 0 indicates:
            //      self.lb + other.lb < min_value &&
            //          self.ub + other.ub > min_value (on k-bits space)
            let u: Inum = n_in_k_bits!(
                self.lb & other.lb & !lb & !(self.ub & other.ub & !ub),
                self.k
            );
            // v < 0 indicates:
            //      min_value <= self.lb + other.lb <= max_value &&
            //          self.ub + other.ub > max_value (on k-bits space)
            let v: Inum = n_in_k_bits!(
                ((self.lb ^ other.lb) | !(self.lb ^ lb)) & (!self.ub & !other.ub & ub),
                self.k
            );
            if (u < 0) || (v < 0) {
                StridedInterval::default_k(self.k)
            } else {
                StridedInterval::new(self.k, gcd(self.s, other.s), lb, ub)
            }
        }
    }
}

impl Sub for StridedInterval {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        self + (-other)
    }
}

impl Mul for StridedInterval {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        if other.is_empty() || self.is_empty() {
            radeco_err!("Empty set cannot be used in arithmetical operation");
            StridedInterval::default()
        } else if self.k != other.k {
            radeco_err!("Multiplication between two strided intervals with different radices");
            StridedInterval::default()
        } else if (self.constant().is_some()) && (other.constant().is_some()) {
            // Two constants
            StridedInterval::from((self.k, n_in_k_bits!(self.lb.wrapping_mul(other.lb), self.k)))
        // Following code is used to handle multipatication with at least one set.
        // The main idea is to find the max bound and min bound, then check whether
        // these two bounds are in the same k-bits-filed.
        //      e.g.
        //          For 4 bits: [-8, 7], [8, 15], [-16, -9] is a 4-bit-filed.
        //          Thus, (9, 14) are in the same 4-bit-filed, even though there is overflow.
        //          On the other hand, (9, 16) is not in the same 4-bit-filed.
        } else if (self.constant().is_some()) || (other.constant().is_some()) {
            // A constant multipate by a set.
            let (cons_si, set_si) = if self.constant().is_some() {
                (&self, &other)
            } else {
                (&other, &self)
            };

            // XXX: Remove Bigint to improve performance
            // XXX: BigInt uses a non-common presentation (in math, not computer
            // science) for Rem and Div:
            //      e.g.
            //          -13 % 8 = -5
            //          -13 / 8 = -1
            //          13 % 8 = 5
            //          13 / 8 = 1
            //      We need some tricks to change it into something like:
            //      e.g.
            //          -13 % 8 = 3
            //          -13 / 8 = -2
            //          13 % 8 = 5
            //          13 / 8 = 1
            //      Thus, we will have a stable step function, like
            //      e.g.
            //          {-4, -3, -2, -1, 0, 1, 2, 3, 4, 5} / 2 =
            //              {-2, -2, -1, -1, 0, 0, 1, 1, 2, 2}

            // Cannot fail in expect
            let n = cons_si
                .constant()
                .expect("StridedInterval is not a constant");
            if n == 0 {
                StridedInterval::from((self.k, 0))
            } else {
                let (_lb, _ub) = if n > 0 {
                    (BigInt::from(set_si.lb), BigInt::from(set_si.ub))
                } else {
                    (BigInt::from(set_si.ub), BigInt::from(set_si.lb))
                };
                let _n = BigInt::from(n);
                // e.g.
                //      4 bits: k_period = 8
                //      5 bits: k_period = 16
                let k_period: BigInt = BigInt::from(max_in_k_bits!(self.k)) + 1;

                // New bounds for Mul
                let __u = _lb.clone() * _n.clone();
                let __v = _ub.clone() * _n.clone();

                let u = period_nr(__u, k_period.clone());
                let v = period_nr(__v, k_period.clone());

                // Check new bounds are inside the same round of k bits
                // (-3, -2) (-1, 0) (1, 2) is in the same round of k bits
                if v.clone() == u.clone()
                    || ((v.clone() % 2 == BigInt::from(0)) && (v - u == BigInt::from(1)))
                {
                    StridedInterval::new(
                        self.k,
                        n_in_k_bits!(set_si.s * n.wrapping_abs(), self.k),
                        n_in_k_bits!(set_si.lb.wrapping_mul(n), self.k),
                        n_in_k_bits!(set_si.ub.wrapping_mul(n), self.k),
                    )
                } else {
                    StridedInterval::default_k(self.k)
                }
            }
        } else {
            // Both are set
            let _poles = vec![
                (
                    BigInt::from(self.lb) * BigInt::from(other.lb),
                    self.lb,
                    other.lb,
                ),
                (
                    BigInt::from(self.lb) * BigInt::from(other.ub),
                    self.lb,
                    other.ub,
                ),
                (
                    BigInt::from(self.ub) * BigInt::from(other.lb),
                    self.ub,
                    other.lb,
                ),
                (
                    BigInt::from(self.ub) * BigInt::from(other.ub),
                    self.ub,
                    other.ub,
                ),
            ];

            // Similar with above code
            let _max = _poles
                .iter()
                .fold(None, |max, x| match max {
                    None => Some(x),
                    Some(y) => Some(if x.0 > y.0 { x } else { y }),
                })
                .expect("No elements in _poles");
            let _min = _poles
                .iter()
                .fold(None, |min, x| match min {
                    None => Some(x),
                    Some(y) => Some(if x.0 < y.0 { x } else { y }),
                })
                .expect("No elements in _poles");

            let k_period: BigInt = BigInt::from(max_in_k_bits!(self.k)) + 1;

            let u = period_nr(_min.0.clone(), k_period.clone());
            let v = period_nr(_max.0.clone(), k_period.clone());

            if v.clone() == u.clone()
                || ((v.clone() % 2 == BigInt::from(0)) && (v - u == BigInt::from(1)))
            {
                StridedInterval::new(
                    self.k,
                    gcd(self.s, other.s),
                    n_in_k_bits!(_min.1.wrapping_mul(_min.2), self.k),
                    n_in_k_bits!(_max.1.wrapping_mul(_max.2), self.k),
                )
            } else {
                StridedInterval::default_k(self.k)
            }
        }
    }
}

impl Div for StridedInterval {
    type Output = Self;

    // Div is difficult because to many special cases.
    fn div(self, other: Self) -> Self {
        if other.is_empty() || self.is_empty() {
            radeco_err!("Empty set cannot be used in arithmetical operation");
            StridedInterval::default()
        } else if self.k != other.k {
            radeco_err!("Division between two strided intervals with different radices");
            StridedInterval::default()
        } else if other.contains(&0) {
            // Divided by zero
            radeco_err!("Divied by zero");
            StridedInterval::default_k(self.k)
        } else if self.contains(&min_in_k_bits!(self.k)) && other.contains(&-1) {
            // overflow, because -min_in_k_bits!(self.k) not in k-bits rand
            StridedInterval::default_k(self.k)
        } else if (self.constant().is_some()) && (other.constant().is_some()) {
            // Two constants
            StridedInterval::from((self.k, self.lb / other.lb))
        } else if other.constant().is_some() {
            // A constant divided by a set.
            let n = other.lb;
            if self.s % n != 0 {
                // if self.s % other.constant() != 0, the stride should be 1
                StridedInterval::new(self.k, 1, self.lb / n, self.ub / n)
            } else {
                // if self.s % other.constant() == 0
                // the situations when self.lb % n != 0 could be split into:
                //      * self.lb < 0 && self.ub < 0: stride == self.s / n
                //      * self.lb >=0 && self.ub >=0: stride == self.s / n
                //      * self.lb < 0 && self.ub > 0: stride == 1
                //          .e.g.
                //              4[-7, 5] / 2 -> {-7, -3, 1, 5} / 2 -> {-3, -1, 0, 2}
                if (self.lb % n != 0) && (self.lb < 0) && (self.ub > 0) {
                    StridedInterval::new(self.k, 1, self.lb / n, self.ub / n)
                } else {
                    StridedInterval::new(self.k, self.s / n, self.lb / n, self.ub / n)
                }
            }
        } else {
            // Divition between two sets
            let mut _poles = vec![
                self.lb / other.lb,
                self.ub / other.lb,
                self.lb / other.ub,
                self.ub / other.ub,
            ];
            if (other.lb < 0) && (other.ub > 0) {
                let u = other.ub % other.s;
                let v = u - other.s;
                _poles.extend_from_slice(&[self.lb / u, self.lb / v, self.ub / u, self.ub / v])
            }
            let _max = _poles
                .iter()
                .fold(None, |max, x| match max {
                    None => Some(x),
                    Some(y) => Some(if x > y { x } else { y }),
                })
                .expect("No elements in _poles");
            let _min = _poles
                .iter()
                .fold(None, |min, x| match min {
                    None => Some(x),
                    Some(y) => Some(if x < y { x } else { y }),
                })
                .expect("No elements in _poles");

            StridedInterval::new(self.k, 1, *_min, *_max)
        }
    }
}

impl Rem for StridedInterval {
    type Output = Self;

    fn rem(self, other: Self) -> Self {
        if other.is_empty() || self.is_empty() {
            radeco_err!("Empty set cannot be used in arithmetical operation");
            StridedInterval::default()
        } else if self.k != other.k {
            radeco_err!("Remainder between two strided intervals with different radices");
            StridedInterval::default()
        } else if other.contains(&0) {
            // Divided by zero
            radeco_err!("Divied by zero");
            StridedInterval::default_k(self.k)
        } else if (self.constant().is_some()) && (other.constant().is_some()) {
            StridedInterval::from((self.k, self.lb % other.lb))
        } else if other.constant().is_some() {
            // Divided by one constant

            // XXX: Trick
            // Use the negative value of n, to avoid overflow
            let neg_n = if other.lb < 0 { other.lb } else { -other.lb };

            if (neg_n <= self.lb) && (neg_n.saturating_add(self.ub) <= 0) {
                // n covers all the value in self, but maybe except self.lb/self.ub:
                //              -n <= self.lb <= self.ub <= n
                let lb = if neg_n == self.lb {
                    // n doesn't cover self.lb
                    self.lb + self.s
                } else {
                    self.lb
                };
                let ub = if neg_n.saturating_add(self.ub) == 0 {
                    // n doesn't cover self.ub
                    self.ub - self.s
                } else {
                    self.ub
                };
                let si = StridedInterval::new(self.k, self.s, lb, ub);
                if (self.lb != lb) || (self.ub != ub) {
                    // self.lb/self.ub % n == 0
                    si.join(&StridedInterval::from((self.k, 0)))
                } else {
                    si
                }
            } else {
                // When we get into this branch, it means n cannot be min_in_k_bits!(self.k).
                // Nice! Finally we could use positive value.
                let n = -neg_n;
                let s = gcd(self.s, n);

                if self.lb / n == self.ub / n {
                    // All numbers are in the same n-filed
                    //      e.g.
                    //          [0x31, 0x3a] % 0x10
                    StridedInterval::new(self.k, self.s, self.lb % n, self.ub % n)
                } else if s == n {
                    // self.s % n == 0, which means the remainder could only be one
                    // or two number.
                    // -41 % 10 == -1
                    // 39 % 10 = 9
                    if (self.lb < 0) && (self.ub > 0) {
                        StridedInterval::new(self.k, n, self.lb % n, self.ub % n)
                    } else {
                        StridedInterval::from((self.k, self.lb % n))
                    }
                } else {
                    // _pos_lb: the smallest non-negative number when mod s
                    // _nes_lb: the smallest negative number when mod s
                    let (_pos_lb, _neg_lb) = if self.lb % s <= 0 {
                        ((self.lb % s + s) % s, self.lb % s + s - n)
                    } else {
                        (self.lb % s, self.lb % s - n)
                    };

                    if (self.lb < 0) && (self.ub > 0) {
                        // -n < i < n
                        StridedInterval::new(self.k, s, _neg_lb, n - 1)
                    } else if (self.lb < 0) && (self.ub <= 0) {
                        // -n < i <= 0
                        StridedInterval::new(self.k, s, _neg_lb, 0)
                    } else {
                        // 0 <= i < n
                        StridedInterval::new(self.k, s, _pos_lb, n - 1)
                    }
                }
            }
        } else {
            // Divided by a set

            // Avoid overflow
            let _lb = if other.lb < 0 {
                !other.lb
            } else {
                other.lb - 1
            };
            let _ub = if other.ub < 0 {
                !other.ub
            } else {
                other.ub - 1
            };
            let n_sub_one = cmp::max(_lb, _ub);

            if self.lb >= 0 {
                // all number in self is non-negative
                StridedInterval::new(self.k, 1, 0, n_sub_one)
            } else if self.ub <= 0 {
                // all number in self is non-positive
                StridedInterval::new(self.k, 1, -n_sub_one, 0)
            } else {
                StridedInterval::new(self.k, 1, -n_sub_one, n_sub_one)
            }
        }
    }
}

impl BitAnd for StridedInterval {
    type Output = Self;

    fn bitand(self, other: Self) -> Self {
        if other.is_empty() || self.is_empty() {
            radeco_err!("Empty set cannot be used in arithmetical operation");
            return StridedInterval::default();
        }
        // In most common situation, bitand work as a mask.
        // Thus, we take care of this special case.
        if self.k == other.k {
            if self.constant().is_some() || other.constant().is_some() {
                let (cons_si, set_si) = if self.constant().is_some() {
                    (&self, &other)
                } else {
                    (&other, &self)
                };
                let n = cons_si.constant().unwrap();
                if (n & (n.wrapping_add(1))) == 0 {
                    if n == 0 {
                        return StridedInterval::from((self.k, 0));
                    } else if n == -1 {
                        return set_si.clone();
                    } else {
                        return *set_si % StridedInterval::from((self.k, n + 1));
                    }
                }
            }
        }
        !((!self) | (!other))
    }
}

impl BitOr for StridedInterval {
    type Output = Self;

    fn bitor(self, other: Self) -> Self {
        if other.is_empty() || self.is_empty() {
            radeco_err!("Empty set cannot be used in arithmetical operation");
            StridedInterval::default()
        } else if self.k != other.k {
            radeco_err!("BitOr between two strided intervals with different radices");
            StridedInterval::default()
        } else if (self.constant().is_some()) && (other.constant().is_some()) {
            // Two constants
            StridedInterval::new(self.k, 0, self.lb | other.lb, self.lb | other.lb)
        } else if self.constant() == Some(0) {
            // self is 0
            other.clone()
        } else if other.constant() == Some(0) {
            // other is 0
            self.clone()
        } else if (self.constant() == Some(-1)) || (other.constant() == Some(-1)) {
            // constant is -1
            StridedInterval::from((self.k, -1))
        } else {
            // Two sets

            // self.s and other.s cannot both be zero at this situation.
            let t = cmp::min(ntz(self.s), ntz(other.s));
            assert!(
                t < self.k,
                "The number of trailing zeroes must be smaller than k"
            );
            // s must be a positive number, because self.s and other.s are positive.
            let s: Inum = (1 as Inum) << t;
            assert!(s > 0, "ntz(k) must be positive");
            let mask = s - 1;
            let r = (self.lb & mask) | (other.lb & mask);

            // Signed min_or goes like following table
            //      si1: s1[a, b]; si2 s2[c, d]
            // ------------------------------------------------------------
            //   a |  b |  c |  d |     signed min_or    |   signed max_or
            // ------------------------------------------------------------
            //  <0 | <0 | <0 | <0 | min_or(a, b, c, d)   | max_or(a, b, c, d)
            //  <0 | <0 | <0 | >=0| a                    | -1
            //  <0 | <0 | >=0| >=0| min_or(a, b, c, d)   | max_or(a, b, c, d)
            //  <0 | >=0| <0 | <0 | c                    | -1
            //  <0 | >=0| <0 | >=0| min(a, c)            | max_or(0, b, 0, d)
            //  <0 | >=0| >=0| >=0| min_or(a, -1, c, d)  | max_or(0, b, c, d)
            //  >=0| >=0| <0 | <0 | min_or(a, b, c, d)   | max_or(a, b, c, d)
            //  >=0| >=0| <0 | >=0| min_or(a, b, c, -1)  | max_or(a, b, 0, d)
            //  >=0| >=0| >=0| >=0| min_or(a, b, c, d)   | max_or(a, b, c, d)

            let (a, b, c, d) = (
                self.lb & (!mask),
                self.ub & (!mask),
                other.lb & (!mask),
                other.ub & (!mask),
            );
            let (lb, ub) = match (a < 0, b < 0, c < 0, d < 0) {
                (true, true, true, true) => (min_or(a, b, c, d), max_or(a, b, c, d)),
                (true, true, true, false) => (a, -1),
                (true, true, false, false) => (min_or(a, b, c, d), max_or(a, b, c, d)),
                (true, false, true, true) => (c, -1),
                (true, false, true, false) => (cmp::min(a, c), max_or(0, b, 0, d)),
                (true, false, false, false) => (min_or(a, -1, c, d), max_or(0, b, c, d)),
                (false, false, true, true) => (min_or(a, b, c, d), max_or(a, b, c, d)),
                (false, false, true, false) => (min_or(a, b, c, -1), max_or(a, b, 0, d)),
                (false, false, false, false) => (min_or(a, b, c, d), max_or(a, b, c, d)),
                _ => {
                    unimplemented!();
                }
            };

            StridedInterval::new(
                self.k,
                n_in_k_bits!(s, self.k),
                n_in_k_bits!(((lb & (!mask)) | r), self.k),
                n_in_k_bits!(((ub & (!mask)) | r), self.k),
            )
        }
    }
}

impl BitXor for StridedInterval {
    type Output = Self;

    fn bitxor(self, other: Self) -> Self {
        (!((!self) | other)) | (!((!other) | self))
    }
}

impl Not for StridedInterval {
    type Output = Self;

    fn not(self) -> Self {
        if self.is_empty() {
            radeco_err!("Empty set cannot be used in arithmetical operation");
            StridedInterval::default()
        } else {
            StridedInterval::new(
                self.k,
                self.s,
                n_in_k_bits!(!self.ub, self.k),
                n_in_k_bits!(!self.lb, self.k),
            )
        }
    }
}

impl Shr for StridedInterval {
    type Output = Self;

    // All shift right in ESIl is logical shift
    fn shr(self, other: Self) -> Self {
        if other.is_empty() || self.is_empty() {
            radeco_err!("Empty set cannot be used in arithmetical operation");
            StridedInterval::default()
        } else if other.lb < 0 {
            radeco_err!("Bitwise shift's operation cannot be negative");
            StridedInterval::default_k(self.k)
        } else if (self.lb < 0) && (self.ub > 0) {
            // Logical shift will distroy the stride when there are negative and
            // positive numbers in self at one time

            let lb = if other.contains(&0) {
                // self.lb will be the smallest number in result strided interval
                self.lb
            } else {
                // _lb_shr_1 is the smallest number in self >> 1
                let _lb_shr_1 = cmp::min(
                    (self.lb >> 1) ^ min_in_k_bits!(self.k),
                    (self.ub - (self.ub / self.s) * self.s) >> 1,
                );
                _lb_shr_1 >> (other.ub - 1)
            };
            let ub = if other.ub == 0 {
                // other == 0[0, 0]
                self.ub
            } else {
                // _ub_shr_1 is the biggest number in self >> 1
                let _ub_shr_1 = cmp::max(
                    ((self.ub - (self.ub / self.s) * self.s - self.s) >> 1)
                        ^ min_in_k_bits!(self.k),
                    self.ub >> 1,
                );
                if other.lb == 0 {
                    cmp::max(self.ub, _ub_shr_1 >> (other.s - 1))
                } else {
                    _ub_shr_1 >> (other.lb - 1)
                }
            };
            StridedInterval::new(
                self.k,
                1,
                n_in_k_bits!(lb, self.k),
                n_in_k_bits!(ub, self.k),
            )
        } else {
            // all numbers in self are with the same sign
            let mut res: Option<StridedInterval> = None;
            let _shr_one = StridedInterval::new(
                self.k,
                cmp::max(self.s / 2, 1),
                (self.lb >> 1) & (!min_in_k_bits!(self.k)) & mask_in_k_bits!(self.k),
                (self.ub >> 1) & (!min_in_k_bits!(self.k)) & mask_in_k_bits!(self.k),
            );
            for i in 0..self.k {
                if other.contains(&(i as Inum)) {
                    let temp = if i == 0 {
                        self.clone()
                    } else {
                        _shr_one / StridedInterval::from((self.k, 1 << (i - 1)))
                    };
                    if res.is_none() {
                        res = Some(temp);
                    } else {
                        res = Some(res.unwrap().join(&temp));
                    }
                }
            }
            if other.ub >= self.k as Inum {
                res.unwrap().join(&StridedInterval::from((self.k, 0)))
            } else {
                res.unwrap()
            }
        }
    }
}

impl Shl for StridedInterval {
    type Output = Self;

    fn shl(self, other: Self) -> Self {
        if other.is_empty() || self.is_empty() {
            radeco_err!("Empty set cannot be used in arithmetical operation");
            StridedInterval::default()
        } else if other.lb < 0 {
            radeco_err!("Bitwise shift's operation cannot be negative");
            StridedInterval::default_k(self.k)
        } else {
            let mut res: Option<StridedInterval> = None;
            for i in 0..self.k {
                if other.contains(&(i as Inum)) {
                    let temp = self * StridedInterval::from((self.k, 1 << i));
                    if res.is_none() {
                        res = Some(temp);
                    } else {
                        res = Some(res.unwrap().join(&temp));
                    }
                }
            }
            if other.ub >= self.k as Inum {
                res.unwrap().join(&StridedInterval::from((self.k, 0)))
            } else {
                res.unwrap()
            }
        }
    }
}

// Implement trait AbstractSet for StridedInterval
impl AbstractSet for StridedInterval {
    fn meet(&self, other: &Self) -> Self {
        if self.is_empty() || other.is_empty() {
            // meet an empty set --> empty set
            StridedInterval::null()
        } else if self.k != other.k {
            radeco_err!("Meet two strided intervals with different radices");
            StridedInterval::default()
        } else if let Some(n) = self.constant() {
            // self is a constant
            if other.contains(&n) {
                StridedInterval::from((self.k, n))
            } else {
                StridedInterval::null()
            }
        } else if let Some(n) = other.constant() {
            // other is a constant
            if self.contains(&n) {
                StridedInterval::from((self.k, n))
            } else {
                StridedInterval::null()
            }
        } else {
            // Meet two set
            // It comes to a very insteresting math problem.

            // self.s and othr.s cannot be zero in this branch
            let s = self.s / gcd(self.s, other.s) * other.s;

            // At beginning, we need to find the smallest number in result set
            if self.lb == other.lb {
                // self.lb is the smallest number in result
                StridedInterval::new(self.k, s, self.lb, cmp::max(self.ub, other.ub))
            } else {
                let (min_lb, min_s, max_lb, max_s) = if self.lb < other.lb {
                    (self.lb, self.s, other.lb, other.s)
                } else {
                    (other.lb, other.s, self.lb, self.s)
                };
                assert!(min_lb < max_lb && min_s > 0 && max_s > 0);
                // For the smallest number n:
                //      min_lb + x * min_s = max_lb + y * max_s = n
                //      ( x >= 0, y >= 0)
                //  =>  max_lb - min_lb = x * min_s - y * max_s
                //  let: s_ = gcd(min_s, max_s)
                //  if (max_lb - min_lb) % s_ != 0
                //      then there is no number in the result set
                //  let: lb_ = (max_lb - min_lb) / s_
                //      --> lb_ != 0
                //  let: min_s_ = min_s / s_
                //  let: max_s_ = max_s / s_
                //      --> gcd(min_s_, max_s_) = 1
                //  =>  lb_ = x * min_s_ - y * max_s_
                //  =>  lb_ % max_s_ = (x * min_s_) % max_s_
                //      because gcd(min_s_, max_s_) = 1, thus we have
                //          (rev_min_s_ * min_s_) % max_s_ = 1
                //  => x % max_s_ = ((lb_ % max_s_) * rev_min_s_) % max_s_
                //  As we want to find the smallest number, which means
                //      x should be as small as it can
                //  let x_ = x % max_s_, then we solve the equation:
                //      lb_ = x_ * min_s_ - y_ * max_s_
                //  thus:
                //      x = x_ + i * max_s_
                //      y = y_ + i * min_s_
                //  then we found the smallest positive i, which make x > 0 && y > 0
                //  n = min_lb + x * min_s

                let s_ = gcd(min_s, max_s);
                if (max_lb - min_lb) % s_ != 0 {
                    StridedInterval::null()
                } else {
                    // lb_ = x * min_s_ - y * max_s_
                    let lb_ = (max_lb - min_lb) / s_;
                    let max_s_ = max_s / s_;
                    let min_s_ = min_s / s_;
                    assert!(lb_ != 0);
                    assert!(gcd(max_s_, min_s_) == 1);

                    // x_ = x % max_s_ = ((lb_ % max_s_) * rev_min_s_) % max_s_
                    let (a, b) = exgcd(min_s_, max_s_);
                    // a * min_s_ + b * min_s_ = gcd(max_s_, min_s_) = 1
                    assert_eq!(a * min_s_ + b * max_s_, gcd(min_s_, max_s_));
                    let rev_min_s_ = a - period_nr(a, max_s_) * max_s_;
                    assert_eq!((rev_min_s_ * min_s_) % max_s_, 1);
                    let x_ = ((lb_ % max_s_) * rev_min_s_) % max_s_;
                    assert!(x_ > 0);

                    // let x_ = x % max_s_, then we solve the equation:
                    //     lb_ = x_ * min_s_ - y_ * max_s_
                    if (x_ * min_s_ - lb_) % max_s_ != 0 {
                        // Cannot find the integer ans for the equation
                        return StridedInterval::null();
                    }
                    let y_ = (x_ * min_s_ - lb_) / max_s_;

                    // thus:
                    //     x = x_ + i * max_s_
                    //     y = y_ + i * min_s_
                    // then we found the smallest positive i, which make x > 0 && y > 0
                    // n = min_lb + x * min_s
                    let (x, y) = if y_ < 0 {
                        let y = y_ - period_nr(y_, min_s_) * min_s_;
                        let i = (y - y_) / min_s_;
                        (x_ + i * max_s_, y)
                    } else {
                        (x_, y_)
                    };

                    assert_eq!(min_lb + x * min_s, max_lb + y * max_s);
                    assert!(x > 0 && y > 0);
                    let n = min_lb + x * min_s;
                    if self.contains(&n) && other.contains(&n) {
                        StridedInterval::new(self.k, s, n, cmp::max(self.ub, other.ub))
                    } else {
                        StridedInterval::null()
                    }
                }
            }
        }
    }

    fn join(&self, other: &Self) -> Self {
        if self.is_empty() {
            other.clone()
        } else if other.is_empty() {
            self.clone()
        } else if self.k != other.k {
            radeco_err!("Join two strided intervals with different radices");
            StridedInterval::default()
        } else {
            let mut s = gcd(self.s, other.s);
            // Trick to avoid overflow
            let (min_lb, max_lb) = (cmp::min(self.lb, other.lb), cmp::max(self.lb, other.lb));
            // add 2 * s to make sure all arguments in gcd is positive
            s = gcd(s, max_lb % s - min_lb % s + 2 * s);
            StridedInterval::new(
                self.k,
                s,
                cmp::min(self.lb, other.lb),
                cmp::max(self.ub, other.ub),
            )
        }
    }

    fn widen(&self, other: &Self) -> Self {
        if self.is_empty() {
            other.clone()
        } else if other.is_empty() {
            self.clone()
        } else if self.k != other.k {
            radeco_err!("Widen two strided intervals with different radices");
            StridedInterval::default()
        } else {
            let s = gcd(self.s, other.s);
            let lb = if self.lb <= other.lb {
                self.lb
            } else {
                min_in_k_bits!(self.k)
            };
            let ub = if self.ub >= other.ub {
                self.ub
            } else {
                max_in_k_bits!(self.k)
            };
            StridedInterval::new(self.k, s, lb, ub)
        }
    }

    fn remove_lower_bound(&self) -> Self {
        if self.is_empty() {
            self.clone()
        } else {
            let min = min_in_k_bits!(self.k);
            let ub_mod_s = self.ub % self.s;
            let min_mod_s = min % self.s; // min_mod_s must be negative
            let offset = (ub_mod_s - min_mod_s + self.s) % self.s;
            assert!(min_mod_s <= 0, "min_in_k_bits must be non-positive");
            StridedInterval::new(self.k, self.s, n_in_k_bits!(min + offset, self.k), self.ub)
        }
    }

    fn set_lower_bound(&self, x: Inum) -> Self {
        let mut si = self.clone();
        if self.is_empty() {
            si
        } else if x > si.ub {
            radeco_warn!("Set a lower bound which is bigger than upper bound");
            StridedInterval::null()
        } else {
            let offset = self.ub - period_nr(self.ub, self.s) * self.s;
            let offset_ = x - period_nr(x, self.s) * self.s;
            si.lb = if offset >= offset_ {
                x + offset - offset_
            } else {
                x + offset - offset_ + self.s
            };
            si.validate();
            si
        }
    }

    fn remove_upper_bound(&self) -> Self {
        if self.is_empty() {
            self.clone()
        } else {
            StridedInterval::new(self.k, self.s, self.lb, max_in_k_bits!(self.k))
        }
    }

    fn set_upper_bound(&self, x: Inum) -> Self {
        let mut si = self.clone();
        if self.is_empty() {
            si
        } else if x < si.lb {
            radeco_warn!("Set a upper bound which is smaller than lower bound");
            StridedInterval::null()
        } else {
            si.ub = x;
            si.validate();
            si
        }
    }

    fn narrow(&self, k: u8) -> Self {
        if self.is_empty() {
            radeco_warn!("Empty StridedInterval cannot be narrow");
            self.clone()
        } else if k > self.k {
            radeco_warn!("StridedInterval cannot be narrowed to a bigger bits");
            self.clone()
        } else if k == self.k {
            self.clone()
        } else {
            // all numbers in si should be positive in self.k-bits-filed
            let mut si = *self & StridedInterval::from((self.k, mask_in_k_bits!(k)));

            // k cannot be _BITS in this branch
            let division_k = min_in_k_bits!(k) & mask_in_k_bits!(k);
            assert!(division_k > 0, "division_k must be positive");
            if si.ub < division_k {
                // all number is si are positive in k-bits-filed
                si.k = k;
                si.validate();
                si
            } else if si.lb >= division_k {
                // all number is si are negative in k-bits-filed
                StridedInterval::new(k, si.s, n_in_k_bits!(si.ub, k), n_in_k_bits!(si.lb, k))
            } else {
                // number is si are both negative and positive in k-bits-filed
                let pos_si = StridedInterval::new(k, si.s, si.lb, max_in_k_bits!(k));
                let neg_si = StridedInterval::new(
                    k,
                    si.s,
                    n_in_k_bits!(si.ub, k) - si.s,
                    n_in_k_bits!(si.ub, k),
                )
                .remove_lower_bound();
                pos_si.join(&neg_si)
            }
        }
    }

    fn sign_extend(&self, k: u8) -> Self {
        let mut si = self.clone();
        if self.is_empty() {
            radeco_warn!("Empty StridedInterval cannot be extended");
            si
        } else if k < self.k {
            radeco_warn!("StridedInterval cannot be extended to a smaller bits");
            si
        } else {
            si.k = k;
            si.validate();
            si
        }
    }

    fn zero_extend(&self, k: u8) -> Self {
        let mut si = self.clone();
        if self.is_empty() {
            radeco_warn!("Empty StridedInterval cannot be narrow");
            si
        } else if k < self.k {
            radeco_warn!("StridedInterval cannot be extended to a smaller bits");
            si
        } else if k == self.k {
            si
        } else {
            if si.ub < 0 {
                // all number in si is negative
                si.lb = n_in_k_bits!(si.lb & mask_in_k_bits!(si.k), k);
                si.ub = n_in_k_bits!(si.ub & mask_in_k_bits!(si.k), k);
                si.k = k;
                si.validate();
                si
            } else if si.lb >= 0 {
                // all number in si is non-negative
                // thus we can change the k-bits directly
                si.k = k;
                si.validate();
                si
            } else {
                // number is si are both negative and positive in k-bits-filed
                let pos_si = StridedInterval::new(k, si.s, si.ub % si.s, si.ub);
                let neg_si = StridedInterval::new(
                    k,
                    si.s,
                    n_in_k_bits!(si.lb & mask_in_k_bits!(si.k), k),
                    n_in_k_bits!(Inum::from(-1) & mask_in_k_bits!(si.k), k),
                );
                pos_si.join(&neg_si)
            }
        }
    }

    fn constant(&self) -> Option<Inum> {
        if self.is_empty() {
            None
        } else if (self.s != 0) || (self.lb != self.ub) {
            None
        } else {
            Some(self.lb)
        }
    }

    // XXX: overflow when capacity > Inum::max_value()
    fn capacity(&self) -> Inum {
        if self.is_empty() {
            0
        } else if self.s == 0 {
            1
        } else {
            (self.ub - self.lb) / self.s + 1
        }
    }

    fn is_empty(&self) -> bool {
        self.k == 0
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! check_overflow {
        ($res:expr, $k:expr) => {
            if let Some(_res) = $res {
                if (_res < min_in_k_bits!($k)) || (_res > max_in_k_bits!($k)) {
                    true
                } else {
                    false
                }
            } else {
                true
            }
        };
    }

    #[test]
    fn strided_interval_test_marco() {
        assert_eq!(-32768, min_in_k_bits!(16));
        assert_eq!(32767, max_in_k_bits!(16));
        assert_eq!(-9223372036854775808, min_in_k_bits!(64));
        assert_eq!(9223372036854775807, max_in_k_bits!(64));
        assert_eq!(0xffff, mask_in_k_bits!(16));
        assert_eq!(-1i64, mask_in_k_bits!(64));
        assert_eq!(0xdeadbeef, n_in_k_bits!(0xdeadbeef, 64));
        assert_eq!(1, n_in_k_bits!(0xf0001, 16));
        assert_eq!(-32767, n_in_k_bits!(0xf8001, 16));
        assert_eq!(
            false,
            check_overflow!((0 as Inum).checked_add(0xdeadbeef), 64)
        );
        assert_eq!(true, check_overflow!(Inum::max_value().checked_add(1), 64));
        assert_eq!(true, check_overflow!((100 as Inum).checked_add(100), 8));
    }

    #[test]
    fn strided_interval_test_basicfn() {
        assert_eq!(ntz(0x0001000), 12);
        assert_eq!(ntz(0x0), 64);
        assert_eq!(ntz(Inum::min_value()), 63);
        assert_eq!(
            min_or(0x0000101, 0x0001001, 0x0010011, 0x0101001),
            0x0010101
        );
        assert_eq!(
            max_or(0x0000101, 0x0001001, 0x0010011, 0x0101001),
            0x0101fff
        );
        let (x, y) = (3, 8);
        let (s, t) = exgcd(x, y);
        assert_eq!(x * s + y * t, gcd(x, y));
        let (x, y) = (8, 3);
        let (s, t) = exgcd(x, y);
        assert_eq!(x * s + y * t, gcd(x, y));
        let (x, y) = (3, 9);
        let (s, t) = exgcd(x, y);
        assert_eq!(x * s + y * t, gcd(x, y));
        let (x, y) = (3, 0);
        let (s, t) = exgcd(x, y);
        assert_eq!(x * s + y * t, gcd(x, y));
        assert_eq!(
            StridedInterval {
                k: _BITS,
                s: 1,
                lb: -9223372036854775808,
                ub: 9223372036854775807
            },
            StridedInterval::default()
        );
        assert_eq!(
            StridedInterval {
                k: _BITS,
                s: 1,
                lb: Inum::min_value(),
                ub: Inum::max_value()
            },
            StridedInterval::default_k(_BITS)
        );
        assert_eq!(
            StridedInterval {
                k: 8,
                s: 1,
                lb: -128,
                ub: 127
            },
            StridedInterval::default_k(8)
        );
        assert_eq!(
            StridedInterval {
                k: _BITS,
                s: 0,
                lb: 0xdeadbeef,
                ub: 0xdeadbeef
            },
            StridedInterval::from(0xdeadbeef)
        );
        assert_eq!(
            StridedInterval {
                k: 0,
                s: 0,
                lb: 0,
                ub: 0
            }, // empty set
            StridedInterval::new(0, -3, -2, 29)
        );
        assert!(StridedInterval::new(64, 4, 0, 4096).contains(&1024));
        assert!(!StridedInterval::new(64, 4, 0, 4096).contains(&1022));
        assert!(!StridedInterval::new(64, 4, 0, 4096).contains(&-4));
        assert!(StridedInterval::new(64, 4, 0, 4096).contains(&vec![0, 4, 16, 20]));
        assert!(!StridedInterval::new(64, 4, 0, 4096).contains(&vec![0, 4, 14, 20]));
        assert!(StridedInterval::new(64, 4, 0, 4096).contains(&StridedInterval::from(16)));
        assert!(!StridedInterval::null().contains(&StridedInterval::from(16)));
        assert!(StridedInterval::new(64, 4, 0, 4096).contains(&StridedInterval::null()));
        assert!(StridedInterval::null().contains(&StridedInterval::null()));
        assert!(!StridedInterval::new(64, 4, 0, 4096).contains(&StridedInterval::from(-16)));
        assert!(!StridedInterval::new(64, 4, 0, 4096).contains(&StridedInterval::from(1022)));
        assert!(
            StridedInterval::new(64, 7, 0, 7000).contains(&StridedInterval::new(64, 49, 0, 490))
        );
        assert!(
            !StridedInterval::new(64, 7, 0, 7000).contains(&StridedInterval::new(64, 50, 0, 100))
        );
        assert!(
            !StridedInterval::new(64, 7, 0, 7000).contains(&StridedInterval::new(64, 49, -1, 48))
        );
        assert!(
            !StridedInterval::new(64, 7, 0, 7000).contains(&StridedInterval::new(64, 49, 1, 40))
        );
    }

    #[test]
    fn strided_interval_test_add() {
        let op1 = StridedInterval::new(4, 0, 7, 7);
        let op2 = StridedInterval::new(4, 0, 7, 7);
        assert_eq!(StridedInterval::new(4, 0, -2, -2), op1 + op2);

        let op1 = StridedInterval::new(4, 1, -2, 7);
        let op2 = StridedInterval::new(4, 1, -5, 0);
        assert_eq!(StridedInterval::new(4, 1, -7, 7), op1 + op2);

        let op1 = StridedInterval::new(4, 1, 1, 7);
        let op2 = StridedInterval::new(4, 1, 2, 6);
        assert_eq!(StridedInterval::new(4, 1, -8, 7), op1 + op2);

        let op1 = StridedInterval::new(4, 1, -5, 2);
        let op2 = StridedInterval::new(4, 1, -4, 2);
        assert_eq!(StridedInterval::new(4, 1, -8, 7), op1 + op2);

        let op1 = StridedInterval::new(4, 1, -5, 7);
        let op2 = StridedInterval::new(4, 1, -5, 6);
        assert_eq!(StridedInterval::new(4, 1, -8, 7), op1 + op2);

        let op1 = StridedInterval::new(4, 1, -8, -5);
        let op2 = StridedInterval::new(4, 1, -8, -5);
        assert_eq!(StridedInterval::new(4, 1, 0, 6), op1 + op2);

        let op1 = StridedInterval::new(4, 1, -4, 7);
        let op2 = StridedInterval::new(4, 1, -3, 6);
        assert_eq!(StridedInterval::new(4, 1, -8, 7), op1 + op2);

        let op1 = StridedInterval::new(4, 1, 5, 7);
        let op2 = StridedInterval::new(4, 1, 5, 6);
        assert_eq!(StridedInterval::new(4, 1, -6, -3), op1 + op2);

        let op1 = StridedInterval::new(4, 4, -1, 3);
        let op2 = StridedInterval::new(4, 2, -2, 4);
        assert_eq!(StridedInterval::new(4, 2, -3, 7), op1 + op2);
    }

    #[test]
    fn strided_interval_test_neg() {
        let op = StridedInterval::new(64, 1, 0xbadcaffe, 0xdeadbeef);
        assert_eq!(StridedInterval::new(64, 1, -0xdeadbeef, -0xbadcaffe), -op);

        let op = StridedInterval::new(4, 1, -8, 2);
        assert_eq!(StridedInterval::new(4, 1, -8, 7), -op);

        let op = StridedInterval::new(4, 2, -8, 2);
        // -{-8, -6, -4, -2, 0, 2} = {-8, 6, 4, 2, 0, -2}
        assert_eq!(StridedInterval::new(4, 2, -8, 6), -op);

        let op = StridedInterval::new(4, 1, -8, -8);
        assert_eq!(StridedInterval::new(4, 1, -8, -8), -op);
    }

    #[test]
    fn strided_interval_test_sub() {
        let op1 = StridedInterval::new(4, 0, 7, 7);
        let op2 = StridedInterval::new(4, 0, 6, 6);
        assert_eq!(StridedInterval::new(4, 0, 1, 1), op1 - op2);

        let op1 = StridedInterval::new(4, 1, -2, 7);
        let op2 = StridedInterval::new(4, 1, -5, 0);
        assert_eq!(StridedInterval::new(4, 1, -8, 7), op1 - op2);

        let op1 = StridedInterval::new(4, 1, 1, 7);
        let op2 = StridedInterval::new(4, 1, 2, 6);
        assert_eq!(StridedInterval::new(4, 1, -5, 5), op1 - op2);

        let op1 = StridedInterval::new(4, 1, -5, 7);
        let op2 = StridedInterval::new(4, 1, -5, 6);
        assert_eq!(StridedInterval::new(4, 1, -8, 7), op1 - op2);

        let op1 = StridedInterval::new(4, 1, -8, -5);
        let op2 = StridedInterval::new(4, 1, -8, -5);
        assert_eq!(StridedInterval::new(4, 1, -8, 7), op1 - op2);
    }

    #[test]
    fn strided_interval_test_mul() {
        let a: Inum = 0xdeadbeef;
        let b: Inum = 0xbadcaffe;
        let op1 = StridedInterval::from(a);
        let op2 = StridedInterval::from(b);
        assert_eq!(
            StridedInterval::from(n_in_k_bits!(a.wrapping_mul(b), _BITS)),
            op1 * op2
        );

        let op1 = StridedInterval::new(_BITS, 16, 0xdeadbeef, 0xdddddddf);
        let op2 = StridedInterval::from(0);
        assert_eq!(StridedInterval::from(0), op1 * op2);

        let op1 = StridedInterval::new(_BITS, 1, 0, b);
        let op2 = StridedInterval::from(a);
        assert_eq!(StridedInterval::default(), op1 * op2);

        let op1 = StridedInterval::new(_BITS, 2, Inum::max_value() - 2, Inum::max_value());
        let op2 = StridedInterval::from(2);
        assert_eq!(StridedInterval::new(_BITS, 4, -6, -2), op1 * op2);

        let op1 = StridedInterval::new(4, 2, 3, 5);
        let op2 = StridedInterval::from((4, -3));
        assert_eq!(StridedInterval::new(4, 6, 1, 7), op1 * op2);

        let op1 = StridedInterval::new(4, 2, 3, 9);
        let op2 = StridedInterval::from((4, -3));
        assert_eq!(StridedInterval::new(4, 1, -8, 7), op1 * op2);

        let op1 = StridedInterval::new(4, 2, 4, 6);
        let op2 = StridedInterval::new(4, 3, -4, -3);
        assert_eq!(StridedInterval::new(4, 1, -8, 0), op1 * op2);

        let op1 = StridedInterval::new(4, 2, 4, 6);
        let op2 = StridedInterval::new(4, 1, -4, -3);
        assert_eq!(StridedInterval::new(4, 1, -8, 4), op1 * op2);

        let op1 = StridedInterval::new(4, 2, 2, 6);
        let op2 = StridedInterval::new(4, 1, -4, -3);
        assert_eq!(StridedInterval::new(4, 1, -8, 7), op1 * op2);
    }

    #[test]
    fn strided_interval_test_div() {
        let op1 = StridedInterval::from((4, -7));
        let op2 = StridedInterval::from((4, 0));
        assert_eq!(StridedInterval::default_k(4), op1 / op2);

        let op1 = StridedInterval::from((4, -7));
        let op2 = StridedInterval::new(4, 2, -4, 6);
        assert_eq!(StridedInterval::default_k(4), op1 / op2);

        let op1 = StridedInterval::from(-7);
        let op2 = StridedInterval::from(2);
        assert_eq!(StridedInterval::from(-3), op1 / op2);

        let op1 = StridedInterval::from((4, -7));
        let op2 = StridedInterval::from((4, 2));
        assert_eq!(StridedInterval::from((4, -3)), op1 / op2);

        let op1 = StridedInterval::new(4, 2, -8, 0);
        let op2 = StridedInterval::from((4, -1));
        assert_eq!(StridedInterval::default_k(4), op1 / op2);

        let op1 = StridedInterval::new(4, 2, -8, 2);
        let op2 = StridedInterval::from((4, 3));
        assert_eq!(StridedInterval::new(4, 1, -2, 0), op1 / op2);

        let op1 = StridedInterval::new(4, 4, -7, 5);
        let op2 = StridedInterval::from((4, 2));
        assert_eq!(StridedInterval::new(4, 1, -3, 2), op1 / op2);

        let op1 = StridedInterval::new(4, 4, -7, -3);
        let op2 = StridedInterval::from((4, 2));
        assert_eq!(StridedInterval::new(4, 2, -3, -1), op1 / op2);

        let op1 = StridedInterval::new(4, 4, -8, 4);
        let op2 = StridedInterval::from((4, 2));
        assert_eq!(StridedInterval::new(4, 2, -4, 2), op1 / op2);

        let op1 = StridedInterval::new(4, 4, -8, 4);
        let op2 = StridedInterval::new(4, 2, 1, 7);
        assert_eq!(StridedInterval::new(4, 1, -8, 4), op1 / op2);

        let op1 = StridedInterval::new(4, 4, -8, 4);
        let op2 = StridedInterval::new(4, 2, -7, 7);
        assert_eq!(StridedInterval::new(4, 1, -8, 7), op1 / op2);

        let op1 = StridedInterval::new(4, 4, -8, 4);
        let op2 = StridedInterval::new(4, 4, -6, 6);
        assert_eq!(StridedInterval::new(4, 1, -4, 4), op1 / op2);
    }

    #[test]
    fn strided_interval_test_rem() {
        // Two constants
        let op1 = StridedInterval::from((4, -7));
        let op2 = StridedInterval::from((4, 0));
        assert_eq!(StridedInterval::default_k(4), op1 % op2);

        let op1 = StridedInterval::from(-7);
        let op2 = StridedInterval::new(_BITS, 2, -4, 6);
        assert_eq!(StridedInterval::default(), op1 % op2);

        // Divided by one constat
        let op1 = StridedInterval::new(_BITS, 20, -41, 1);
        let op2 = StridedInterval::from(Inum::min_value());
        assert_eq!(op1, op1 % op2);

        let op1 = StridedInterval::new(_BITS, 20, -41, 1);
        let op2 = StridedInterval::from(-42);
        assert_eq!(op1, op1 % op2);

        let op1 = StridedInterval::new(_BITS, 20, -41, 39);
        let op2 = StridedInterval::from(10);
        assert_eq!(StridedInterval::new(_BITS, 10, -1, 9), op1 % op2);

        let op1 = StridedInterval::new(32, 20, 19, 59);
        let op2 = StridedInterval::from((32, 10));
        assert_eq!(StridedInterval::from((32, 9)), op1 % op2);

        let op1 = StridedInterval::new(32, 20, 19, 59);
        let op2 = StridedInterval::from((32, 16));
        assert_eq!(StridedInterval::new(32, 4, 3, 15), op1 % op2);

        let op1 = StridedInterval::new(32, 20, -41, 59);
        let op2 = StridedInterval::from((32, 16));
        assert_eq!(StridedInterval::new(32, 4, -13, 15), op1 % op2);

        let op1 = StridedInterval::new(32, 20, -41, -1);
        let op2 = StridedInterval::from((32, 16));
        assert_eq!(StridedInterval::new(32, 4, -13, -1), op1 % op2);

        let op1 = StridedInterval::new(32, 20, -40, 0);
        let op2 = StridedInterval::from((32, 16));
        assert_eq!(StridedInterval::new(32, 4, -12, 0), op1 % op2);

        let op1 = StridedInterval::new(32, 20, 0, 40);
        let op2 = StridedInterval::from((32, 16));
        assert_eq!(StridedInterval::new(32, 4, 0, 12), op1 % op2);

        let op1 = StridedInterval::new(_BITS, 20, -40, 40);
        let op2 = StridedInterval::new(_BITS, 1, Inum::min_value(), -1);
        assert_eq!(
            StridedInterval::new(_BITS, 1, Inum::min_value() + 1, Inum::max_value()),
            op1 % op2
        );

        let op1 = StridedInterval::new(_BITS, 20, 0, 40);
        let op2 = StridedInterval::new(_BITS, 1, Inum::min_value(), -1);
        assert_eq!(
            StridedInterval::new(_BITS, 1, 0, Inum::max_value()),
            op1 % op2
        );

        let op1 = StridedInterval::new(_BITS, 20, -40, 0);
        let op2 = StridedInterval::new(_BITS, 1, Inum::min_value(), -1);
        assert_eq!(
            StridedInterval::new(_BITS, 1, Inum::min_value() + 1, 0),
            op1 % op2
        );
    }

    #[test]
    // All bitwise operations are based on bitop and not
    fn strided_interval_test_bitop() {
        let op1 = StridedInterval::new(8, 4, 0, 0x10);
        assert_eq!(StridedInterval::new(8, 4, -17, -1), !op1);

        let op1 = StridedInterval::from((8, 0xff));
        let op2 = StridedInterval::new(8, 8, 4, 68);
        assert_eq!(op2, op1 & op2);
        assert_eq!(op2, op2 & op1);

        let op1 = StridedInterval::from((8, 0x0f));
        let op2 = StridedInterval::new(8, 3, 0x31, 0x3a);
        let op3 = StridedInterval::new(8, 3, 0x1, 0xa);
        assert_eq!(op3, op1 & op2);
        assert_eq!(op3, op2 & op1);

        let op1 = StridedInterval::from(-1);
        let op2 = StridedInterval::new(_BITS, 1, Inum::min_value(), Inum::max_value() - 1);
        assert_eq!(op2, op1 & op2);
        assert_eq!(op2, op2 & op1);

        let op1 = StridedInterval::from(0);
        let op2 = StridedInterval::new(_BITS, 1, Inum::min_value(), Inum::max_value() - 1);
        assert_eq!(op1, op1 & op2);
        assert_eq!(op1, op2 & op1);

        let op1 = StridedInterval::from((8, 0xff));
        let op2 = StridedInterval::new(8, 8, 4, 68);
        assert_eq!(!op2, op1 ^ op2);
        assert_eq!(!op2, op2 ^ op1);

        let op1 = StridedInterval::from(-1);
        let op2 = StridedInterval::new(_BITS, 1, Inum::min_value(), Inum::max_value() - 1);
        assert_eq!(!op2, op1 ^ op2);
        assert_eq!(!op2, op2 ^ op1);

        let op1 = StridedInterval::from((4, 0x1));
        let op2 = StridedInterval::from((8, 0x6));
        assert_eq!(StridedInterval::default(), op1 | op2);

        let op1 = StridedInterval::new(8, 0x2, 0x1, 0x5); // {1, 3, 5}
        let op2 = StridedInterval::new(8, 0x6, 0x3, 0x9); // {3, 9}
                                                          // {3, 7, 9, 0xb, 0xd}
        assert_eq!(StridedInterval::new(8, 0x2, 0x3, 0xd), op1 | op2);

        let op1 = StridedInterval::from((8, -1));
        let op2 = StridedInterval::new(8, 0x6, 0x3, 0x69);
        assert_eq!(op1, op1 | op2);
        assert_eq!(op1, op2 | op1);

        let op1 = StridedInterval::from((4, 0x1));
        let op2 = StridedInterval::from((4, 0x6));
        assert_eq!(StridedInterval::from((4, 0x7)), op1 | op2);

        let op1 = StridedInterval::from(0);
        let op2 = StridedInterval::new(64, 1, 45, 59);
        assert_eq!(op2, op1 | op2);
        assert_eq!(op2, op2 | op1);

        let op1 = StridedInterval::new(64, 0x10, 0x10, 0x50);
        let op2 = StridedInterval::new(64, 1, 0, 2);
        assert_eq!(StridedInterval::new(64, 0x10, 0x10, 0x140), op1 << op2);

        let op1 = StridedInterval::new(4, 1, -1, 1);
        let op2 = StridedInterval::from((4, 1));
        assert_eq!(StridedInterval::new(4, 1, 0, 7), op1 >> op2);

        let op1 = StridedInterval::new(4, 1, -1, 1);
        let op2 = StridedInterval::new(4, 1, 0, 1);
        assert_eq!(StridedInterval::new(4, 1, -1, 7), op1 >> op2);

        let op1 = StridedInterval::new(8, 16, 8, 40);
        let op2 = StridedInterval::new(8, 1, 0, 2);
        assert_eq!(StridedInterval::new(8, 2, 2, 40), op1 >> op2);

        let op1 = StridedInterval::new(8, 16, 9, 41);
        let op2 = StridedInterval::new(8, 1, 0, 2);
        assert_eq!(StridedInterval::new(8, 1, 2, 41), op1 >> op2);

        let op1 = StridedInterval::new(8, 16, -40, -8);
        let op2 = StridedInterval::new(8, 1, 0, 2);
        assert_eq!(StridedInterval::new(8, 2, -40, 124), op1 >> op2);
    }

    #[test]
    fn strided_interval_test_setop() {
        let op1 = StridedInterval::new(16, 30, 1, 901);
        assert_eq!(StridedInterval::null(), op1.set_lower_bound(902));
        assert_eq!(
            StridedInterval::new(16, 30, 31, 901),
            op1.set_lower_bound(2)
        );

        let op1 = StridedInterval::new(16, 30, 1, 901);
        assert_eq!(StridedInterval::null(), op1.set_upper_bound(-1));
        assert_eq!(StridedInterval::new(16, 30, 1, 31), op1.set_upper_bound(33));

        let op1 = StridedInterval::new(16, 30, 1, 901);
        let op2 = StridedInterval::new(16, 15, 6, 606);
        let op3 = StridedInterval::null();
        assert_eq!(StridedInterval::new(16, 15, 1, 901), op1.widen(&op2));
        assert_eq!(
            StridedInterval::new(16, 15, min_in_k_bits!(16), max_in_k_bits!(16)),
            op2.widen(&op1)
        );
        assert_eq!(op1, op1.widen(&op3));
        assert_eq!(op1, op3.widen(&op1));

        let op1 = StridedInterval::new(16, 30, 1, 901);
        let op2 = StridedInterval::new(16, 15, -4, 696);
        assert_eq!(
            StridedInterval::new(16, 15, min_in_k_bits!(16), 901),
            op1.widen(&op2)
        );
        assert_eq!(
            StridedInterval::new(16, 15, -4, max_in_k_bits!(16)),
            op2.widen(&op1)
        );

        let op1 = StridedInterval::new(16, 30, 1, 901);
        let op2 = StridedInterval::new(16, 15, 6, 606);
        assert_eq!(StridedInterval::new(16, 5, 1, 901), op1.join(&op2));

        let op1 = StridedInterval::new(16, 30, 1, 901);
        let op2 = StridedInterval::null();
        assert_eq!(StridedInterval::new(16, 30, 1, 901), op1.join(&op2));
        assert_eq!(StridedInterval::new(16, 30, 1, 901), op2.join(&op1));

        let op1 = StridedInterval::new(16, 30, 1, 901);
        let op2 = StridedInterval::null();
        assert_eq!(op2, op1.meet(&op2));
        assert_eq!(op2, op2.meet(&op1));

        let op1 = StridedInterval::new(16, 30, 1, 901);
        let op2 = StridedInterval::from((16, 31));
        let op3 = StridedInterval::from((16, 32));
        assert_eq!(op2, op1.meet(&op2));
        assert_eq!(op2, op2.meet(&op1));
        assert_eq!(StridedInterval::null(), op1.meet(&op3));
        assert_eq!(StridedInterval::null(), op3.meet(&op1));
        assert_eq!(StridedInterval::null(), op2.meet(&op3));
        assert_eq!(StridedInterval::null(), op3.meet(&op2));

        let op1 = StridedInterval::new(16, 30, 1, 901);
        let op2 = StridedInterval::new(16, 15, 1, 601);
        assert_eq!(StridedInterval::new(16, 30, 1, 901), op1.meet(&op2));
        assert_eq!(StridedInterval::new(16, 30, 1, 901), op2.meet(&op1));

        let op1 = StridedInterval::new(16, 12, 1, 121);
        let op2 = StridedInterval::new(16, 15, 6, 606);
        assert_eq!(StridedInterval::null(), op1.meet(&op2));
        assert_eq!(StridedInterval::null(), op2.meet(&op1));

        let op1 = StridedInterval::new(16, 12, 1, 121);
        let op2 = StridedInterval::new(16, 15, 10, 610);
        assert_eq!(StridedInterval::new(16, 60, 25, 565), op1.meet(&op2));
        assert_eq!(StridedInterval::new(16, 60, 25, 565), op2.meet(&op1));

        let op1 = StridedInterval::new(16, 12, 1, 121);
        let op2 = StridedInterval::new(16, 15, 40, 610);
        assert_eq!(StridedInterval::new(16, 60, 85, 565), op1.meet(&op2));
        assert_eq!(StridedInterval::new(16, 60, 85, 565), op2.meet(&op1));

        let op1 = StridedInterval::new(16, 12, 1, 73);
        let op2 = StridedInterval::new(16, 15, 40, 610);
        assert_eq!(StridedInterval::null(), op1.meet(&op2));
        assert_eq!(StridedInterval::null(), op2.meet(&op1));

        let op1 = StridedInterval::new(4, 3, 0, 6);
        assert_eq!(StridedInterval::new(4, 3, -6, 6), op1.remove_lower_bound());

        let op1 = StridedInterval::new(4, 3, 1, 7);
        assert_eq!(StridedInterval::new(4, 3, -8, 7), op1.remove_lower_bound());

        let op1 = StridedInterval::new(4, 3, 2, 5);
        assert_eq!(StridedInterval::new(4, 3, -7, 5), op1.remove_lower_bound());

        let op1 = StridedInterval::new(4, 3, -7, 2);
        assert_eq!(StridedInterval::new(4, 3, -7, 5), op1.remove_upper_bound());

        let op1 = StridedInterval::new(4, 3, -7, 2);
        assert_eq!(StridedInterval::new(4, 3, -7, 2), op1.narrow(4));

        let op1 = StridedInterval::new(8, 4, 0x10, 0x3c);
        assert_eq!(StridedInterval::new(4, 4, -0x8, 0x4), op1.narrow(4));

        let op1 = StridedInterval::new(8, 3, 0x10, 0x1c);
        // op1.narrow(4) = {0, 3, 6, 9, 12} -(4bits)-> {0, 3, 6, -7, -4}
        assert_eq!(StridedInterval::new(4, 1, -7, 6), op1.narrow(4));

        let op1 = StridedInterval::new(8, 3, 0x10, 0x16);
        assert_eq!(StridedInterval::new(4, 3, 0, 6), op1.narrow(4));

        let op1 = StridedInterval::new(8, 3, 0x19, 0x1f);
        assert_eq!(StridedInterval::new(4, 3, -7, -1), op1.narrow(4));

        let op1 = StridedInterval::new(8, 3, 0x19, 0x1f);
        assert_eq!(StridedInterval::new(16, 3, 0x19, 0x1f), op1.sign_extend(16));

        let op1 = StridedInterval::new(4, 3, 0x1, 0x7);
        assert_eq!(StridedInterval::new(8, 3, 0x1, 0x7), op1.zero_extend(8));

        let op1 = StridedInterval::new(4, 3, -8, -2);
        assert_eq!(StridedInterval::new(8, 3, 0x8, 0xe), op1.zero_extend(8));

        let op1 = StridedInterval::new(4, 3, -8, 7);
        // {-8, -5, -2, 1, 3, 7} -->
        //      {8, 11, 14, 1, 3, 7}
        assert_eq!(StridedInterval::new(8, 1, 1, 0xe), op1.zero_extend(8));
    }
}
