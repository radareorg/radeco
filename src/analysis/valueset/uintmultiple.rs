// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

use super::{KnownBits, SIntRange, ScannableSet, UIntMultiple, UIntRange, ValueSet};
use super::{EMPTY_SINTRANGE, EMPTY_UINTMULTIPLE, EMPTY_UINTRANGE};
use std;
use std::cmp::{max, min};
use std::ops::{BitAnd, BitOr};
use super::math::{gcd_lcm, multiplicative_inverse, tzmsk};

const U64MIN: u64 = std::u64::MIN;  // 0x0000000000000000;
const U64MAX: u64 = std::u64::MAX;  // 0xffffffffffffffff;
const S64MIN: i64 = std::i64::MIN;  // -0x8000000000000000;
const S64MAX: i64 = std::i64::MAX;  // 0x7fffffffffffffff;

impl ValueSet<u64> for UIntMultiple {
    fn contains(&self, value: u64) -> bool {
        if self.modulus == 0 {
            return false;
        }
        value % self.modulus == self.residue
    }
}

impl ScannableSet<u64> for UIntMultiple {
    fn scan_up(&self, n: u64) -> Option<u64> {
        let io = self.modulus - self.residue;
        if n > U64MAX - io {
            return Option::None;
        }
        let t = (n + io) % self.modulus;
        Option::Some(if t == 0 {
            n
        } else {
            n + (self.modulus - t)
        })
    }
    fn scan_dn(&self, n: u64) -> Option<u64> {
        if n < self.residue {
            return Option::None;
        }
        Option::Some(n - (n - self.residue) % self.modulus)
    }
}

impl UIntMultiple {
    pub fn as_knownbits(&self) -> KnownBits {
        let z = tzmsk(self.modulus);
        KnownBits {
            zerobits: z & !self.residue,
            onebits: z & self.residue,
        }
    }
    // dkreuter: it seems pointless to convert a multiple constraint to a range
    pub fn as_urange(&self) -> UIntRange {
        if self.modulus == 0 {
            return EMPTY_UINTRANGE;
        }
        UIntRange {
            min: self.scan_up(U64MIN).unwrap_or(U64MAX),
            max: self.scan_dn(U64MAX).unwrap_or(U64MIN),
        }
    }
    pub fn as_srange(&self) -> SIntRange {
        if self.modulus == 0 {
            return EMPTY_SINTRANGE;
        }
        SIntRange {
            min: self.scan_up(S64MIN as u64)
                     .or_else(|| self.scan_up(U64MIN))
                     .unwrap_or(S64MIN as u64) as i64,
            max: self.scan_dn(S64MAX as u64)
                     .or_else(|| self.scan_dn(U64MAX))
                     .unwrap_or(S64MAX as u64) as i64,
        }
    }
}

impl<'a, 'b> BitAnd<&'a UIntMultiple> for &'b UIntMultiple {
    type Output = UIntMultiple;

    fn bitand(self, rhs: &UIntMultiple) -> UIntMultiple {
        // calls the euclidean algorithm three times
        // TODO: get a mathematician to reduce this
        let (gcd, lcm) = gcd_lcm(self.modulus, rhs.modulus);
        let ioff = self.residue % gcd;
        if ioff != rhs.residue % gcd {
            return EMPTY_UINTMULTIPLE;
        }

        // See http://artofproblemsolving.com/wiki/index.php/Chinese_Remainder_Theorem
        let y1 = self.residue / gcd;
        let y2 = rhs.residue / gcd;
        let d1 = self.modulus / gcd;
        let d2 = rhs.modulus / gcd;

        let x = if d1 == 1 {
            y2
        } else if d2 == 1 {
            y1
        } else {
            let m = d1 * d2;
            let b1 = d2; // = m / d1;
            let b2 = d1; // = m / d2;
            let a1 = match multiplicative_inverse(b1, d1) {
                None => return EMPTY_UINTMULTIPLE,
                Some(a1) => a1,
            };
            let a2 = match multiplicative_inverse(b2, d2) {
                None => return EMPTY_UINTMULTIPLE,
                Some(a2) => a2,
            };
            let x1 = a1 * b1 * y1;
            let x2 = a2 * b2 * y2;
            (x1 + x2) % m
        };

        let offset = ioff + gcd * x;

        assert_eq!(offset % self.modulus, self.residue);
        assert_eq!(offset % rhs.modulus, rhs.residue);

        UIntMultiple {
            modulus: lcm,
            residue: offset,
        }
    }
}

impl<'a, 'b> BitOr<&'a UIntMultiple> for &'b UIntMultiple {
    type Output = UIntMultiple;

    fn bitor(self, rhs: &UIntMultiple) -> UIntMultiple {

        // because this operator only merges two at a time the guess will be
        // conservative

        let m = min(self.modulus, rhs.modulus);
        let n = max(self.modulus, rhs.modulus);
        let ra = self.residue % m;
        let rb = rhs.residue % m;
        let r = min(ra, rb);

        if n % m != 0 {
            return UIntMultiple {
                modulus: 1,
                residue: 0,
            };
        }

        let diff = (m + ra - rb) % m;
        if diff == 0 {
            UIntMultiple {
                modulus: m,
                residue: r,
            }
        } else if diff * 2 == m {
            UIntMultiple {
                modulus: m / 2,
                residue: r,
            }
        } else {
            UIntMultiple {
                modulus: 1,
                residue: 0,
            }
        }
    }
}
