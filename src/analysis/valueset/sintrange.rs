// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

use super::{KnownBits, SIntRange, UIntMultiple, UIntRange, ValueSet};
use std::cmp::{max, min};
use std::ops::{BitAnd, BitOr};

impl ValueSet<u64> for SIntRange {
    fn contains(&self, value: u64) -> bool {
        (self.min <= (value as i64)) && ((value as i64) <= self.max)
    }
}

/// A value set that includes all i64 values between a minimum and a maximum
impl SIntRange {
    // TODO
    fn as_knownbits(&self) -> KnownBits {
        KnownBits {
            zerobits: 0,
            onebits: 0,
        }
    }
    fn as_umultiple(&self) -> UIntMultiple {
        UIntMultiple {
            modulus: 0,
            residue: 0,
        }
    }
    fn as_urange(&self) -> UIntRange {
        UIntRange { min: 0, max: 0 }
    }
}

impl<'a, 'b> BitAnd<&'a SIntRange> for &'b SIntRange {
	type Output = SIntRange;

    fn bitand(self, rhs: &SIntRange) -> SIntRange {
        SIntRange {
            min: max(self.min, rhs.min),
            max: min(self.max, rhs.max),
        }
    }
}

impl<'a, 'b> BitOr<&'a SIntRange> for &'b SIntRange {
	type Output = SIntRange;

    fn bitor(self, rhs: &SIntRange) -> SIntRange {
        SIntRange {
            min: min(self.min, rhs.min),
            max: max(self.max, rhs.max),
        }
    }
}
