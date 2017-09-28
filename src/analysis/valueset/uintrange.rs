// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

use super::{KnownBits, SIntRange, UIntMultiple, UIntRange, ValueSet};
use std::cmp::{max, min};
use std::ops::{BitAnd, BitOr};

impl ValueSet<u64> for UIntRange {
    fn contains(&self, value: u64) -> bool {
        (self.min <= value) && (value <= self.max)
    }
}

/// A value set that includes all u64 values between a minimum and a maximum
impl UIntRange {
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
    fn as_srange(&self) -> SIntRange {
        SIntRange { min: 0, max: 0 }
    }
}

impl<'a, 'b> BitAnd<&'a UIntRange> for &'b UIntRange {
    type Output = UIntRange;

    fn bitand(self, rhs: &UIntRange) -> UIntRange {
        UIntRange {
            min: max(self.min, rhs.min),
            max: min(self.max, rhs.max),
        }
    }
}

impl<'a, 'b> BitOr<&'a UIntRange> for &'b UIntRange {
    type Output = UIntRange;

    fn bitor(self, rhs: &UIntRange) -> UIntRange {
        UIntRange {
            min: min(self.min, rhs.min),
            max: max(self.max, rhs.max),
        }
    }
}
