// Value sets

use std::cmp::{min, max};
use std::fmt::Debug;
use std::ops::{BitAnd, BitOr};
use super::helper::{blcic, tzmsk, gcd_lcm, multiplicative_inverse};

// TODO: Find appropriate consts from stdlib
const U64MIN: u64 =  0x0000000000000000;
const U64MAX: u64 =  0xffffffffffffffff;
const S64MIN: i64 = -0x8000000000000000;
const S64MAX: i64 =  0x7fffffffffffffff;

pub trait ValueSet<T>: Debug /*+ BitAnd<Self> + BitOr<Self>*/ {
	fn contains(&self, value: T) -> bool;
}

#[derive(Clone, Copy, Debug)]
pub struct KnownBits { pub zerobits: u64, pub onebits: u64 }
#[derive(Clone, Copy, Debug)]
pub struct UIntMultiple { pub modulus: u64, pub residue: u64 }
#[derive(Clone, Copy, Debug)]
pub struct UIntRange { pub min: u64, pub max: u64 }
#[derive(Clone, Copy, Debug)]
pub struct SIntRange { pub min: i64, pub max: i64 }

const EMPTY_UINTMULTIPLE: UIntMultiple = UIntMultiple { modulus: 0, residue: 0 };
const EMPTY_UINTRANGE:    UIntRange    = UIntRange { min: U64MAX, max: U64MIN };
const EMPTY_SINTRANGE:    SIntRange    = SIntRange { min: S64MAX, max: S64MIN };

impl ValueSet<u64> for KnownBits {
	fn contains(&self, value: u64) -> bool {
		if self.zerobits & self.onebits != 0 {
			return false // pattern unfulfillable
		}
		value & (self.zerobits | self.onebits) == self.onebits
	}
}

impl ValueSet<u64> for UIntMultiple {
	fn contains(&self, value: u64) -> bool {
		if self.modulus == 0 { return false }
		value % self.modulus == self.residue
	}
}

impl ValueSet<u64> for UIntRange {
	fn contains(&self, value: u64) -> bool { (self.min <= value) && (value <= self.max) }
}

impl ValueSet<u64> for SIntRange {
	fn contains(&self, value: u64) -> bool { (self.min <= (value as i64)) && ((value as i64) <= self.max) }
}

impl KnownBits {
	pub fn as_umultiple(&self) -> UIntMultiple {
		let fixedbits = self.zerobits | self.onebits;
		let f_blcic = blcic(fixedbits);
		UIntMultiple {
			modulus: f_blcic,
			residue: (f_blcic - 1) & self.onebits
		}
	}
	pub fn as_urange(&self) -> UIntRange {
		let fixedbits = self.zerobits | self.onebits;
		UIntRange {
			min: (0x0000000000000000 & !fixedbits) | self.onebits,
			max: (0xffffffffffffffff & !fixedbits) | self.onebits
		}
	}
	pub fn as_srange(&self) -> SIntRange {
		let fixedbits = self.zerobits | self.onebits;
		SIntRange {
			min: ((0x8000000000000000 & !fixedbits) | self.onebits) as i64,
			max: ((0x7fffffffffffffff & !fixedbits) | self.onebits) as i64
		}
	}
}

impl UIntMultiple {
	pub fn as_knownbits(&self) -> KnownBits {
		let z = tzmsk(self.modulus);
		KnownBits {
			zerobits: z & !self.residue,
			onebits:  z &  self.residue
		}
	}
	// dkreuter: it seems pointless to convert a multiple constraint to a range
	pub fn as_urange(&self) -> UIntRange {
		if self.modulus == 0 { return EMPTY_UINTRANGE }
		UIntRange {
			min: self.scan_up(U64MIN).unwrap_or(U64MAX),
			max: self.scan_dn(U64MAX).unwrap_or(U64MIN)
		}
	}
	pub fn as_srange(&self) -> SIntRange {
		if self.modulus == 0 { return EMPTY_SINTRANGE }
		SIntRange {
			min: self.scan_up(S64MIN as u64).or_else(|| self.scan_up(U64MIN)).unwrap_or(S64MIN as u64) as i64,
			max: self.scan_dn(S64MAX as u64).or_else(|| self.scan_dn(U64MAX)).unwrap_or(S64MAX as u64) as i64,
		}
	}

	// helpers
	fn scan_up(&self, n: u64) -> Option<u64> {
		let io = self.modulus - self.residue;
		if n > U64MAX - io { return Option::None }
		let t = (n + io) % self.modulus;
		Option::Some(if t == 0 { n } else { n + (self.modulus - t) })
	}
	fn scan_dn(&self, n: u64) -> Option<u64> {
		if n < self.residue { return Option::None }
		Option::Some(n - (n-self.residue)%self.modulus)
	}
}

/* TODO
impl UIntRange {
	fn as_knownbits(&self) -> KnownBits { /* TODO */ }
	fn as_umultiple(&self) -> UIntMultiple { /* TODO */ }
	fn as_srange(&self) -> SIntRange { /* TODO */ }
}

impl SIntRange {
	fn as_knownbits(&self) -> KnownBits { /* TODO */ }
	fn as_umultiple(&self) -> UIntMultiple { /* TODO */ }
	fn as_urange(&self) -> UIntRange { /* TODO */ }
}
*/

impl<'a, 'b> BitAnd<&'a KnownBits> for &'b KnownBits {
	type Output = KnownBits;

	fn bitand(self, rhs: &KnownBits) -> KnownBits {
		KnownBits {
			zerobits: self.zerobits | rhs.zerobits,
			onebits: self.onebits | rhs.onebits
		}
	}
}

impl<'a, 'b> BitOr<&'a KnownBits> for &'b KnownBits {
	type Output = KnownBits;

	fn bitor(self, rhs: &KnownBits) -> KnownBits {
		KnownBits {
			zerobits: self.zerobits & rhs.zerobits,
			onebits: self.onebits & rhs.onebits
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
			return EMPTY_UINTMULTIPLE
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
			let a1 = match multiplicative_inverse(b1, d1) { None => return EMPTY_UINTMULTIPLE, Some(a1) => a1 };
			let a2 = match multiplicative_inverse(b2, d2) { None => return EMPTY_UINTMULTIPLE, Some(a2) => a2 };
			let x1 = a1 * b1 * y1;
			let x2 = a2 * b2 * y2;
			(x1 + x2) % m
		};

		let offset = ioff + gcd * x;

		assert_eq!(offset % self.modulus, self.residue);
		assert_eq!(offset % rhs.modulus, rhs.residue);

		UIntMultiple {
			modulus: lcm,
			residue: offset
		}
	}
}

/* TODO
impl<'a, 'b> BitOr<&'a UIntMultiple> for &'b UIntMultiple {
	type Output = UIntMultiple;

	fn bitor(self, rhs: &UIntMultiple) -> UIntMultiple {
	}
}

*/

impl<'a, 'b> BitAnd<&'a UIntRange> for &'b UIntRange {
	type Output = UIntRange;

	fn bitand(self, rhs: &UIntRange) -> UIntRange {
		UIntRange {
			min: max(self.min, rhs.min),
			max: min(self.max, rhs.max)
		}
	}
}

impl<'a, 'b> BitOr<&'a UIntRange> for &'b UIntRange {
	type Output = UIntRange;

	fn bitor(self, rhs: &UIntRange) -> UIntRange {
		UIntRange {
			min: min(self.min, rhs.min),
			max: max(self.max, rhs.max)
		}
	}
}

impl<'a, 'b> BitAnd<&'a SIntRange> for &'b SIntRange {
	type Output = SIntRange;

	fn bitand(self, rhs: &SIntRange) -> SIntRange {
		SIntRange {
			min: max(self.min, rhs.min),
			max: min(self.max, rhs.max)
		}
	}
}

impl<'a, 'b> BitOr<&'a SIntRange> for &'b SIntRange {
	type Output = SIntRange;

	fn bitor(self, rhs: &SIntRange) -> SIntRange {
		SIntRange {
			min: min(self.min, rhs.min),
			max: max(self.max, rhs.max)
		}
	}
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

bitand_impl! { KnownBits UIntMultiple UIntRange SIntRange }

macro_rules! bitor_impl {
    ($($t:ty)*) => ($(
        forward_ref_binop! { impl BitOr, bitor for $t, $t }
    )*)
}

bitor_impl! { KnownBits /* UIntMultiple */ UIntRange SIntRange }
