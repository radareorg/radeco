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
pub struct UIntMultiple { pub factor: u64, pub offset: u64 }
#[derive(Clone, Copy, Debug)]
pub struct UIntRange { pub min: u64, pub max: u64 }
#[derive(Clone, Copy, Debug)]
pub struct SIntRange { pub min: i64, pub max: i64 }

const EMPTY_UINTMULTIPLE: UIntMultiple = UIntMultiple { factor: 0, offset: 0 };
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
		if self.factor == 0 { return false }
		value % self.factor == self.offset
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
			factor: f_blcic,
			offset: (f_blcic - 1) & self.onebits
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
		let z = tzmsk(self.factor);
		KnownBits {
			zerobits: z & !self.offset,
			onebits:  z &  self.offset
		}
	}
	// dkreuter: it seems pointless to convert a multiple constraint to a range
	pub fn as_urange(&self) -> UIntRange {
		if self.factor == 0 { return EMPTY_UINTRANGE }
		UIntRange {
			min: self.scan_up(U64MIN).unwrap_or(U64MAX),
			max: self.scan_dn(U64MAX).unwrap_or(U64MIN)
		}
	}
	pub fn as_srange(&self) -> SIntRange {
		if self.factor == 0 { return EMPTY_SINTRANGE }
		SIntRange {
			min: self.scan_up(S64MIN as u64).or_else(|| self.scan_up(U64MIN)).unwrap_or(S64MIN as u64) as i64,
			max: self.scan_dn(S64MAX as u64).or_else(|| self.scan_dn(U64MAX)).unwrap_or(S64MAX as u64) as i64,
		}
	}

	// helpers
	fn scan_up(&self, n: u64) -> Option<u64> {
		let io = self.factor - self.offset;
		if n > U64MAX - io { return Option::None }
		let t = (n + io) % self.factor;
		Option::Some(if t == 0 { n } else { n + (self.factor - t) })
	}
	fn scan_dn(&self, n: u64) -> Option<u64> {
		if n < self.offset { return Option::None }
		Option::Some(n - (n-self.offset)%self.factor)
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
		let (gcd, lcm) = gcd_lcm(self.factor, rhs.factor);
		println!("gcd={:?} lcm={:?}", gcd, lcm);
		let ioff = self.offset % gcd;
		if ioff != rhs.offset % gcd {
			return EMPTY_UINTMULTIPLE
		}
		/* TODO
		let a = self.offset / gcd;
		let b = rhs.offset / gcd;
		let c = self.factor / gcd;
		let d = rhs.factor / gcd;

		// special cases when a==0 or b==0 or c==1 or d==1 (for speedups)

		// reconstruct i from j
		let i = b+d*j % c;
		let j = a+c*i % d;

		let i = match multiplicative_inverse(d, c) { None => return EMPTY_UINTMULTIPLE, Some(i) => i };
		let j = match multiplicative_inverse(c, d) { None => return EMPTY_UINTMULTIPLE, Some(j) => j };

		let offset1 = (ioff + (i*b+j*a)*gcd) % lcm;
		let offset2 = (ioff + (i*a+j*b)*gcd) % lcm;
		println!("{:?} {:?}", *self, *rhs);
		println!("ioff={:?} + lcm={:?} - ((i={:?}*b={:?} + j={:?}*a={:?}) * {:?}) % lcm = {:?}", ioff, lcm, i, b, j, a, gcd, offset1);
		println!("ioff={:?} + lcm={:?} - ((i={:?}*a={:?} + j={:?}*b={:?}) * {:?}) % lcm = {:?}", ioff, lcm, i, a, j, b, gcd, offset2);

		*/
		let offset = 0;

		assert_eq!(offset % self.factor, self.offset);
		assert_eq!(offset % rhs.factor, rhs.offset);

		UIntMultiple {
			factor: lcm,
			offset: offset
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
