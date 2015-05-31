// Value sets

	// helper functions
	// warning: rust uses '!' as bitwise not operator
	// blcic(!x) = tzmsk(x)+1 = product of the '2's of x's prime decomposition
	fn blcic(x: u64) -> u64 { (x+1) & !x } // 101111-> 10000
	fn tzmsk(x: u64) -> u64 { (x-1) & !x } // 010000->  1111
	fn gcd_lcm(mut m: u64, mut n: u64) -> (u64, u64) {
		let p = m*n;
		while m != 0 { let (m, n) = (n % m, m); }
		(n, if n != 0 {p/n} else {0})
	}
	// TODO: Find appropriate consts from stdlib
	const u64min: u64 =  0x0000000000000000;
	const u64max: u64 =  0xffffffffffffffff;
	const s64min: i64 = -0x8000000000000000;
	const s64max: i64 =  0x7fffffffffffffff;

use std::cmp::{min, max};
use std::ops::{BitAnd, BitOr};

pub trait ValueSet<T> {
	fn contains(&self, value: T) -> bool;
}

pub struct KnownBits { zerobits: u64, onebits:  u64 }
pub struct UIntMultiple { factor: u64, offset: u64 }
pub struct UIntRange { min: u64, max: u64 }
pub struct SIntRange { min: i64, max: i64 }

const empty_uintmultiple: UIntMultiple = UIntMultiple { factor: 0, offset: 0 };
const empty_uintrange:    UIntRange    = UIntRange { min: u64max, max: u64min };
const empty_sintrange:    SIntRange    = SIntRange { min: s64max, max: s64min };

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
	fn as_umultiple(&self) -> UIntMultiple {
		let fixedbits = self.zerobits | self.onebits;
		let f_blcic = blcic(fixedbits);
		UIntMultiple {
			factor: f_blcic,
			offset: (f_blcic - 1) & self.onebits
		}
	}
	fn as_urange(&self) -> UIntRange {
		let fixedbits = self.zerobits | self.onebits;
		UIntRange {
			min: (0x0000000000000000 & !fixedbits) | self.onebits,
			max: (0xffffffffffffffff & !fixedbits) | self.onebits
		}
	}
	fn as_srange(&self) -> UIntRange {
		let fixedbits = self.zerobits | self.onebits;
		UIntRange {
			min: (0x8000000000000000 & !fixedbits) | self.onebits,
			max: (0x7fffffffffffffff & !fixedbits) | self.onebits
		}
	}
}

impl UIntMultiple {
	fn as_knownbits(&self) -> KnownBits {
		let z = tzmsk(self.factor);
		KnownBits {
			zerobits: z,
			onebits:  self.offset & z
		}
	}
	// dkreuter: it seems pointless to convert a multiple constraint to a range
	fn as_urange(&self) -> UIntRange {
		if self.factor == 0 { return empty_uintrange }
		UIntRange {
			min: self.scan_up(u64min).unwrap_or(u64min),
			max: self.scan_dn(u64max).unwrap_or(u64max)
		}
	}
	fn as_srange(&self) -> SIntRange {
		if self.factor == 0 { return empty_sintrange }
		SIntRange {
			min: self.scan_up(s64min as u64).or_else(|| self.scan_up(u64min)).unwrap_or(s64min as u64) as i64,
			max: self.scan_dn(s64max as u64).or_else(|| self.scan_dn(u64max)).unwrap_or(s64max as u64) as i64,
		}
	}

	// helpers
	fn scan_up(&self, n: u64) -> Option<u64> {
		if n > u64max-self.factor+self.offset { return Option::None }
		let t = n%self.factor;
		Option::Some(if t == 0 { n } else { n + (self.factor - t) })
	}
	fn scan_dn(&self, n: u64) -> Option<u64> {
		if n < self.offset { return Option::None }
		Option::Some(n - n%self.factor)
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

impl BitAnd for KnownBits {
	type Output = KnownBits;

	fn bitand(self, rhs: KnownBits) -> KnownBits {
		KnownBits {
			zerobits: self.zerobits | rhs.zerobits,
			onebits: self.onebits | rhs.onebits
		}
	}
}

impl BitOr for KnownBits {
	type Output = KnownBits;

	fn bitor(self, rhs: KnownBits) -> KnownBits {
		KnownBits {
			zerobits: self.zerobits & rhs.zerobits,
			onebits: self.onebits & rhs.onebits
		}
	}
}

impl BitAnd for UIntMultiple {
	type Output = UIntMultiple;

	fn bitand(self, rhs: UIntMultiple) -> UIntMultiple {
		let (gcd, lcm) = gcd_lcm(self.factor, rhs.factor);
		let offset = self.offset % gcd;
		if offset != rhs.offset % gcd {
			return empty_uintmultiple
		}
		let x = 0; /* TODO */
		UIntMultiple {
			factor: lcm,
			offset: offset + gcd * x
		}
	}
}

/* TODO
impl BitOr for UIntMultiple {
	type Output = UIntMultiple;

	fn bitor(self, rhs: UIntMultiple) -> UIntMultiple {
	}
}
*/

impl BitAnd for UIntRange {
	type Output = UIntRange;

	fn bitand(self, rhs: UIntRange) -> UIntRange {
		UIntRange {
			min: max(self.min, rhs.min),
			max: min(self.max, rhs.max)
		}
	}
}

impl BitOr for UIntRange {
	type Output = UIntRange;

	fn bitor(self, rhs: UIntRange) -> UIntRange {
		UIntRange {
			min: min(self.min, rhs.min),
			max: max(self.max, rhs.max)
		}
	}
}

impl BitAnd for SIntRange {
	type Output = SIntRange;

	fn bitand(self, rhs: SIntRange) -> SIntRange {
		SIntRange {
			min: max(self.min, rhs.min),
			max: min(self.max, rhs.max)
		}
	}
}

impl BitOr for SIntRange {
	type Output = SIntRange;

	fn bitor(self, rhs: SIntRange) -> SIntRange {
		SIntRange {
			min: min(self.min, rhs.min),
			max: max(self.max, rhs.max)
		}
	}
}
