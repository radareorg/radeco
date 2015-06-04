use super::{ValueSet, KnownBits, UIntMultiple, UIntRange, SIntRange};
use std::ops::{BitAnd, BitOr};
use util::blcic;

impl ValueSet<u64> for KnownBits {
	fn contains(&self, value: u64) -> bool {
		if self.zerobits & self.onebits != 0 {
			return false // pattern unfulfillable
		}
		value & (self.zerobits | self.onebits) == self.onebits
	}
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
