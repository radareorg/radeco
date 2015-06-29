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
			min: (u64::min_value() & !fixedbits) | self.onebits,
			max: (u64::max_value() & !fixedbits) | self.onebits
		}
	}
	pub fn as_srange(&self) -> SIntRange {
		let fixedbits = self.zerobits | self.onebits;
		SIntRange {
			min: ((i64::min_value() as u64 & !fixedbits) | self.onebits) as i64,
			max: ((i64::max_value() as u64 & !fixedbits) | self.onebits) as i64
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
