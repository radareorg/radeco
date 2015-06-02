use super::value_int::{ValueSet, KnownBits, UIntMultiple, UIntRange, SIntRange};
use super::helper::{blcic, tzmsk, multiplicative_inverse};
use std::ops::BitAnd;

/* Helper tests */

fn confirm_multiplicative_inverse(a: u64, n: u64) {
	let x = multiplicative_inverse(a, n).unwrap();
	assert!((a*x) % n == 1);
}

#[test]
fn test_blcic() {
	assert_eq!(blcic(0x0000000000000000), 0x0000000000000001);
	assert_eq!(blcic(0x0000ffff0000ffff), 0x0000000000010000);
	assert_eq!(blcic(0x00000000ffff0000), 0x0000000000000001);
	assert_eq!(blcic(0xffffffffffffffff), 0x0000000000000000);
}

#[test]
fn test_tzmsk() {
	assert_eq!(tzmsk(0x0000000000000000), 0xffffffffffffffff);
	assert_eq!(tzmsk(0x0000ffff0000ffff), 0x0000000000000000);
	assert_eq!(tzmsk(0x00000000ffff0000), 0x000000000000ffff);
	assert_eq!(tzmsk(0xffffffffffffffff), 0x0000000000000000);
}

fn test_multiplicative_inverse_recursively(depth: usize, m: u64, n: u64) {
	if n != 1 {
		confirm_multiplicative_inverse(m, n);
		confirm_multiplicative_inverse(n, m);
	}
	if depth > 0 {
		// See http://en.wikipedia.org/wiki/Coprime_integers#Generating_all_coprime_pairs
		test_multiplicative_inverse_recursively(depth-1, 2*m-n, m);
		test_multiplicative_inverse_recursively(depth-1, 2*m+n, m);
		test_multiplicative_inverse_recursively(depth-1, m+2*n, n);
	}
}

#[test]
fn test_multiplicative_inverse() {
	test_multiplicative_inverse_recursively(0, 2, 1);
	test_multiplicative_inverse_recursively(0, 3, 1);
}

#[test]
fn test_blcic_tzmsk_equiv() {
	for &i in &[
		0x0000000000000000,
		0x0000ffff0000ffff,
		0x00000000ffff0000,
		0xffffffff0000ffff,
		0xffff0000ffff0000,
		0xffffffffffffffff
	] {
		assert_eq!(blcic(!i), tzmsk(i).wrapping_add(1));
	}
}

/* ValueSet tests */

fn confirm_valueset_contains(u: &ValueSet<u64>, samples: &[u64]) {
	for &sample in samples {
		assert!(u.contains(sample));
	}
}

fn confirm_valueset_contains_not(u: &ValueSet<u64>, samples: &[u64]) {
	for &sample in samples {
		assert!(!u.contains(sample));
	}
}

fn confirm_valueset_and<'a, 'b, T>(u: &'a T, v: &'b T, samples: &[u64])
	where
		T: ValueSet<u64>,
		&'a T: BitAnd<&'b T, Output=T>
{
	let uv: T = u & v;
	for &sample in samples {
		assert_eq!((u.contains(sample) && v.contains(sample)), uv.contains(sample));
	}
}

/*
fn confirm_valueset_or(u: &ValueSet<u64>, v: &ValueSet<u64>, samples: &[u64]) {
	let uv = u | v;
	for &sample in samples {
		assert_eq!((u.contains(sample) || v.contains(sample)), uv.contains(sample));
	}
}
*/

fn confirm_valueset_weaker(u: &ValueSet<u64>, v: &ValueSet<u64>, samples: &[u64]) {
	for &sample in samples {
		assert!(!u.contains(sample) || v.contains(sample));
	}
}

#[test]
fn test_uintmultiple_membership() {
	let m5_2 = UIntMultiple{modulus: 5, residue: 2};
	confirm_valueset_contains    (&m5_2, &[2, 7, 12]);
	confirm_valueset_contains_not(&m5_2, &[0, 1, 3, 10, 11, 13]);

	let m0_2 = UIntMultiple{modulus: 0, residue: 2};
	confirm_valueset_contains_not(&m0_2, &[0, 1, 2, 3]);

	let m5_0 = UIntMultiple{modulus: 5, residue: 0};
	confirm_valueset_contains    (&m5_0, &[0, 5, 10]);
	confirm_valueset_contains_not(&m5_0, &[1, 4, 6, 9, 11]);
}

#[test]
fn test_urange_membership() {
	let u5_10 = UIntRange{min: 5, max: 10};
	confirm_valueset_contains(&u5_10, &[5, 6, 7, 8, 9, 10]);
	confirm_valueset_contains_not(&u5_10, &[0, 1, 2, 3, 4, 11, 12,
		u64::min_value(),
		u64::max_value(),
		i64::max_value() as u64,
		i64::min_value() as u64,
	]);
}

#[test]
fn test_srange_membership() {
	let s5_5 = SIntRange{min:-5, max: 5};
	confirm_valueset_contains(&s5_5, &[
		u64::max_value() - 4,
		u64::max_value(),
		u64::min_value(),
		5
	]);
	confirm_valueset_contains_not(&s5_5, &[
		u64::max_value() - 5,
		i64::max_value() as u64,
		i64::min_value() as u64,
		6
	]);
}

#[test]
fn test_uintmultiple_conversions() {

	let seq = [
		 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11,
		12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
		(u64::max_value() - 11),        (u64::max_value() - 10),        (u64::max_value() - 9),
		(u64::max_value() -  8),        (u64::max_value() -  7),        (u64::max_value() - 6),
		(u64::max_value() -  5),        (u64::max_value() -  4),        (u64::max_value() - 3),
		(u64::max_value() -  2),        (u64::max_value() -  1),        (u64::max_value() - 0),
		(i64::max_value() -  0) as u64, (i64::max_value() -  1) as u64, (i64::max_value() - 2) as u64,
		(i64::max_value() -  3) as u64, (i64::max_value() -  4) as u64, (i64::max_value() - 5) as u64,
		(i64::min_value() +  0) as u64, (i64::min_value() +  1) as u64, (i64::min_value() + 2) as u64,
		(i64::min_value() +  3) as u64, (i64::min_value() +  4) as u64, (i64::min_value() + 5) as u64
	];


	let empty = UIntMultiple{modulus: 0, residue: 3};
	confirm_valueset_weaker(&empty, &empty.as_knownbits(), &seq);
	confirm_valueset_weaker(&empty, &empty.as_urange(), &seq);
	confirm_valueset_weaker(&empty, &empty.as_srange(), &seq);

	let m12_6 = UIntMultiple{modulus: 12, residue: 6};
	confirm_valueset_weaker(&m12_6, &m12_6.as_knownbits(), &seq);
	confirm_valueset_weaker(&m12_6, &m12_6.as_urange(), &seq);
	confirm_valueset_weaker(&m12_6, &m12_6.as_srange(), &seq);

	let b12_6: KnownBits = m12_6.as_knownbits();
	assert_eq!(b12_6.zerobits, 0x1);
	assert_eq!(b12_6.onebits , 0x2);
}

#[test]
fn test_knownbits_conversions() {
	let upperknown = KnownBits{zerobits: 0xffff000000000000, onebits: 0x0000ffff00000000};
	let upper_urange = upperknown.as_urange();
	let upper_srange = upperknown.as_srange();
	assert_eq!(upper_urange.min, 0x0000ffff00000000);
	assert_eq!(upper_urange.max, 0x0000ffffffffffff);
	assert_eq!(upper_srange.min, 0x0000ffff00000000);
	assert_eq!(upper_srange.max, 0x0000ffffffffffff);

	let lowerknown = KnownBits{zerobits: 0x00000000ffff0000, onebits: 0x000000000000ffff};
	let lower_urange = lowerknown.as_urange();
	let lower_srange = lowerknown.as_srange();
	assert_eq!(lower_urange.min,  0x000000000000ffff);
	assert_eq!(lower_urange.max,  0xffffffff0000ffff);
	assert_eq!(lower_srange.min, -0x7fffffffffff0001);
	assert_eq!(lower_srange.max,  0x7fffffff0000ffff);
}

#[test]
fn test_uintmultiple_intersection() {
	for a in 2..6 {
		for i in 0..a {
			let x = UIntMultiple { modulus: a, residue: i };

			for b in 3..15 {
				for j in 0..b {
					let y = UIntMultiple { modulus: b, residue: j };

					confirm_valueset_and(&x, &y, &(0..a*b).collect::<Vec<_>>());

				}
			}

		}
	}
}
