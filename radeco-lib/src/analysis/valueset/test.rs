// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

use super::{KnownBits, SIntRange, ScannableSet, UIntMultiple, UIntRange, ValueSet};
use std::ops::{BitAnd, BitOr};

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
    where T: ValueSet<u64>,
          &'a T: BitAnd<&'b T, Output = T>
{
    let uv: T = u & v;
    for &sample in samples {
        assert_eq!((u.contains(sample) && v.contains(sample)),
                   uv.contains(sample));
    }
}

fn confirm_valueset_or<'a, 'b, T>(u: &'a T, v: &'b T, samples: &[u64])
    where T: ValueSet<u64>,
          &'a T: BitOr<&'b T, Output = T>
{
    let uv = u | v;
    for &sample in samples {
        assert_eq!((u.contains(sample) || v.contains(sample)),
                   uv.contains(sample));
    }
}

fn confirm_valueset_or_weaker<'a, 'b, T>(u: &'a T, v: &'b T, samples: &[u64])
    where T: ValueSet<u64>,
          &'a T: BitOr<&'b T, Output = T>
{
    let uv = u | v;
    for &sample in samples {
        assert!(!(u.contains(sample) || v.contains(sample)) || uv.contains(sample));
    }
}

fn confirm_valueset_weaker(u: &ValueSet<u64>, v: &ValueSet<u64>, samples: &[u64]) {
    for &sample in samples {
        assert!(!u.contains(sample) || v.contains(sample));
    }
}

#[test]
fn test_uintmultiple_membership() {
    let m5_2 = UIntMultiple {
        modulus: 5,
        residue: 2,
    };
    confirm_valueset_contains(&m5_2, &[2, 7, 12]);
    confirm_valueset_contains_not(&m5_2, &[0, 1, 3, 10, 11, 13]);

    let m0_2 = UIntMultiple {
        modulus: 0,
        residue: 2,
    };
    confirm_valueset_contains_not(&m0_2, &[0, 1, 2, 3]);

    let m5_0 = UIntMultiple {
        modulus: 5,
        residue: 0,
    };
    confirm_valueset_contains(&m5_0, &[0, 5, 10]);
    confirm_valueset_contains_not(&m5_0, &[1, 4, 6, 9, 11]);
}

#[test]
fn test_urange_membership() {
    let u5_10 = UIntRange { min: 5, max: 10 };
    confirm_valueset_contains(&u5_10, &[5, 6, 7, 8, 9, 10]);
    confirm_valueset_contains_not(&u5_10,
                                  &[0,
                                    1,
                                    2,
                                    3,
                                    4,
                                    11,
                                    12,
                                    u64::min_value(),
                                    u64::max_value(),
                                    i64::max_value() as u64,
                                    i64::min_value() as u64]);
}

#[test]
fn test_srange_membership() {
    let s5_5 = SIntRange { min: -5, max: 5 };
    confirm_valueset_contains(&s5_5,
                              &[u64::max_value() - 4, u64::max_value(), u64::min_value(), 5]);
    confirm_valueset_contains_not(&s5_5,
                                  &[u64::max_value() - 5,
                                    i64::max_value() as u64,
                                    i64::min_value() as u64,
                                    6]);
}

#[test]
fn test_uintmultiple_conversions() {

    let seq = [0,
               1,
               2,
               3,
               4,
               5,
               6,
               7,
               8,
               9,
               10,
               11,
               12,
               13,
               14,
               15,
               16,
               17,
               18,
               19,
               20,
               21,
               22,
               23,
               (u64::max_value() - 11),
               (u64::max_value() - 10),
               (u64::max_value() - 9),
               (u64::max_value() - 8),
               (u64::max_value() - 7),
               (u64::max_value() - 6),
               (u64::max_value() - 5),
               (u64::max_value() - 4),
               (u64::max_value() - 3),
               (u64::max_value() - 2),
               (u64::max_value() - 1),
               (u64::max_value() - 0),
               (i64::max_value() - 0) as u64,
               (i64::max_value() - 1) as u64,
               (i64::max_value() - 2) as u64,
               (i64::max_value() - 3) as u64,
               (i64::max_value() - 4) as u64,
               (i64::max_value() - 5) as u64,
               (i64::min_value() + 0) as u64,
               (i64::min_value() + 1) as u64,
               (i64::min_value() + 2) as u64,
               (i64::min_value() + 3) as u64,
               (i64::min_value() + 4) as u64,
               (i64::min_value() + 5) as u64];


    let empty = UIntMultiple {
        modulus: 0,
        residue: 3,
    };
    confirm_valueset_weaker(&empty, &empty.as_knownbits(), &seq);
    confirm_valueset_weaker(&empty, &empty.as_urange(), &seq);
    confirm_valueset_weaker(&empty, &empty.as_srange(), &seq);

    let m12_6 = UIntMultiple {
        modulus: 12,
        residue: 6,
    };
    confirm_valueset_weaker(&m12_6, &m12_6.as_knownbits(), &seq);
    confirm_valueset_weaker(&m12_6, &m12_6.as_urange(), &seq);
    confirm_valueset_weaker(&m12_6, &m12_6.as_srange(), &seq);

    let b12_6: KnownBits = m12_6.as_knownbits();
    assert_eq!(b12_6.zerobits, 0x1);
    assert_eq!(b12_6.onebits, 0x2);
}

#[test]
fn test_knownbits_conversions() {
    let upperknown = KnownBits {
        zerobits: 0xffff000000000000,
        onebits: 0x0000ffff00000000,
    };
    let upper_urange = upperknown.as_urange();
    let upper_srange = upperknown.as_srange();
    let upper_mult = upperknown.as_umultiple();
    assert_eq!(upper_urange.min, 0x0000ffff00000000);
    assert_eq!(upper_urange.max, 0x0000ffffffffffff);
    assert_eq!(upper_srange.min, 0x0000ffff00000000);
    assert_eq!(upper_srange.max, 0x0000ffffffffffff);
    assert_eq!(upper_mult.modulus, 1);
    assert_eq!(upper_mult.residue, 0);

    let lowerknown = KnownBits {
        zerobits: 0x00000000ffff0000,
        onebits: 0x000000000000ffff,
    };
    let lower_urange = lowerknown.as_urange();
    let lower_srange = lowerknown.as_srange();
    let lower_mult = lowerknown.as_umultiple();
    assert_eq!(lower_urange.min, 0x000000000000ffff);
    assert_eq!(lower_urange.max, 0xffffffff0000ffff);
    assert_eq!(lower_srange.min, -0x7fffffffffff0001);
    assert_eq!(lower_srange.max, 0x7fffffff0000ffff);
    assert_eq!(lower_mult.modulus, 0x100000000);
    assert_eq!(lower_mult.residue, 0xffff);
}

#[test]
fn test_uintmultiple_intersection() {
    for a in 2..6 {
        for i in 0..a {
            let x = UIntMultiple {
                modulus: a,
                residue: i,
            };

            for b in 3..15 {
                for j in 0..b {
                    let y = UIntMultiple {
                        modulus: b,
                        residue: j,
                    };

                    confirm_valueset_and(&x, &y, &(0..a * b + 2).collect::<Vec<_>>());
                    confirm_valueset_or_weaker(&x, &y, &(0..a * b + 2).collect::<Vec<_>>());

                }
            }

        }
    }
}

#[test]
fn test_knownbits_scan() {
    let kb = KnownBits {
        onebits: 0b0100100,
        zerobits: 0b0010010,
    };
    assert_eq!(kb.scan_up(0b0000000).unwrap(), 0b0100100);
    assert_eq!(kb.scan_up(0b0000001).unwrap(), 0b0100100);
    assert_eq!(kb.scan_up(0b0000010).unwrap(), 0b0100100);
    assert_eq!(kb.scan_up(0b0000011).unwrap(), 0b0100100);
    assert_eq!(kb.scan_up(0b0000100).unwrap(), 0b0100100);
    assert_eq!(kb.scan_up(0b0100101).unwrap(), 0b0100101);
    assert_eq!(kb.scan_up(0b0100110).unwrap(), 0b0101100);
    assert_eq!(kb.scan_up(0b0100111).unwrap(), 0b0101100);
    assert_eq!(kb.scan_up(0b0101100).unwrap(), 0b0101100);
    assert_eq!(kb.scan_up(0b0101101).unwrap(), 0b0101101);
    assert_eq!(kb.scan_up(0b0101110).unwrap(), 0b1100100);
    assert_eq!(kb.scan_up(0b0101111).unwrap(), 0b1100100);
    assert_eq!(kb.scan_up(0b0110000).unwrap(), 0b1100100);
    assert_eq!(kb.scan_up(0b1101101).unwrap(), 0b1101101);
}
