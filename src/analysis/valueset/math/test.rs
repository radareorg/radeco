// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

use super::{blcic, multiplicative_inverse, tzmsk};

fn confirm_multiplicative_inverse(a: u64, n: u64) {
    let x = multiplicative_inverse(a, n).unwrap();
    assert!((a * x) % n == 1);
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
        // See http://en.wikipedia.
        // org/wiki/Coprime_integers#Generating_all_coprime_pairs
        test_multiplicative_inverse_recursively(depth - 1, 2 * m - n, m);
        test_multiplicative_inverse_recursively(depth - 1, 2 * m + n, m);
        test_multiplicative_inverse_recursively(depth - 1, m + 2 * n, n);
    }
}

#[test]
fn test_multiplicative_inverse() {
    test_multiplicative_inverse_recursively(3, 2, 1);
    test_multiplicative_inverse_recursively(3, 3, 1);
}

#[test]
fn test_blcic_tzmsk_equiv() {
    for &i in &[0x0000000000000000,
                0x00000000ffff0000,
                0x0000ffff0000ffff,
                0x0000ffffffffffff,
                0xffff000000000000,
                0xffff0000ffff0000,
                0xffffffff0000ffff,
                0xffffffffffffffff] {
        assert_eq!(blcic(!i), tzmsk(i).wrapping_add(1));
    }
}
