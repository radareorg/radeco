// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

#[cfg(test)]
mod test;

// warning: rust uses '!' as bitwise not operator
// blcic(!x) = tzmsk(x)+1 = product of the '2's of x's prime decomposition
pub fn blcic(x: u64) -> u64 {
    x.wrapping_add(1) & !x
} // 101111-> 10000
pub fn tzmsk(x: u64) -> u64 {
    x.wrapping_sub(1) & !x
} // 010000->  1111
pub fn bitsmear(mut smear: u64) -> u64 {
    smear |= smear >> 32;
    smear |= smear >> 16;
    smear |= smear >> 8;
    smear |= smear >> 4;
    smear |= smear >> 2;
    smear |= smear >> 1;
    smear
}

pub fn gcd_lcm(mut m: u64, mut n: u64) -> (u64, u64) {
    let p = m * n;
    while m != 0 {
        let o = m;
        m = n % m;
        n = o;
    }
    (n,
     if n == 0 {
        0
    } else {
        p / n
    })
}

pub fn multiplicative_inverse(mut a: u64, n: u64) -> Option<u64> {

    // println!("{:?} {:?}", a, n);

    if n == 0 {
        return Option::None;
    }
    a %= n;
    // println!(" ({:?} {:?})", a, n);
    if a == 0 {
        return Option::None;
    }

    let mut t: u64 = 0;
    let mut r: u64 = n;
    let mut nt: u64 = 1;
    let mut nr: u64 = a;

    while nr != 0 {
        
        let (ot, or) = (nt, nr);
        let q = match r.checked_div(nr) {
            Some(x) => x,
            None => return Option::None;
        };

        //nt = (t + q * (n - nt)) % n;
        
        nt = match (match t.checked_add(q) {
            Some(layer1) => {
                match layer1.checked_mul(match n.checked_sub(nt) {
                    Some(layer2) => layer2,
                    None => return Option::None
                }) {
                    Some(layer2) => layer2,
                    None => return Option::None
                }
            },
            None => return Option::None
        }).checked_rem(n) {
            Some(x) => x,
            None => return Option::None
        };
        
        // nr = r - q * nr;
        
        nr = match r.checked_sub(match q.checked_mul(nr) {
            Some(x) => x,
            None => return Option::None
        }) {
            Some(x) => x,
            None => return Option::None
        };
        t = ot;
        r = or;
    }
    if r > 1 {
        return Option::None;
    }
    // println!("= {:?}", t);
    Option::Some(t)
}
