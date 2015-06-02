// helper functions
// warning: rust uses '!' as bitwise not operator
// blcic(!x) = tzmsk(x)+1 = product of the '2's of x's prime decomposition

pub fn blcic(x: u64) -> u64 { x.wrapping_add(1) & !x } // 101111-> 10000
pub fn tzmsk(x: u64) -> u64 { x.wrapping_sub(1) & !x } // 010000->  1111

pub fn gcd_lcm(mut m: u64, mut n: u64) -> (u64, u64) {
	let p = m*n;
	while m != 0 { let o = m; m = n % m; n = o; }
	(n, if n != 0 {p/n} else {0})
}

pub fn multiplicative_inverse(mut a: u64, n: u64) -> Option<u64> {

	println!("{:?} {:?}", a, n);

	if n == 0 { return Option::None }
	a %= n;
	println!(" ({:?} {:?})", a, n);
	if a == 0 { return Option::None }

	let mut  t: u64 = 0;
	let mut  r: u64 = n;
	let mut nt: u64 = 1;
	let mut nr: u64 = a;

	while nr != 0 {
		// TODO: make sure q*nt never overflows
		let (ot, or) = (nt, nr);
		let q = r / nr;

		// TODO: make sure this doesn't overflow
		nt = (t + q*(n-nt)) % n;
		nr = r - q * nr;
		t = ot;
		r = or;
	}
	if r > 1 { return Option::None }
	println!("= {:?}", t);
	Option::Some(t)
}
