extern crate radeco;

use radeco::backend::D;
use radeco::backend::lang_c;
use radeco::backend::scf::SCFNode::{Statement, Cond};

fn main() {
	let s1 = Box::new(Statement::<D>("puts(\"Hello, world!\");".to_string()));
	let s2 = Box::new(Statement::<D>("return 0;".to_string()));
	let c = Box::new(Cond::<D>{
		cond: "argc < 2".to_string(),
		body: s1,
		alt:  s2
	});
	if let Cond::<D>{cond: _, ref body, ref alt} = *c {
		println!("{:?}", body);
		println!("{:?}", alt);
	}
	println!("{:?}", c);
	println!("{}", lang_c::serialize(&c));
}
