#[macro_use]
extern crate docopt;
extern crate rustc_serialize;
extern crate radeco_lib;

mod structs;
mod errors;
mod args;

use std::process::exit;

fn main() {
	let radeco = args::Radeco::init();
	if radeco.is_none() {
		exit(0);
	}
	let radeco = radeco.unwrap();
	if radeco.is_err() {
		println!("[X] {}", radeco.err().unwrap());
		exit(2);
	}

	let mut radeco = radeco.unwrap();
	let result = radeco.run();

	match result {
		Ok(_) => {
			radeco.output();
		},
		Err(e) => {
			println!("[X] Error: {}", e);
		},
	}
}
