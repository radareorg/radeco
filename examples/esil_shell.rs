extern crate radeco;

use std::io;
use std::io::Write;

#[warn(unused_imports)]
use radeco::frontend::esil;

fn main() {
	let mut reader = io::stdin();
	let mut writer = io::stdout();
	let mut input = String::new();
	while reader.read_line(&mut input).unwrap() > 0 {
		writer.write(input.as_bytes()).unwrap();
		input.truncate(0);
	}
}
