extern crate radeco;

use radeco::frontend::esil;
use std::io;
use std::io::Write;

fn main() {
	let mut reader = io::stdin();
	let mut writer = io::stdout();
	let mut input = String::new();
	while reader.read_line(&mut input).unwrap() > 0 {
		writer.write(input.as_bytes()).unwrap();
		input.truncate(0);
	}
}
