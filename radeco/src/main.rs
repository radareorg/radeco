#![allow(unused_variables, dead_code)]
#[macro_use]
extern crate r2pipe;
extern crate docopt;
extern crate rustc_serialize;
extern crate radeco_lib;

mod radeco;
mod structs;
mod errors;
mod args;

//use self::radeco::Radeco;
//use self::r2pipe::R2Pipe;
use std::process::exit;
//use std::fs::{File};
//use std::io::{Write, Read};

//fn spawn_shell(bname: String) -> i32 {
	//radeco::spawn_shell(bname)
//}

fn main() {
	let radeco = args::init();
	if radeco.is_none() {
		exit(0);
	}
	
	let radeco = radeco.unwrap();
	if radeco.is_err() {
		println!("[X] {}", radeco.err().unwrap());
		exit(2);
	}

	let radeco = radeco.unwrap();

	let result = radeco.run();

	match result {
		Ok(ref r) => { // Do something
		},
		Err(e) => {
		},
	}
}
