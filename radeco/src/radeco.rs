extern crate r2pipe;
extern crate radeco_lib;

use std::io;
use std::io::prelude::*;
use self::r2pipe::R2Pipe;
use self::radeco_lib::frontend::r2::R2;

pub struct Radeco {
	filename: String,
	r2p: Option<R2Pipe>,
	r2p2: Option<R2>,
}

pub fn spawn_shell(bname: String) -> i32 {
	let mut r2 = R2::new(&*bname);
	loop {
		let mut input = String::new();
		print!("r2>> ");
		io::stdout().flush().ok();
		io::stdin().read_line(&mut input).ok();
		let input = input.trim();
		if input.len() == 0 || input == "exit" {
			r2.send("quit");
			break;
		}
		r2.send(&*input);
		print!("{}", r2.recv());
	}
	0
}

impl Radeco {
	/* TODO: use readline, dietline or any other rust-friendly prompt library */
	pub fn shell(&mut self) -> bool {
		println!("shellify {:?}", self.filename);
		let mut stdin = io::stdin();
		'mainloop: loop {
			let mut input = String::new();
			print!("> ");
			io::stdout().flush().ok();
			stdin.read_line(&mut input).ok();

			if input == "" || input == "exit\n" {
				break 'mainloop;
			}

			match self.r2p.as_mut() {
				Some(p) => println!("{}", p.cmd(input.trim())),
				None => println!("No r2p")
			}
		}
		true
	}

	pub fn close(&mut self) -> bool {
		match self.r2p.as_mut() {
			Some(p) => p.close(),
			None => println!("")
		}
		true
	}

	pub fn file(filename: String) -> Result<Radeco,&'static str> {
		let mut r = Radeco{filename: filename.to_owned(), r2p: None, r2p2: None};
		let r2p = R2Pipe::spawn(&*filename).unwrap();
		r.r2p = Some(r2p);
		// Err("[r2pipe] initialization error")
		Ok(r)
	}

	pub fn pipe() -> Result<Radeco,&'static str> {
		let mut r = Radeco{filename: "".to_owned(), r2p: None, r2p2: None};
		let r2p = R2Pipe::open().unwrap();
		r.r2p = Some(r2p);
		// Err("[r2pipe] initialization error")
		Ok(r)
	}
}
