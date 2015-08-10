extern crate r2pipe;

use std::io;
use std::io::prelude::*;
use self::r2pipe::R2Pipe;

pub struct Radeco {
    filename: String,
    r2p: Option<R2Pipe>,
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
        let mut r = Radeco{filename: filename.to_owned(), r2p: None};
	let r2p = R2Pipe::spawn (&*filename).unwrap();
	r.r2p = Some(r2p);
        // Err("[r2pipe] initialization error")
        Ok(r)
    }

    pub fn pipe() -> Result<Radeco,&'static str> {
        let mut r = Radeco{filename: "".to_owned(), r2p: None};
	let r2p = R2Pipe::open ().unwrap();
	r.r2p = Some(r2p);
        // Err("[r2pipe] initialization error")
        Ok(r)
    }
}
