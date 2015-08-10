extern crate r2pipe;

use std::io;
use std::io::prelude::*;
use self::r2pipe::R2Pipe;

pub struct Radeco<'a> {
    filename: String,
    r2p: Option<&'a mut R2Pipe>,
}

impl<'a> Radeco<'a> {
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
    
    pub fn file(filename: String) -> Result<Radeco<'a>,&'static str> {
        let r = Radeco{filename: filename.to_owned(), r2p: None};
        println!("Warning: Build with --feature deprecated until this refactor is finished");
	//let r2p = R2Pipe::spawn (&*filename).unwrap();
	//r.r2p = Some(&mut r2p); // XXX this is wrong
        // Err("[r2pipe] initialization error")
        Ok(r)
    }
}
