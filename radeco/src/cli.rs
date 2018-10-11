extern crate docopt;

use self::docopt::{Docopt, Error};
use std::process;

pub fn parse_args(usage: &str) -> (Option<String>, bool, bool) {

    let s = Some(env!("VERSION_STR").to_string());
    let args = Docopt::new(usage).and_then(|d| d.help(true).version(s).parse());
    match args {
        Err(Error::WithProgramUsage(box Error::Help, ref msg)) |
        Err(Error::Version(ref msg)) => {
            println!("{}", msg);
            process::exit(0)
        }
        Ok(a) => {
            let is_append = a.get_bool("--append");
            let is_batch = a.get_bool("--batch");
            let bin = a.get_str("BIN");
            if is_batch && bin.len() == 0 {
                eprintln!("Pass a binary for batch mode");
                process::exit(0);
            }
            (Some(bin.to_string()), is_append, is_batch)
        },
        _ => (None, false, false),
    }
}
