extern crate docopt;

use self::docopt::{Docopt, Error};
use std::process;

pub fn parse_args(usage: &str) -> Option<String> {

    let s = Some(env!("CARGO_PKG_VERSION").to_string());
    let args = Docopt::new(usage).and_then(|d| d.help(true).version(s).parse());

    match args {
        Err(Error::WithProgramUsage(box Error::Help, ref msg)) |
        Err(Error::Version(ref msg)) => {
            println!("{}", msg);
            process::exit(0)
        }
        Ok(a) => Some(a.get_str("<bin>").to_string()),
        _ => None,
    }
}
