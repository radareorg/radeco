#[macro_use]
extern crate r2pipe;
extern crate docopt;
extern crate rustc_serialize;

#[cfg(not(feature = "deprecated"))]
mod radeco;
#[cfg(not(feature = "deprecated"))]
use radeco::*;

#[cfg(feature = "deprecated")]
use std::io;
#[cfg(feature = "deprecated")]
use std::io::prelude::*;
#[cfg(feature = "deprecated")]
use r2pipe::R2Pipe;
use docopt::Docopt;
use std::process::exit;

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

#[derive(Debug, RustcDecodable)]
struct Args {
    flag_output: String,
    flag_version: bool,
    flag_shell: bool,
    arg_file: String,
}

static USAGE: &'static str = "
radeco.

Usage:
  radeco <file>
  radeco --shell <file>
  radeco --output=<output> <file>
  radeco --version

Options:
  -h --help        Show this screen.
  --version        Show version.
  --shell          Run interactive prompt.
  --output=<mode>  Select output mode.
";

/* TODO: use readline, dietline or any other rust-friendly prompt library */
#[cfg(feature = "deprecated")]
fn radeco_shell(r2p: &mut R2Pipe, _: &Args) {
    let mut stdin = io::stdin();
    'mainloop: loop {
        let mut input = String::new();
        print!("> ");
        io::stdout().flush().ok();
        stdin.read_line(&mut input).ok();

        if input == "" || input == "exit\n" {
            break 'mainloop;
        }

        println!("{}", r2p.cmd(input.trim()));
    }
}

#[cfg(feature = "deprecated")]
fn radeco_file(args: &Args) -> i32 {
    let file = &*args.arg_file;
    let mut r2p = open_pipe!(file).unwrap();
    println!("r2pipe {:?}", file);
    println!("cmd {:?}", r2p.cmd("?V"));
    if args.flag_output != ""{
        println!("TODO: --output");
    }
    if args.flag_shell {
        radeco_shell(&mut r2p, args);
    }
    r2p.close();
    0
}

#[cfg(not(feature = "deprecated"))]
fn radeco_file(args:&Args) -> i32 {
    let mut r = Radeco::file("/bin/ls".to_owned()).unwrap();
    if args.flag_shell {
        r.shell();
    } else {
        println!("Batch stuff here");
    }
    r.close();
    0
}

fn main() {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());
    if args.flag_version {
        println!("Version: {:?}", VERSION);
        exit(0);
    }
    if args.arg_file != "" {
        exit(radeco_file(&args));
    }
    println!("{:?}", args);
}
