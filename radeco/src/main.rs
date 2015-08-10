#[macro_use]
extern crate r2pipe;
extern crate docopt;
extern crate rustc_serialize;

mod radeco;
use radeco::*;

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

fn radeco_pipe(args:&Args) -> i32 {
    let mut r = Radeco::pipe().unwrap();
    if args.flag_shell {
        r.shell();
    } else {
        println!("Running radeco via pipe");
	// TODO get offset, function, bin info, ..
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
        if args.arg_file == "-" {
            exit(radeco_pipe(&args));
        } else {
            exit(radeco_file(&args));
        }
    }
    println!("{:?}", args);
}
