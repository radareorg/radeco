#![allow(unused_variables, dead_code)]
#[macro_use]
extern crate r2pipe;
extern crate docopt;
extern crate rustc_serialize;
extern crate radeco_lib;
//extern crate core;

mod radeco;
mod structs;

use rustc_serialize::json;
use docopt::Docopt;
use std::process::exit;
use std::fs::{File};
use std::io::{Write, Read};

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

#[derive(Debug, RustcDecodable)]
struct Args {
    flag_output: String,
    flag_version: bool,
    flag_shell: bool,
	flag_help: bool,
    arg_file: Option<String>,
	flag_from_json: bool,
	flag_json_builder: bool,
	cmd_run: bool,
}

static USAGE: &'static str = "
radeco. The radare2 decompiler.

Usage:
  radeco <file>
  radeco [options]
  radeco run [options] [<file>]
  radeco --shell <file>
  radeco --output=<output> <file>
  radeco --version

Options:
  --help                 Show this screen.
  --version              Show version.
  --shell                Run interactive prompt.
  --output=<mode>        Select output mode.
  --from-json            Run radeco based on config and information
                         from input json file. Needs an input file.
  --json-builder         Interactive shell used to build the config
                         json for radeco. When used with run, the 
                         config generated is automatically used to
                         run radeco rather than dumping it to a file.
";

fn spawn_shell(bname: String) -> i32 {
	radeco::spawn_shell(bname)
}

//fn radeco_pipe(args:&Args) -> i32 {
	//let mut r = Radeco::pipe().unwrap();
	//if args.flag_shell {
		//r.shell();
	//} else {
		//println!("Running radeco via pipe");
		//// TODO get offset, function, bin info, ..
	//}
	//r.close();
	//0
//}

fn write_file(fname: String, res: String) {
	let mut file = File::create(fname).unwrap();
	file.write_all(res.as_bytes()).unwrap();
}

fn json_builder(run: bool) {
	let inp = structs::input_builder();
	let mut runner = inp.validate().unwrap();
	if !run {
		let mut name = inp.name.clone().unwrap();
		let json = json::as_pretty_json(&inp);
		let raw_json = format!("{}", json);
		name.push_str(".json");
		write_file(name, raw_json);
	} else {
		runner.run();
		runner.dump();
	}
}

fn read_json(fname: String) -> structs::Input {
	let mut raw_json = String::new();
	let _ = File::open(fname).unwrap().read_to_string(&mut raw_json);
	json::decode(&*raw_json).unwrap()
}

fn main() {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());
    if args.flag_version {
        println!("Version: {:?}", VERSION);
        exit(0);
    }

	if args.flag_help {
		println!("{}", USAGE);
		exit(0);
	}

    //if args.arg_file != "" {
        //if args.arg_file == "-" {
            //exit(radeco_pipe(&args));
        //} else {
            //exit(radeco_file(&args));
        //}
    //}
	
	if args.flag_json_builder {
		json_builder(args.cmd_run);
	}

	if args.flag_from_json {
		if args.arg_file.is_none() {
			println!("{}", USAGE);
		}

		let inp = read_json(args.arg_file.clone().unwrap());
		let mut runner = inp.validate().unwrap();
		runner.run();
		runner.dump();
	}

	if args.flag_shell {
		if args.arg_file.is_none() {
			println!("{}", USAGE);
			exit(0);
		}
		let status = spawn_shell(args.arg_file.clone().unwrap());
		exit(status);
	}
}
