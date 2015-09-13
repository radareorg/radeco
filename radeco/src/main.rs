#![allow(unused_variables, dead_code)]
#[macro_use]
extern crate r2pipe;
extern crate docopt;
extern crate rustc_serialize;
extern crate radeco_lib;

mod radeco;
mod structs;
mod errors;

//use self::radeco::Radeco;
use self::r2pipe::R2Pipe;
use rustc_serialize::json;
use docopt::Docopt;
use std::process::exit;
use std::fs::{File};
use std::io::{Write, Read};

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

#[derive(Debug, RustcDecodable)]
struct Args {
    flag_output: Option<String>,
    arg_esil: Option<String>,
    flag_version: bool,
    flag_address: String,
    flag_verbose: bool,
    flag_shell: bool,
    flag_help: bool,
    arg_file: Option<String>,
    flag_config: bool,
    flag_make_config: bool,
    cmd_run: bool,
}

static USAGE: &'static str = "
radeco. The radare2 decompiler.

Usage:
  radeco <file>
  radeco [options] [<file>]
  radeco run [options] [<file>]
  radeco --shell <file>
  radeco --output=<output> <file>
  radeco --version
  radeco --verbose

Options:
  --config <file>        Run decompilation using json rules file.
  --make-config <file>   Wizard to make the JSON config.
  -a --address=<addr>    Address of function to decompile.
  -e --esil=<expr>       Evaluate given esil expression.
  -o --output=<mode>     Output mode (c, ssa, json, r2, r2g, dot).
  --verbose              Display verbose output.
  --shell                Run interactive prompt.
  --version              Show version.
  --help                 Show this screen.
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

fn make_config(bin_name:String, esil_opt:Option<String>, addr:String, run: bool) {
    let inp = structs::input_builder(bin_name, esil_opt, addr);
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

fn json_slurp(fname: String) -> Result<structs::Input, errors::ReadErr> {
    let mut fh = try!(File::open(fname));
    let mut raw_json = String::new();
    try!(fh.read_to_string(&mut raw_json));
    json::decode(&*raw_json).map_err(|x| errors::ReadErr::DecodeErr(x))
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

    let mut outmode: String = "c".to_string();
    let mut address: String = "entry0".to_string();

    if args.flag_output.is_some() {
        let flag_output = args.flag_output.clone().unwrap();
        if match &*flag_output {
            "c"|"C"|"r2"|"dot"|"r2g"|"json"|"ssa" => true,
            _ => false,
        } {
            println!("--> output mode is ok");
            outmode = flag_output;
        } else {
            println!("Invalid mode for --output=<mode>");
            exit(1);
        }
    }

    if args.flag_address != "" {
        address = args.flag_address;
    }

    if args.flag_make_config {
        let filename = args.arg_file.clone().unwrap();
        let mut esil: Option<String> = None;
        if let Some(esilstr) = args.arg_esil.clone() {
            esil = Some(esilstr);
        }
        println!("MAKE CONFIG");
        make_config(filename, esil, address, true); //args.cmd_run);
        exit(0);
    }

    if args.flag_config {
        if args.arg_file.is_none() {
            println!("{}", USAGE);
        }
        let filename = args.arg_file.clone().unwrap();
        let r = json_slurp(filename);
        if let Ok(inp) =  r {
            let mut runner = inp.validate().unwrap();
            runner.run();
            runner.dump();
        } else {
            println!("[x] {}", r.err().unwrap())
        }
        exit(0);
    }

    if args.flag_shell {
        if args.arg_file.is_none() {
            println!("{}", USAGE);
            exit(0);
        }
        let file = args.arg_file.clone().unwrap();
        exit(spawn_shell(file));
    }

    /* perform batch decompilation */
    if let Some (file) = args.arg_file.clone() {
        if let Ok(r2p) = R2Pipe::spawn(&*file) {
            println!("OK");
        } else {
            // XXX: r2pipe doesnt returns fail if the target file doesnt exist or r2 fails to run
            println!("Cannot find file");
        }
    } else {
        if let Ok(mut r2p) = R2Pipe::open() {
            println!("Running from r2");
            println!("--> address {}", address);
            println!("--> output {}", outmode);
            r2p.cmd (&*format!("s {}", address));
            //r2p.cmd ("af");
            let res = r2p.cmd("pd");
            println!("--> {}", res);

            //let mut r = Radeco::pipe().unwrap();
            //r.close();
            /* perform decompilation of function at current offset */
            //if args.arg_file != "" {
            //  if args.arg_file == "-" {
            //      exit(
            //  } else {
            //      exit(radeco_file(&args));
            //  }
            //}
            // XXX it hangs here, r2pipe looks buggy
            r2p.close();
        } else {
            if args.flag_output.is_some() {
                println!("Missing file");
            } else {
                println!("{}", USAGE);
            }
            exit(1);
        }
    }
}
