use docopt::Docopt;
use radeco;
use errors::ArgError;
use r2pipe::R2Pipe;
use rustc_serialize::json;
use structs;
use std::fs::{File};
use std::io::{Write, Read};

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

#[derive(Debug, RustcDecodable)]
struct Args {
	arg_esil: Option<String>,
	arg_file: Option<String>,
	arg_output: Option<String>,
	arg_address: Option<String>,
	cmd_run: bool,
	flag_address: bool,
	flag_config: bool,
	flag_help: bool,
	flag_make_config: bool,
	flag_output: bool,
	flag_shell: bool,
	flag_verbose: bool,
	flag_version: bool,
}

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

pub type ArgResult<T> = Option<Result<T, ArgError>>;

fn write_file(fname: String, res: String) {
	let mut file = File::create(fname).unwrap();
	file.write_all(res.as_bytes()).unwrap();
}


fn make_config(bin_name:String, esil_opt:Option<String>,
			   addr: String, run: bool)
{
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


fn json_slurp(fname: String) -> Result<structs::Input, ArgError> {
	let mut fh = try!(File::open(fname));
	let mut raw_json = String::new();
	try!(fh.read_to_string(&mut raw_json));
	json::decode(&*raw_json).map_err(|x| ArgError::DecodeError(x))
}

pub fn init() -> ArgResult<radeco::Radeco> {
	let args: Args = Docopt::new(USAGE)
		.and_then(|d| d.decode())
		.unwrap_or_else(|e| e.exit());

	if args.flag_version {
		println!("Version: {:?}", VERSION);
		return None;
	} else if args.flag_help {
		println!("{}", USAGE);
		return None;
	}

	let mut outmode: String = "c".to_owned();
	let mut address: String = "entry0".to_owned();

	if args.flag_output {
		let arg_output: String = args.arg_output.clone().unwrap();
		let valid = ["c" ,"C" ,"r2" ,"dot" ,"r2g" ,"json" ,"ssa"];
		if !valid.contains(&&*arg_output) {
			let e = "Invalid mode for --output=<mode>";
			return Some(Err(ArgError::InvalidArgument(e.to_owned())));
		}
		outmode = arg_output
	};

	if args.flag_address {
		address = args.arg_address.unwrap();
	}

	if args.flag_make_config {
		let filename = args.arg_file.clone().unwrap();
		let mut esil: Option<String> = None;
		if let Some(esilstr) = args.arg_esil.clone() {
			esil = Some(esilstr);
		}
		make_config(filename, esil, address, true);
		return None;
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
			println!("[X] {}", r.err().unwrap())
		}
		return None;
	}

	//if args.flag_shell {
		//if args.arg_file.is_none() {
			//println!("{}", USAGE);
			//return None
		//}
		//let file = args.arg_file.clone().unwrap();
		//exit(spawn_shell(file));
	//}

	/* perform batch decompilation */
	if let Some(file) = args.arg_file {
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
			if args.flag_output {
				println!("Missing file");
			} else {
				println!("{}", USAGE);
			}
			return None;
		}
	}

	Some(Ok(radeco::Radeco::new()))
}
