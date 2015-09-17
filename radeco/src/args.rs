use docopt::Docopt;
use radeco_lib;
use errors::ArgError;
use rustc_serialize::json;
use structs;
use std::fs::{File};
use std::io::{Write, Read};
use std::path::Path;

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
  --config <file>               Run decompilation using json rules file.
  --make-config <file>          Wizard to make the JSON config.
  -a --address=<addr>           Address of function to decompile.
  -e --esil=<esil_expr>         Evaluate given esil expression.
  -o --output=<mode>            Output mode (c, ssa, json, r2, r2g, dot).
  --verbose                     Display verbose output.
  --shell                       Run interactive prompt.
  --version                     Show version.
  --help                        Show this screen.
";

#[derive(Debug, RustcDecodable)]
struct Args {
	arg_esil_expr: Option<Vec<String>>,
	arg_file: Option<String>,
	arg_output: Option<String>,
	cmd_run: bool,
	flag_address: Option<String>,
	flag_config: bool,
	flag_help: bool,
	flag_make_config: bool,
	flag_output: Option<Vec<String>>,
	flag_shell: bool,
	flag_verbose: bool,
	flag_version: bool,
	flag_esil: Option<Vec<String>>,
}

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

pub type ArgResult<T> = Option<Result<T, ArgError>>;

fn json_slurp(fname: String) -> Result<structs::Input, ArgError> {
	let mut fh = try!(File::open(fname));
	let mut raw_json = String::new();
	try!(fh.read_to_string(&mut raw_json));
	json::decode(&*raw_json).map_err(|x| ArgError::DecodeError(x))
}

fn make_config() -> structs::Input
{
	structs::input_builder()
}

fn write_file(fname: String, res: String) {
	let mut file = File::create(fname).unwrap();
	file.write_all(res.as_bytes()).unwrap();
}


pub struct Radeco {
	bin_name: Option<String>,
	esil: Option<Vec<String>>,
	addr: String,
	name: Option<String>,
	outpath: String,
	stages: Vec<usize>,
	verbose: bool,
	runner: Option<radeco_lib::utils::Runner>,
}

enum Mode {
	R2Pipe,
	Standalone,
}

impl Radeco {

	// Return the mode radeco is operating in:
	fn mode() -> Mode {
		if radeco_lib::frontend::r2::R2::in_session() {
			Mode::R2Pipe
		} else {
			Mode::Standalone
		}
	}

	// Setup the defaults for the arguments:
	// This depends on the mode radeco is operating in i.e:
	//     - r2pipe, when called from within radare2
	//                    ( vs )
	//     - standalone, when radeco spawns radare2
	// 
	// Below is the list of defaults that are set depending on the mode.
	//     - bin_name: binary loaded in r2 vs None
	//     - esil: None vs None
	//     - addr: current address vs entry0
	//     - name: bin_name vs None
	//     - outpath: bin_name_out vs outputs
	//     - stages: All vs All
	//     - verbose: false vs false
	fn defaults() -> structs::Input {
		let bin_name;
		let addr;
		let name;
		let outpath;
		let outmodes = vec![structs::Outmode::Dot];
		let stages = vec![0, 1, 2, 3, 4, 5, 6];

		match Radeco::mode() {
			Mode::R2Pipe => {
				// Open the existing r2 session.
				let mut r2 = radeco_lib::frontend::r2::R2::new(None).unwrap();
				addr = {
					r2.send("s");
					r2.recv().trim().to_owned()
				};
				// TODO: Error Handling here.
				let bin_info = r2.get_bin_info().unwrap();
				let core = bin_info.core.unwrap();
				let path = match core.file.as_ref() {
					Some(ref f) => {
						bin_name = Some(format!("{}", f));
						Path::new(f.clone())
					},
					None => panic!("No file open in r2"),
				};
				let file = path.file_name()
				               .unwrap()
							   .to_str()
							   .map(|s| s.to_owned());

				outpath = format!("./{}_out", bin_name.as_ref().unwrap());
				name = Some(format!("{}.run", bin_name.as_ref().unwrap()));
			},
			Mode::Standalone => {
				bin_name = None;
				addr = "entry0".to_owned();
				name = None;
				outpath = "./outputs".to_owned();
			},
		}

		structs::Input {
			bin_name: bin_name,
			esil: None,
			addr: Some(addr),
			name: name,
			outpath: Some(outpath),
			stages: stages,
			outmodes: Some(outmodes),
			verbose: Some(false),
		}
	}

	pub fn new(input: structs::Input) -> ArgResult<Radeco> {
		let runner = input.validate();
		if runner.is_err() {
			return Some(Err(runner.err().unwrap()));
		}

		let radeco = Radeco {
			bin_name: input.bin_name.clone(),
			esil: input.esil.clone(),
			addr: input.addr.clone().unwrap(),
			name: input.name.clone(),
			outpath: input.outpath.clone().unwrap(),
			stages: input.stages.clone(),
			verbose: input.verbose.clone().unwrap(),
			runner: runner.ok(),
		};

		Some(Ok(radeco))
	}

	pub fn init() -> ArgResult<Radeco> {
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
		
		// Make config file and run radeco with it
		if args.flag_make_config {
			let input = make_config();
			return Radeco::new(input);
		}
		
		// Run from a predefined configuration file
		if args.flag_config {
			if args.arg_file.is_none() {
				let e = "No argument provided for config";
				return Some(Err(ArgError::InvalidArgument(e.to_owned())));
			}
			let filename = args.arg_file.clone().unwrap();
			let input = json_slurp(filename);
			if input.is_err() {
				return Some(Err(input.err().unwrap()))
			}
			return Radeco::new(input.unwrap());
		}

		println!("{:?}", args);
		// TODO: Decide if defaults have to be a part of Input or Radeco.
		let mut input = Radeco::defaults();

		println!("Defaults set to: {:?}", input);

		// Get binary name
		input.bin_name = args.arg_file.clone();

		// Address to begin decompilation/analysis
		if args.flag_address.is_some() {
			input.addr = args.flag_address
		}

		if args.flag_esil.is_some() {
			input.esil = args.flag_esil
		}

		// Verbose configuration
		input.verbose = Some(args.flag_verbose);

		if args.flag_output.is_some() {
			let arg_output: Vec<String> = args.flag_output.unwrap();
			// TODO: Expand outmodes. For now, we only output the dot.
			// Which is already the default behaviour.
		};

		println!("Final input: {:?}", input);
		Radeco::new(input)
	}

	pub fn run(&mut self) -> Result<(), String> {
		println!("Running radeco!");
		match self.runner.as_mut(){
			Some(ref mut runner) => runner.run(),
			None => panic!("Uninitialized instance of radeco!"),
		}
		Ok(())
	}
}
