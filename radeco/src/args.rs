use docopt::Docopt;
use radeco_lib;
use errors::ArgError;
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

Options:
  --config <json_file>     Run decompilation using json rules file.
  --make-config <file>     Wizard to make the JSON config.
  -a --address=<addr>      Address of function to decompile.
  -e --esil=<esil_expr>    Evaluate given esil expression.
  -o --output=<output>     Specify output directory.
  -p --pipeline=<pipe>     Stages in the pipeline. Comma separated values.
                           Prefix the string with '=' (such as =ssa)
                           to obtain the output the stage.
                           Valid values: c,C,r2,ssa,cfg,const,dce,verify
  -q --quiet               Display silent output.
  -s --shell               Run interactive prompt.
  -v --version             Show version.
  -h --help                Show this screen.
";

#[derive(Debug, RustcDecodable)]
struct Args {
	arg_esil_expr: Option<Vec<String>>,
	arg_file: Option<String>,
	cmd_run: bool,
	flag_address: Option<String>,
	flag_config: Option<String>,
	flag_help: bool,
	flag_make_config: bool,
	flag_shell: bool,
	flag_quiet: bool,
	flag_version: bool,
	flag_output: Option<String>,
	flag_esil: Option<Vec<String>>,
	flag_pipeline: Option<String>,
}

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

pub type ArgResult<T> = Option<Result<T, ArgError>>;

fn json_slurp(fname: String) -> Result<structs::Input, ArgError> {
	let mut fh = try!(File::open(fname));
	let mut raw_json = String::new();
	try!(fh.read_to_string(&mut raw_json));
	json::decode(&*raw_json).map_err(|x| ArgError::DecodeError(x))
}

fn make_config() -> structs::Input {
	structs::input_builder()
}

fn write_file(fname: String, res: String) {
	let mut file = File::create(fname).unwrap();
	file.write_all(res.as_bytes()).unwrap();
}

#[allow(dead_code)]
pub struct Radeco {
	bin_name: Option<String>,
	esil: Option<Vec<String>>,
	addr: String,
	name: Option<String>,
	outpath: String,
	stages: Vec<usize>,
	quiet: bool,
	runner: Option<radeco_lib::utils::Runner>,
	outmodes: Option<Vec<u16>>,
}

impl Radeco {
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
			quiet: input.quiet.clone().unwrap(),
			runner: runner.ok(),
			outmodes: input.outmodes.clone(),
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
			{
				let mut name = input.name.clone().unwrap();
				let json = json::as_pretty_json(&input);
				let raw_json = format!("{}", json);
				name.push_str(".json");
				write_file(name, raw_json);
			}
			return Radeco::new(input);
		}

		// Run from a predefined configuration file
		if args.flag_config.is_some() {
			let filename = args.flag_config.clone().unwrap();
			let input = json_slurp(filename);
			if input.is_err() {
				return Some(Err(input.err().unwrap()))
			}
			return Radeco::new(input.unwrap());
		}

		let mut input = structs::Input::defaults();

		if args.arg_file.is_some() {
			input.bin_name = args.arg_file;
		}
		if args.flag_address.is_some() {
			input.addr = args.flag_address;
		}
		if args.flag_esil.is_some() {
			input.esil = args.flag_esil;
		}
		input.quiet = Some(!args.flag_quiet);
		if args.flag_output.is_some() {
			input.outpath = args.flag_output;
		}

		if args.flag_pipeline.is_some() {
			let mut i = 0;
			let pipe = args.flag_pipeline.unwrap();
			{
				let mut _tmp_stages = Vec::<usize>::new();
				let mut _tmp_out = Vec::<u16>::new();
				for stage in pipe.split(',') {
					let mut j = 0;
					if stage.chars().nth(0).unwrap() == '=' {
						_tmp_out.push(i);
						j = 1;
					}
					let n = match &stage[j..] {
						"c" | "C" => 7,
						"r2" => 0,
						"esil" => 1,
						"cfg" => 2,
						"ssa" => 3,
						"const" => 4,
						"dce" => 5,
						"verify" => 6,
						_ => {
							let e = format!("Invalid expression {} in stages", stage[1..].to_owned());
							panic!(e)
						},
					};
					_tmp_stages.push(n);
					i += 1;
				}
				input.stages = _tmp_stages;
				input.outmodes = Some(_tmp_out);
			}
		}

		Radeco::new(input)
	}

	pub fn run(&mut self) -> Result<(), String> {
		match self.runner.as_mut(){
			Some(ref mut runner) => {
				if runner.is_verbose() {
					println!("[*] Starting radeco with config: ");
					println!("{}", runner);
				}
				runner.run()
			},
			None => panic!("Uninitialized instance of radeco!"),
		}
		Ok(())
	}

	pub fn output(&mut self) {
		match self.runner.as_mut() {
			Some(ref mut runner) => {
				runner.output(self.outmodes.clone());
			},
			None => panic!("Uninitialized instance of radeco!"),
		}
	}
}
