//! Contains the struct that stores the user's parsed input.

use std::io;
use std::fs;
use std::path::{Path};
use radeco_lib::utils::{Pipeline, Runner, Analysis, Pipeout};
use radeco_lib::frontend::r2;
use errors::ArgError;

#[derive(RustcDecodable, RustcEncodable, Debug, Clone)]
pub enum Outmode {
	Pseudo,
	Dot,
}
	
#[derive(RustcDecodable, RustcEncodable, Debug, Clone)]
pub struct Input {
	pub bin_name: Option<String>,
	pub esil: Option<Vec<String>>,
	pub addr: Option<String>,
	pub name: Option<String>,
	pub outpath: Option<String>,
	pub stages: Vec<usize>,
	pub quiet: Option<bool>,
	pub outmodes: Option<Vec<u16>>,
}

#[derive(Debug, Clone, Copy)]
enum Mode {
	R2Pipe,
	Standalone,
}

// Return the mode radeco is operating in:
fn mode() -> Mode {
	if r2::R2::in_session() {
		Mode::R2Pipe
	} else {
		Mode::Standalone
	}
}

impl Input {
	fn new(bin_name: Option<String>, esil: Option<Vec<String>>,
		   addr: Option<String>, name: Option<String>,
		   outpath: Option<String>, stages: Vec<usize>,
		   outmodes: Option<Vec<u16>>, quiet: bool) -> Input
	{
		Input {
			bin_name: bin_name,
			esil: esil,
			addr: addr,
			name: name,
			outpath: outpath,
			stages: stages.clone(),
			quiet: Some(quiet),
			outmodes: outmodes,
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
	//     - quiet : false vs false
	pub fn defaults() -> Input {
		let bin_name;
		let addr;
		let name;
		let outpath;
		let outmodes = vec![];
		let stages = vec![0, 1, 2, 3, 4, 5];

		match mode() {
			Mode::R2Pipe => {
				let mut r2 = r2::R2::new(None).unwrap();
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
				outpath = format!("./{}_out", file.as_ref().unwrap());
				name = Some(format!("{}.run", file.as_ref().unwrap()));
			},
			Mode::Standalone => {
				bin_name = None;
				addr = "entry0".to_owned();
				name = None;
				outpath = "./outputs".to_owned();
			},
		}

		Input {
			bin_name: bin_name,
			esil: None,
			addr: Some(addr),
			name: name,
			outpath: Some(outpath),
			stages: stages,
			outmodes: Some(outmodes),
			quiet: Some(false),
		}
	}

	pub fn validate(&self) -> Result<Runner, ArgError> {
		if self.bin_name.is_none() && self.esil.is_none() {
			return Err(ArgError::NoSource);
		}

		let bin = if self.bin_name.is_some() {
			let _n = &self.bin_name.clone().unwrap();
			let metadata = fs::metadata(_n);
			if metadata.is_err() {
				return Err(ArgError::InvalidSource("File does not exist".to_owned()));
			}
			if !metadata.unwrap().is_file() {
				return Err(ArgError::InvalidSource("File does not exist".to_owned()));
			}
			self.bin_name.clone()
		} else {
			None
		};

		let esil = if self.esil.is_some() {
			if bin.is_some() {
				return Err(ArgError::MultipleSources);
			}
			self.esil.clone()
		} else {
			None
		};

		let addr = if self.addr.is_some() {
			if bin.is_none() {
				return Err(ArgError::InvalidArgument("Address provided without source binary".to_owned()));
			}
			self.addr.clone()
		} else {
			if bin.is_some() {
				return Err(ArgError::MissingArgument("Address".to_owned()));
			}
			None
		};

		let name = self.name.clone().unwrap_or("radeco_out".to_owned());

		let outpath = self.outpath.clone().unwrap_or("./outputs".to_owned());
		fs::create_dir_all(&outpath).ok().expect("Unable to create directory for output.");

		let possible = vec![
			Pipeline::ReadFromR2,
			Pipeline::ParseEsil,
			Pipeline::CFG,
			Pipeline::SSA,
			Pipeline::AnalyzeSSA(Analysis::ConstProp),
			Pipeline::DCE,
			Pipeline::Verify,
			Pipeline::CWriter,
		];

		let pipeline = self.stages.iter().map(|s| possible[*s]).collect::<Vec<Pipeline>>();
		let quiet = self.quiet.unwrap_or(false);
		let mut runner = Runner::new(name, bin, addr, quiet, pipeline, Some(outpath));
		// Handle the case where the input is raw esil.
		if esil.is_some() {
			let filepath = self.bin_name.clone();
			let mut r2 = r2::R2::new(filepath).unwrap();
			r2.init();
			let r = r2.get_reg_info().ok();
			runner.state.pipeout = Some(Pipeout::Esil(esil.unwrap()));
			runner.state.reg_info = r;
		}
		Ok(runner)
	}
}

macro_rules! input_or_none {
	($inp: expr) => {
		if $inp.is_empty() {
			None
		} else {
			Some($inp)
		}
	}
}

macro_rules! read {
	() => {
		{
			let mut inp = String::new();
			io::stdin().read_line(&mut inp).ok();
			inp = inp.trim().to_owned();
			input_or_none!(inp)
		}
	}
}

pub fn input_builder() -> Input {
	println!("Radeco Input Builder.");
	println!("Binary name (if esil, skip this): ");
	let bin_name = read!();
	println!("Raw esil (skip if binary name was specified): ");
	let esil: Option<Vec<String>> = {
		let mut _tmpv = Vec::new();
		loop {
			let _esil = read!();
			if _esil.is_none() {
				break;
			}
			_tmpv.push(_esil.unwrap());
		}
		if _tmpv.is_empty() {
			None
		} else {
			Some(_tmpv)
		}
	};
	println!("Address to read instruction from (skip if raw esil): ");
	let addr = read!();
	println!("Name of this run: ");
	let name = read!();
	println!("Output directory: ");
	let out_dir = read!();

	let mut pipe = Vec::<usize>::new();
	loop {
		println!("Stages in pipeline\n{}{}{}{}{}{}{}{}",
				 "0. R2\n",
				 "1. Parse Esil\n",
				 "2. Build CFG\n",
				 "3. Build SSA\n",
				 "4. Constant Propagation\n",
				 "5. Dead Code Elemination\n",
				 "6. Verify\n",
				 "(Enter comma separated choices):");

		let _pipe = read!();
		if _pipe.is_some() {
			for i in _pipe.unwrap().split(",") {
				let j: usize = i.parse::<usize>().unwrap();
				pipe.push(j);
			}
		}

		if !pipe.is_empty() { break; }
		println!("Pipeline cannot be empty!");
	}

	// TODO: honor Args.flag_verbose
	let verbose = {
		println!("Verbose (y/N):");
		let _v = read!();
		_v.or(Some("n".to_owned())).map(|c| match &*c {
			"y" => true,
			"n" => false,
			_ => false,
		}).unwrap()
	};

	let outmodes = Some((0..(pipe.len() - 1) as u16).collect::<Vec<_>>());

	Input::new(bin_name, esil, addr, name, out_dir, pipe, outmodes, verbose)
}
