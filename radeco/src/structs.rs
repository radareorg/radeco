//! Contains the struct that stores the user's parsed input.

use std::io;
use std::fs;
use std::path::{Path};
use radeco_lib::utils::{Pipeline, Runner, Analysis};

#[derive(RustcDecodable, RustcEncodable, Debug, Clone)]
pub struct Input {
	bin_name: Option<String>,
	esil: Option<String>,
	addr: Option<String>,
	pub name: Option<String>,
	outpath: Option<String>,
	stages: Vec<usize>,
	verbose: Option<bool>,
}

impl Input {

	fn new(bin_name: Option<String>, esil: Option<String>,
		   addr: Option<String>, name: Option<String>,
		   outpath: Option<String>, stages: Vec<usize>,
		   verbose: bool) -> Input
	{
		Input {
			bin_name: bin_name,
			esil: esil,
			addr: addr,
			name: name,
			outpath: outpath,
			stages: stages.clone(),
			verbose: Some(verbose),
		}
	}

	pub fn validate(&self) -> Result<Runner, &str> {
		if self.bin_name.is_none() && self.esil.is_none() {
			return Err("No Source!");
		}

		let bin = if self.bin_name.is_some() {
			let _n = &self.bin_name.clone().unwrap();
			let p = Path::new(_n);
			// TODO: Check existence
			//if !p.exists() {
				//return Err("Binary not found");
			//}
			self.bin_name.clone()
		} else {
			None
		};

		let esil = if self.esil.is_some() {
			if bin.is_some() {
				return Err("Multiple sources detected. Please ensure that one
				of bin_name or esil is not set!");
			}
			self.esil.clone()
		} else {
			None
		};

		let addr = if self.addr.is_some() {
			if bin.is_none() {
				return Err("Address field filled in without a binary to load");
			}
			self.addr.clone()
		} else {
			if bin.is_some() {
				return Err("No Address specified for the loaded binary");
			}
			None
		};

		let name = self.name.clone().or(Some("radeco_out".to_owned()));

		let outpath = self.outpath.clone().or(Some("./outputs/".to_owned()))
		                     .map(|p| fs::create_dir_all(&p).ok());

		let possible = vec![
			Pipeline::ReadFromR2,
			Pipeline::ParseEsil,
			Pipeline::CFG,
			Pipeline::SSA,
			Pipeline::AnalyzeSSA(Analysis::ConstProp),
			Pipeline::DCE,
			Pipeline::Verify
		];

		let pipeline = self.stages.iter().map(|s| possible[*s]).collect::<Vec<Pipeline>>();
		let verbose = self.verbose.unwrap_or(false);

		// TODO: Handle the case of raw esil
		Ok(Runner::new(name.unwrap(), bin, addr, verbose, pipeline))
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
	let esil = read!();
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
	
	let verbose = {
		println!("Verbose (y/N):");
		let _v = read!();
		_v.or(Some("n".to_owned())).map(|c| match &*c {
			"y" => true,
			"n" => false,
			_ => false,
		}).unwrap()
	};

	Input::new(bin_name, esil, addr, name, out_dir, pipe, verbose)
}
