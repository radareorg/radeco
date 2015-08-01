#![allow(dead_code, unused_variables, unused_imports)]
#![cfg(test)]
/// The utils module is used by other tests to make writing tests easier.
///
/// The idea is to provide tests with a neat interface that allows tests to define a pipeline of
/// transformations on the input.
/// The test is then returned with a result struct that contains information about all the
/// intermediate stages to assert! for correctness.

extern crate radeco;

use radeco::frontend::parser::{Parser};
use radeco::frontend::structs::{LOpInfo, LRegInfo};
use radeco::frontend::r2::R2;
use radeco::middle::ir::{MInst};
use radeco::middle::cfg::CFG;
use radeco::middle::dot;
use radeco::middle::ssa::SSAStorage;

use std::io::prelude::*;
use std::fs::{File, create_dir};

fn write_file(fname: &str, res: String) {
	let mut file = File::create(fname).ok().expect("Error. Cannot create file!\n");
	file.write_all(res.as_bytes()).ok().expect("Error. Cannot write file!\n");
}

#[derive(Clone, Copy, Debug)]
pub enum Analysis {
	ConstProp,
}

#[derive(Clone, Copy, Debug)]
pub enum Pipeline {
	ReadFromR2,
	ParseEsil,
	CFG,
	SSA,
	AnalyzeSSA(Analysis),
	CWriter,
}

impl Pipeline {
	fn to_string(&self) -> String {
		let s = match *self {
			Pipeline::ReadFromR2 => "r2",
			Pipeline::ParseEsil => "radecoIR",
			Pipeline::CFG => "cfg",
			Pipeline::SSA => "ssa",
			Pipeline::AnalyzeSSA(a) => unimplemented!(),
			Pipeline::CWriter => unimplemented!(),
		};
		s.to_string()
	}
}



// Enum that represents the output of a stage in the Pipeline.
// Every stage has a corresponding Pipeout type.
#[derive(Clone)]
pub enum Pipeout {
	Esil(Vec<String>),
	LOpInfo(Vec<LOpInfo>),
	Instructions(Vec<MInst>),
	CFG(CFG),
	SSA(SSAStorage),
}

// States all the vars important to various stages of the pipeline together.
// Also acts as the result struct.
pub struct State<'a> {
	r2: Option<R2>,
	esil: Option<Vec<String>>,
	reg_info: Option<LRegInfo>,
	p: Option<Parser<'a>>,
	cfg: Option<CFG>,
	ssa: Option<SSAStorage>,
	pipeout: Option<Pipeout>,
}

impl<'a> State<'a> {
	fn new() -> State<'a> {
		State {
			r2: None,
			esil: None,
			reg_info: None,
			p: None,
			cfg: None,
			ssa: None,
			pipeout: None,
		}
	}
}

pub struct Test<'a> {
	name: String,
	bin_name: Option<String>,
	addr: Option<String>,
	pipeline: Vec<Pipeline>,
	results: Vec<Pipeout>,
	state: State<'a>,
}

impl<'a> Test<'a> {

	pub fn new(name: String, bin_name: Option<String>,
			   addr: Option<String>, pipeline: Vec<Pipeline>) -> Test<'a> {
		Test {
			name: name,
			bin_name: bin_name,
			addr: addr,
			pipeline: pipeline,
			results: Vec::new(),
			state: State::new(),
		}
	}

	fn read_from_r2(&mut self) {
		assert!(!self.bin_name.is_none());
		assert!(!self.addr.is_none());

		let bin_name = self.bin_name.clone().unwrap();
		let mut r2 = R2::new(&*bin_name);
		r2.init();
		self.state.reg_info = Some(r2.get_reg_info().unwrap());
		let addr = self.addr.clone().unwrap();
		let func_info = r2.get_function(&*addr).unwrap();
		self.state.pipeout = Some(Pipeout::LOpInfo(func_info.ops.unwrap()));
		self.state.r2 = Some(r2);
	}

	fn parse_esil(&mut self) {
		let pipein = self.state.pipeout.clone().unwrap();
		match pipein {
			Pipeout::Esil(s)      => { },
			Pipeout::LOpInfo(ops) => { },
			_                     => panic!("Incompatible type found in the pipeline!"),
		}
	}

	fn construct_cfg(&mut self) {
		let pipein = self.state.pipeout.clone().unwrap();
		match pipein {
			Pipeout::Instructions(insts) => { },
			_ => panic!("Incompatible type found in the pipeline!"),
		}
	}

	fn construct_ssa(&mut self) {
		let pipein = self.state.pipeout.clone().unwrap();
		match pipein {
			Pipeout::CFG(cfg) => { },
			_ => panic!("Incompatible type found in the pipeline!"),
		}
	}

	pub fn run_test(&mut self) {
		let pipe_iter = self.pipeline.clone();
		for stage in pipe_iter.iter() {
			match *stage {
				Pipeline::ReadFromR2 => self.read_from_r2(),
				Pipeline::ParseEsil => self.parse_esil(),
				Pipeline::CFG => self.construct_cfg(),
				Pipeline::SSA => self.construct_ssa(),
				Pipeline::AnalyzeSSA(a) => unimplemented!(),
				Pipeline::CWriter => unimplemented!(),
			}
		}
	}
}
