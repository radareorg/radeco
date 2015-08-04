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
use radeco::frontend::esilssa::SSAConstruction;
use radeco::middle::ir::{MInst};
use radeco::middle::cfg::CFG;
use radeco::middle::dot;
use radeco::middle::ssa::SSAStorage;
use radeco::analysis::constant_propagation::constant;

use std::io::prelude::*;
use std::fs::{File, create_dir};

macro_rules! out {
	($str: expr, $m: expr) => { if $m { println!($str) } }
}

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

// Enum that represents the output of a stage in the Pipeline.
// Every stage has a corresponding Pipeout type.
#[derive(Clone)]
pub enum Pipeout {
	Esil(Vec<String>),
	LOpInfo(Vec<LOpInfo>),
	Instructions {i: Vec<MInst>},
	CFG {cfg: CFG},
	SSA {ssa: SSAStorage},
}

impl Pipeout {
	fn to_string(&self) -> String {
		let s = match *self {
			Pipeout::Esil(_) => "esil",
			Pipeout::LOpInfo(_) => "esil",
			Pipeout::Instructions {i: _} => "radecoIR",
			Pipeout::CFG {cfg: _} => "cfg",
			Pipeout::SSA {ssa: _} => "ssa",
		};
		s.to_string()
	}
}

// States all the vars important to various stages of the pipeline together.
// Also acts as the result struct.
#[allow(dead_code)]
pub struct State<'a> {
	r2: Option<R2>,
	esil: Option<Vec<String>>,
	pub reg_info: Option<LRegInfo>,
	p: Option<Parser<'a>>,
	cfg: Option<CFG>,
	ssa: Option<SSAStorage>,
	pub pipeout: Option<Pipeout>,
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
	verbose: bool,
	pipeline: Vec<Pipeline>,
	results: Vec<Pipeout>,
	pub state: State<'a>,
}

impl<'a> Test<'a> {

	pub fn new(name: String, bin_name: Option<String>,
			   addr: Option<String>, verbose: bool,
			   pipeline: Vec<Pipeline>) -> Test<'a> {
		Test {
			name: name,
			bin_name: bin_name,
			addr: addr,
			verbose: verbose,
			pipeline: pipeline,
			results: Vec::new(),
			state: State::new(),
		}
	}

	fn set_reg_info(&mut self, reg_info: &LRegInfo) {
		self.state.reg_info = Some(reg_info.clone());
	}

	fn set_pipeout(&mut self, pipeout: &Pipeout) {
		self.state.pipeout = Some(pipeout.clone());
	}

	fn read_from_r2(&mut self) {
		assert!(!self.bin_name.is_none());
		assert!(!self.addr.is_none());
		out!("[*] Reading from R2", self.verbose);
		let bin_name = self.bin_name.clone().unwrap();
		let mut r2 = R2::new(&*bin_name);
		r2.init();
		let reg_info = r2.get_reg_info().unwrap();
		self.set_reg_info(&reg_info);
		let addr = self.addr.clone().unwrap();
		let func_info = r2.get_function(&*addr).unwrap();
		self.set_pipeout(&Pipeout::LOpInfo(func_info.ops.unwrap()));
		self.state.r2 = Some(r2);
	}

	fn parse_esil(&mut self) {
		let pipein = self.state.pipeout.clone().unwrap();
		out!("[*] Parsing ESIL", self.verbose);
		let mut p = Parser::new(None);
		if let Some(ref r) = self.state.reg_info {
			p.set_register_profile(r);
		}
		match pipein {
			Pipeout::Esil(strs) => { 
				for _str in strs {
					p.parse_str(&*_str).ok();
				}
			},
			Pipeout::LOpInfo(mut ops) => { 
				for op in ops.iter_mut() {
					p.parse_opinfo(op).ok();
				}
			},
			_  => panic!("Incompatible type found in the pipeline!"),
		}

		let insts = p.emit_insts();
		let pipeout = Pipeout::Instructions {i: insts};
		self.set_pipeout(&pipeout);
		self.state.p = Some(p);
	}

	fn construct_cfg(&mut self) {
		let mut pipein = self.state.pipeout.clone().unwrap();
		out!("[*] Starting CFG Construction", self.verbose);
		match pipein {
			Pipeout::Instructions {i: ref mut insts} => { 
				let mut cfg = CFG::new();
				cfg.build(insts);
				let pipeout = Pipeout::CFG {cfg: cfg.clone()};
				self.set_pipeout(&pipeout);
				self.state.cfg = Some(cfg);
			},
			_ => panic!("Incompatible type found in the pipeline!"),
		}
	}

	fn construct_ssa(&mut self) {
		// TODO: Relax this condition.
		assert!(self.state.reg_info.is_some());
		out!("[*] Starting SSA Construction", self.verbose);
		let pipein = self.state.pipeout.clone().unwrap();
		let r = self.state.reg_info.clone().unwrap();
		match pipein {
			Pipeout::CFG {ref cfg} => {
				let mut ssa = SSAStorage::new();
				{
					let mut con = SSAConstruction::new(&mut ssa, &r);
					con.run(cfg);
				}
				let pipeout = Pipeout::SSA {ssa: ssa.clone()};
				self.set_pipeout(&pipeout);
				self.state.ssa = Some(ssa);
			},
			_ => panic!("Incompatible type found in the pipeline!"),
		}
	}

	fn analyze(&mut self, analysis: &Analysis) {
		let pipein = self.state.pipeout.clone().unwrap();
		let mut ssa = if let Pipeout::SSA { ssa } = pipein {
			ssa
		} else {
			panic!("Incompatible type found in the pipeline!");
		};
		match *analysis {
			Analysis::ConstProp => {
				let mut analyzer = constant::Analyzer::new(&mut ssa);
				analyzer.analyze();
				analyzer.dump();
			},
		}
	}

	pub fn run(&mut self) {
		let pipe_iter = self.pipeline.clone();
		for stage in pipe_iter.iter() {
			match *stage {
				Pipeline::ReadFromR2 => self.read_from_r2(),
				Pipeline::ParseEsil => self.parse_esil(),
				Pipeline::CFG => self.construct_cfg(),
				Pipeline::SSA => self.construct_ssa(),
				Pipeline::AnalyzeSSA(ref a) => self.analyze(a),
				Pipeline::CWriter => unimplemented!(),
			}
			self.results.push(self.state.pipeout.clone().unwrap());
		}
	}

	pub fn dump(&self) {
		for res in self.results.iter() {
			let mut write_out = String::new();
			let mut ext;
			match *res {
				Pipeout::Esil(ref s) => { 
					ext = "esil";
					for esil in s {
						let tmp = format!("{}\n",esil);
						write_out.push_str(&*tmp);
					}
				},
				Pipeout::LOpInfo(ref ops) => { 
					ext = "esil";
					for op in ops {
						let tmp = format!("{}:\t{}\n", op.offset.unwrap(), op.esil.clone().unwrap());
						write_out.push_str(&*tmp);
					}
				},
				Pipeout::Instructions {i: ref insts} => { 
					ext = "insts";
					for inst in insts {
						let tmp = format!("0x{:08X}:\t{}\n", inst.addr.val, inst);
						write_out.push_str(&*tmp);
					}
				},
				Pipeout::CFG {ref cfg} => { 
					ext = "dot";
					let tmp = dot::emit_dot(cfg);
					write_out.push_str(&*tmp);
				},
				Pipeout::SSA {ref ssa} => {
					ext = "dot";
					let tmp = dot::emit_dot(ssa);
					write_out.push_str(&*tmp);
				},
			}
			let dir = format!("outputs/{}/", self.name);
			create_dir(&*dir).ok();
			let fname = format!("{}{}_{}.{}", dir, self.name, res.to_string(), ext);
			write_file(&*fname, write_out);
		}
	}
}
