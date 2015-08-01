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
use radeco::frontend::structs::{LOpInfo};
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
pub enum Pipeout {
	LOpInfo(Vec<LOpInfo>),
	Instructions(Vec<MInst>),
	CFG(CFG),
	SSA(SSAStorage),
}

// Bundles all the vars important to various stages of the pipeline together.
// Also acts as the result struct.
pub struct Bundle<'a> {
	r2: Option<R2>,
	esil: Option<Vec<String>>,
	p: Option<Parser<'a>>,
	cfg: Option<CFG>,
	ssa: Option<SSAStorage>,
	pipeout: Option<Pipeout>,
}

impl<'a> Bundle<'a> {
	fn new() -> Bundle<'a> {
		Bundle {
			r2: None,
			esil: None,
			p: None,
			cfg: None,
			ssa: None,
			pipeout: None,
		}
	}
}

// TODO: Improve this result struct.
// Right now: `f` -> Filename to write out `res` to.
pub struct Results {
	f: String,
	res: String,
}

// Function to initialize all your vars to sane defaults.
fn init_vars() {
}

// Function to run the integration test.
// The common pipeline is esil -> cfg -> ssa.
// Returns a result struct which can be used to assert! the results.
fn run_test(pipeline: &Vec<Pipeline>, bundle: &mut Bundle) -> Results {
	for stage in pipeline.iter() {
		match *stage {
			Pipeline::ReadFromR2 => { },
			Pipeline::ParseEsil => { },
			Pipeline::CFG => { },
			Pipeline::SSA => { },
			Pipeline::AnalyzeSSA(a) => unimplemented!(),
			Pipeline::CWriter => unimplemented!(),
		}
	}
	unimplemented!();
}
