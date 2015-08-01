extern crate radeco;

#[cfg(test)]
pub mod esil_to_ssa {
    
	use radeco::frontend::{parser, r2, esilssa};
	use radeco::middle::{cfg};
	use radeco::middle::dot;
	use radeco::middle::ssastorage::SSAStorage;
	
	use std::env;
	use std;
	use std::io::prelude::*;
	use std::fs::{File, create_dir};

	fn write_file(fname: &str, res: String) {
		let mut file = File::create(fname).ok().expect("Error. Cannot create file!\n");
		file.write_all(res.as_bytes()).ok().expect("Error. Cannot write file!\n");
	}

	pub enum Analysis {
		ConstProp,
	}

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
	pub enum Pipeout {
		Foo,
	}

	// Bundles all the vars important to various stages of the pipeline together.
	// Also acts as the result struct.
	pub struct Bundle {
		r2: Option<R2>,
		esil: Option<Vec<String>>,
		p: Option<Parser>,
		cfg: Option<CFG>,
		ssa: Option<SSAStorage>,
		pipeout: Option<Pipeout>,
	}

	impl Bundle {
		fn new() -> Bundle {
			Bundle {
				r2: None,
				esil: None,
				p: None,
				cfg: None,
				ssa: None,
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
			match stage {
				Pipeline::ReadFromR2 => { },
				Pipeline::ParseEsil => { },
				Pipeline::CFG => { },
				Pipeline::SSA => { },
				Pipeline::AnalyzeSSA(a) => unimplemented!(),
				Pipeline::CWriter => unimplemented!(),
			}
		}
	}

	#[test]
	fn test1() {
		let bin_name = "./ex-bins/simple2";
		let mut r2 = r2::R2::new(bin_name);
		r2.init();

		let reg_info = r2.get_reg_info().unwrap();

		// Make a manual esil string for this small test.
		let esil = "4,5,+,6,*";

		let mut p = parser::Parser::new(None);
		println!("[*] Begin Parse.");
		println!("    > {} ", esil);
		p.set_register_profile(&reg_info);

		p.parse_str(esil);
		let mut cfg = cfg::CFG::new();
		cfg.build(&mut (p.emit_insts()));
		let mut ssa = SSAStorage::new();
		
		{
			let mut con = esilssa::SSAConstruction::new(&mut ssa, &reg_info);
			con.run(&cfg);
		}

		let res_cfg = dot::emit_dot(&cfg);
		let res_ssa = dot::emit_dot(&ssa);

		std::fs::create_dir("outputs").ok();

		let outfile = "outputs/ex5-cfg.dot";
		write_file(outfile, res_cfg);
		println!("[*] Run `./scripts/genpng.sh {}` to generate the cfg-graph.", outfile);

		let outfile = "outputs/ex5-ssa.dot";
		write_file(outfile, res_ssa);
		println!("[*] Run `./scripts/genpng.sh {}` to generate the ssa-graph.", outfile);
	}
}
