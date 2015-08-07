extern crate radeco;

pub mod utils;

use utils::{Pipeline, Test, Pipeout, Analysis};
use radeco::frontend::r2::R2;

#[test]
fn test1() {
	let pipeline = vec![Pipeline::ReadFromR2, Pipeline::ParseEsil,
	                    Pipeline::CFG, Pipeline::SSA];
	
	let test_name = "test1".to_string();
	let bin_name = Some("./ex-bins/simple2".to_string());
	let addr = Some("sym.main".to_string());
	let mut test = Test::new(test_name, bin_name, addr, false, pipeline);
	test.run();
	test.dump();
}

#[test]
fn test_analysis() {
	let esil = vec!["4,5,+".to_string(), "6,*".to_string(),
	                "100,>,zf,=".to_string(),
					"5,rax,=".to_string(),
					"6,rbx,=".to_string(),
					"7,rbx,=".to_string()
	               ];
	let test_name = "test2".to_string();
    
	// Get a new r2 instance.
    let mut r2 = R2::new("./ex-bins/simple2");
    // Initialize with sane defaults.
    r2.init();
    let r = r2.get_reg_info().unwrap();

	let pipeline = vec![
		Pipeline::ParseEsil,
		Pipeline::CFG,
		Pipeline::SSA,
		Pipeline::AnalyzeSSA(Analysis::ConstProp),
		Pipeline::DCE
	];

	let mut test = Test::new(test_name, None, None, true, pipeline);
	test.state.pipeout = Some(Pipeout::Esil(esil));
	test.state.reg_info = Some(r.clone());
	test.run();
	test.dump();
}
