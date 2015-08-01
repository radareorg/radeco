//use super::utils;
//use radeco::frontend::{parser, r2, esilssa};
//use radeco::middle::{cfg};
//use radeco::middle::dot;
//use radeco::middle::ssa::SSAStorage;

//#[test]
//fn test1() {
	//let bin_name = "./ex-bins/simple2";
	//let mut r2 = r2::R2::new(bin_name);
	//r2.init();

	//let reg_info = r2.get_reg_info().unwrap();

	//// Make a manual esil string for this small test.
	//let esil = "4,5,+,6,*";

	//let mut p = parser::Parser::new(None);
	//println!("[*] Begin Parse.");
	//println!("    > {} ", esil);
	//p.set_register_profile(&reg_info);

	//p.parse_str(esil);
	//let mut cfg = cfg::CFG::new();
	//cfg.build(&mut (p.emit_insts()));
	//let mut ssa = SSAStorage::new();

	//{
		//let mut con = esilssa::SSAConstruction::new(&mut ssa, &reg_info);
		//con.run(&cfg);
	//}

	//let res_cfg = dot::emit_dot(&cfg);
	//let res_ssa = dot::emit_dot(&ssa);

	//std::fs::create_dir("outputs").ok();

	//let outfile = "outputs/ex5-cfg.dot";
	//write_file(outfile, res_cfg);
	//println!("[*] Run `./scripts/genpng.sh {}` to generate the cfg-graph.", outfile);

	//let outfile = "outputs/ex5-ssa.dot";
	//write_file(outfile, res_ssa);
	//println!("[*] Run `./scripts/genpng.sh {}` to generate the ssa-graph.", outfile);
//}
