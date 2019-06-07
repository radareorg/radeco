// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//extern crate radeco_lib;
//extern crate r2pipe;

//use radeco_lib::utils::{Pipeline, Runner, Pipeout, Analysis};
//use r2pipe::R2;

//#[test]
//fn test1() {
//let pipeline = vec![Pipeline::ReadFromR2, Pipeline::ParseEsil,
//Pipeline::CFG, Pipeline::SSA];

//let test_name = "test1".to_owned();
//let bin_name = Some("./ex-bins/simple2".to_owned());
//let addr = Some("sym.main".to_owned());
//let mut test = Runner::new(test_name, bin_name, addr, false, pipeline, None);
//test.run();
//test.output(None);
//}

//#[test]
//fn test_analysis1() {
//let esil = vec!["4,5,+".to_owned(), "6,*".to_owned(),
//"100,>,zf,=".to_owned(),
//"5,rax,=".to_owned(),
//"6,rbx,=".to_owned(),
//"7,rbx,=".to_owned()
//];

//let test_name = "test2".to_owned();
//// Get a new r2 instance.
//let mut r2 = R2::new(Some("./ex-bins/simple2".to_owned())).unwrap();
//// Initialize with sane defaults.
//r2.init();
//let r = r2.get_reg_info().unwrap();

//let pipeline = vec![
//Pipeline::ParseEsil,
//Pipeline::CFG,
//Pipeline::SSA,
//Pipeline::Verify,
//Pipeline::AnalyzeSSA(Analysis::ConstProp),
//Pipeline::DCE,
//Pipeline::Verify
//];

//let mut test = Runner::new(test_name, None, None, true, pipeline, None);
//test.state.pipeout = Some(Pipeout::Esil(esil));
//test.state.reg_info = Some(r.clone());
//test.run();
//test.output(None);
//}

//#[test]
//fn test_analysis2() {
//let test_name = "test_analysis".to_owned();
//let bin_name = Some("./ex-bins/constprop.o".to_owned());
//let addr = Some("entry0".to_owned());
//let pipeline = vec![
//Pipeline::ReadFromR2,
//Pipeline::ParseEsil,
//Pipeline::CFG,
//Pipeline::SSA
////Pipeline::Verify
////Pipeline::DCE,
////Pipeline::AnalyzeSSA(Analysis::ConstProp),
////Pipeline::DCE
//];
//let mut test = Runner::new(test_name, bin_name, addr, true, pipeline, None);
//test.run();
//test.output(None);
//}

////#[test]
////fn tachikoma() {
////let test_name = "tachikoma".to_string();
////let bin_name = Some("./ex-bins/tachikoma".to_string());
////let addr = Some("fcn.0002b401".to_string());
////let pipeline = vec![
////Pipeline::ReadFromR2,
////Pipeline::ParseEsil,
////Pipeline::CFG,
////Pipeline::SSA,
////Pipeline::Verify
//////Pipeline::DCE,
//////Pipeline::Verify
//////Pipeline::AnalyzeSSA(Analysis::ConstProp),
//////Pipeline::DCE
////];
////let mut test = Runner::new(test_name, bin_name, addr, true, pipeline, None);
////test.run();
////test.dump();
////}
