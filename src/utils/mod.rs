// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Module implements Pipeline and Runner structs to make radeco-lib easier to
//! use.

#[macro_use]
pub mod logger;

//use std::io::prelude::*;
//use std::fs;
//use std::fs::File;
//use std::fmt;
//use std::path::PathBuf;

//use r2pipe::structs::{LOpInfo, LRegInfo};
//use r2pipe::R2;

//use frontend::parser::Parser;
////use frontend::esilssa::SSAConstruction;
//use middle::ir::MInst;
//use middle::cfg::CFG;
//use middle::dot;
//use middle::dce;
//use middle::ssa::SSAStorage;
//use analysis::constant_propagation::constant;
//use middle::ssa::verifier;

//macro_rules! out {
//($str: expr, $m: expr) => { if $m { println!($str) } }
//}

//#[derive(Clone, Copy, Debug)]
//pub enum Analysis {
//ConstProp,
//}

//#[derive(Clone, Copy, Debug)]
//pub enum Pipeline {
//ReadFromR2,
//ParseEsil,
//CFG,
//SSA,
//AnalyzeSSA(Analysis),
//DCE,
//Verify,
//CWriter,
//}

//// Enum that represents the output of a stage in the Pipeline.
//// Every stage has a corresponding Pipeout type.
//#[derive(Clone)]
//pub enum Pipeout {
//Esil(Vec<String>),
//LOpInfo(Vec<LOpInfo>),
//Instructions {
//i: Vec<MInst>,
//},
//CFG {
//cfg: CFG,
//},
//SSA {
//ssa: SSAStorage,
//},
//}

//impl Pipeout {
//fn to_string(&self) -> String {
//let s = match *self {
//Pipeout::Esil(_) => "esil",
//Pipeout::LOpInfo(_) => "esil",
//Pipeout::Instructions {i: _} => "ir",
//Pipeout::CFG {cfg: _} => "cfg",
//Pipeout::SSA {ssa: _} => "ssa",
//};
//s.to_string()
//}
//}

//// States all the vars important to various stages of the pipeline together.
//// Also acts as the result struct.
//#[allow(dead_code)]
//pub struct State {
//r2: Option<R2>,
//esil: Option<Vec<String>>,
//pub reg_info: Option<LRegInfo>,
//p: Option<Parser>,
//cfg: Option<CFG>,
//ssa: Option<SSAStorage>,
//pub pipeout: Option<Pipeout>,
//}

//impl State {
//fn new() -> State {
//State {
//r2: None,
//esil: None,
//reg_info: None,
//p: None,
//cfg: None,
//ssa: None,
//pipeout: None,
//}
//}
//}

//pub struct Runner {
//name: String,
//bin_name: Option<String>,
//addr: Option<String>,
//verbose: bool,
//pipeline: Vec<Pipeline>,
//results: Vec<Pipeout>,
//outpath: String,
//pub state: State,
//}

//impl fmt::Display for Pipeline {
//fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//match *self {
//Pipeline::ReadFromR2 => write!(f, "{}", "r2"),
//Pipeline::ParseEsil => write!(f, "{}", "Parse ESIL"),
//Pipeline::CFG => write!(f, "{}", "Control Flow Graph Construction"),
//Pipeline::SSA => write!(f, "{}", "SSA Construction"),
//Pipeline::AnalyzeSSA(_) => write!(f, "{}", "Constant Propagation"),
//Pipeline::DCE => write!(f, "{}", "Dead Code Elimination"),
//Pipeline::Verify => write!(f, "{}", "Verify SSA"),
//Pipeline::CWriter => write!(f, "{}", "C Writer"),
//}
//}
//}

//impl fmt::Display for Runner {
//fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//let mut res;
//res = format!("Name: {}\n", self.name);
//res = format!("{}Binary: {}\n",
//res,
//self.bin_name.as_ref().unwrap_or(&"-".to_owned()));
//res = format!("{}Address: {}\n",
//res,
//self.addr.as_ref().unwrap_or(&"-".to_owned()));
//res = format!("{}Pipeline:\n", res);
//for p in &self.pipeline {
//res = format!("{}    - {}\n", res, p);
//}
//res = format!("{}Output: {}", res, self.outpath);
//write!(f, "{}", res)
//}
//}

//impl Runner {

//pub fn new(name: String,
//bin_name: Option<String>,
//addr: Option<String>,
//verbose: bool,
//pipeline: Vec<Pipeline>,
//outpath: Option<String>)
//-> Runner {
//Runner {
//name: name,
//bin_name: bin_name,
//addr: addr,
//verbose: verbose,
//pipeline: pipeline,
//results: Vec::new(),
//state: State::new(),
//outpath: outpath.unwrap_or("./outputs".to_owned()),
//}
//}

//pub fn is_verbose(&self) -> bool {
//self.verbose
//}

//fn set_reg_info(&mut self, reg_info: &LRegInfo) {
//self.state.reg_info = Some(reg_info.clone());
//}

//fn set_pipeout(&mut self, pipeout: &Pipeout) {
//self.state.pipeout = Some(pipeout.clone());
//}

//fn read_from_r2(&mut self) {
//// assert!(!self.bin_name.is_none());
//// assert!(!self.addr.is_none());
//out!("[*] Reading from R2", self.verbose);

//if self.state.r2.is_none() {
//let bin_name = self.bin_name.clone();
//// TODO: Error Handling
//let mut _r2 = R2::new(bin_name).unwrap();
//_r2.init();
//self.state.r2 = Some(_r2);
//}
//let func_info;
//let reg_info;
//match self.state.r2.as_mut() {
//Some(r2) => {
//reg_info = r2.get_reg_info().unwrap();
//let addr = self.addr.clone().unwrap();
//func_info = r2.get_function(&*addr).unwrap();
//}
//None => panic!("Unable to Initialize r2. Something is wrong!"),
//}

//self.set_reg_info(&reg_info);
//self.set_pipeout(&Pipeout::LOpInfo(func_info.ops.unwrap()));
//}

//fn parse_esil(&mut self) {
//let pipein = self.state.pipeout.clone().unwrap();
//out!("[*] Parsing ESIL", self.verbose);
//let mut p = Parser::new(None);
//if let Some(ref r) = self.state.reg_info {
//p.set_register_profile(r);
//}
//match pipein {
//Pipeout::Esil(strs) => {
//for _str in strs {
//p.parse_str(&*_str).ok();
//}
//}
//Pipeout::LOpInfo(mut ops) => {
//for op in ops.iter_mut() {
//p.parse_opinfo(op).ok();
//}
//}
//_ => panic!("Incompatible type found in the pipeline!"),
//}

//let insts = p.emit_insts();
//let pipeout = Pipeout::Instructions { i: insts };
//self.set_pipeout(&pipeout);
//self.state.p = Some(p);
//}

//fn construct_cfg(&mut self) {
//let mut pipein = self.state.pipeout.clone().unwrap();
//out!("[*] Starting CFG Construction", self.verbose);
//match pipein {
//Pipeout::Instructions {i: ref mut insts} => {
//let mut cfg = CFG::new();
//cfg.build(insts);
//let pipeout = Pipeout::CFG { cfg: cfg.clone() };
//self.set_pipeout(&pipeout);
//self.state.cfg = Some(cfg);
//}
//_ => panic!("Incompatible type found in the pipeline!"),
//}
//}

//fn construct_ssa(&mut self) {
//// TODO: Relax this condition.
//assert!(self.state.reg_info.is_some());
//out!("[*] Starting SSA Construction", self.verbose);
//let pipein = self.state.pipeout.clone().unwrap();
//let r = self.state.reg_info.clone().unwrap();
//match pipein {
//Pipeout::CFG {ref cfg} => {
//let mut ssa = SSAStorage::new();
//{
////let mut con = SSAConstruction::new(&mut ssa, &r);
////con.run(cfg);
//}
//let pipeout = Pipeout::SSA { ssa: ssa.clone() };
//self.set_pipeout(&pipeout);
//self.state.ssa = Some(ssa);
//}
//_ => panic!("Incompatible type found in the pipeline!"),
//}
//}

//fn analyze(&mut self, analysis: &Analysis) {
//out!("[*] Starting Analysis", self.verbose);
//let pipein = self.state.pipeout.clone().unwrap();
//let mut ssa = if let Pipeout::SSA { ssa } = pipein {
//ssa
//} else {
//panic!("Incompatible type found in the pipeline!");
//};
//let ssa = match *analysis {
//Analysis::ConstProp => {
//let mut analyzer = constant::Analyzer::new(&mut ssa);
//analyzer.analyze();
//analyzer.emit_ssa()
//}
//};

//self.set_pipeout(&Pipeout::SSA { ssa: ssa.clone() });
//}

//fn dce(&mut self) {
//out!("[*] Running DCE", self.verbose);
//let pipein = self.state.pipeout.clone().unwrap();
//let mut ssa = if let Pipeout::SSA { ssa } = pipein {
//ssa
//} else {
//panic!("Incompatible type found in the pipeline!");
//};

//{
//dce::collect(&mut ssa);
//}

//self.set_pipeout(&Pipeout::SSA { ssa: ssa.clone() });
//}

//fn verify(&mut self) {
//out!("[*] Verifying the Integrity of SSA.", self.verbose);
//let pipein = self.state.pipeout.clone().unwrap();
//let ssa = if let Pipeout::SSA { ssa } = pipein {
//ssa
//} else {
//panic!("Incompatible type found in the pipeline!");
//};

//{
//verifier::verify(&ssa).unwrap();
//}
//}

//// TODO: Return Error. Never panic!()
//pub fn run(&mut self) {
//let pipe_iter = self.pipeline.clone();
//for stage in pipe_iter.iter() {
//match *stage {
//Pipeline::ReadFromR2 => self.read_from_r2(),
//Pipeline::ParseEsil => self.parse_esil(),
//Pipeline::CFG => self.construct_cfg(),
//Pipeline::SSA => self.construct_ssa(),
//Pipeline::AnalyzeSSA(ref a) => self.analyze(a),
//Pipeline::DCE => self.dce(),
//Pipeline::Verify => self.verify(),
//Pipeline::CWriter => unimplemented!(),
//}
//self.results.push(self.state.pipeout.clone().unwrap());
//}
//}

//fn write_file(&self, fname: PathBuf, res: String) {
//let mut file = File::create(fname).ok().expect("Error. Cannot create file!\n");
//file.write_all(res.as_bytes()).ok().expect("Error. Cannot write file!\n");
//}

//pub fn output(&self, phases: Option<Vec<u16>>) {
//let mut phase_num = 0;
//let count = self.pipeline.len() - 1;
//let phases = phases.unwrap_or((0..count as u16).collect::<Vec<_>>());
//for res in self.results.iter() {

//if !phases.contains(&phase_num) {
//phase_num += 1;
//continue;
//}
//let mut write_out = String::new();
//let ext;
//match *res {
//Pipeout::Esil(ref s) => {
//ext = "esil";
//for esil in s {
//let tmp = format!("{}\n", esil);
//write_out.push_str(&*tmp);
//}
//}
//Pipeout::LOpInfo(ref ops) => {
//ext = "esil";
//for op in ops {
//let tmp = format!("{}:\t{}\n",
//op.offset.unwrap(),
//op.esil.clone().unwrap());
//write_out.push_str(&*tmp);
//}
//}
//Pipeout::Instructions {i: ref insts} => {
//ext = "insts";
//for inst in insts {
//let tmp = format!("0x{:08X}:\t{}\n", inst.addr.val, inst);
//write_out.push_str(&*tmp);
//}
//}
//Pipeout::CFG {ref cfg} => {
//ext = "dot";
//let tmp = dot::emit_dot(cfg);
//write_out.push_str(&*tmp);
//}
//Pipeout::SSA {ref ssa} => {
//ext = "dot";
//let tmp = dot::emit_dot(ssa);
//write_out.push_str(&*tmp);
//}
//}

//// Format of output file name:
//// test_name-type-phase_num.ext
//let mut p = PathBuf::from(&self.outpath);
//p.push(self.name.clone());
//let pstr = format!("{}-res_{}-phase{}.{}",
//self.name,
//res.to_string(),
//phase_num.to_string(),
//ext);
//fs::create_dir_all(&p).ok();
//p.push(pstr);
//self.write_file(p, write_out);
//phase_num += 1;
//}
//}
//}
