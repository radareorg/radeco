
extern crate radeco_lib;
extern crate r2pipe;
extern crate rustc_serialize;

use std::fs::File;
use std::io::prelude::*;
use rustc_serialize::json;
use std::io;

use r2pipe::structs::{LAliasInfo, LFunctionInfo, LOpInfo, LRegInfo};

use radeco_lib::frontend::ssaconstructor::*;
use radeco_lib::middle::ssa::ssastorage::SSAStorage;
use radeco_lib::middle::ssa::ssa_traits::{SSA, SSAMod, SSAWalk};
use radeco_lib::middle::ir_writer::IRWriter;
use radeco_lib::middle::dot;
use radeco_lib::middle::dce;
use radeco_lib::utils;
use radeco_lib::analysis::sccp::sccp;
use radeco_lib::analysis::cse::cse::CSE;

const REGISTER_PROFILE: &'static str = "register_profile";
const INSTRUCTIONS: &'static str = "instructions2.json";


fn before_test(reg_profile: &mut LRegInfo, instructions: &mut LFunctionInfo, from: &str) {
    // Enable for debugging only.
    let mut register_profile = File::open(REGISTER_PROFILE).unwrap();
    let mut s = String::new();
    register_profile.read_to_string(&mut s).unwrap();
    *reg_profile = json::decode(&*s).unwrap();
    let mut instruction_file = File::open(from).unwrap();
    let mut s = String::new();
    instruction_file.read_to_string(&mut s).unwrap();
    *instructions = json::decode(&*s).unwrap();
}

fn run_construction() -> SSAStorage {
    let mut reg_profile = Default::default();
    let mut instructions = Default::default();
    before_test(&mut reg_profile, &mut instructions, INSTRUCTIONS);
    let mut ssa = SSAStorage::new();
    {
        let mut constructor = SSAConstruct::new(&mut ssa, &reg_profile);
        constructor.run(instructions.ops.unwrap());
    }
    {
        dce::collect(&mut ssa);
    }
    { 
        let tmp = dot::emit_dot(&ssa);
        let mut f = File::create("ct1.dot").unwrap();
        f.write_all(tmp.as_bytes()).expect("Write failed!");
    }
    ssa
}

fn run_sccp(ssa: &mut SSAStorage) -> SSAStorage {
    let mut ssa = {
        let mut analyzer = sccp::Analyzer::new(ssa);
        analyzer.analyze();
        analyzer.emit_ssa()
    };
    {
        dce::collect(&mut ssa);
    }
    ssa
}

fn run_cse(ssa: &mut SSAStorage) -> SSAStorage {
    {
        let mut cse = CSE::new(ssa);
        cse.run();
    }
    ssa.clone()
}

#[test]
fn ct1_construction() {
    let ssa = run_construction();
    let mut writer: IRWriter = Default::default();
    writer.emit_il(Some("main".to_owned()), &ssa, &mut io::stdout());
}

#[test]
fn ct1_sccp() {
    let ssa = {
        let mut ssa_ = run_construction();
        run_sccp(&mut ssa_)
    };
    let mut writer: IRWriter = Default::default();
    writer.emit_il(Some("main".to_owned()), &ssa, &mut io::stdout());
}

#[test]
fn ct1_cse() {
    let ssa = {
        let mut ssa_ = run_construction();
        run_cse(&mut ssa_)
    };
    let mut writer: IRWriter = Default::default();
    writer.emit_il(Some("main".to_owned()), &ssa, &mut io::stdout());
}

#[test]
fn ct1_cse_sccp() {
    let ssa = {
        let mut ssa_ = run_construction();
        {
            run_cse(&mut ssa_);
        }
        run_sccp(&mut ssa_)
    };
    let mut writer: IRWriter = Default::default();
    writer.emit_il(Some("main".to_owned()), &ssa, &mut io::stdout());
}
