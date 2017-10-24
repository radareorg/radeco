//! Runs decompiler stages on main function of /bin/ls

#[macro_use] extern crate radeco_lib;
extern crate r2pipe;
extern crate r2api;
extern crate serde_json;

use std::fs::File;
use std::io::prelude::*;

use r2api::structs::{LFunctionInfo, LRegInfo};

use radeco_lib::frontend::ssaconstructor::SSAConstruct;
use radeco_lib::middle::ssa::ssastorage::SSAStorage;
use radeco_lib::middle::ssa::verifier;
use radeco_lib::middle::ir_writer::IRWriter;
use radeco_lib::middle::{dce, dot};
use radeco_lib::analysis::sccp;
use radeco_lib::analysis::cse::cse::CSE;

use radeco_lib::analysis::matcher::gmatch;
use radeco_lib::backend::x86::x86_idioms;

const REGISTER_PROFILE: &'static str = "test_files/x86_register_profile.json";
const INSTRUCTIONS: &'static str = "test_files/bin_ls_instructions.json";

fn before_test(reg_profile: &mut LRegInfo, instructions: &mut LFunctionInfo, from: &str) {
    let mut register_profile = File::open(REGISTER_PROFILE).unwrap();
    let mut s = String::new();
    register_profile.read_to_string(&mut s).unwrap();
    *reg_profile = serde_json::from_str(&*s).unwrap();
    let mut instruction_file = File::open(from).unwrap();
    let mut s = String::new();
    instruction_file.read_to_string(&mut s).unwrap();
    *instructions = serde_json::from_str(&*s).unwrap();
}

fn run_construction() -> SSAStorage {
    let mut reg_profile = Default::default();
    let mut instructions = Default::default();
    before_test(&mut reg_profile, &mut instructions, INSTRUCTIONS);
    let mut ssa = SSAStorage::new();
    {
        let mut constructor = SSAConstruct::new(&mut ssa, &reg_profile);
        constructor.run(instructions.ops.unwrap().as_slice());
    }
    {
        dce::collect(&mut ssa);
    }
    {
        let tmp = dot::emit_dot(&ssa);
        let mut f = File::create("bin_ls.dot").unwrap();
        f.write_all(tmp.as_bytes()).expect("Write failed!");
    }
    ssa
}

fn run_sccp(ssa: &mut SSAStorage) -> SSAStorage {
    let mut analyzer = sccp::Analyzer::new(ssa);
    analyzer.analyze();
    analyzer.emit_ssa()
}

fn run_cse(ssa: &mut SSAStorage) -> SSAStorage {
    {
        let mut cse = CSE::new(ssa);
        cse.run();
    }
    ssa.clone()
}

#[test]
fn bin_ls_construction() {
    let ssa = run_construction();
    verifier::verify(&ssa).unwrap();
    let mut writer: IRWriter = Default::default();
    println!("{}", writer.emit_il(Some("main".to_owned()), &ssa));
}

#[test]
fn bin_ls_sccp() {
    let ssa = {
        let mut ssa_ = run_construction();
        run_sccp(&mut ssa_)
    };
    verifier::verify(&ssa).unwrap();
    let mut writer: IRWriter = Default::default();
    println!("{}", writer.emit_il(Some("main".to_owned()), &ssa));
}

#[test]
fn bin_ls_cse() {
    let ssa = {
        let mut ssa_ = run_construction();
        run_cse(&mut ssa_)
    };
    verifier::verify(&ssa).unwrap();
    let mut writer: IRWriter = Default::default();
    println!("{}", writer.emit_il(Some("main".to_owned()), &ssa));
}

#[test]
fn bin_ls_cse_sccp() {
    let ssa = {
        let mut ssa_ = run_construction();
        {
            run_cse(&mut ssa_);
        }
        run_sccp(&mut ssa_)
    };
    verifier::verify(&ssa).unwrap();
    let mut writer: IRWriter = Default::default();
    println!("{}", writer.emit_il(Some("main".to_owned()), &ssa));
}

#[test]
fn bin_ls_grep_replace() {
    let ssa = {
        let mut ssa_ = run_construction();
        {
            run_cse(&mut ssa_);
        }
        grep_and_replace!(&mut ssa_, "(OpSub (OpSub rsp, #x8), %1)" => "&var_C");
        grep_and_replace!(&mut ssa_, "(OpSub (OpSub rsp, #x8), #x8)" => "&var_8");
        grep_and_replace!(&mut ssa_, "(OpLoad (OpStore %1, &var_C, %3), &var_C)" => "%3");
        grep_and_replace!(&mut ssa_, "(OpLoad (OpStore %1, &var_8, %3), &var_8)" => "%3");
        grep_and_replace!(&mut ssa_, "(OpStore (OpStore %1, %2, %3), %2, %4)" => "(OpStore %1, %2, %4)");
        grep_and_replace!(&mut ssa_, "(OpStore mem, (OpSub rsp, #x8), rbp)" => "mem'");
        run_sccp(&mut ssa_)
    };
    let mut writer: IRWriter = Default::default();
    println!("{}", writer.emit_il(Some("main".to_owned()), &ssa));
}

#[test]
fn bin_ls_x86_idioms() {
    let mut ssa = run_construction();
    {
        x86_idioms::replace(&mut ssa);
    }
    {
        dce::collect(&mut ssa);
    }
    let mut writer: IRWriter = Default::default();
    println!("{}", writer.emit_il(Some("main".to_owned()), &ssa));
}
