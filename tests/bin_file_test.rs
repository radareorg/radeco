//! Runs decompiler stages on main function of /bin/ls

#[macro_use] extern crate radeco_lib;
extern crate r2pipe;
extern crate r2api;
extern crate serde_json;
extern crate petgraph;

use std::fs::File;
use std::io::prelude::*;

use petgraph::graph::NodeIndex;

use r2api::structs::{LFunctionInfo, LRegInfo};

use radeco_lib::frontend::bindings::{RadecoBindings, RBind, RBindings, Binding};
use radeco_lib::frontend::containers::{RadecoFunction, RFunction};
use radeco_lib::frontend::containers::{RadecoModule, RModule};
use radeco_lib::frontend::source::FileSource;
use radeco_lib::middle::ssa::ssastorage::SSAStorage;
use radeco_lib::middle::ir_writer::IRWriter;
use radeco_lib::middle::{dce, dot};
use radeco_lib::analysis::sccp;
use radeco_lib::analysis::cse::cse::CSE;
use radeco_lib::analysis::valueset::fixcall::CallFixer;


pub type DefaultFnTy = RadecoFunction<RadecoBindings<Binding<NodeIndex>>>;

fn run_dce(rmod: &mut RadecoModule<DefaultFnTy>) 
{
    let functions = rmod.functions.clone();
    let mut matched_func_vec: Vec<u64> =
        functions.iter().map(|(fn_addr, rfn)| fn_addr.clone()).collect();
    for fn_addr in &matched_func_vec {
        let rfn = rmod.functions.get_mut(fn_addr)
                        .expect("RadecoFunction Not Found!");
        let ssa = rfn.ssa_mut();
        dce::collect(ssa);
    }
}

fn run_sccp(rmod: &mut RadecoModule<DefaultFnTy>) 
{
    let functions = rmod.functions.clone();
    let mut matched_func_vec: Vec<u64> =
        functions.iter().map(|(fn_addr, rfn)| fn_addr.clone()).collect();
    for fn_addr in &matched_func_vec {
        let mut writer: IRWriter = Default::default();
        let rfn = rmod.functions.get_mut(fn_addr)
                        .expect("RadecoFunction Not Found!");
        let mut ssa = rfn.ssa_mut().clone();
        let mut analyzer = sccp::Analyzer::new(&mut ssa);
        analyzer.analyze();
        rfn.ssa = analyzer.emit_ssa();
    }
}

fn run_cse(rmod: &mut RadecoModule<DefaultFnTy>) 
{
    let functions = rmod.functions.clone();
    let mut matched_func_vec: Vec<u64> =
        functions.iter().map(|(fn_addr, rfn)| fn_addr.clone()).collect();
    for fn_addr in &matched_func_vec {
        let rfn = rmod.functions.get_mut(fn_addr)
                        .expect("RadecoFunction Not Found!");
        let ssa = rfn.ssa_mut();
        let mut cse = CSE::new(ssa);
        cse.run();
    }
}

fn run_write(rmod: &RadecoModule<DefaultFnTy>) 
{
    let functions = rmod.functions.clone();
    let mut matched_func_vec: Vec<u64> =
        functions.iter().map(|(fn_addr, rfn)| fn_addr.clone()).collect();
    for fn_addr in &matched_func_vec {
        let mut writer: IRWriter = Default::default();
        let rfn = rmod.functions.get(fn_addr)
                        .expect("RadecoFunction Not Found!");
        let ssa = rfn.ssa_ref();
        println!("{}", writer.emit_il(Some("main".to_owned()), ssa));
    }
}

#[test]
fn bin_file_construction() {
    let mut fsource = FileSource::open(Some("./test_files/bin_file/bin_file"));
    let mut rmod = RadecoModule::from(&mut fsource);

    run_write(&rmod);
}

#[test]
fn bin_file_dce() {
    let mut fsource = FileSource::open(Some("./test_files/bin_file/bin_file"));
    let mut rmod = RadecoModule::from(&mut fsource);

    run_dce(&mut rmod);
    run_write(&rmod);
}

#[test]
fn bin_file_dce_sccp() {
    let mut fsource = FileSource::open(Some("./test_files/bin_file/bin_file"));
    let mut rmod = RadecoModule::from(&mut fsource);

    run_dce(&mut rmod);
    run_sccp(&mut rmod);
    run_write(&rmod);
}

#[test]
fn bin_file_dce_cse() {
    let mut fsource = FileSource::open(Some("./test_files/bin_file/bin_file"));
    let mut rmod = RadecoModule::from(&mut fsource);

    run_dce(&mut rmod);
    run_cse(&mut rmod);
    run_write(&rmod);
}

#[test]
fn bin_file_dce_cse_sccp() {
    let mut fsource = FileSource::open(Some("./test_files/bin_file/bin_file"));
    let mut rmod = RadecoModule::from(&mut fsource);

    run_dce(&mut rmod);
    run_cse(&mut rmod);
    run_sccp(&mut rmod);
    run_write(&rmod);
}

#[test]
fn bin_file_dce_fix() {
    let mut fsource = FileSource::open(Some("./test_files/bin_file/bin_file"));
    let mut rmod = RadecoModule::from(&mut fsource);

    run_dce(&mut rmod);
    {
        let mut callfixer = CallFixer::new(&mut rmod);
        callfixer.rounded_analysis();
    }
    run_write(&rmod);
}

#[test]
fn bin_file_dce_fix_sccp() {
    let mut fsource = FileSource::open(Some("./test_files/bin_file/bin_file"));
    let mut rmod = RadecoModule::from(&mut fsource);

    run_dce(&mut rmod);
    {
        let mut callfixer = CallFixer::new(&mut rmod);
        callfixer.rounded_analysis();
    }
    run_sccp(&mut rmod);
    run_write(&rmod);
}

#[test]
fn bin_file_dce_fix_cse() {
    let mut fsource = FileSource::open(Some("./test_files/bin_file/bin_file"));
    let mut rmod = RadecoModule::from(&mut fsource);

    run_dce(&mut rmod);
    {
        let mut callfixer = CallFixer::new(&mut rmod);
        callfixer.rounded_analysis();
    }
    run_cse(&mut rmod);
    run_write(&rmod);
}

#[test]
fn bin_file_dce_fix_cse_sccp() {
    let mut fsource = FileSource::open(Some("./test_files/bin_file/bin_file"));
    let mut rmod = RadecoModule::from(&mut fsource);

    run_dce(&mut rmod);
    {
        let mut callfixer = CallFixer::new(&mut rmod);
        callfixer.rounded_analysis();
    }
    run_cse(&mut rmod);
    run_sccp(&mut rmod);
    run_write(&rmod);
}
