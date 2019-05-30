use super::c_ast::CAST;
use super::c_cfg;
use super::c_cfg_builder;
use crate::frontend::radeco_containers::RadecoFunction;
use crate::middle::ir_reader;
use crate::middle::regfile::SubRegisterFile;
use serde_json;
use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::sync::Arc;

const REGISTER_PROFILE: &'static str = "test_files/x86_register_profile.json";

lazy_static! {
    static ref REGISTER_FILE: Arc<SubRegisterFile> = {
        let s = fs::read_to_string(REGISTER_PROFILE).unwrap();
        let reg_profile = serde_json::from_str(&*s).unwrap();
        Arc::new(SubRegisterFile::new(&reg_profile))
    };
}

fn run_ssa_file<P: AsRef<Path>>(file: P) -> Result<CAST, &'static str> {
    let mut rfn = RadecoFunction::default();
    *rfn.ssa_mut() = ir_reader::parse_il(&fs::read_to_string(file).unwrap(), REGISTER_FILE.clone());
    let ccfg = c_cfg_builder::recover_c_cfg(&rfn, &HashMap::new(), &HashMap::new());

    c_cfg::ctrl_flow_struct::structure_and_convert(ccfg)
}

#[test]
fn bin1_is_ok() {
    assert!(run_ssa_file("test_files/bin1_main_ssa").is_ok());
}

#[test]
fn loopy_is_ok() {
    assert!(run_ssa_file("test_files/loopy_main_ssa").is_ok());
}
