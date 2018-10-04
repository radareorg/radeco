use base64;
use r2pipe::{R2Pipe, R2};
use radeco_lib;
use radeco_lib::analysis::cse::cse::CSE;
use radeco_lib::analysis::interproc::fixcall::CallFixer;
use radeco_lib::analysis::sccp;
use radeco_lib::backend::lang_c::c_cfg::ctrl_flow_struct;
use radeco_lib::backend::lang_c::c_cfg::CCFGVerifier;
use radeco_lib::backend::lang_c::c_cfg_builder;
use radeco_lib::frontend::radeco_containers::*;
use radeco_lib::middle::ir_writer;
use radeco_lib::middle::regfile::SubRegisterFile;
use radeco_lib::middle::ssa::ssastorage::SSAStorage;
use radeco_lib::middle::ssa::verifier;
use radeco_lib::middle::{dce, dot};
use std::cell::RefCell;
use std::collections::HashMap;
use std::process;
use std::rc::Rc;
use std::str;
use std::sync::Arc;

thread_local!(pub static PROJ: RefCell<Option<RadecoProject>> = RefCell::new(None););

pub fn fn_list(proj: &RadecoProject) -> Vec<String> {
    proj.iter()
        .map(|i| i.module)
        .flat_map(|rmod| rmod.functions.values())
        .map(|s| s.name.to_string())
        .collect()
}

pub fn get_function<'a>(name: &str, proj: &'a RadecoProject) -> Option<&'a RadecoFunction> {
    proj.iter()
        .map(|i| i.module)
        .flat_map(|rmod| rmod.functions.values())
        .filter(|rfn| rfn.name == name)
        .next()
}

pub fn get_function_mut<'a>(
    name: &str,
    proj: &'a mut RadecoProject,
) -> Option<&'a mut RadecoFunction> {
    proj.iter_mut()
        .map(|i| i.module)
        .flat_map(|rmod| rmod.functions.values_mut())
        .filter(|rfn| rfn.name == name)
        .next()
}

pub fn analyze_mod(regfile: Arc<SubRegisterFile>, rmod: &mut RadecoModule) {
    // Analyze preserved for all functions.
    {
        println!("[*] Fixing Callee Information");
        let bp_name = regfile.get_name_by_alias(&"BP".to_string());
        let bp_name = bp_name.map(|s| s.to_owned());
        let sp_name = regfile.get_name_by_alias(&"SP".to_string());
        let sp_name = sp_name.map(|s| s.to_owned());
        let mut callfixer = CallFixer::new(rmod, bp_name, sp_name);
        callfixer.rounded_analysis();
    }

    // Fix call sites
    radeco_lib::analysis::functions::fix_ssa_opcalls::go(rmod);

    // Infer calling conventions
    radeco_lib::analysis::functions::infer_regusage::run(rmod, &*regfile);
}

pub fn analyze(rfn: &mut RadecoFunction) {
    println!("[+] Analyzing: {} @ {:#x}", rfn.name, rfn.offset);
    {
        println!("  [*] Eliminating Dead Code");
        dce::collect(rfn.ssa_mut());
    }
    let mut ssa = {
        // Constant Propagation (sccp)
        println!("  [*] Propagating Constants");
        let mut analyzer = sccp::Analyzer::new(rfn.ssa_mut());
        analyzer.analyze();
        analyzer.emit_ssa()
    };
    {
        println!("  [*] Eliminating More DeadCode");
        dce::collect(&mut ssa);
    }
    *rfn.ssa_mut() = ssa;
    {
        // Common SubExpression Elimination (cse)
        println!("  [*] Eliminating Common SubExpressions");
        let mut cse = CSE::new(rfn.ssa_mut());
        cse.run();
    }
    {
        // Verify SSA
        println!("  [*] Verifying SSA's Validity");
        match verifier::verify(rfn.ssa()) {
            Err(e) => {
                println!("  [*] Found Error: {}", e);
                process::exit(255);
            }
            Ok(_) => {}
        }
    }
}

pub fn emit_ir(rfn: &RadecoFunction) -> String {
    println!("  [*] Writing out IR");
    let mut res = String::new();
    ir_writer::emit_il(&mut res, Some(rfn.name.to_string()), rfn.ssa()).unwrap();
    res
}

pub fn emit_dot(ssa: &SSAStorage) -> String {
    dot::emit_dot(ssa)
}

pub fn decompile(
    rfn: &RadecoFunction,
    func_name_map: &HashMap<u64, String>,
    strings: &HashMap<u64, String>,
) -> Result<String, String> {
    let c_cfg = c_cfg_builder::recover_c_cfg(rfn, func_name_map, strings);
    if let Err(err) = CCFGVerifier::verify(&c_cfg) {
        println!("CCFG verification failed {}", err);
    }
    ctrl_flow_struct::structure_and_convert(c_cfg)
        .map(|s| s.print())
        .map_err(|e| e.to_string())
}

pub fn load_proj_by_path(path: &str) -> RadecoProject {
    let mut p = ProjectLoader::new().path(path).load();
    let regfile = p.regfile().clone();
    for mut xy in p.iter_mut() {
        analyze_mod(regfile.clone(), xy.module);
    }
    p
}

pub fn load_proj_tcp(url: &str) -> Result<RadecoProject, &'static str> {
    let r2p = R2Pipe::tcp(url)?;
    Ok(load_project_by_r2pipe(r2p))
}

pub fn load_proj_http(url: &str) -> Result<RadecoProject, &'static str> {
    let r2p = R2Pipe::http(url)?;
    Ok(load_project_by_r2pipe(r2p))
}

pub fn load_project_by_r2pipe(r2p: R2Pipe) -> RadecoProject {
    let r2 = R2::from(r2p);
    let r2w = Rc::new(RefCell::new(r2));
    let mut p = ProjectLoader::new().source(Rc::new(r2w)).load();
    let regfile = p.regfile().clone();
    for mut xy in p.iter_mut() {
        analyze_mod(regfile.clone(), xy.module);
    }
    p
}

pub fn func_names(rmod: &RadecoModule) -> HashMap<u64, String> {
    rmod.functions
        .iter()
        .map(|(&addr, f)| (addr, f.name.to_string()))
        .collect()
}

pub fn strings(rmod: &RadecoModule) -> HashMap<u64, String> {
    rmod.strings()
        .iter()
        .filter(|ref s| s.paddr.is_some() && s.string.is_some())
        .cloned()
        .map(|s| {
            let (addr, _s) = (s.vaddr.unwrap(), s.string.unwrap());
            let bytes = base64::decode(&_s).unwrap_or(Vec::new());
            let ret_string = match str::from_utf8(bytes.as_slice()) {
                Ok(v) => v.to_string(),
                Err(_e) => _s,
            };
            (addr, ret_string)
        }).collect()
}
