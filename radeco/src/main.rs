#[cfg(feature = "trace_log")]
extern crate env_logger;

extern crate radeco_lib;
extern crate r2pipe;
extern crate r2api;
extern crate base64;
extern crate rustyline;

use radeco_lib::analysis::cse::cse::CSE;
use radeco_lib::analysis::interproc::fixcall::CallFixer;
use radeco_lib::analysis::sccp;
use radeco_lib::backend::lang_c::c_cfg::CCFGVerifier;
use radeco_lib::backend::lang_c::c_cfg_builder;
use radeco_lib::frontend::radeco_containers::*;
use radeco_lib::middle::{dce, dot};
use radeco_lib::middle::ir_reader::parse_il;
use radeco_lib::middle::ir_writer;
use radeco_lib::middle::regfile::SubRegisterFile;
use radeco_lib::middle::ssa::ssastorage::SSAStorage;
use radeco_lib::middle::ssa::verifier;
use rustyline::Editor;


use rustyline::error::ReadlineError;

use std::collections::HashMap;
use std::process;
use std::str;
use std::sync::Arc;

// On unix platforms you can use ANSI escape sequences
#[cfg(unix)]
static PROMPT: &'static str = "\x1b[1;32m>>\x1b[0m ";

// Windows consoles typically don't support ANSI escape sequences out
// of the box
#[cfg(windows)]
static PROMPT: &'static str = ">> ";

fn main() {
    let mut rl = Editor::<()>::new();
    let mut proj = None;
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let mut terms = line.split_whitespace();
                let o1 = terms.next();
                let o2 = terms.next();
                cmd(o1, o2, &mut proj);
            }
            Err(ReadlineError::Interrupted) |
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}

mod command {
    pub const LOAD: &'static str = "load";
    pub const FNLIST: &'static str = "fn_list";
    pub const ANALYZE: &'static str = "analyze";
    pub const DOT: &'static str = "dot";
    pub const IR: &'static str = "ir";
    pub const DECOMPILE: &'static str = "decompile";
}

fn cmd(op1: Option<&str>, op2: Option<&str>, proj_opt: &mut Option<RadecoProject>) {
    match (op1, op2) {
        (Some(command::LOAD), Some(s)) => {
            let mut p = ProjectLoader::new().path(s).load();
            let regfile = p.regfile().clone();
            for mut xy in p.iter_mut() {
                analyze_mod(regfile.clone(), xy.module);
            }
            *proj_opt = Some(p);
            return;
        }
        _ => {}
    };
    if proj_opt.is_none() {
        println!("Load a project first");
        return;
    }
    let mut proj = proj_opt.as_mut().unwrap();
    match (op1, op2) {
        (Some(command::ANALYZE), Some("*")) => {
            let rfns = proj.iter_mut().map(|i| i.module).flat_map(|rmod| {
                rmod.functions.values_mut()
            });
            for rfn in rfns {
                analyze(rfn);
            }
        }
        (Some(command::FNLIST), _) => {
            let funcs = fn_list(&proj);
            println!("{}", funcs.join("\n"));
        }
        (Some(command::ANALYZE), Some(f)) => {
            if let Some(rfn) = get_function_mut(f, &mut proj) {
                analyze(rfn);
            } else {
                println!("{} is not found", f);
            }
        }
        (Some(command::DOT), Some(f)) => {
            if let Some(rfn) = get_function(f, &proj) {
                println!("{}", emit_dot(rfn.ssa()));
            } else {
                println!("{} is not found", f);
            }
        }
        (Some(command::IR), Some(f)) => {
            if let Some(rfn) = get_function(f, &proj) {
                println!("{}", emit_ir(rfn));
            } else {
                println!("{} is not found", f);
            }
        }
        (Some(command::DECOMPILE), Some(f)) => {
            if let Some(rfn) = get_function(f, &proj) {
                let rmod = proj.iter().map(|i| i.module).next().unwrap();
                let func_name_map = rmod.functions
                    .iter()
                    .map(|(&addr, f)| (addr, f.name.to_string()))
                    .collect();
                let strings = rmod.strings()
                    .iter()
                    .filter(|ref s| s.vaddr.is_some() && s.string.is_some())
                    .cloned()
                    .map(|s| {
                        let (addr, _s) = (s.vaddr.unwrap(), s.string.unwrap());
                        let bytes = base64::decode(&_s).unwrap_or(Vec::new());
                        let ret_string = match str::from_utf8(bytes.as_slice()) {
                            Ok(v) => v.to_string(),
                            Err(_e) => _s,
                        };
                        (addr, ret_string)
                    })
                    .collect();
                println!("{}", decompile(rfn, &func_name_map, &strings));
            } else {
                println!("{} is not found", f);
            }
        }
        _ => {}
    }
}

fn fn_list(proj: &RadecoProject) -> Vec<String> {
    proj.iter()
        .map(|i| i.module)
        .flat_map(|rmod| rmod.functions.values())
        .map(|s| s.name.to_string())
        .collect()
}

fn get_function<'a>(name: &str, proj: &'a RadecoProject) -> Option<&'a RadecoFunction> {
    proj.iter()
        .map(|i| i.module)
        .flat_map(|rmod| rmod.functions.values())
        .filter(|rfn| rfn.name == name)
        .next()
}

fn get_function_mut<'a>(name: &str, proj: &'a mut RadecoProject) -> Option<&'a mut RadecoFunction> {
    proj.iter_mut()
        .map(|i| i.module)
        .flat_map(|rmod| rmod.functions.values_mut())
        .filter(|rfn| rfn.name == name)
        .next()
}

fn analyze_mod(regfile: Arc<SubRegisterFile>, rmod: &mut RadecoModule) {
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

fn analyze(rfn: &mut RadecoFunction) {
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

fn emit_ir(rfn: &RadecoFunction) -> String {
    println!("  [*] Writing out IR");
    let mut res = String::new();
    ir_writer::emit_il(&mut res, Some(rfn.name.to_string()), rfn.ssa()).unwrap();
    res
}

fn parse_ir(name: &str, ir_str: &str, regfile: Arc<SubRegisterFile>) {
    println!("  [*] Testing IR parser");
    let parsed = parse_il(ir_str, regfile.clone());
    let mut res = String::new();
    ir_writer::emit_il(&mut res, Some(name.to_string()), &parsed).unwrap();
    for (orig, roundtrip) in ir_str.lines().zip(res.lines()) {
        if orig != roundtrip {
            println!("  FAILED TO ROUND-TRIP: \"{}\" => \"{}\"", orig, roundtrip);
        }
    }
}

fn emit_dot(ssa: &SSAStorage) -> String {
    dot::emit_dot(ssa)
}

fn decompile(
    rfn: &RadecoFunction,
    func_name_map: &HashMap<u64, String>,
    strings: &HashMap<u64, String>,
) -> String {
    let c_cfg = c_cfg_builder::recover_c_cfg(rfn, func_name_map, strings);
    if let Err(err) = CCFGVerifier::verify(&c_cfg) {
        println!("CCFG verification failed");
        println!("{}", err);
    }
    c_cfg.to_c_ast().print()
}
