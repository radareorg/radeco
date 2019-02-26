use base64;
use r2pipe::{R2Pipe, R2};
use radeco_lib::analysis::engine::{Engine, RadecoEngine};
use radeco_lib::backend::lang_c::c_cfg::ctrl_flow_struct;
use radeco_lib::backend::lang_c::c_cfg::CCFGVerifier;
use radeco_lib::backend::lang_c::c_cfg_builder;
use radeco_lib::frontend::radeco_containers::*;
use radeco_lib::middle::ir_writer;
use radeco_lib::middle::ssa::ssastorage::SSAStorage;
use radeco_lib::middle::dot;
use std::cell::RefCell;
use std::collections::HashMap;
use std::panic;
use std::rc::Rc;
use std::str;

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

pub fn fn_rename(old_f: &str, new_f: &str, proj: &mut RadecoProject) {
    if let Some(_) = get_function(new_f, proj) {
        println!("there is already a function called: {}", new_f);
        return;
    }

    let mut found = false;

    for module in proj.iter_mut() {
        let module = module.module;
        let off = module.iter()
            .find(|rfn| rfn.function.1.name == old_f)
            .map(|rfn| rfn.function.1.offset);

        if let Some(off) = off {
            module.function_rename(off, new_f);
            found = true;
        }
    }

    if !found {
        println!("function not found: {}", old_f);
    }
}

pub fn analyze(rfn: &mut RadecoFunction, max_it: u32) {
    let engine = RadecoEngine::new(max_it);
    engine.run_func(rfn);
}

pub fn analyze_all_functions<'a>(proj: &'a mut RadecoProject, max_it: u32) {
    let rfns = proj
        .iter_mut()
        .map(|i| i.module)
        .flat_map(|rmod| rmod.functions.values_mut());
    for rfn in rfns {
        analyze(rfn, max_it);
    }
}

pub fn emit_ir(rfn: &RadecoFunction) -> String {
    eprintln!("  [*] Writing out IR");
    let mut res = String::new();
    ir_writer::emit_il(&mut res, Some(rfn.name.to_string()), rfn.ssa()).unwrap();
    res
}

pub fn emit_dot(ssa: &SSAStorage) -> String {
    dot::emit_dot(ssa)
}

pub fn decompile_all_functions<'a>(proj: &'a RadecoProject) -> String {
    let mut decompiled_funcs = Vec::new();
    let funcs = fn_list(&proj);
    for f in &funcs {
        match decompile(f, &proj) {
            Ok(res) => {
                decompiled_funcs.push(res);
            }
            Err(err) => {
                eprintln!("{}", err);
            }
        };
    }
    decompiled_funcs.join("\n")
}

pub fn decompile<'a>(name: &str, proj: &'a RadecoProject) -> Result<String, String> {
    if let Some(rfn) = get_function(name, &proj) {
        let rmod = proj.iter().map(|i| i.module).next().unwrap();
        let func_name_map = func_names(&rmod);
        let strings = strings(&rmod);
        decompile_priv(rfn, &func_name_map, &strings)
    } else {
        Err(format!("{} is not found.", name))
    }
}

fn decompile_priv(
    rfn: &RadecoFunction,
    func_name_map: &HashMap<u64, String>,
    strings: &HashMap<u64, String>,
) -> Result<String, String> {
    let c_cfg_result =
        panic::catch_unwind(|| c_cfg_builder::recover_c_cfg(rfn, func_name_map, strings));

    if c_cfg_result.is_err() {
        return Err("Failed to recover C control flow graph".to_string());
    };

    let c_cfg = c_cfg_result.unwrap();

    if let Err(err) = CCFGVerifier::verify(&c_cfg) {
        eprintln!("CCFG verification failed {}", err);
    }

    let result = panic::catch_unwind(|| {
        ctrl_flow_struct::structure_and_convert(c_cfg.clone())
            .map(|s| s.print())
            .map_err(|e| e.to_string())
    });
    match result {
        Ok(code) => code,
        Err(_) => {
            println!("Control flow structuring failed");
            Ok(c_cfg.to_c_ast().print())
        }
    }
}

pub fn load_proj_by_path(path: &str, max_it: u32) -> RadecoProject {
    let mut p = ProjectLoader::new().path(path).load();
    let regfile = p.regfile().clone();
    for mut xy in p.iter_mut() {
        let engine = RadecoEngine::new(max_it);
        engine.run_module(xy.module, &*regfile.clone());
    }
    p
}

pub fn load_proj_tcp(url: &str, max_it: u32) -> Result<RadecoProject, &'static str> {
    let r2p = R2Pipe::tcp(url)?;
    Ok(load_project_by_r2pipe(r2p, max_it))
}

pub fn load_proj_http(url: &str, max_it: u32) -> Result<RadecoProject, &'static str> {
    let r2p = R2Pipe::http(url)?;
    Ok(load_project_by_r2pipe(r2p, max_it))
}

pub fn load_project_by_r2pipe(r2p: R2Pipe, max_it: u32) -> RadecoProject {
    let r2 = R2::from(r2p);
    let r2w = Rc::new(RefCell::new(r2));
    let mut p = ProjectLoader::new().source(Rc::new(r2w)).load();
    let regfile = p.regfile().clone();
    for mut xy in p.iter_mut() {
        let engine = RadecoEngine::new(max_it);
        engine.run_module(xy.module, &*regfile.clone());
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
        })
        .collect()
}
