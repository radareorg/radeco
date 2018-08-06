#[cfg(feature = "trace_log")]
extern crate env_logger;

extern crate radeco_lib;
extern crate r2pipe;
extern crate r2api;
extern crate base64;

mod cli;


use radeco_lib::analysis::cse::cse::CSE;
use radeco_lib::analysis::interproc::fixcall::CallFixer;
use radeco_lib::analysis::sccp;
use radeco_lib::backend::lang_c::c_ast::CAST;
use radeco_lib::backend::lang_c::c_cfg::CCFGVerifier;
use radeco_lib::backend::lang_c::c_cfg_builder;
use radeco_lib::frontend::radeco_containers::ProjectLoader;
use radeco_lib::frontend::radeco_source::FileSource;
use radeco_lib::middle::{dce, dot};
use radeco_lib::middle::ir_reader::parse_il;
use radeco_lib::middle::ir_writer;
use radeco_lib::middle::ssa::verifier;
use std::env;
use std::fs::{self, File};
use std::io::Write;
use std::path::PathBuf;
use std::process;
use std::rc::Rc;
use std::str;

const USAGE: &'static str = "
Usage:
  radeco [-f <names>...] [--filesource] <target>
  radeco \
                             (-h | --help)

Options:
    -h, --help          Show this screen.
    \
                             -f, --functions     Analayze only some functions.
    --filesource        \
                             Use json files as input instead of an executable.
";

fn main() {
    #[cfg(feature = "trace_log")] env_logger::init();

    let (requested_functions, is_filesource) = cli::init_for_args(USAGE);

    let proj_name = env::args().nth(env::args().len() - 1).unwrap();
    let mut rproj = {
        if is_filesource {
            let source = FileSource::open(&proj_name);
            ProjectLoader::new().source(Rc::new(source)).load()
        } else {
            ProjectLoader::new().path(&proj_name).load()
        }
    };
    let regfile = rproj.regfile().clone();
    for mut xy in rproj.iter_mut() {
        let rmod = &mut xy.module;
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
                    Err(e) => _s,
                };
                (addr, ret_string)
            })
            .collect();
        let mut dir = PathBuf::from(".");
        dir.push(format!("{}_out", proj_name));
        fs::create_dir_all(&dir).expect("Failed to create directory");

        // Reduce the complexity of rmod.functions to just a vec of (u64,&String)
        // for easier extraction and matching

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

        // Filter the data if the user provided some args to be matched upon
        let matched_func_addrs = if requested_functions.len() != 0 {

            //XXX Impl in more efficient way
            let mut matched_func_vec: Vec<(u64, &str)> = rmod.iter()
                .map(|_f| {
                    let f = _f.function.1;
                    (f.offset.clone(), &*f.name)
                })
                .collect();

            let all_func_names: Vec<(&str)> =
                matched_func_vec.iter().map(|&(_, name)| name).collect();
            matched_func_vec = filter_with(
                &matched_func_vec,
                &requested_functions
                    .iter()
                    .map(|s| &s[..])
                    .collect::<Vec<_>>(),
            );
            cli::print_match_summary(
                &matched_func_vec,
                &requested_functions
                    .iter()
                    .map(|s| &s[..])
                    .collect::<Vec<_>>(),
                &all_func_names,
            );
            matched_func_vec
                .into_iter()
                .map(|(addr, _)| addr)
                .collect::<Vec<_>>()
        } else {
            Vec::new()
        };

        // Main file to contain IRs of all rfns
        let mut ffm = {
            let mut fname = PathBuf::from(&dir);
            fname.push("main");
            File::create(&fname).expect("Unable to create file")
        };

        for addr in matched_func_addrs {

            let ref mut rfn = rmod.functions.get_mut(&addr).unwrap();

            println!("[+] Analyzing: {} @ {:#x}", rfn.name, addr);
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
            {
                // TODO
                // // Building memory SSA.
                // let _memory_ssa = {
                //     // Generate MemorySSA
                //     println!("  [*] Generating Memory SSA");
                //     let mut mssa = MemorySSA::new(rfn.ssa());
                //     mssa.gather_variables(rfn.datarefs(), &rfn.locals(),
                //                           &rfn.call_refs(&rmod.callgraph));
                //     mssa.run();
                //     mssa
                // };
            }
            //if false {
            //    if (!rfn.name.eq("sym.main")) & (!rfn.name.eq("main")) {
            //        continue;
            //    }
            //    println!("  [*] Analyzing Value Sets");
            //    let fn_analyzer = FnAnalyzer::from((*rfn).clone());
            //    let a_store_fn = fn_analyzer.analyze_rfn();
            //    for (a_loc, strided_interval) in a_store_fn.store {
            //        if let A_Loc{addr: AbstractAddress::Node{..}, ..} = a_loc {
            //            continue;
            //        };
            //        println!("{}", a_loc);
            //        println!("Strided Interval: {}", strided_interval);
            //    };
            //}

            //{
            //// Expreimental reference marking pass
            //println!("  [*] Unstable> Marking References");
            //let mut rmark = mark_refs::ReferenceMarker { };
            //rmark.resolve_refs(&mut rfn.ssa);
            //}

            let mut fname = PathBuf::from(&dir);
            fname.push(rfn.name.as_ref());

            let fn_ir_str = {
                ///////////////////////////
                // Write out the IR file
                //////////////////////////
                println!("  [*] Writing out IR");
                let mut res = String::new();
                ir_writer::emit_il(&mut res, Some(rfn.name.to_string()), rfn.ssa()).unwrap();
                let mut ff = File::create(&fname).expect("Unable to create file");
                write!(ff, "{}", res).expect("Error writing to file");
                writeln!(ffm, "{}", res).expect("Error writing to file");
                // Set as a comment in radare2
                match rmod.source.as_mut().unwrap().send(format!(
                    "CC, {} @ {}",
                    fname.to_str().unwrap(),
                    addr
                )) {
                    Ok(_) => {}
                    Err(e) => println!("{:?}", e),
                };
                res
            };

            {
                //////////////////////////////
                // Try parsing the IR file
                /////////////////////////////
                println!("  [*] Testing IR parser");
                let parsed = parse_il(&fn_ir_str, regfile.clone());
                let mut res = String::new();
                ir_writer::emit_il(&mut res, Some((*rfn.name).to_owned()), &parsed).unwrap();
                for (orig, roundtrip) in fn_ir_str.lines().zip(res.lines()) {
                    if orig != roundtrip {
                        println!("  FAILED TO ROUND-TRIP: \"{}\" => \"{}\"", orig, roundtrip);
                    }
                }
            }

            {
                ////////////////////////
                // Generate DOT file
                ///////////////////////
                println!("  [*] Generating dot");
                let mut df = File::create(format!("{}.dot", fname.to_string_lossy()))
                    .expect("Unable to create .dot file");
                let dot = dot::emit_dot(rfn.ssa());
                writeln!(df, "{}", dot).expect("Error writing to file");
            }

            let c_cfg = c_cfg_builder::recover_c_cfg(&rfn, &func_name_map, &strings);

            if let Err(err) = CCFGVerifier::verify(&c_cfg) {
                println!("CCFG verification failed");
                println!("{}", err);
            }

            {
                ////////////////////////
                // Generate pseudo-C CFG Dot file
                ///////////////////////
                println!("  [*] Generating psuedo code");
                let mut df = File::create(format!("{}.c.dot", fname.to_string_lossy()))
                    .expect("Unable to create .c file");
                let dot = c_cfg.dot_str();
                writeln!(df, "{}", dot).expect("Error writing to file");
            }

            {
                ////////////////////////
                // Generate pseudo-C code
                ///////////////////////
                println!("  [*] Generating psuedo code");
                let mut df = File::create(format!("{}.c", fname.to_string_lossy()))
                    .expect("Unable to create .c file");
                let code = c_cfg.to_c_ast().print();
                writeln!(df, "{}", code).expect("Error writing to file");
            }
        }

        match rmod.source.as_mut().unwrap().send(
            "e scr.color=true".to_string(),
        ) {
            Ok(()) => {}
            Err(e) => println!("{:?}", e),
        }
    }
}

// Filters the functions that were in Radeco output AND requested by user
fn filter_with<'a>(
    all_funcs: &Vec<(u64, &'a str)>,
    requested: &Vec<&'a str>,
) -> Vec<(u64, &'a str)> {

    all_funcs
        .iter()
        .filter(|&&(_, name)| {
            requested.iter().any(|user_req| &name == user_req)
        })
        .map(|&(addr, name)| (addr, name))
        .collect()
}
