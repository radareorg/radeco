#[cfg(feature="trace_log")] extern crate env_logger;

extern crate radeco_lib;
extern crate r2pipe;
extern crate r2api;
extern crate petgraph;

mod cli;

use std::env;
use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process;
use petgraph::graph::NodeIndex;

use r2pipe::r2::R2;
use r2api::api_trait::R2Api;
use radeco_lib::analysis::cse::cse::CSE;
use radeco_lib::analysis::sccp;
//use radeco_lib::analysis::valueset::analyzer_wysinwyx::FnAnalyzer;
//use radeco_lib::analysis::valueset::mem_structs::{A_Loc,AbstractAddress};
use radeco_lib::analysis::interproc::fixcall::CallFixer;
use radeco_lib::frontend::radeco_containers::{ProjectLoader, RadecoProject, RadecoModule};
use radeco_lib::middle::{dce, dot};
use radeco_lib::middle::ir_writer::IRWriter;
use radeco_lib::middle::ir_reader::parse_il;
use radeco_lib::middle::ssa::memoryssa::MemorySSA;
use radeco_lib::middle::ssa::verifier;

//use radeco_lib::analysis::mark_refs;

const USAGE: &'static str = "
Usage: minidec [-f <names>...] <target>

Options:
    -f, --functions  Analayze only some functions
";

fn main() {
    #[cfg(feature="trace_log")] env_logger::init();

    let requested_functions = cli::init_for_args(USAGE);

    let proj_name = env::args().nth(env::args().len() - 1).unwrap();
    let mut rproj = {
        ProjectLoader::new().path(&proj_name).load()
    };
    let regfile = rproj.regfile().clone();
    for mut xy in rproj.iter_mut() {
        let rmod = &mut xy.module;
        let mut dir = PathBuf::from(".");
        dir.push(format!("{}_out", proj_name));
        dir.push(format!("{}_out", rmod.name()));
        fs::create_dir_all(&dir).expect("Failed to create directory");

        // Reduce the complexity of rmod.functions to just a vec of (u64,&String)
        // for easier extraction and matching
   
        // Analyze preserved for all functions.
        {
            // println!("[*] Fixing Callee Information");
            // let bp_name = regfile.get_name_by_alias(&"BP".to_string());
            // let sp_name = regfile.get_name_by_alias(&"SP".to_string());
            // let mut callfixer = CallFixer::new(rmod, bp_name, sp_name);
            // callfixer.rounded_analysis();
        }
        // Filter the data if the user provided some args to be matched upon
        let matched_func_addrs = if requested_functions.len() != 0 {
 
            //XXX Impl in more efficient way
            let mut matched_func_vec: Vec<(u64, &str)> =
                rmod.iter().map(|_f| {
                    let f = _f.function.1;
                    (f.offset.clone(), &*f.name)
                }).collect();
                // rmod.iter().map(|(fn_addr, rfn)| (fn_addr.clone(), &rfn.name)).collect();

            let all_func_names: Vec<(&str)> =
                matched_func_vec.iter().map(|&(_, name)| name).collect();
            matched_func_vec = filter_with(&matched_func_vec,
                                           &requested_functions.iter().map(|s| &s[..]).collect::<Vec<_>>());
            cli::print_match_summary(&matched_func_vec,
                                     &requested_functions.iter().map(|s| &s[..]).collect::<Vec<_>>(),
                                     &all_func_names);
            matched_func_vec.into_iter().map(|(addr, _)| addr).collect::<Vec<_>>()
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
                    Ok(_) => {  }
                }
            }
            {
                // Building memory SSA.
                let _memory_ssa = {
                    // Generate MemorySSA
                    println!("  [*] Generating Memory SSA");
                    let mut mssa = MemorySSA::new(rfn.ssa());
                    //TODO issue119
                    mssa.gather_variables(rfn.datarefs(), rfn.locals(),
                                          &rfn.call_sites(&rmod.callgraph));
                    mssa.run();
                    mssa
                };
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
                let mut ff = File::create(&fname).expect("Unable to create file");
                let mut writer: IRWriter = Default::default();
                let res = writer.emit_il(Some(rfn.name.to_string()), rfn.ssa());
                writeln!(ff, "{}", res).expect("Error writing to file");
                writeln!(ffm, "{}", res).expect("Error writing to file");
                // Set as a comment in radare2
                rmod.source.as_mut().unwrap().send(&format!("CC, {} @ {}", fname.to_str().unwrap(), addr));
                res
            };
        
            {
                //////////////////////////////
                // Try parsing the IR file
                /////////////////////////////
                println!("  [*] Testing IR parser");
                let parsed = parse_il(&fn_ir_str);
                let mut writer: IRWriter = Default::default();
                let res = writer.emit_il(Some((*rfn.name).to_owned()), &parsed);
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
                fname.set_extension("dot");
                let mut df = File::create(&fname).expect("Unable to create .dot file");
                let dot = dot::emit_dot(rfn.ssa());
                writeln!(df, "{}", dot).expect("Error writing to file");
            }
        }

        rmod.source.as_mut().unwrap().send("e scr.color=true");
    }
}

// Filters the functions that were in Radeco output AND requested by user
fn filter_with<'a>(all_funcs: &Vec<(u64, &'a str)>,
                   requested: &Vec<&'a str>)
                   -> Vec<(u64, &'a str)> {

    all_funcs.iter()
        .filter(|&&(_, name)| requested.iter().any(|user_req| &name == user_req))
        .map(|&(addr, name)| (addr, name))
        .collect()
}
