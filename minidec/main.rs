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
use radeco_lib::frontend::containers::RadecoModule;
use radeco_lib::middle::{dce, dot};
use radeco_lib::middle::ir_writer::IRWriter;
use radeco_lib::middle::ssa::memoryssa::MemorySSA;
use radeco_lib::middle::ssa::verifier;

//use radeco_lib::analysis::mark_refs;

const USAGE: &'static str = "
Usage: minidec [-f <names>...] <target>

Options:
    -f, --functions  Analayze only some functions
";

fn main() {
    #[cfg(feature="trace_log")] env_logger::init().unwrap();

    let requested_functions = cli::init_for_args(USAGE);

    let mut dir;
    let mut r2 = R2::new::<String>(env::args().nth(env::args().len() - 1))
        .expect("Unable to open r2");
    r2.init();
    let mut rmod = {
        let bin_info = r2.bin_info().expect("Failed to load bin_info");
        let fname = bin_info.core.unwrap().file.unwrap();
        let fname = Path::new(&fname).file_stem().unwrap();
        let fname = format!("{}_out", fname.to_str().unwrap());
        dir = PathBuf::from(".");
        dir.push(&fname);
        fs::create_dir_all(&dir).expect("Failed to create directory");
        println!("[*] Constructing ...");
        RadecoModule::from(&mut r2)
    };

    // Reduce the complexity of rmod.functions to just a vec of (u64,&String)
    // for easier extraction and matching
    //
    // Extract all exsisting function addresses and names
    let functions = rmod.functions.clone();
    let mut matched_func_vec: Vec<(u64, &String)> =
        functions.iter().map(|(fn_addr, rfn)| (fn_addr.clone(), &rfn.name)).collect();

    // Analyze preserved for all functions.
    {
        println!("[*] Fixing Callee Information");
        let mut callfixer = CallFixer::new(&mut rmod);
        callfixer.rounded_analysis();
    }

    // Filter the data if the user provided some args to be matched upon
    if requested_functions.len() != 0 {
        let all_func_names: Vec<(&String)> =
            matched_func_vec.iter().map(|&(_, name)| name).collect();

        matched_func_vec = filter_with(&matched_func_vec, &requested_functions);
        cli::print_match_summary(&matched_func_vec, &requested_functions, &all_func_names);
    }


    // Main file to contain IRs of all rfns
    let mut ffm;
    {
        let mut fname = PathBuf::from(&dir);
        fname.push("main");
        ffm = File::create(&fname).expect("Unable to create file");
    }

    for (addr, _) in matched_func_vec {

        let ref mut rfn = rmod.functions.get_mut(&addr).unwrap();

        println!("[+] Analyzing: {} @ {:#x}", rfn.name, addr);
        {
            println!("  [*] Eliminating Dead Code");
            dce::collect(&mut rfn.ssa);
        }
        let mut ssa = {
            // Constant Propagation (sccp)
            println!("  [*] Propagating Constants");
            let mut analyzer = sccp::Analyzer::new(&mut rfn.ssa);
            analyzer.analyze();
            analyzer.emit_ssa()
        };
        {
            println!("  [*] Eliminating More DeadCode");
            dce::collect(&mut ssa);
        }
        rfn.ssa = ssa;
        {
            // Common SubExpression Elimination (cse)
            println!("  [*] Eliminating Common SubExpressions");
            let mut cse = CSE::new(&mut rfn.ssa);
            cse.run();
        }
        {
            // Verify SSA 
            println!("  [*] Verifying SSA's Validity");
            match verifier::verify(&rfn.ssa) {
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
                let mut mssa = MemorySSA::new(&rfn.ssa);
                mssa.gather_variables(&rfn.datarefs, &rfn.locals, 
                                      &Some(rfn.call_ctx.iter().cloned()
                                            .map(|x| if x.ssa_ref.is_some() {
                                                x.ssa_ref.unwrap() } else {
                                                    NodeIndex::end()
                                                }).collect()));
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
        fname.push(&rfn.name);

        {
            ///////////////////////////
            // Write out the IR file
            //////////////////////////
            println!("  [*] Writing out IR");
            let mut ff = File::create(&fname).expect("Unable to create file");
            let mut writer: IRWriter = Default::default();
            let res = writer.emit_il(Some(rfn.name.clone()), &rfn.ssa);
            writeln!(ff, "{}", res).expect("Error writing to file");
            writeln!(ffm, "{}", res).expect("Error writing to file");
            // Set as a comment in radare2
            rmod.src.as_mut().unwrap().send(&format!("CC, {} @ {}", fname.to_str().unwrap(), addr));
        }
        
        {
            ////////////////////////
            // Generate DOT file
            ///////////////////////
            println!("  [*] Generating dot");
            fname.set_extension("dot");
            let mut df = File::create(&fname).expect("Unable to create .dot file");
            let dot = dot::emit_dot(&rfn.ssa);
            writeln!(df, "{}", dot).expect("Error writing to file");
        }
    }

    rmod.src.as_mut().unwrap().send("e scr.color=true")
}

// Filters the functions that were in Radeco output AND requested by user
fn filter_with<'a>(all_funcs: &Vec<(u64, &'a String)>,
                   requested: &Vec<String>)
                   -> Vec<(u64, &'a String)> {

    all_funcs.iter()
        .filter(|&&(_, name)| requested.iter().any(|user_req: &String| *name == *user_req))
        .map(|&(addr, name)| (addr, name))
        .collect()
}
