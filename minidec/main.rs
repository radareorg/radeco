extern crate radeco_lib;
extern crate r2pipe;
extern crate clap;

mod cli;

use r2pipe::r2::R2;
use radeco_lib::analysis::cse::CSE;
use radeco_lib::analysis::sccp;
use radeco_lib::frontend::containers::RadecoModule;
use radeco_lib::middle::dce;
use radeco_lib::middle::ir_writer::IRWriter;
use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};

fn main() {
    // Creates cmd args settings then gets the matching params
    let matches = cli::create_args().get_matches();

    let mut dir;
    let mut r2 = R2::new::<String>(Some("/home/ahmed/radare2/radeco-lib/ex-bins/hello-linux-x86_64"
            .to_owned()))
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

    // Main file to contain IRs of all rfns
    let mut ffm;
    let mut fname = PathBuf::from(&dir);
    fname.push("main");
    ffm = File::create(&fname).expect("Unable to create file");

    let mut requested_functions: Vec<String> = Vec::new();
    let output = matches.values_of("functions");

    if let Some(params) = output {
        requested_functions = params.map(|st| st.to_owned()).collect();
    }

    // Find matches between the provided command line args and what radeco found
    // print the matching function names
    // ?? : rmod.functions returns (&u64,RFunction) the names from cmd args are matched with the
    // name field in RFunction, the name is then not needed to access the function
    // info, only the u64 address is needed, so the unnecessary info are filtered out
    let matched_func_vec: Vec<(u64, &String)> = rmod.functions
            .iter()
            .map(|(fn_addr, rfn)| (*fn_addr, &rfn.name)) // Tuple of u64 and &String
            .filter(|&(_, &ref fn_name): &(u64, &String)| { // Types are kept just for clearence
                requested_functions.iter()
                    .any(|ref user_req: &String| fn_name == *user_req)
            })
            .collect();

    if requested_functions.len() != 0 {
        // Function names found by Radeco, print those incase nothing was matched
        let all_func_names: Vec<&String> =
            rmod.functions.values().map(|&ref rfn| &rfn.name).collect();

        // Print the summary of matched functions
        cli::print_match_summary(&matched_func_vec, &requested_functions, &all_func_names);
    }

    for (addr, _) in matched_func_vec {

        let ref mut rfn = rmod.functions.get(&addr).unwrap().clone();

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
        println!("  [*] Writing out IR");

        let mut writer: IRWriter = Default::default();
        let res = writer.emit_il(Some(rfn.name.clone()), &rfn.ssa);

        writeln!(ffm, "{}", res).expect("Error writing to file");
        rmod.src.as_mut().unwrap().send(&format!("CC, {} @ {}", fname.to_str().unwrap(), addr));
    }

    rmod.src.as_mut().unwrap().send("e scr.color=true")
}
