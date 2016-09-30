#[macro_use] extern crate radeco_lib;
extern crate r2pipe;

use std::path::{Path, PathBuf};
use std::fs::{self, File};
use std::io::Write;

use r2pipe::r2::R2;

use radeco_lib::frontend::containers::RadecoModule;
use radeco_lib::analysis::sccp::sccp;
use radeco_lib::analysis::cse::cse::CSE;
use radeco_lib::middle::{dce};
use radeco_lib::middle::ir_writer::IRWriter;

fn main() {
    let mut dir;
    let mut r2 = R2::new::<String>(None).expect("Unable to open r2");
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
    {
        let mut fname = PathBuf::from(&dir);
        fname.push("main");
        ffm = File::create(&fname).expect("Unable to create file");
    }

    for (ref addr, ref mut rfn) in rmod.functions.iter_mut() {
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
        let mut fname = PathBuf::from(&dir);
        fname.push(&rfn.name);
        let mut ff = File::create(&fname).expect("Unable to create file");
        let mut writer: IRWriter = Default::default();
        let res = writer.emit_il(Some(rfn.name.clone()), &rfn.ssa);
        writeln!(ff,  "{}", res).expect("Error writing to file");
        writeln!(ffm, "{}", res).expect("Error writing to file");
        rmod.src.as_mut().unwrap().send(&format!("CC, {} @ {}", fname.to_str().unwrap(), addr));
    }

    rmod.src.as_mut().unwrap().send(&format!("e scr.color=true"))
}
