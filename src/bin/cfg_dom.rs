// Example4:
//   * Get instructions for a complete function.
//   * Make a CFG.
//   * Build a DOMTree.
//   * Emit Dot for CFG and Dom.

extern crate radeco_lib;

use radeco_lib::frontend::{parser, r2};
use radeco_lib::middle::{cfg};
use radeco_lib::middle::dot;
use radeco_lib::analysis::dom::{DomTree};

use std::io::prelude::*;
use std::fs::{File, create_dir};

fn write_file(fname: &str, res: String) {
    let mut file = File::create(fname).ok().expect("Error. Cannot create file!\n");
    file.write_all(res.as_bytes()).ok().expect("Error. Cannot write file!\n");
}

#[cfg_attr(test, allow(dead_code))]
fn main() {
    // Get a new r2 instance.
    let mut r2 = r2::R2::new("./ex-bins/key");
    
    // Initialize with sane defaults.
    r2.init();

    // Get Instructions for 'sym.main'
    let func_info = r2.get_function("sym.main");

    // Get the ops. We should handle error here. But for this example,
    // Just panic is fine.
    let mut ops = func_info.unwrap().ops.unwrap();
    println!("[*] Got ops.");

    // Initialize the parser with default configurations.
    let mut p = parser::Parser::new(None);
    println!("[*] Begin Parse.");
    
    // Get the register profile for the binary an hook it up with the parser.
    let r = r2.get_reg_info().unwrap();
    p.set_register_profile(&r);

    for op in ops.iter_mut() {
        p.parse_opinfo(op).ok();
    }

    println!("[*] Begin CFG Generation.");
    let mut cfg = cfg::CFG::new();
    cfg.build(&mut (p.emit_insts()));
    
    println!("[*] Starting DOMTree Construction.");
    let dom = DomTree::build_dom_tree(&cfg.g, cfg.entry.clone());
    
    println!("[*] Begin Dot generation.");
    let res_dom = dot::emit_dot(&dom);
    let res_cfg = dot::emit_dot(&cfg);

    std::fs::create_dir("outputs").ok();

    let outfile = "outputs/ex4-cfg.dot";
    write_file(outfile, res_cfg);
    println!("[*] Run `./scripts/genpng.sh {}` to generate the graph.", outfile);

    let outfile = "outputs/ex4-dom.dot";
    write_file(outfile, res_dom);
    println!("[*] Run `./scripts/genpng.sh {}` to generate the graph dom-graph.", outfile);
}
