// this file is included when compiling src/bin/*.rs or tests
// maybe we shouldn't define 'main' here

extern crate radeco;
use radeco::frontend::{parser, r2};
use radeco::middle::{cfg};
use radeco::middle::dot::Dot;

use std::io::prelude::*;
use std::fs::File;

fn make_dot(g: cfg::CFG, outfile: &str) {
    let mut dot_file = File::create(outfile).ok().expect("Error. Cannot create file!\n");
    dot_file.write_all(g.to_dot().as_bytes()).ok().expect("Error. Cannot write file!\n");
    println!("[*] Dot file written!");
    println!("[*] Run `./scripts/genpng.sh {}` to generate the graph.", outfile);
}

// attribute to ignore unused 'main' when running tests
#[cfg_attr(test, allow(dead_code))]
fn main() {
    //let mut p = esil::Parser::new();
    //let exprs = ["rax,rbx,+", "rax,=", "rax,rax,^", "zf,?{,6,rip,=,}", "rax,rax,^", "rax,rax,+="];
    //for exp in &exprs {
        //p.parse(exp.to_string(), None);
    //}
    //let mut cfg = cfg::CFG::new();
    //cfg.build(&mut (p.emit_insts()));
    //dot::make_dot(cfg);

    let mut r2 = r2::R2::new("./key");
    r2.init();
    let r = r2.get_reg_info();
    println!("{:?}", r);
    let mut p = parser::Parser::new(None);
    p.set_register_profile(&r);

    //let mut ops = r2.get_insts(Some(2), Some("sym.main"));
    //println!("[*] Got ops.");
    //let mut p = esil::Parser::new(None);
    //println!("[*] Begin Parse.");
    //for op in ops.iter_mut() {
        //p.parse_opinfo(op).ok();
    //}

    //println!("[*] Begin CFG Generation.");
    //let mut cfg = cfg::CFG::new();
    //cfg.build(&mut (p.emit_insts()));

    //println!("[*] Dot generation.");
    //make_dot(cfg, "cfg.dot");
}
