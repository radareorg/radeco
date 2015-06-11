// this file is included when compiling src/bin/*.rs or tests
// maybe we shouldn't define 'main' here

extern crate radeco;
use radeco::frontend::{esil, cfg, dot};

fn parse_verbose (p: &mut esil::Parser, expression: &'static str) {
    println!("< {}", expression.to_string());
    if let Err(e) = p.parse(expression, None) {
        panic!("Error: {:?}", e)
    }
    for inst in &p.emit_insts() {
        println!("> {}", inst);
    }
}

// attribute to ignore unused 'main' when running tests
#[cfg_attr(test, allow(dead_code))]
fn main() {
    let mut p = esil::Parser::new();
    let exprs = ["rax,rbx,+", "rax,=", "rax,rax,^", "zf,?{,6,rip,=,}", "rax,rax,^", "rax,rax,+="];
    for exp in &exprs {
        parse_verbose(&mut p, exp);
    }
    let mut cfg = cfg::CFG::new();
    cfg.build(&(p.emit_insts()));
    dot::make_dot(cfg);
}
