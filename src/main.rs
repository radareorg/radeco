// this file is included when compiling src/bin/*.rs or tests
// maybe we shouldn't define 'main' here

extern crate radeco;
use radeco::frontend::{esil, r2};
use radeco::middle::{cfg, dot};

//fn parse_verbose (p: &mut esil::Parser, expression: &'static str) {
    //println!("< {}", expression.to_string());
    //if let Err(e) = p.parse(expression, None) {
        //panic!("Error: {:?}", e)
    //}
    //for inst in &p.emit_insts() {
        //println!("> {}", inst);
    //}
//}

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
    let ops = r2.get_function("sym.main").ops;
    println!("[*] Got ops.");
    let mut p = esil::Parser::new();
    println!("[*] Begin Parse.");
    for op in ops {
        if op.esil.len() == 0 {
            continue;
        }
        println!("   0x{:x}> {}", op.offset, op.esil);
        p.parse(op.esil, Some(op.offset)).ok().expect("Error");
    }

    let insts = p.emit_insts();
    println!("");
    println!("");

    for i in &insts {
        println!("{}", i);
    }

    println!("");
    println!("[*] Begin CFG Generation.");
    let mut cfg = cfg::CFG::new();
    cfg.build(&mut (p.emit_insts()));

    println!("[*] Dot generation.");
    dot::make_dot(cfg);
}
