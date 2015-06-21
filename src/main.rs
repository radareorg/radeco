// this file is included when compiling src/bin/*.rs or tests
// maybe we shouldn't define 'main' here

extern crate radeco;

use std::io::Write;
use radeco::frontend::esil;
use std::env;

fn parse_verbose (p: &mut esil::Parser, expression: &str) {
    println!("< {}", expression);
    if let Err(e) = p.parse(expression) {
        panic!("Error: {:?}", e)
    }
    for inst in &p.emit_insts() {
        println!("> {}", inst);
    }
}

// attribute to ignore unused 'main' when running tests
#[cfg_attr(test, allow(dead_code))]
fn main() {
    let helpmsg = "Usage: radeco [esil-expr]\n";
    let mut p = esil::Parser::new();
    let expression: String;
    if env::args().count() > 1 {
        let arg1 = env::args().nth(1).unwrap();
        if arg1 == "-h" {
            std::io::stderr().write(helpmsg.as_bytes()).unwrap();
            std::process::exit(0);
        } else {
            expression = arg1;
        }
    } else {
        expression = "rax,rbx,+".to_string();
    }
    parse_verbose(&mut p, &*expression);
}
