// this file is included when compiling src/bin/*.rs or tests
// maybe we shouldn't define 'main' here

extern crate radeco;
use radeco::frontend::{esil, cfg};

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
    let mut expression = "rax,rbx,+";
    let mut p = esil::Parser::new();
    
    parse_verbose(&mut p, expression);
    expression = "rax,=";
    parse_verbose(&mut p, expression);
    expression = "rax,rax,^";
    parse_verbose(&mut p, expression);

    cfg::make_graph(p.emit_insts());
}
