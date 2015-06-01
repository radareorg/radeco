// this file is included when compiling src/bin/*.rs or tests
// maybe we shouldn't define 'main' here

extern crate radeco;
use radeco::frontend::esil;

fn parse_verbose (p: &mut esil::Parser, expression: &'static str) {
    println!("< {}", expression.to_string());
    p.parse(expression);
    for inst in &p.emit_insts() {
        println!("> {}", inst);
    }
}

// attribute to ignore unused 'main' when running tests
#[cfg_attr(test, allow(dead_code))]
fn main() {
    let expression = "rax,rbx,-=[4]";
    let mut p = esil::Parser::new();
    parse_verbose(&mut p, expression);
}
