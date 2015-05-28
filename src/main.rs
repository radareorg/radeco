// this file is included when compiling src/bin/*.rs or tests
// maybe we shouldn't define 'main' here

extern crate radeco;
use radeco::frontend::esil;

// attribute to ignore unused 'main' when running tests
#[cfg_attr(test, allow(dead_code))]
fn main() {
    let mut p = esil::Parser::new();
    p.parse("eax,ebx,^=,eax,ebx,+=");
    for inst in &p.emit_insts() {
        println!("{}", inst.to_string());
    }
}
