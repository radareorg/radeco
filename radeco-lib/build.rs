extern crate lalrpop;

fn main() {
    lalrpop::Configuration::new().process_file("src/middle/ir_reader/parser.lalrpop").unwrap();
}
