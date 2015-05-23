// this file is included when compiling src/bin/*.rs or tests
// maybe we shouldn't define 'main' here

extern crate radeco;

// attribute to ignore unused 'main' when running tests
#[cfg_attr(test, allow(dead_code))]
fn main() {
}
