#![allow(dead_code)]

pub mod writer;
pub mod ast;

use super::Function;
use middle::ssa::SSA;

struct CSSABuilder {
	i: usize
}

impl CSSABuilder {
	fn add_function<T: SSA>(&mut self, _: &Function<T>) {

	}
}
