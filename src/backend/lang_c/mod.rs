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

#[test]
fn cwrite_test() {
	use std::rc::Rc;
	use self::ast::*;
	use self::writer::*;

	let x = Exp_::BinaryExp(
		BinaryOperator::Mul,
		Box::new(Exp_::PrimaryExp(
			PrimaryExp::Id(Rc::new("foo".to_string()))
		)),
		Box::new(Exp_::BinaryExp(
			BinaryOperator::Add,
			Box::new(Exp_::PrimaryExp(
				PrimaryExp::Id(Rc::new("foo".to_string()))
			)),
			Box::new(Exp_::PrimaryExp(
				PrimaryExp::Const(Const::UInt(1))
			))
		))
	);

	let mut writer = CWriter::new();
	x.write(&mut writer);
	println!("{}", writer.get());
}
