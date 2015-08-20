use std::string::ToString;
use super::ast::*;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum PrecedenceLevel {
	Comma,
	Assignment,
	Conditional,
	LogicalOr,
	LogicalAnd,
	InclusiveOr,
	ExclusiveOr,
	And,
	Equality,
	Relational,
	Shift,
	Additive,
	Mult,
	Cast,
	Unary,
	Postfix,
	Primary,
}

impl PrecedenceLevel {
	fn step(self) -> Self { use self::PrecedenceLevel::*; match self {
		LogicalOr   => LogicalAnd,
		LogicalAnd  => InclusiveOr,
		InclusiveOr => ExclusiveOr,
		ExclusiveOr => And,
		And         => Equality,
		Equality    => Relational,
		Relational  => Shift,
		Shift       => Additive,
		Additive    => Mult,
		_           => Primary,
	}}
}

trait CWriteable {
	fn write(&self, &mut CWriter);
}

trait CWriter {
	fn push_str(&mut self, &str);
}

fn operator_precedence(expr: &Exp_) -> PrecedenceLevel {
	match *expr {
		Exp_::CommaExp(_, _)          => PrecedenceLevel::Comma,
		Exp_::AssignmentExp(_, _, _)  => PrecedenceLevel::Assignment,
		Exp_::ConditionalExp(_, _, _) => PrecedenceLevel::LogicalOr,
		Exp_::BinaryExp(ref op, _, _) => {
			use super::ast::BinaryOperator::*;
			match *op {
				LogicalOr             => PrecedenceLevel::LogicalOr,
				LogicalAnd            => PrecedenceLevel::LogicalAnd,
				InclusiveOr           => PrecedenceLevel::InclusiveOr,
				ExclusiveOr           => PrecedenceLevel::ExclusiveOr,
				And                   => PrecedenceLevel::And,
				Eq | Ne               => PrecedenceLevel::Equality,
				Lt | Gt | Le | Ge     => PrecedenceLevel::Relational,
				Left | Right          => PrecedenceLevel::Shift,
				Add | Sub             => PrecedenceLevel::Additive,
				Mul | Div | Mod       => PrecedenceLevel::Mult,
			}
		}
		Exp_::Cast(_, _)              => PrecedenceLevel::Cast,
		Exp_::UnaryExp(_, _)          => PrecedenceLevel::Unary,
		Exp_::SizeofExp(_)            => PrecedenceLevel::Unary,
		Exp_::SizeofTy(_)             => PrecedenceLevel::Unary,
		Exp_::IndexExp(_, _)          => PrecedenceLevel::Postfix,
		Exp_::CallExp(_, _)           => PrecedenceLevel::Postfix,
		Exp_::DotExp(_, _)            => PrecedenceLevel::Postfix,
		Exp_::ArrowExp(_, _)          => PrecedenceLevel::Postfix,
		Exp_::PostIncDec(_, _)        => PrecedenceLevel::Postfix,
		Exp_::PrimaryExp(_)           => PrecedenceLevel::Primary,
	}
}

fn op_string(op: BinaryOperator) -> &'static str {
	use super::ast::BinaryOperator::*;
	match op {
		LogicalOr   => "||",
		LogicalAnd  => "&&",
		InclusiveOr => "|",
		ExclusiveOr => "^",
		And         => "&",
		Eq          => "==",
		Ne          => "!=",
		Lt          => "<",
		Gt          => ">",
		Le          => "<=",
		Ge          => ">=",
		Left        => "<<",
		Right       => ">>",
		Add         => "+",
		Sub         => "-",
		Mul         => "*",
		Div         => "/",
		Mod         => "%",
	}
}

fn aop_string(op: BinaryOperator) -> &'static str {
	use super::ast::BinaryOperator::*;
	match op {
		LogicalOr   => panic!(),
		LogicalAnd  => panic!(),
		InclusiveOr => "|=",
		ExclusiveOr => "^=",
		And         => "&=",
		Eq          => panic!(),
		Ne          => panic!(),
		Lt          => panic!(),
		Gt          => panic!(),
		Le          => panic!(),
		Ge          => panic!(),
		Left        => "<<=",
		Right       => ">>=",
		Add         => "+=",
		Sub         => "-=",
		Mul         => "*=",
		Div         => "/=",
		Mod         => "%=",
	}
}

fn unary_string(op: UnaryOperator) -> &'static str {
	use super::ast::UnaryOperator::*;
	match op {
		AddrOf     => "&",
		Deref      => "*",
		Plus       => "+",
		Minus      => "-",
		Not        => "~",
		LogicalNot => "!",
	}
}

fn incdec_string(incdec: IncDec) -> &'static str {
	use super::ast::IncDec::*;
	match incdec {
		Inc => "inc",
		Dec => "dec",
	}	
}

/*impl CWriteable for Pointer {
	fn write(&self, w: &mut CWriter) {
		for tycls in self {
			w.push_str("*");
			tycls.write(w);
		}
	}
}*/

struct PLP<'a> (&'a mut CWriter, PrecedenceLevel);

impl<'a> PLP<'a> {
	fn write_expr(&mut self, pl: PrecedenceLevel, exp: &Exp_) {
		if pl < self.1 { self.0.push_str("(") }
		exp.write(self.0);
		if pl < self.1 { self.0.push_str(")") }
	}

	fn write_str(&mut self, string: &str) {
		self.0.push_str(string);
	}

	fn write_typeref(&mut self, tr: TypeRef) {
		self.0.push_str("/* TODO: write_typeref */")
	}

	fn write_comma_list<T>(&mut self, rep: &Vec<T>, func: &Fn(&T) -> ()) {
		let mut first = true;
		for item in rep {
			if !first {
				self.0.push_str(", ");
			}
			func(&item);
		}
	}
}

impl CWriteable for Exp_ {
	fn write(&self, w: &mut CWriter) {
		use self::PrecedenceLevel::*;

		let pl: PrecedenceLevel = operator_precedence(self);

		let plp = PLP(w, pl);

		match *self {
			Exp_::CommaExp(lhs, rhs) => {
				plp.write_expr(Comma, lhs);
				plp.write_str(", ");
				plp.write_expr(Assignment, rhs);
			},
			Exp_::AssignmentExp(op, lhs, rhs) => {
				plp.write_expr(Unary, lhs);
				plp.write_str(aop_string(op));
				plp.write_expr(Assignment, rhs);
			},
			Exp_::ConditionalExp(cond, true_exp, false_exp) => {
				plp.write_expr(LogicalOr, cond);
				plp.write_str(" ? ");
				plp.write_expr(Comma, true_exp);
				plp.write_str(" : ");
				plp.write_expr(Conditional, false_exp);
			},
			Exp_::BinaryExp(op, lhs, rhs) => {
				let lhs_pl = pl;
				let rhs_pl = pl.step();
				plp.write_expr(lhs_pl, lhs);
				plp.write_str(op_string(op));
				plp.write_expr(rhs_pl, rhs);
			},
			Exp_::Cast(tr, exp) => {
				plp.write_str("(");
				plp.write_typeref(tr);
				plp.write_str(")");
				plp.write_expr(Cast, exp);
			},
			Exp_::UnaryExp(op, exp) => {
				plp.write_str(unary_string(op));
				plp.write_expr(Cast, exp);
			},
			Exp_::IncDecExp(op, exp) => {
				plp.write_str(incdec_string(op));
				plp.write_expr(Unary, exp);
			},
			Exp_::SizeofExp(exp) => {
				plp.write_str("sizeof ");
				plp.write_expr(Unary, exp)
			},
			Exp_::SizeofTy(tr) => {
				plp.write_str("sizeof (");
				plp.write_typeref(tr);
				plp.write_str(")");
			},
			Exp_::IndexExp(base, index) => {
				plp.write_expr(Postfix, base);
				plp.write_str("[");
				plp.write_expr(Comma, index);
				plp.write_str("]");
			},
			Exp_::CallExp(exp, args) => {
				plp.write_expr(Postfix, exp);
				plp.write_str("(");
				plp.write_comma_list(args, |arg| plp.write_expr(Assignment, arg));
				plp.write_str(")");
			},
			Exp_::DotExp(exp, id) => {
				plp.write_expr(Postfix, exp);
				plp.write_str(".");
				plp.write_str(&*id);
			},
			Exp_::ArrowExp(exp, id) => {
				plp.write_expr(Postfix, exp);
				plp.write_str("->");
				plp.write_str(&*id);
			},
			Exp_::PostIncDec(incdec, exp) => {
				plp.write_expr(Postfix, exp);
				plp.write_str(incdec_string(incdec));
			},
			Exp_::PrimaryExp(ref pexp) => { match *pexp {
				PrimaryExp::Id(id) => plp.write_str(id),
				PrimaryExp::Const(ref value) => { match *value {
					Const::UInt(value)             => plp.write_str(value.to_string()),
					Const::Int(value)              => plp.write_str(value.to_string()),
					Const::Float(value)            => plp.write_str(value.to_string()),
					Const::EnumerationConst(value) => plp.write_str(value),
				}},
				PrimaryExp::String(value) => plp.write_str(format!("{:?}", value)),
			}},
		}
	}
}

