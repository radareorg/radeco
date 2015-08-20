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
/*
	fn push_expr(&mut self, outer_expr: &Expr, inner_expr: &Expr, assoc_side: bool) {
		let inner_prec = operator_precedence(inner_expr);
		let outer_prec = operator_precedence(outer_expr);
		if inner_prec > outer_prec || assoc_side && inner_prec == outer_prec {

			inner_expr.write(self);

		} else {

			self.push_str("(");
			inner_expr.write(self);
			self.push_str(")");
		}
	}
*/
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
		Exp_::UnaryExp(_)             => PrecedenceLevel::Unary,
		Exp_::SizeofExp(_)            => PrecedenceLevel::Unary,
		Exp_::SizeofTy(_)             => PrecedenceLevel::Unary,
		Exp_::IndexExp(_, _)          => PrecedenceLevel::Postfix,
		Exp_::CallExp(_, _)           => PrecedenceLevel::Postfix,
		Exp_::DotExp(_, _)            => PrecedenceLevel::Postfix,
		Exp_::ArrowExp(_, _)          => PrecedenceLevel::Postfix,
		Exp_::PostIncrement(_)	      => PrecedenceLevel::Postfix,
		Exp_::PostDecrement(_)        => PrecedenceLevel::Postfix,
		Exp_::PrimaryExp(_)           => PrecedenceLevel::Primary,
	}
}

impl CWriteable for BinaryOperator {
	fn write(&self, w: &mut CWriter) {
		use super::ast::BinaryOperator::*;
		let s = match *self {
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
		};
		w.push_str(s);
	}
}

impl CWriteable for AssignmentOperator {
	fn write(&self, w: &mut CWriter) {
		use super::ast::BinaryOperator::*;
		let s = match *self.0 {
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
		};
		w.push_str(s);
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

impl CWriteable for Exp_ {
	fn write(&self, w: &mut CWriter) {
		use super::ast::PrecedenceLevel::*;
		match *self {
			Exp_::CommaExp(lhs, rhs) => {
				write_expr(Comma, lhs);
				write_str(", ");
				write_expr(Assignment, rhs);
			},
			Exp_::AssignmentExp(op, lhs, rhs) => {
				write_expr(Unary, lhs);
				write_str(aop_string(op));
				write_expr(Assignment, rhs);
			},
			Exp_::ConditionalExp(cond, true_exp, false_exp) => {
				write_expr(LogicalOr, cond);
				write_str(" ? ");
				write_expr(Comma, true_exp);
				write_str(" : ");
				write_expr(Conditional, false_exp);
			},
			Exp_::BinaryExp(op, lhs, rhs) => {
				let lhs_pl = operator_precedence(op);
				let rhs_pl = lhs_pl.step();
				write_expr(lhs_pl, lhs);
				write_str(op_string(op));
				write_expr(rhs_pl, rhs);
			},
			Exp_::Cast(tr, exp) => {
				write_str("(");
				write_typeref(tr);
				write_str(")");
				write_expr(Cast, exp);
			},
			Exp_::UnaryExp(op, exp) => {
				write_str(unary_str(op));
				write_expr(Cast, exp);
			},
			Exp_::IncDecExp(op, exp) => {
				write_str(incdec_str(op));
				write_expr(Unary, exp);
			},
			Exp_::SizeofExp(exp) => {
				write_str("sizeof ");
				write_expr(Unary, exp)
			},
			Exp_::SizeofTy(tr) => {
				write_str("sizeof (");
				write_typeref(tr);
				write_str(")");
			},
			Exp_::IndexExp(base, index) => {
				write_expr(Postfix, base);
				write_str("[");
				write_expr(Comma, index);
				write_str("]");
			},
			Exp_::CallExp(exp, args) => {
				write_expr(Postfix, exp);
				write_str("(");
				write_comma_list(args, |arg| write_expr(Assignment, arg));
				write_str(")");
			},
			Exp_::DotExp(exp, id) => {
				write_expr(Postfix, exp);
				write_str(".");
				write_id(id);
			},
			Exp_::ArrowExp(exp, id) => {
				write_expr(Postfix, exp);
				write_str("->");
				write_id(id);
			},
			Exp_::PostIncDec(incdec, exp) => {
				write_expr(Postfix, exp);
				write_str(incdec.to_string());
			},
			Exp_::PrimaryExp(pexp) => {
				
			},
		}
	}
}

