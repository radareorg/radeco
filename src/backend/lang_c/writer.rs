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

/*impl CWriteable for Pointer {
	fn write(&self, w: &mut CWriter) {
		for tycls in self {
			w.push_str("*");
			tycls.write(w);
		}
	}
}*/

