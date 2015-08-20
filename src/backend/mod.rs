//! Components converting SSA to more high level representations.

pub mod lang_c;
pub mod scf;

use middle::ssa::SSA;

#[allow(dead_code)]
struct Function<'a, T: SSA + 'a> {
	ssa: &'a T,
	scf: scf::AST_,
	aref: Vec<T::ActionRef>,
	vref: Vec<T::ValueRef>,
}
