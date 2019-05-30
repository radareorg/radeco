pub trait AstContext {
    type Block;
    type Variable;
    type BoolVariable;
    type Condition: 'static;
}

pub trait AstContextMut: AstContext {
    /// Returns a new unused `Variable`. It must be initialized to some value,
    /// but it doesn't matter what.
    fn mk_fresh_var(&mut self) -> Self::Variable;

    /// Returns a new unused `Variable` that is initialized with zero.
    fn mk_fresh_var_zeroed(&mut self) -> Self::Variable;

    /// Returns a new unused `BoolVariable`. It must be initialized to some
    /// value, but it doesn't matter what.
    fn mk_fresh_bool_var(&mut self) -> Self::BoolVariable;

    /// Returns a `Condition` that represents `var` being equal to `val`.
    fn mk_cond_equals(&mut self, var: &Self::Variable, val: u64) -> Self::Condition;

    /// Returns a `Condition` that represents the value of `var`.
    fn mk_cond_from_bool_var(&mut self, var: &Self::BoolVariable) -> Self::Condition;

    /// Returns a new `Block` whose only effect is to assign `val` to `var`.
    fn mk_var_assign(&mut self, var: &Self::Variable, val: u64) -> Self::Block;

    /// Returns a new `Block` whose only effect is to assign the value of `cond`
    /// to `var`.
    fn mk_bool_var_assign(
        &mut self,
        var: &Self::BoolVariable,
        cond: &Self::Condition,
    ) -> Self::Block;
}
