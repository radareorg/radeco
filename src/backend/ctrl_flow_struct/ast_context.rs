pub trait AstContext {
    type Block;
    type Variable;
    type Condition: 'static;
}

pub trait AstContextMut: AstContext {
    /// Returns a new unused `Variable`.
    fn mk_fresh_var(&mut self) -> Self::Variable;

    /// Returns a `Condition` that represents `var` being equal to `val`.
    fn mk_cond_equals(&mut self, var: &Self::Variable, val: u64) -> Self::Condition;

    /// Returns a new `Block` whose only effect is to assign `val` to `var`.
    fn mk_var_assign(&mut self, var: &Self::Variable, val: u64) -> Self::Block;

    /// Returns a new `Block` whose only effect is "break".
    fn mk_break(&mut self) -> Self::Block;
}
