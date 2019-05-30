//! See [`run`].

use super::ast::AstNode as AstNodeC;
use super::ast_context::AstContextMut;
use super::condition;
use super::{AstNode, CondContext, CondVar, RegionAstContext};

use std::mem;

/// For every condition that is used more than once, introduces a boolean
/// variable to store the value of the condition the first time it's used
/// and replaces all subsequent uses of the condition with the new variable.
///
/// `Loop`s should never appear here since acyclic refinement can't introduce
/// them and any "internal" loops are wrapped in `BasicBlock`s.
pub(super) fn run<'cd, A: AstContextMut>(
    actx: &mut A,
    cctx: CondContext<'cd, A>,
    conds: &[CondVar<'cd, A>],
    mut ast: AstNode<'cd, RegionAstContext<'cd, A>>,
) -> AstNode<'cd, RegionAstContext<'cd, A>> {
    for &check_cond in conds {
        let opt_assign_info = {
            // find all uses
            let mut cuf = ConditionUseFinder::new(check_cond);
            cuf.run(&mut ast);
            let mut uses = cuf.uses;

            if uses.len() > 1 {
                // replace condition uses with a new boolean variable
                let bool_var = actx.mk_fresh_bool_var();
                let new_cond = cctx.new_var(actx.mk_cond_from_bool_var(&bool_var));
                for use_cond in &mut uses {
                    **use_cond = cctx.replace_var_in(**use_cond, check_cond, new_cond);
                }

                // escape info from borrow of `ast` (unnecessary after nll)
                let first_use: *const _ = uses.swap_remove(0);
                Some((bool_var, first_use))
            } else {
                None
            }
        };

        if let Some((bool_var, first_use)) = opt_assign_info {
            // insert the assignment of `bool_var`
            let assign = actx.mk_bool_var_assign(&bool_var, &check_cond);
            let _res = place_assign(&mut ast, AstNodeC::BasicBlock(assign), first_use);
            debug_assert!(_res.is_err());
        }
    }

    ast
}

struct ConditionUseFinder<'a, 'cd: 'a, C: 'cd> {
    find_cond: condition::VarRef<'cd, C>,
    uses: Vec<&'a mut condition::Condition<'cd, C>>,
}

impl<'a, 'cd, C> ConditionUseFinder<'a, 'cd, C> {
    fn new(find_cond: condition::VarRef<'cd, C>) -> Self {
        Self {
            find_cond,
            uses: Vec::new(),
        }
    }

    fn run<B, V>(&mut self, ast: &'a mut AstNodeC<B, condition::Condition<'cd, C>, V>) {
        use self::AstNodeC::*;
        match ast {
            BasicBlock(_) => (),
            Seq(seq) => {
                for a in seq {
                    self.run(a);
                }
            }
            Cond(c, t, oe) => {
                if c.contains_var(self.find_cond) {
                    self.uses.push(c);
                }
                self.run(t);
                if let Some(e) = oe.as_mut() {
                    self.run(e);
                }
            }
            Loop(_, _) => panic!("found loop"),
            Break => (),
            Switch(_, cases, default) => {
                for (_, a) in cases {
                    self.run(a);
                }
                self.run(default);
            }
        }
    }
}

/// Find a `Cond` node whose condition is `first_use` and replace it with
/// `assign` followed by that node.
/// The raw pointer `first_use` is used only as a unique identifier for the
/// `Cond` node and is never dereferenced, so this is safe.
fn place_assign<'cd, B, C, V>(
    ast: &mut AstNodeC<B, condition::Condition<'cd, C>, V>,
    mut assign: B,
    first_use: *const condition::Condition<'cd, C>,
) -> Result<B, ()> {
    use self::AstNodeC::*;

    // (ab)use `?` to return early if assign was placed in a child of this
    let is_here = match ast {
        BasicBlock(_) => false,
        Seq(seq) => {
            for a in seq {
                assign = place_assign(a, assign, first_use)?
            }
            false
        }
        Cond(c, t, oe) => {
            if first_use == c {
                true
            } else {
                assign = place_assign(t, assign, first_use)?;
                if let Some(e) = oe.as_mut() {
                    assign = place_assign(e, assign, first_use)?;
                }
                false
            }
        }
        Loop(_, _) => panic!("found loop"),
        Break => false,
        Switch(_, cases, default) => {
            for (_, a) in cases {
                assign = place_assign(a, assign, first_use)?;
            }
            assign = place_assign(default, assign, first_use)?;
            false
        }
    };

    if is_here {
        // place `assign`
        *ast = Seq(vec![
            BasicBlock(assign),
            mem::replace(ast, AstNodeC::default()),
        ]);
        Err(())
    } else {
        // continue
        Ok(assign)
    }
}
