//! API for working with conditions.
//!
//! Conditions are arena-allocated. Only freely-copyable references are exposed
//! through this API. This allows them to be manipulated without worrying about
//! ownership.

// TODO: Conditions should probably be interned, but I can't find a crate for
//       that and I don't want to write one right now either.

#[cfg(test)]
mod test;

use linear_map::set::LinearSet;
use typed_arena::Arena;

use std::fmt;
use std::hash::{Hash, Hasher};

/// A condition. This can be freely copied.
/// Use [`Context`] to make one.
pub struct Condition<'cd, T: 'cd>(&'cd CondVariants<'cd, T>);

/// *Really* a condition. These are the referents of [`Condition`]s and are
/// what live in [`Storage`]s.
enum CondVariants<'cd, T: 'cd> {
    /// a variable, negated if the bool is `true`
    Var(bool, VarRef<'cd, T>),
    /// a conjunction or disjunction of *two or more* conditions
    /// except for [`Storage::true_`]/[`Storage::false_`]
    // can't use `HashSet` because `HashSet` itself isn't `Hash`
    Expr(Op, LinearSet<Condition<'cd, T>>),
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
enum Op {
    And,
    Or,
}

/// Wrapper for comparing and hashing by pointer
#[derive(Debug)]
struct VarRef<'cd, T: 'cd>(&'cd T);

/// Helper for creating new conditions.
/// Use [`Storage::cctx`] to make one.
#[derive(Debug)]
pub struct Context<'cd, T: 'cd> {
    store: &'cd Storage<'cd, T>,
}

/// Backing storage for [`Condition`]s.
pub struct Storage<'cd, T: 'cd> {
    vars: Arena<T>,
    conds: Arena<CondVariants<'cd, T>>,
    true_: CondVariants<'cd, T>,
    false_: CondVariants<'cd, T>,
}

use self::CondVariants::*;

impl<'cd, T> Context<'cd, T> {
    pub fn mk_var(&self, t: T) -> Condition<'cd, T> {
        self.store.mk_cond(Var(false, self.store.mk_var(t)))
    }

    pub fn mk_and(&self, l: Condition<'cd, T>, r: Condition<'cd, T>) -> Condition<'cd, T> {
        self.mk_expr(Op::And, l, r)
    }
    pub fn mk_or(&self, l: Condition<'cd, T>, r: Condition<'cd, T>) -> Condition<'cd, T> {
        self.mk_expr(Op::Or, l, r)
    }
    fn mk_expr(&self, mk_op: Op, l: Condition<'cd, T>, r: Condition<'cd, T>) -> Condition<'cd, T> {
        match (l.0, r.0) {
            // check for annihilators
            (c, _) | (_, c) if Condition(&c).is_annihilator(mk_op) => {
                self.mk_empty_op(mk_op.dual())
            }
            // inline expressions with the same op
            (&Expr(lop, ref lopn_v), &Expr(rop, ref ropn_v)) if lop == mk_op && rop == mk_op => {
                let mut builder = self.expr_builder(mk_op, lopn_v.clone());
                for &ropn in ropn_v {
                    builder.insert(ropn);
                }
                builder.finish()
            }
            (&Expr(op, ref opn_v), c) | (c, &Expr(op, ref opn_v)) if op == mk_op => {
                let mut builder = self.expr_builder(mk_op, opn_v.clone());
                builder.insert(Condition(&c));
                builder.finish()
            }
            // nothing to simplify
            (_, _) => {
                let mut builder = self.expr_builder(mk_op, LinearSet::new());
                builder.insert(l);
                builder.insert(r);
                builder.finish()
            }
        }
    }

    pub fn mk_or_from_iter<I>(&self, iter: I) -> Condition<'cd, T>
    where
        I: IntoIterator<Item = Condition<'cd, T>>,
    {
        self.mk_expr_from_iter(Op::Or, iter)
    }
    pub fn mk_and_from_iter<I>(&self, iter: I) -> Condition<'cd, T>
    where
        I: IntoIterator<Item = Condition<'cd, T>>,
    {
        self.mk_expr_from_iter(Op::And, iter)
    }
    fn mk_expr_from_iter<I>(&self, mk_op: Op, iter: I) -> Condition<'cd, T>
    where
        I: IntoIterator<Item = Condition<'cd, T>>,
    {
        let mut builder = self.expr_builder(mk_op, LinearSet::new());
        for cond in iter {
            match cond.0 {
                // check for annihilators
                _ if cond.is_annihilator(mk_op) => return self.mk_empty_op(mk_op.dual()),
                // inline expressions with the same op
                &Expr(op, ref opn_v) if op == mk_op => {
                    if builder.opn_v.is_empty() {
                        builder.opn_v = opn_v.clone();
                    } else {
                        for &opn in opn_v {
                            builder.insert(opn)
                        }
                    }
                }
                // nothing to simplify
                _ => builder.insert(cond),
            }
        }
        builder.finish()
    }

    fn remove_operand(
        &self,
        cond_op: Op,
        cond: Condition<'cd, T>,
        to_remove: Condition<'cd, T>,
    ) -> Condition<'cd, T> {
        match cond.0 {
            &Var(_, _) => {
                debug_assert!(cond == to_remove);
                self.mk_empty_op(cond_op)
            }
            &Expr(_c_op, ref c_opn_v) => {
                debug_assert!(_c_op == cond_op);
                let mut new_c_opn_v = c_opn_v.clone();
                let _removed = new_c_opn_v.remove(&to_remove);
                debug_assert!(_removed);
                self.store_expr(cond_op, new_c_opn_v)
            }
        }
    }

    fn expr_builder(&self, op: Op, opn_v: LinearSet<Condition<'cd, T>>) -> ExprBuilder<'cd, T> {
        ExprBuilder {
            cctx: *self,
            op,
            opn_v,
        }
    }

    fn store_expr(&self, mk_op: Op, mk_opn_v: LinearSet<Condition<'cd, T>>) -> Condition<'cd, T> {
        match mk_opn_v.len() {
            0 => self.mk_empty_op(mk_op), // avoid duplicate "true"/"false"
            1 => {
                let v: Vec<_> = mk_opn_v.into();
                v[0]
            }
            _ => self.store.mk_cond(Expr(mk_op, mk_opn_v)),
        }
    }

    pub fn mk_not(&self, cond: Condition<'cd, T>) -> Condition<'cd, T> {
        match cond.0 {
            &Var(inv, vr) => self.store.mk_cond(Var(!inv, vr)),
            &Expr(op, ref op_v) => {
                self.store_expr(op.dual(), op_v.iter().map(|&c| self.mk_not(c)).collect())
            }
        }
    }

    fn mk_empty_op(&self, mk_op: Op) -> Condition<'cd, T> {
        match mk_op {
            Op::And => self.mk_true(),
            Op::Or => self.mk_false(),
        }
    }
    pub fn mk_true(&self) -> Condition<'cd, T> {
        Condition(&self.store.true_)
    }
    pub fn mk_false(&self) -> Condition<'cd, T> {
        Condition(&self.store.false_)
    }
}

struct ExprBuilder<'cd, T: 'cd> {
    cctx: Context<'cd, T>,
    op: Op,
    opn_v: LinearSet<Condition<'cd, T>>,
}

impl<'cd, T> ExprBuilder<'cd, T> {
    fn insert(&mut self, cond: Condition<'cd, T>) {
        match cond.0 {
            &Var(_, _) => {
                if !self.try_undistribute(cond, cond) {
                    self.opn_v.insert(cond);
                }
            }
            &Expr(_c_op, ref c_opn_v) => {
                debug_assert!(_c_op.dual() == self.op);
                let inserted = c_opn_v.iter().any(|&c_opn| {
                    if let &Var(_, _) = c_opn.0 {
                        self.try_undistribute(c_opn, cond)
                    } else {
                        false
                    }
                });
                if !inserted {
                    self.opn_v.insert(cond);
                }
            }
        }
    }

    /// try to undistribute: (assuming self.op == Op::And)
    /// inserting `Or{var, ..o1}` into `And{Or{var, ..o2}, ..a1}`
    /// ==> `And{Or{var, ..o1}, Or{var, ..o2},  ..a1}`
    /// ==> `And{Or{var, And{Or{..o1}, Or{..o2}}}, ..a1}`
    ///
    /// Returns true if `ins_cond` was inserted
    fn try_undistribute(
        &mut self,
        ins_var: Condition<'cd, T>,
        ins_cond: Condition<'cd, T>,
    ) -> bool {
        debug_assert!(ins_var.expr_op() == None);
        // look for a `Or{var, ..o2}`
        let opt_part_2 = self
            .opn_v
            .iter()
            .find(|&&opn| match opn.0 {
                &Var(_, _) => opn == ins_var,
                &Expr(_opn_op, ref opn_opn_v) => {
                    debug_assert!(_opn_op.dual() == self.op);
                    opn_opn_v.iter().any(|&opn_opn| opn_opn == ins_var)
                }
            })
            .cloned();

        if let Some(part_2) = opt_part_2 {
            let _removed = self.opn_v.remove(&part_2);
            debug_assert!(_removed);

            // make `Or{..o1}`, `Or{..o2}`
            let part_1 = self.cctx.remove_operand(self.op.dual(), ins_cond, ins_var);
            let new_part_2 = self.cctx.remove_operand(self.op.dual(), part_2, ins_var);

            // make `Or{var, And{Or{..o1}, Or{..o2}}}`
            let to_insert = self.cctx.mk_expr(
                self.op.dual(),
                ins_var,
                self.cctx.mk_expr(self.op, part_1, new_part_2),
            );

            debug_assert!(to_insert.expr_op() != Some(self.op));
            self.opn_v.insert(to_insert);
            true
        } else {
            false
        }
    }

    fn finish(self) -> Condition<'cd, T> {
        self.cctx.store_expr(self.op, self.opn_v)
    }
}

impl<'cd, T> Storage<'cd, T> {
    pub fn new() -> Self {
        Self {
            vars: Arena::new(),
            conds: Arena::new(),
            true_: Expr(Op::And, LinearSet::new()),
            false_: Expr(Op::Or, LinearSet::new()),
        }
    }

    pub fn cctx(&'cd self) -> Context<'cd, T> {
        Context { store: self }
    }

    fn mk_cond(&'cd self, c: CondVariants<'cd, T>) -> Condition<'cd, T> {
        // NOTE: if/when interning gets implemented, it should happen here
        Condition(self.conds.alloc(c))
    }

    fn mk_var(&'cd self, t: T) -> VarRef<'cd, T> {
        VarRef(self.vars.alloc(t))
    }
}

impl Op {
    fn dual(&self) -> Op {
        match self {
            Op::And => Op::Or,
            Op::Or => Op::And,
        }
    }
}

impl<'cd, T> Condition<'cd, T> {
    fn expr_op(&self) -> Option<Op> {
        match self.0 {
            &Var(_, _) => None,
            &Expr(op, _) => Some(op),
        }
    }

    fn is_annihilator(&self, for_op: Op) -> bool {
        match self.0 {
            &Var(_, _) => false,
            &Expr(op, ref opn_v) => op.dual() == for_op && opn_v.is_empty(),
        }
    }
}

impl<'cd, T> fmt::Debug for Storage<'cd, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str("<Storage>")
    }
}

impl<'cd, T> Copy for Context<'cd, T> {}
impl<'cd, T> Clone for Context<'cd, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'cd, T> Copy for Condition<'cd, T> {}
impl<'cd, T> Clone for Condition<'cd, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'cd, T: fmt::Debug> fmt::Debug for Condition<'cd, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            Var(false, t) => write!(f, "{:?}", t.0),
            Var(true, t) => write!(f, "-{:?}", t.0),
            Expr(Op::And, c) => write!(f, "And{:?}", c),
            Expr(Op::Or, c) => write!(f, "Or{:?}", c),
        }
    }
}

impl<'cd, T> Copy for VarRef<'cd, T> {}
impl<'cd, T> Clone for VarRef<'cd, T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<'cd, T> Eq for VarRef<'cd, T> {}
impl<'cd, T> PartialEq for VarRef<'cd, T> {
    fn eq(&self, rhs: &Self) -> bool {
        use std::ptr;
        ptr::eq(self.0, rhs.0)
    }
}
impl<'cd, T> Hash for VarRef<'cd, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let p: *const _ = self.0;
        p.hash(state)
    }
}

// manual impls because `#[derive]` would require `T: (Partial)Eq/Hash`

impl<'cd, T> Eq for CondVariants<'cd, T> {}
impl<'cd, T> PartialEq for CondVariants<'cd, T> {
    fn eq(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Var(l0, l1), Var(r0, r1)) => (l0, l1) == (r0, r1),
            (Expr(l0, l1), Expr(r0, r1)) => (l0, l1) == (r0, r1),
            _ => false,
        }
    }
}

impl<'cd, T> Eq for Condition<'cd, T> {}
impl<'cd, T> PartialEq for Condition<'cd, T> {
    fn eq(&self, rhs: &Self) -> bool {
        self.0 == rhs.0
    }
}
