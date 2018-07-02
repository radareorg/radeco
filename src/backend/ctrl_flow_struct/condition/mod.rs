//! API for working with conditions.
//!
//! Conditions are arena-allocated. Only freely-copyable references are exposed
//! through this API. This allows them to be manipulated without worrying about
//! ownership.
//!
//! # Example
//! ```rust
//! # use radeco_lib::backend::ctrl_flow_struct::condition::*;
//! // setup
//! let cstore = Storage::new();
//! let cctx = cstore.cctx();
//!
//! // make some variables
//! let a = cctx.mk_var("a");
//! let b = cctx.mk_var("b");
//!
//! // make some expressions
//! let cond1 = cctx.mk_not(cctx.mk_and(a, b));
//! let cond2 = cctx.mk_or(cctx.mk_not(a), cctx.mk_not(b));
//! assert_eq!(cond1, cond2);
//! ```

// TODO: Conditions should probably be interned, but I can't find a crate for
//       that and I don't want to write one right now either.

#[cfg(test)]
mod test;

use linear_map::set::LinearSet;
use typed_arena::Arena;

use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops;

macro_rules! impl_copy {
    ($name:ident) => {
        impl<'cd, T> Copy for $name<'cd, T> {}
        impl<'cd, T> Clone for $name<'cd, T> {
            fn clone(&self) -> Self {
                *self
            }
        }
    };
}

/// A condition. This can be freely copied.
/// Use [`Context`] to make one.
pub struct Condition<'cd, T: 'cd>(&'cd CondVariants<'cd, T>);
impl_copy!{Condition}

/// *Really* a condition. These are the referents of [`Condition`]s and are
/// what live in [`Storage`]s.
enum CondVariants<'cd, T: 'cd> {
    /// a variable, possibly negated
    Var(Negation, VarRef<'cd, T>),
    /// a conjunction or disjunction of *two or more* conditions
    /// except for [`Storage::true_`]/[`Storage::false_`]
    ///
    /// operands must not be expressions with the same operation type as this
    // can't use `HashSet` because `HashSet` itself isn't `Hash`
    Expr(Op, LinearSet<Condition<'cd, T>>),
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
enum Negation {
    Normal,
    Negated,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
enum Op {
    And,
    Or,
}

/// Wrapper for comparing and hashing by pointer
#[derive(Debug)]
struct VarRef<'cd, T: 'cd>(&'cd T);
impl_copy!{VarRef}

/// Helper for creating new conditions. This can be freely copied.
/// Use [`Storage::cctx`] to make one.
#[derive(Debug)]
pub struct Context<'cd, T: 'cd> {
    store: &'cd Storage<'cd, T>,
}
impl_copy!{Context}

/// Backing storage for [`Condition`]s and entry point to this module's API.
pub struct Storage<'cd, T: 'cd> {
    vars: Arena<T>,
    conds: Arena<CondVariants<'cd, T>>,
    true_: CondVariants<'cd, T>,
    false_: CondVariants<'cd, T>,
}

use self::CondVariants::*;

impl<'cd, T> Context<'cd, T> {
    /// Creates a new boolean variable. This will not compare equal to any
    /// previously created variable, regardless of the underlying values.
    pub fn mk_var(self, t: T) -> Condition<'cd, T> {
        self.store
            .mk_cond(Var(Negation::Normal, self.store.mk_var(t)))
    }

    /// Creates the logical conjunction (And) of two conditions.
    pub fn mk_and(self, l: Condition<'cd, T>, r: Condition<'cd, T>) -> Condition<'cd, T> {
        self.mk_expr(Op::And, l, r)
    }
    /// Creates the logical disjunction (Or) of two conditions.
    pub fn mk_or(self, l: Condition<'cd, T>, r: Condition<'cd, T>) -> Condition<'cd, T> {
        self.mk_expr(Op::Or, l, r)
    }
    fn mk_expr(self, mk_op: Op, l: Condition<'cd, T>, r: Condition<'cd, T>) -> Condition<'cd, T> {
        self.mk_expr0(mk_op, l, r)
            .unwrap_or(self.mk_empty_op(mk_op.dual()))
    }
    fn mk_expr0(
        self,
        mk_op: Op,
        l: Condition<'cd, T>,
        r: Condition<'cd, T>,
    ) -> Result<Condition<'cd, T>, ()> {
        Ok(match (
            ExprView::new(l, mk_op.dual()),
            ExprView::new(r, mk_op.dual()),
        ) {
            (Ok(lev), Ok(rev)) => {
                if lev.is_empty() || rev.is_empty() {
                    // annihilator
                    self.mk_empty_op(mk_op.dual())
                } else {
                    // nothing to simplify
                    let mut builder = self.expr_builder(mk_op, LinearSet::new());
                    builder.insert_one(lev)?;
                    builder.insert_one(rev)?;
                    builder.finish()
                }
            }
            (Err(opn_v), Ok(ev)) | (Ok(ev), Err(opn_v)) => {
                if ev.is_empty() {
                    // annihilator
                    self.mk_empty_op(mk_op.dual())
                } else {
                    // inline
                    let mut builder = self.expr_builder(mk_op, opn_v.clone());
                    builder.insert_one(ev)?;
                    builder.finish()
                }
            }
            (Err(lopn_v), Err(ropn_v)) => {
                // inline
                let (aopn_v, bopn_v) = if lopn_v.len() >= ropn_v.len() {
                    (lopn_v, ropn_v)
                } else {
                    (ropn_v, lopn_v)
                };
                let mut builder = self.expr_builder(mk_op, aopn_v.clone());
                for &bopn in bopn_v {
                    builder.insert_one(ExprView::new_or_panic(bopn, mk_op.dual()))?;
                }
                builder.finish()
            }
        })
    }

    /// Creates the logical conjunction (And) of several conditions.
    /// [`Context::mk_and`] may be more efficient for exactly two conditions.
    pub fn mk_and_from_iter<I>(self, iter: I) -> Condition<'cd, T>
    where
        I: IntoIterator<Item = Condition<'cd, T>>,
    {
        self.mk_expr_from_iter(Op::And, iter)
            .unwrap_or(self.mk_false())
    }
    /// Creates the logical disjunction (Or) of several conditions.
    /// [`Context::mk_or`] may be more efficient for exactly two conditions.
    pub fn mk_or_from_iter<I>(self, iter: I) -> Condition<'cd, T>
    where
        I: IntoIterator<Item = Condition<'cd, T>>,
    {
        self.mk_expr_from_iter(Op::Or, iter)
            .unwrap_or(self.mk_true())
    }
    fn mk_expr_from_iter<I>(self, mk_op: Op, iter: I) -> Result<Condition<'cd, T>, ()>
    where
        I: IntoIterator<Item = Condition<'cd, T>>,
    {
        let mut builder = self.expr_builder(mk_op, LinearSet::new());
        for cond in iter {
            builder.insert(cond)?;
        }
        Ok(builder.finish())
    }

    fn expr_builder(self, op: Op, opn_v: LinearSet<Condition<'cd, T>>) -> ExprBuilder<'cd, T> {
        ExprBuilder {
            cctx: self,
            op,
            opn_v,
        }
    }

    fn store_expr(self, mk_op: Op, mk_opn_v: LinearSet<Condition<'cd, T>>) -> Condition<'cd, T> {
        match mk_opn_v.len() {
            0 => self.mk_empty_op(mk_op), // avoid duplicate "true"/"false"
            1 => {
                let v: Vec<_> = mk_opn_v.into();
                v[0]
            }
            _ => {
                debug_assert!(mk_opn_v.iter().all(|&c| c.expr_op() != Some(mk_op)));
                self.store.mk_cond(Expr(mk_op, mk_opn_v))
            }
        }
    }

    /// Creates the logical negation (Not) of a condition.
    pub fn mk_not(self, cond: Condition<'cd, T>) -> Condition<'cd, T> {
        match cond.0 {
            &Var(inv, vr) => self.store.mk_cond(Var(!inv, vr)),
            &Expr(op, ref op_v) => {
                self.store_expr(op.dual(), op_v.iter().map(|&c| self.mk_not(c)).collect())
            }
        }
    }

    fn mk_empty_op(self, mk_op: Op) -> Condition<'cd, T> {
        match mk_op {
            Op::And => self.mk_true(),
            Op::Or => self.mk_false(),
        }
    }
    /// Creates a tautology (True).
    pub fn mk_true(self) -> Condition<'cd, T> {
        Condition(&self.store.true_)
    }
    /// Creates a contradiction (False).
    pub fn mk_false(self) -> Condition<'cd, T> {
        Condition(&self.store.false_)
    }
}

impl<'cd, T> Condition<'cd, T> {
    fn expr_op(self) -> Option<Op> {
        match self.0 {
            &Var(_, _) => None,
            &Expr(op, _) => Some(op),
        }
    }

    fn is_annihilator(self, for_op: Op) -> bool {
        match self.0 {
            &Var(_, _) => false,
            &Expr(op, ref opn_v) => op.dual() == for_op && opn_v.is_empty(),
        }
    }
}

struct ExprBuilder<'cd, T: 'cd> {
    cctx: Context<'cd, T>,
    op: Op,
    opn_v: LinearSet<Condition<'cd, T>>,
}

impl<'cd, T> ExprBuilder<'cd, T> {
    /// Returns `Err(())` if this expression was annihilated.
    fn insert(&mut self, cond: Condition<'cd, T>) -> Result<(), ()> {
        match ExprView::new(cond, self.op.dual()) {
            Ok(ev) => {
                if ev.is_empty() {
                    // annihilator
                    Err(())?;
                } else {
                    // nothing to simplify
                    self.insert_one(ev)?;
                }
            }
            Err(opn_v) => {
                // inline
                for &opn in opn_v {
                    let o = self.op.dual();
                    self.insert_one(ExprView::new_or_panic(opn, o))?;
                }
            }
        }

        Ok(())
    }

    /// Returns `Err(())` if this expression was annihilated.
    fn insert_one(&mut self, ev: ExprView<'cd, T>) -> Result<(), ()> {
        debug_assert!(ev.op.dual() == self.op);
        debug_assert!(!ev.is_empty());
        let p1_opn = ev.cond;

        // (examples assume self.op == Op::And)
        // try to undistribute:
        // inserting `Or{var, ..o1}` into `And{Or{var, ..o2}, ..a1}`
        // ==> `And{Or{var, ..o1}, Or{var, ..o2}, ..a1}`
        // ==> `And{Or{var, And{Or{..o1}, Or{..o2}}}, ..a1}`
        //
        // try to distribute and annihilate:
        // inserting `Or{var, ..o1}` into `And{Or{-var, ..o2}, ..a1}`
        // ==> `And{Or{var, ..o1}, Or{-var, ..o2}, ..a1}`
        // ==> `And{Or{And{var, -var}, And{var, Or{..o2}}, And{Or{..o1}, -var}, And{Or{..o1}, Or{..o2}}}, ..a1}`
        // ==> `And{Or{And{var, Or{..o2}}, And{Or{..o1}, -var}}, ..a1}`
        // which simplifies if either `o1` or `o2` are empty

        let res = ev.try_for_each_var(|p1_var, p1_inv, p1_vr, p1_found| {
            // look for a `Or{+/- var, ..o2}`
            let opt_part_2 = self
                .opn_v
                .iter()
                .filter_map(|&opn| {
                    ExprView::new_or_panic(opn, self.op.dual())
                        .find_var(p1_vr)
                        .map(|x| (opn, x))
                })
                .next();

            if let Some((p2_opn, (p2_inv, p2_found))) = opt_part_2 {
                if p1_inv == p2_inv {
                    // found `Or{var, ..o2}`

                    // TODO: inefficient
                    let _removed = self.opn_v.remove(&p2_opn);
                    debug_assert!(_removed);

                    let to_insert = if p1_found.is_alone() || p2_found.is_alone() {
                        // either `o1` or `o2` is empty:
                        // `Or{var, And{Or{..o1}, Or{..o2}}}`
                        // ==> `Or{var, False}`
                        // ==> `var`
                        p1_var
                    } else {
                        // make `Or{..o1}`, `Or{..o2}`
                        let new_part_1 = p1_found.clone_remove(self.cctx);
                        let new_part_2 = p2_found.clone_remove(self.cctx);

                        // make `Or{var, And{Or{..o1}, Or{..o2}}}`
                        self.cctx.mk_expr(
                            self.op.dual(),
                            p1_var,
                            self.cctx.mk_expr(self.op, new_part_1, new_part_2),
                        )
                    };

                    debug_assert!(to_insert.expr_op() != Some(self.op));
                    self.opn_v.insert(to_insert);

                    // inserted, so break loop
                    Err(Ok(()))
                } else {
                    // found `Or{-var, ..o2}`
                    match (p1_found.is_alone(), p2_found.is_alone()) {
                        (true, true) => {
                            // `And{Or{And{var, Or{..o2}}, And{Or{..o1}, -var}}, ..a1}`
                            // ==> `And{Or{And{var, False}, And{False, -var}}, ..a1}`
                            // ==> `And{Or{False, False}, ..a1}`
                            // ==> `And{False, ..a1}`
                            // ==> `False`

                            // whole expr is annihilated, so break loop
                            Err(Err(()))
                        }
                        (false, true) => {
                            // `And{Or{And{var, Or{..o2}}, And{Or{..o1}, -var}}, ..a1}`
                            // ==> `And{Or{And{var, False}, And{Or{..o1}, -var}}, ..a1}`
                            // ==> `And{Or{False, And{Or{..o1}, -var}}, ..a1}`
                            // ==> `And{Or{..o1}, -var, ..a1}`

                            // `Or{-var, ..o2}` == `-var`
                            debug_assert!(p2_opn.expr_op() == None);

                            // technically, we did this:
                            // self.opn_v.remove(&p2_opn);
                            // self.opn_v.insert(p2_opn);

                            // inserted, so break loop
                            let cctx = self.cctx;
                            Err(self.insert(p1_found.clone_remove(cctx)))
                        }
                        (true, false) => {
                            // `And{Or{And{var, Or{..o2}}, And{Or{..o1}, -var}}, ..a1}`
                            // ==> `And{Or{And{var, Or{..o2}}, And{False, -var}}, ..a1}`
                            // ==> `And{Or{And{var, Or{..o2}}, False}, ..a1}`
                            // ==> `And{var, Or{..o2}, ..a1}`

                            // `Or{var, ..o1}` == `var`
                            debug_assert!(p1_opn.expr_op() == None);

                            // TODO: inefficient
                            let _removed = self.opn_v.remove(&p2_opn);
                            debug_assert!(_removed);

                            self.opn_v.insert(p1_opn);

                            // inserted, so break loop
                            let cctx = self.cctx;
                            Err(self.insert(p2_found.clone_remove(cctx)))
                        }
                        (false, false) => {
                            // nothing to annihilate
                            // continue
                            Ok(())
                        }
                    }
                }
            } else {
                // continue
                Ok(())
            }
        });
        match res {
            Ok(()) => {
                // could not simplify; just insert directly
                self.opn_v.insert(p1_opn);
                Ok(())
            }
            Err(x) => x,
        }
    }

    fn finish(self) -> Condition<'cd, T> {
        self.cctx.store_expr(self.op, self.opn_v)
    }
}

struct ExprView<'cd, T: 'cd> {
    cond: Condition<'cd, T>,
    op: Op,
}
impl_copy!{ExprView}

impl<'cd, T> ExprView<'cd, T> {
    fn new(cond: Condition<'cd, T>, op: Op) -> Result<Self, &'cd LinearSet<Condition<'cd, T>>> {
        match cond.0 {
            &Var(_, _) => Ok(ExprView { op, cond }),
            &Expr(c_op, ref c_opn_v) => {
                if c_op == op {
                    Ok(ExprView { op, cond })
                } else {
                    Err(c_opn_v)
                }
            }
        }
    }

    fn new_or_panic(cond: Condition<'cd, T>, op: Op) -> Self {
        // can only `expect` when `T: Debug`
        ExprView::new(cond, op).unwrap_or_else(|_| panic!("cond could not be viewed as {:?}", op))
    }

    fn is_empty(self) -> bool {
        match self.cond.0 {
            &Var(_, _) => false,
            &Expr(_, ref opn_v) => opn_v.is_empty(),
        }
    }

    fn try_for_each_var<E, F>(self, mut func: F) -> Result<(), E>
    where
        F: FnMut(Condition<'cd, T>, Negation, VarRef<'cd, T>, FoundVar<'cd, T>) -> Result<(), E>,
    {
        match self.cond.0 {
            &Var(inv, vr) => func(self.cond, inv, vr, FoundVar(self.op, None))?,
            &Expr(_, ref opn_v) => {
                for &opn in opn_v {
                    if let &Var(opn_inv, opn_vr) = opn.0 {
                        func(opn, opn_inv, opn_vr, FoundVar(self.op, Some((opn_v, opn))))?
                    }
                }
            }
        }
        Ok(())
    }

    fn find_var(self, find_vr: VarRef<'cd, T>) -> Option<(Negation, FoundVar<'cd, T>)> {
        self.try_for_each_var(|_, inv, vr, found| {
            if vr == find_vr {
                Err((inv, found))
            } else {
                Ok(())
            }
        }).err()
    }
}

struct FoundVar<'cd, T: 'cd>(
    Op,
    Option<(&'cd LinearSet<Condition<'cd, T>>, Condition<'cd, T>)>,
);

impl<'cd, T> FoundVar<'cd, T> {
    fn is_alone(&self) -> bool {
        self.1.is_none()
    }

    fn clone_remove(self, cctx: Context<'cd, T>) -> Condition<'cd, T> {
        match self.1 {
            None => cctx.mk_empty_op(self.0),
            Some((opn_v, to_remove)) => {
                let mut new_opn_v = opn_v.clone();
                // TODO: inefficient
                let _removed = new_opn_v.remove(&to_remove);
                debug_assert!(_removed);
                cctx.store_expr(self.0, new_opn_v)
            }
        }
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

    /// Creates a [`Context`] to create conditions allocated in this [`Storage`].
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

impl<'cd, T> Default for Storage<'cd, T> {
    fn default() -> Self {
        Self::new()
    }
}

impl Op {
    fn dual(self) -> Op {
        match self {
            Op::And => Op::Or,
            Op::Or => Op::And,
        }
    }
}

impl<'cd, T> fmt::Debug for Storage<'cd, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str("<Storage>")
    }
}

impl<'cd, T: fmt::Debug> fmt::Debug for Condition<'cd, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            Var(Negation::Normal, t) => write!(f, "{:?}", t.0),
            Var(Negation::Negated, t) => write!(f, "-{:?}", t.0),
            Expr(Op::And, c) => write!(f, "And{:?}", c),
            Expr(Op::Or, c) => write!(f, "Or{:?}", c),
        }
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

impl ops::Not for Negation {
    type Output = Negation;
    fn not(self) -> Negation {
        match self {
            Negation::Normal => Negation::Negated,
            Negation::Negated => Negation::Normal,
        }
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
