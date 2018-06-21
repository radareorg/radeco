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
    Expr(Op, LinearSet<Condition<'cd, T>>),
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
enum Op {
    And,
    Or,
}

/// Wrapper for comparing references and hashing by pointer
#[derive(Debug)]
struct VarRef<'cd, T: 'cd>(&'cd T);

/// Helper for creating new conditions.
/// Use [`Storage::cctx`] to make one.
#[derive(Copy, Clone, Debug)]
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
        let set: LinearSet<_> = match (l.0, r.0) {
            // check for annihilators
            (&Expr(op, ref op_v), _) | (_, &Expr(op, ref op_v))
                if op.dual() == mk_op && op_v.is_empty() =>
            {
                return self.mk_empty_op(op);
            }
            // inline expressions with the same op
            (&Expr(lop, ref lop_v), &Expr(rop, ref rop_v)) if lop == mk_op && rop == mk_op => {
                lop_v | rop_v
            }
            (&Expr(op, ref op_v), c) | (c, &Expr(op, ref op_v)) if op == mk_op => {
                let mut set = op_v.clone();
                set.insert(Condition(&c));
                set
            }
            // nothing to simplify
            (_, _) => [l, r].iter().cloned().collect(),
        };

        self.store_expr(mk_op, set)
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
        let mut set = LinearSet::new();
        for cond in iter {
            match cond.0 {
                // check for annihilators
                &Expr(op, ref op_v) if op.dual() == mk_op && op_v.is_empty() => {
                    return self.mk_empty_op(op);
                }
                // inline expressions with the same op
                &Expr(op, ref op_v) if op == mk_op => {
                    set.extend(op_v);
                }
                // nothing to simplify
                _ => {
                    set.insert(cond);
                }
            }
        }

        self.store_expr(mk_op, set)
    }

    fn store_expr(&self, op: Op, set: LinearSet<Condition<'cd, T>>) -> Condition<'cd, T> {
        match set.len() {
            0 => self.mk_empty_op(op), // avoid duplicate "true"/"false"
            1 => {
                let v: Vec<_> = set.into();
                v[0]
            }
            _ => self.store.mk_cond(Expr(op, set)),
        }
    }

    pub fn mk_not(&self, cond: Condition<'cd, T>) -> Condition<'cd, T> {
        match cond.0 {
            &Var(inv, vr) => self.store.mk_cond(Var(!inv, vr)),
            &Expr(op, ref op_v) => self.store_expr(
                op.dual(),
                op_v.iter().map(|&c| self.mk_not(c)).collect(),
            ),
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

impl<'cd, T> fmt::Debug for Storage<'cd, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str("<Storage>")
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
            Var(false, t) => write!(f, "{:?}", t),
            Var(true, t) => write!(f, "-{:?}", t),
            Expr(Op::And, c) => write!(f, "And({:?})", c),
            Expr(Op::Or, c) => write!(f, "Or({:?})", c),
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
