//! API for working with conditions.
//!
//! Conditions are arena-allocated. Only freely-copyable references are exposed
//! through this API. This allows them to be manipulated without worrying about
//! ownership.

use std::fmt;
use std::ptr;
use typed_arena::Arena;

/// A condition. This can be freely copied.
/// Use [`ConditionContext`] to make one.
pub struct BaseCondition<'cd, T: 'cd>(&'cd BaseConditionS<'cd, T>);

/// *Really* a condition. These are the referents of [`BaseCondition`]s and are
/// what live in [`ConditionStorage`]s.
enum BaseConditionS<'cd, T: 'cd> {
    Simple(T),
    Not(BaseCondition<'cd, T>),
    And(Box<[BaseCondition<'cd, T>]>),
    Or(Box<[BaseCondition<'cd, T>]>),
}

/// Helper for creating new conditions.
#[derive(Copy, Clone, Debug)]
pub struct ConditionContext<'cd, T: 'cd> {
    store: &'cd ConditionStorage<'cd, T>,
    true_: BaseCondition<'cd, T>,
    false_: BaseCondition<'cd, T>,
}

/// Backing storage for [`BaseCondition`]s.
pub struct ConditionStorage<'cd, T: 'cd> {
    arena: Arena<BaseConditionS<'cd, T>>,
}

impl<'cd, T> ConditionContext<'cd, T> {
    pub fn new(store: &'cd ConditionStorage<'cd, T>) -> Self {
        let true_ = store.mk_cond(BaseConditionS::And(Vec::new().into_boxed_slice()));
        let false_ = store.mk_cond(BaseConditionS::Or(Vec::new().into_boxed_slice()));

        ConditionContext {
            store,
            true_,
            false_,
        }
    }

    pub fn mk_simple(&self, t: T) -> BaseCondition<'cd, T> {
        self.store.mk_cond(BaseConditionS::Simple(t))
    }

    pub fn mk_and(
        &self,
        l: BaseCondition<'cd, T>,
        r: BaseCondition<'cd, T>,
    ) -> BaseCondition<'cd, T> {
        let vec = match (l.0, r.0) {
            (BaseConditionS::And(lv), BaseConditionS::And(rv)) => {
                let mut vec = Vec::with_capacity(lv.len() + rv.len());
                vec.extend_from_slice(lv);
                vec.extend_from_slice(rv);
                vec
            }
            (BaseConditionS::And(lv), _) => {
                let mut vec = Vec::with_capacity(lv.len() + 1);
                vec.extend_from_slice(lv);
                vec.push(r);
                vec
            }
            (_, BaseConditionS::And(rv)) => {
                let mut vec = Vec::with_capacity(1 + rv.len());
                vec.push(l);
                vec.extend_from_slice(rv);
                vec
            }
            (_, _) => vec![l, r],
        };
        if let &[c] = vec.as_slice() {
            // TODO: is it worth making this case not allocate?
            c
        } else {
            self.store
                .mk_cond(BaseConditionS::And(vec.into_boxed_slice()))
        }
    }

    pub fn mk_or_from_iter<I>(&self, iter: I) -> BaseCondition<'cd, T>
    where
        I: IntoIterator<Item = BaseCondition<'cd, T>>,
    {
        let mut vec = Vec::new();
        for cond in iter {
            if let BaseConditionS::Or(v) = cond.0 {
                vec.extend_from_slice(v);
            } else {
                vec.push(cond);
            }
        }
        if let &[c] = vec.as_slice() {
            // TODO: is it worth making this case not allocate?
            c
        } else {
            self.store
                .mk_cond(BaseConditionS::Or(vec.into_boxed_slice()))
        }
    }

    pub fn mk_not(&self, cond: BaseCondition<'cd, T>) -> BaseCondition<'cd, T> {
        if ptr::eq(cond.0, self.true_.0) {
            self.false_
        } else if ptr::eq(cond.0, self.false_.0) {
            self.true_
        } else if let &BaseConditionS::Not(c) = cond.0 {
            c
        } else {
            self.store.mk_cond(BaseConditionS::Not(cond))
        }
    }

    pub fn mk_true(&self) -> BaseCondition<'cd, T> {
        self.true_
    }
    pub fn mk_false(&self) -> BaseCondition<'cd, T> {
        self.false_
    }
}

impl<'cd, T> ConditionStorage<'cd, T> {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
        }
    }

    fn mk_cond(&'cd self, c: BaseConditionS<'cd, T>) -> BaseCondition<'cd, T> {
        BaseCondition(self.arena.alloc(c))
    }
}

impl<'cd, T> fmt::Debug for ConditionStorage<'cd, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str("<ConditionStorage>")
    }
}

impl<'cd, T> Clone for BaseCondition<'cd, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'cd, T> Copy for BaseCondition<'cd, T> {}

impl<'cd, T: fmt::Debug> fmt::Debug for BaseCondition<'cd, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            BaseConditionS::Simple(t) => write!(f, "{:?}", t),
            BaseConditionS::Not(c) => write!(f, "-{:?}", c),
            BaseConditionS::And(c) => write!(f, "And({:?})", c),
            BaseConditionS::Or(c) => write!(f, "Or({:?})", c),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let cstore = ConditionStorage::new();
        let cctx = ConditionContext::new(&cstore);
        let ab = mk_ab(cctx);
        println!("{:?}", ab);
    }

    fn mk_ab<'cd>(cctx: ConditionContext<'cd, &'static str>) -> BaseCondition<'cd, &'static str> {
        let a = cctx.mk_simple("a");
        let b = cctx.mk_simple("b");
        let ab = cctx.mk_and(a, b);
        ab
    }
}
