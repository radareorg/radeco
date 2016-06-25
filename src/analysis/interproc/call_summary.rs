//! Fills out the call summary information for `RFunction`

use std::collections::HashSet;
use std::marker::PhantomData;
use analysis::interproc::transfer::{Propagate, Transfer};
use frontend::containers::{RFunction, RModule};

#[derive(Clone, Debug)]
pub struct InterProc<'a, M, T>
    where M: 'a + RModule,
          T: Transfer<M> + Propagate<M>
{
    summarized: HashSet<M::FnRef>,
    rmod: &'a mut M,
    foo: PhantomData<T>,
}

impl<'a, M, T> InterProc<'a, M, T>
    where M: 'a + RModule,
          T: Transfer<M> + Propagate<M>
{
    pub fn new(rmod: &mut M) -> InterProc<'a, M, T> {
        InterProc {
            summarized: HashSet::new(),
            rmod: rmod,
            foo: PhantomData,
        }
    }

    pub fn summarize_module(&mut self) {
        for f in &self.rmod.functions() {
            self.summarize_function(f)
        }
    }

    fn summarize_function(&mut self, rfn: &M::FnRef) {
        // If the current function has already been summarized, return.
        if self.summarized.contains(rfn) {
            return;
        }

        // Analyze all children of the present node in call graph.
        let callees = self.rmod.callees_of(rfn);
        for call in &callees {
            self.summarize_function(call);
        }

        // Propagate changes and remove deadcode based on the analysis information from
        // the
        // children. Perform context translations from caller to callee etc.
        // TODO.

        {
            let fun = self.rmod.get_fn_mut(rfn);
            CallSummarizer::generate_summary(fun);
        }

        // Insert the current function into summarized set.
        self.summarized.insert(*rfn);
    }
}
