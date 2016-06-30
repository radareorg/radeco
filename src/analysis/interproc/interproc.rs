//! Fills out the call summary information for `RFunction`

use std::collections::HashSet;
use std::marker::PhantomData;
use analysis::interproc::transfer::{InterProcAnalysis};
use frontend::containers::{RFunction, RModule};

#[derive(Debug)]
pub struct InterProcAnalyzer<'a, M, T>
    where M: 'a + RModule,
          T: InterProcAnalysis<M>,
{
    analyzed: HashSet<M::FnRef>,
    rmod: &'a mut M,
    analyzer: T,
}

impl<'a, M, T> InterProcAnalyzer<'a, M, T>
    where M: 'a + RModule,
          T: InterProcAnalysis<M>,
{
    pub fn new(rmod: &'a mut M) -> InterProcAnalyzer<'a, M, T> {
        InterProcAnalyzer {
            analyzed: HashSet::new(),
            rmod: rmod,
            analyzer: T::new(),
        }
    }

    pub fn analyze_module<Y: 'a + RModule, X: InterProcAnalysis<Y>>(ssa: &'a mut Y) {
        let mut ipa = InterProcAnalyzer::<Y, X>::new(ssa);
        for f in &ipa.rmod.functions() {
            ipa.analyze_function(f)
        }
    }

    fn analyze_function(&mut self, rfn: &M::FnRef) {
        // If the current function has already been analyzed, return.
        if self.analyzed.contains(rfn) {
            return;
        }

        // Analyze all children of the present node in call graph.
        let callees = self.rmod.callees_of(rfn);
        for call in &callees {
            self.analyze_function(call);
        }

        // Propagate changes and remove deadcode based on the analysis information from
        // the children. Perform context translations from caller to callee etc.
        // TODO.
        {
            // Pull changes from callee.
            self.analyzer.propagate(self.rmod, rfn);
            // Analyze transfer function for the current function.
            self.analyzer.transfer(self.rmod, rfn);
        }

        // Insert the current function into analyzed set.
        self.analyzed.insert(*rfn);
    }
}
