//! Fills out the call summary information for `RFunction`

use std::collections::HashSet;
use analysis::interproc::transfer::InterProcAnalysis;
use frontend::containers::RModule;

#[derive(Debug)]
pub struct InterProcAnalyzer<'a, 'b, M, T>
    where M: 'a + RModule<'b>,
          T: InterProcAnalysis<'b, M>
{
    analyzed: HashSet<M::FnRef>,
    rmod: &'a mut M,
    analyzer: T,
}

pub fn analyze_module<'a, 'b, M, A>(ssa: &'a mut M) 
    where M: RModule<'b>,
          A: InterProcAnalysis<'b, M> {
    let mut ipa = InterProcAnalyzer::<M, A>::new(ssa);
    for f in &ipa.rmod.functions() {
        ipa.analyze_function(f);
    }
}

impl<'a,'b, M, T> InterProcAnalyzer<'a, 'b, M, T>
    where M: RModule<'b>,
          T: InterProcAnalysis<'b, M>,
{
    pub fn new(rmod: &'a mut M) -> InterProcAnalyzer<'a, 'b, M, T> {
        InterProcAnalyzer {
            analyzed: HashSet::new(),
            rmod: rmod,
            analyzer: T::new(),
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

#[cfg(test)]
mod test {
    use super::*;
    use frontend::source::FileSource;
    // use frontend::source::Source;
    use frontend::containers::*;
    use middle::ir_writer::IRWriter;
    use middle::dce;
    use analysis::interproc::summary;
    // use r2pipe::r2::R2;

    #[test]
    fn ipa_t1() {
        //let mut r2 = R2::new(Some("./ct1_sccp_ex.o")).expect("Failed to open r2");
        //r2.init();
        //let mut fsource = FileSource::from(r2);
        let mut fsource = FileSource::open(Some("./test_files/ct1_sccp_ex/ct1_sccp_ex"));
        let mut rmod = RadecoModule::from(&mut fsource);
        {
            analyze_module::<_, summary::CallSummary>(&mut rmod);
        }

        for (ref addr, ref mut rfn) in rmod.functions.iter_mut() {
            {
                dce::collect(&mut rfn.ssa);
            }
            //println!("Binds: {:?}", rfn.bindings.bindings());
            println!("Info for: {:#x}", addr);
            println!("Local Variable info: {:#?}", rfn.locals());
            println!("Arg info: {:#?}", rfn.args());
            //println!("Returns info: {:?}", rfn.returns());
            let mut writer: IRWriter = Default::default();
            println!("{}", writer.emit_il(Some(rfn.name.clone()), &rfn.ssa));
        }
    }
}
