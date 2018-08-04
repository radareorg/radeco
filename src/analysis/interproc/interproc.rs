//! Fills out the call summary information for `RFunction`

use std::collections::HashSet;
use analysis::interproc::transfer::InterProcAnalysis;
use frontend::radeco_containers::RadecoModule;

#[derive(Debug)]
pub struct InterProcAnalyzer<'a, T>
    where T: InterProcAnalysis
{
    analyzed: HashSet<u64>,
    rmod: &'a mut RadecoModule,
    analyzer: T,
}

pub fn analyze_module<'a, A>(ssa: &'a mut RadecoModule)
    where A: InterProcAnalysis {
    let mut ipa = InterProcAnalyzer::<'a, A>::new(ssa);
    let fs = ipa.rmod.functions.clone();
    for (_, f) in fs {
        ipa.analyze_function(f.offset);
    }
}

impl<'a, T> InterProcAnalyzer<'a, T>
    where T: InterProcAnalysis
{
    pub fn new(rmod: &'a mut RadecoModule) -> InterProcAnalyzer<'a, T> {
        InterProcAnalyzer {
            analyzed: HashSet::new(),
            rmod: rmod,
            analyzer: T::new(),
        }
    }

    fn analyze_function(&mut self, func_addr: u64) {
        // If the current function has already been analyzed, return.
        if self.analyzed.contains(&func_addr) {
            return;
        }
        // Analyze all children of the present node in call graph.
        let callees = self.rmod.function(func_addr).map(|rfn| {
            self.rmod.callees_of(rfn)
        }).unwrap_or(Vec::new());

        for (call, _) in callees {
            self.analyze_function(call);
        }

        // Propagate changes and remove deadcode based on the analysis information from
        // the children. Perform context translations from caller to callee etc.
        // TODO.
        {
            // Pull changes from callee.
            self.analyzer.propagate(self.rmod, func_addr);
            // Analyze transfer function for the current function.
            self.analyzer.transfer(self.rmod, func_addr);
        }

        // Insert the current function into analyzed set.
        self.analyzed.insert(func_addr);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use frontend::radeco_source::FileSource;
    use frontend::radeco_containers::{ProjectLoader, RadecoModule};
    use middle::ir_writer;
    use middle::dce;
    use analysis::interproc::summary;
    use std::rc::Rc;

    #[test]
    #[ignore]
    fn ipa_t1() {
        // let mut rproj = ProjectLoader::new().path("./ct1_sccp_ex.o").load();
        let mut fsource = FileSource::open("./test_files/ct1_sccp_ex/ct1_sccp_ex");
        let mut rproj = ProjectLoader::new().source(Rc::new(fsource)).load();
        for mut xy in rproj.iter_mut() {
            let mut rmod = &mut xy.module;
            {
                analyze_module::<summary::CallSummary>(&mut rmod);
            }

            for (ref addr, ref mut rfn) in rmod.functions.iter_mut() {
                {
                    dce::collect(rfn.ssa_mut());
                }
                //println!("Binds: {:?}", rfn.bindings.bindings());
                println!("Info for: {:#x}", addr);
                println!("Local Variable info: {:#?}", rfn.locals());
                println!("Arg info: {:#?}", rfn.args());
                //println!("Returns info: {:?}", rfn.returns());
                let mut il = String::new();
                ir_writer::emit_il(&mut il, Some(rfn.name.clone().to_string()), rfn.ssa()).unwrap();
                println!("{}", il);
            }
        }
    }
}
