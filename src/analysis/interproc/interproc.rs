//! Fills out the call summary information for `RFunction`

use analysis::analyzer::{Analyzer, AnalyzerKind, AnalyzerResult, ModuleAnalyzer};
use analysis::interproc::transfer::InterProcAnalysis;
use frontend::radeco_containers::RadecoModule;
use std::collections::HashSet;
use std::fmt::Debug;

#[derive(Debug)]
pub struct InterProcAnalyzer<T>
where
    T: InterProcAnalysis,
{
    analyzed: HashSet<u64>,
    analyzer: T,
}

impl<T> InterProcAnalyzer<T>
where
    T: InterProcAnalysis + Debug,
{
    pub fn new() -> InterProcAnalyzer<T> {
        InterProcAnalyzer {
            analyzed: HashSet::new(),
            analyzer: T::new(),
        }
    }

    fn analyze_function(&mut self, rmod: &mut RadecoModule, func_addr: u64) {
        // If the current function has already been analyzed, return.
        if self.analyzed.contains(&func_addr) {
            return;
        }
        // Analyze all children of the present node in call graph.
        let callees = rmod
            .function(func_addr)
            .map(|rfn| rmod.callees_of(rfn))
            .unwrap_or(Vec::new());

        for (call, _) in callees {
            self.analyze_function(rmod, call);
        }

        // Propagate changes and remove deadcode based on the analysis information from
        // the children. Perform context translations from caller to callee etc.
        // TODO.
        {
            // Pull changes from callee.
            self.analyzer.propagate(rmod, func_addr);
            // Analyze transfer function for the current function.
            self.analyzer.transfer(rmod, func_addr);
        }

        // Insert the current function into analyzed set.
        self.analyzed.insert(func_addr);
    }
}

impl<T: 'static> Analyzer for InterProcAnalyzer<T>
where
    T: InterProcAnalysis + Debug,
{
    fn name(&self) -> String {
        "interproc".to_owned()
    }

    fn kind(&self) -> AnalyzerKind {
        AnalyzerKind::InterProc
    }

    fn requires(&self) -> Vec<AnalyzerKind> {
        Vec::new()
    }
}

impl<T: 'static> ModuleAnalyzer for InterProcAnalyzer<T>
where
    T: InterProcAnalysis + Debug,
{
    fn analyze(&mut self, rmod: &mut RadecoModule) -> Option<Box<AnalyzerResult>> {
        let fs = rmod.functions.clone();
        for (_, f) in fs {
            self.analyze_function(rmod, f.offset);
        }

        None
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use analysis::interproc::summary;
    use frontend::radeco_containers::ProjectLoader;
    use frontend::radeco_source::FileSource;
    use middle::dce;
    use middle::ir_writer;
    use std::rc::Rc;

    #[test]
    #[ignore]
    fn ipa_t1() {
        // let mut rproj = ProjectLoader::new().path("./ct1_sccp_ex.o").load();
        let fsource = FileSource::open("./test_files/ct1_sccp_ex/ct1_sccp_ex");
        let mut rproj = ProjectLoader::new().source(Rc::new(fsource)).load();
        for mut xy in rproj.iter_mut() {
            let mut rmod = &mut xy.module;
            {
                let mut analyzer: InterProcAnalyzer<summary::CallSummary> = InterProcAnalyzer::new();
                analyzer.analyze(&mut rmod);
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
