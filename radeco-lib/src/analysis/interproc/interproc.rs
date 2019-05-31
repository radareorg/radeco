//! Fills out the call summary information for `RFunction`

use crate::analysis::analyzer::{
    Action, Analyzer, AnalyzerInfo, AnalyzerKind, AnalyzerResult, Change, ModuleAnalyzer,
};
use crate::analysis::interproc::transfer::InterProcAnalysis;
use crate::frontend::radeco_containers::RadecoModule;

use std::any::Any;
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

const NAME: &str = "interproc";
const REQUIRES: &[AnalyzerKind] = &[];

pub const INFO: AnalyzerInfo = AnalyzerInfo {
    name: NAME,
    kind: AnalyzerKind::InterProc,
    requires: REQUIRES,
    uses_policy: false,
};

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
    fn info(&self) -> &'static AnalyzerInfo {
        &INFO
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl<T: 'static> ModuleAnalyzer for InterProcAnalyzer<T>
where
    T: InterProcAnalysis + Debug,
{
    fn analyze<F: FnMut(Box<dyn Change>) -> Action>(
        &mut self,
        rmod: &mut RadecoModule,
        _policy: Option<F>,
    ) -> Option<Box<dyn AnalyzerResult>> {
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
    use crate::analysis::analyzer::{all, FuncAnalyzer};
    use crate::analysis::dce::DCE;
    use crate::analysis::interproc::summary;
    use crate::frontend::radeco_containers::ProjectLoader;
    use crate::frontend::radeco_source::FileSource;
    use crate::middle::ir_writer;
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
                let mut analyzer: InterProcAnalyzer<summary::CallSummary> =
                    InterProcAnalyzer::new();
                analyzer.analyze(&mut rmod, Some(all));
            }

            for (ref addr, ref mut rfn) in rmod.functions.iter_mut() {
                let mut dce = DCE::new();
                dce.analyze(rfn, Some(all));

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
