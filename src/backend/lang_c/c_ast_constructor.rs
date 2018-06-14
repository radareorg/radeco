//! Pass to transform the SSA created into the C-AST.
//!
//!

use std::fmt::Write;
use std::collections::HashMap;

use frontend::radeco_containers::{RadecoModule, RadecoFunction};
use backend::lang_c::c_simple::{CAST, Ty, BTy};

#[derive(Clone, Debug, Default)] pub struct CWriter {
    c_ast: HashMap<u64, CAST>,
}

impl CWriter {
    pub fn new() -> CWriter {
        CWriter {
            c_ast: HashMap::new(),
        }
    }

    // Converts argument information inside RFunction into arguments for the C Function.
    fn add_function_arguments(&self, rfn: &RadecoFunction, cast: &mut CAST) {
        let mut args = Vec::new();
        for (i, &_) in rfn.args().iter().enumerate() {
            // TODO: Add real type
            args.push((Ty::new(BTy::Int, true, 0), format!("arg_{}", i)));
        }
        cast.function_args(args.as_ref());
    }

    // Converts local variables information inside RFunction to C variable declarations.
    fn add_locals(&self, rfn: &RadecoFunction, cast: &mut CAST) {
        let mut locals = Vec::new();
        for (i, &_) in rfn.locals().iter().enumerate() {
            locals.push(format!("locals_{}", i));
        }
        // TODO: Add real type
        if !locals.is_empty() {
            cast.declare_vars(Ty::new(BTy::Int, false, 0), locals.as_ref(), false);
        }
    }

    /// Converts a RFunction to CAST and stores it internally.
    /// This can later be retrieved by using the address the key.
    pub fn rfn_to_c_ast(&mut self, rfn: &RadecoFunction, key: u64) {
        let mut ast = CAST::new(&rfn.name);
        self.add_function_arguments(rfn, &mut ast);
        self.add_locals(rfn, &mut ast);
        self.c_ast.insert(key, ast);
    }

    /// Converts all the functions inside the current RModule to CAST and
    /// stores it internally. This can later be emitted.
    pub fn rmod_to_c_ast(&mut self, rmod: &RadecoModule) {
        for (_, ref rfn) in rmod.functions.iter() {
            self.rfn_to_c_ast(rfn, rfn.offset);
        }
    }

    /// Emit C code for a particular function.
    pub fn emit_fn<T>(&self, fn_idx: u64, w: &mut T) where T: Write {
        write!(w, "{}\n", self.c_ast[&fn_idx].print());
    }

    /// Emit C code for all the functions that the current C-Emitter contains.
    pub fn emit<T>(&self, w: &mut T) where T: Write {
        for c in self.c_ast.values() {
            write!(w, "{}\n", c.print());
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use middle::dce;
    use frontend::radeco_source::FileSource;
    use frontend::radeco_containers::{ProjectLoader, RadecoModule};
    use analysis::interproc::interproc::analyze_module;
    use analysis::interproc::summary;
    use r2pipe::r2::R2;
    use r2api::api_trait::R2Api;
    use std::rc::Rc;

    #[test]
    #[ignore]
    // Incomplete testcase
    fn test_emit_c() {
        // let mut fsource = FileSource::open(Some("./test_files/ct1_sccp_ex/ct1_sccp_ex"));
        // let mut rproj = ProjectLoader::new().source(Rc::new(fsource)).load();
        let mut rproj = ProjectLoader::new().path("./ct1_sccp_ex.o").load();
        for mut xy in rproj.iter_mut() {
            let mut rmod = &mut xy.module;
            // let mut rmod = RadecoModule::from(&mut fsource);
            for (ref addr, ref mut rfn) in rmod.functions.iter_mut() {
                dce::collect(rfn.ssa_mut());
            }

            {
                analyze_module::<summary::CallSummary>(&mut rmod);
            }

            let mut writer = CWriter::new();
            writer.rmod_to_c_ast(&rmod);
            let mut res = String::new();
            writer.emit(&mut res);
            println!("{}", res);
        }
    }
}
