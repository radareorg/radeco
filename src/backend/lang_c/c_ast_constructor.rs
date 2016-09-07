//! Pass to transform the SSA created into the C-AST.
//!
//!

use std::io::Write;
use std::collections::HashMap;

use frontend::containers::{RModule, RFunction};
use backend::lang_c::c_simple::CAST;

#[derive(Clone, Debug, Default)]
pub struct CWriter {
    c_ast: HashMap<u64, CAST>,
}

impl CWriter {
    pub fn new() -> CWriter {
        CWriter {
            c_ast: HashMap::new(),
        }
    }

    /// Converts a RFunction to CAST and stores it internally.
    /// This can later be retrieved by using the address the key.
    pub fn rfn_to_c_ast<F: RFunction>(&mut self, rfn: &F, key: u64) {
        let mut ast = CAST::new(&rfn.fn_name());
        // Insert the newly constructed AST into the HashMap for later use.
        self.c_ast.insert(key, ast);
    }

    /// Converts all the functions inside the current RModule to CAST and
    /// stores it internally. This can later be emitted.
    pub fn rmod_to_c_ast<'a, M: RModule<'a>>(&mut self, rmod: &M) {
        for rfn_idx in rmod.functions().iter() {
            if let Some(rfn) = rmod.function_by_ref(rfn_idx) {
                let fn_ref: u64 = (*rfn_idx).into();
                self.rfn_to_c_ast(rfn, fn_ref);
            }
        }
    }

    /// Emit C code for a particular function.
    pub fn emit_fn<T>(&self, fn_idx: u64, w: &mut T) where T: Write {
        unimplemented!()
    }

    /// Emit C code for all the functions that the current C-Emitter contains.
    pub fn emit<T>(&self, w: &mut T) where T: Write {
        unimplemented!()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_emit_c() {
    }
}
