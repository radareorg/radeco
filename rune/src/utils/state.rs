use std::fs::File;
use std::io::prelude::*;


use std::collections::HashMap;

use serde_json::{to_string, from_reader};
use crate::utils::utils::{Key, new_rune_ctx};


use crate::context::rune_ctx::RuneContext;

use crate::memory::seg_mem::SegMem;
use crate::regstore::regfile::RuneRegFile;

use r2pipe::r2::R2;

// TODO: Allow to convert this to a r2 project. This will be useful in the long run.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RInitialState {
    start_addr: Option<u64>,
    // end_addr: Option<u64>,
    breakpoints: Option<Vec<u64>>,
    sym_vars: Option<HashMap<Key, u64>>,
    constants: Option<HashMap<Key, (u64, u64)>>,
    env_vars: Option<HashMap<String, String>>,
}

impl RInitialState {
    pub fn new() -> RInitialState {
        Default::default()
    }

    pub fn get_string(&self) -> String {
        to_string(self).unwrap()
    }

    // TODO: Again, fix this shit.
    pub fn get_breakpoints(&self) -> Vec<u64> {
        if let Some(ref bp) = self.breakpoints {
            bp.clone()
        } else {
            Vec::new()
        }
    }

    pub fn set_start_addr(&mut self, start_addr: u64) {
        self.start_addr = Some(start_addr);
    }

    /*
    pub fn set_end_addr(&mut self, end_addr: u64) {
        self.end_addr = Some(end_addr);
    }
    */

    pub fn add_breakpoint(&mut self, bp: u64) {
        if let Some(ref mut breakpoints) = self.breakpoints {
            breakpoints.push(bp);
        }
    }

    pub fn add_const(&mut self, const_val: (Key, u64)) {
        // FIXME
        if let Some(ref mut constants) = self.constants {
            constants.insert(const_val.0, (const_val.1, 64));
        }
    }

    pub fn add_sym(&mut self, sym_val: Key) {
        // Assume when we set memory as sym, we set one byte
        if let Some(ref mut sym_vars) = self.sym_vars {
            sym_vars.insert(sym_val, 8);
        }
    }

    pub fn write_to_json(&self) {
        let mut file = File::create("state.json").unwrap();
        let s = to_string(&self).unwrap();
        let _ = file.write_all(s.as_bytes());
    }

    pub fn import_from_json<T: AsRef<str>>(path: T) -> RInitialState {
        let v = path.as_ref();
        let file = File::open(v).unwrap();
        from_reader(file).unwrap()
    }

    pub fn create_context(&self, r2: &mut R2) -> RuneContext<SegMem, RuneRegFile>
    {
        new_rune_ctx(self.start_addr, self.sym_vars.clone(), self.constants.clone(), r2)
    }
}

impl Default for RInitialState {
    fn default() -> RInitialState {
        RInitialState {
            start_addr: Some(0x0000),
            // end_addr: Some(0x8000),
            breakpoints: Some(Vec::new()),
            constants: Some(HashMap::new()),
            sym_vars: Some(HashMap::new()),
            env_vars: Some(HashMap::new()),
        }
    }
}
