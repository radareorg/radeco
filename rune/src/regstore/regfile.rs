use petgraph::graph::NodeIndex;
use std::collections::HashMap;

use crate::regstore::regstore::{RegStore, RegStoreAPI, RegEntry};

use r2api::structs::LRegInfo;
use libsmt::backends::smtlib2::{SMTLib2};
use libsmt::logics::qf_abv;
use libsmt::theories::bitvec;
use libsmt::backends::backend::SMTBackend;

#[derive(Clone, Debug, Default)]
pub struct RuneRegFile {
    current_regs: Vec<Option<NodeIndex>>,
    regfile: HashMap<String, RegEntry>,
    alias_info: HashMap<String, String>,
}

impl RegStore for RuneRegFile {
    type VarRef = NodeIndex;

    fn new(reginfo: &mut LRegInfo) -> RuneRegFile {
        let mut cur_regs = Vec::new();
        let mut regfile = HashMap::new();
        let mut seen_offsets = Vec::new();
        let mut alias_info = HashMap::new();
        reginfo.reg_info.sort_by(|x, y| (y.offset + y.size).cmp(&(x.offset + x.size)));
        for register in &reginfo.reg_info {
            let (idx, s_bit, e_bit, is_whole) = if !seen_offsets.contains(&register.offset) &&
                                                   (register.type_str == "gpr" || register.type_str == "flg") {
                cur_regs.push(None);
                seen_offsets.push(register.offset);
                (cur_regs.len() - 1, 0, register.size - 1, true)
            } else {
                let mut found = 0;
                for (i, offset) in seen_offsets.iter().enumerate() {
                    if register.offset == *offset {
                        found = i;
                        break;
                    }
                }
                (found, 0, register.size - 1, false)
            };

            regfile.insert(register.name.clone(),
                           RegEntry::new(register.name.clone(), idx, s_bit, e_bit, is_whole, None));
        }

        for alias in &reginfo.alias_info {
            alias_info.insert(alias.role_str.clone(), alias.reg.clone());
            // Add this alias info in the corresponding RegEntry too.
            if let Some(info) = regfile.get_mut(&alias.reg) {
                info.alias = Some(alias.role_str.clone());
            }
        }

        RuneRegFile {
            current_regs: cur_regs,
            regfile: regfile,
            alias_info: alias_info,
        }
    }

    fn read(&mut self, reg_name: &str, solver: &mut SMTLib2<qf_abv::QF_ABV>) -> NodeIndex {
        let rentry = &self.regfile.get(reg_name).expect("Unknown Register");
        let idx = self.current_regs[rentry.idx].expect("Unset register - Undefined Behavior. \
                                                        Consider setting an initial value before use!");
        if rentry.is_whole {
            idx
        } else {
            solver.assert(bitvec::OpCodes::Extract((rentry.end_bit) as u64, 0), &[idx])
        }
    }

    // TODO: This is not totally correct as the sizes of registers may not match.
    fn write(&mut self, dest: &str, source: NodeIndex) -> Option<NodeIndex> {
        let rentry = &self.regfile[dest];
        let e_old = self.current_regs[rentry.idx];
        self.current_regs[rentry.idx] = Some(source);
        e_old
    }

    fn get_reg_entry(&self, r_string: &str) -> RegEntry {
        self.regfile[r_string].clone()
    }

    fn get_reg_ref(&self, r_string: &str) -> Option<NodeIndex> {
        let rentry = &self.regfile[r_string];
        self.current_regs[rentry.idx]
    }

    fn set_reg(&mut self, r_string: &str, cval: NodeIndex) {
        let idx = self.regfile[r_string].idx;
        self.current_regs[idx] = Some(cval);
    }
 
}

impl RegStoreAPI for RuneRegFile {
    fn get_regs(&self) -> Vec<Option<NodeIndex>> {
        self.current_regs.clone()
    }
}
