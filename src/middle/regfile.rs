// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Contains the struct `SubRegisterFile` which extends `PhiPlacer`s
//! functionality by reads and writes to partial registers.

use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::From;

use r2api::structs::LRegInfo;

use middle::ssa::ssa_traits::ValueInfo;
use middle::ir;


#[derive(Clone, Copy, Debug, Default)]
pub struct SubRegister {
    pub base: u64,
    pub shift: u64,
    pub width: u64,
}

impl SubRegister {
    fn new(base: u64, shift: u64, width: u64) -> SubRegister {
        SubRegister {
            base: base,
            shift: shift,
            width: width,
        }
    }
}

/// A structure containing information about whole and partial registers of a platform.
/// Upon creation it builds a vector of `ValueType`s representing whole registers
/// to be added to a `PhiPlacer`.
///
/// It can then translate accesses to partial registers to accesses of whole registers.
/// Shifts and masks are added automatically.
#[derive(Clone, Debug, Default)]
pub struct SubRegisterFile {
    /// `ValueType`s of whole registers ready to be added to a `PhiPlacer`.
    /// The index within `PhiPlacer` to the first register is needed
    /// as argument `base` to `read_register` and `write_register` later.
    pub whole_registers: Vec<ValueInfo>,
    /// Contains the respective names for the registers described in `whole_registers`
    pub whole_names: Vec<String>,
    pub named_registers: HashMap<String, SubRegister>,
    /// Contains the alias information for some registers.
    pub alias_info: HashMap<String, String>,
    /// Contains the type information for every registers.
    pub type_info: HashMap<String, String>,
}

impl SubRegisterFile {
    /// Creates a new SubRegisterFile based on a provided register profile.
    pub fn new(reg_info: &LRegInfo) -> SubRegisterFile {
        let mut aliases: HashMap<String, String> = HashMap::new();
        for reg in &reg_info.alias_info {
            aliases.insert(reg.role_str.clone(), reg.reg.clone());
        }

        let mut slices = HashMap::new();
        let mut events: Vec<SubRegister> = Vec::new();
        let mut types: HashMap<String, String> = HashMap::new();
        for (i, reg) in reg_info.reg_info.iter().enumerate() {
            types.insert(reg.name.clone(), reg.type_str.clone());
            if reg.type_str == "fpu" {
                continue;
            } // st7 from "fpu" overlaps with zf from "gpr" (r2 bug?)
            if reg.name.ends_with("flags") {
                continue;
            } // HARDCODED x86
            events.push(SubRegister::new(i as u64, reg.offset as u64, reg.size as u64));
        }

        events.sort_by(|a, b| {
            let o = a.shift.cmp(&b.shift);
            if let Ordering::Equal = o {
                (b.width + b.shift).cmp(&(a.width + a.shift))
            } else {
                o
            }
        });

        let mut current = SubRegister::new(0, 0, 0);
        let mut whole: Vec<ValueInfo> = Vec::new();
        let mut names: Vec<String> = Vec::new();
        for &ev in &events {
            let name = &reg_info.reg_info[ev.base as usize].name;
            let cur_until = current.shift + current.width;
            if ev.shift >= cur_until {
                current = ev;

                radeco_trace!("regfile_mappings|{} -> {}", whole.len(), &name);

                whole.push(ValueInfo::new_unresolved(ir::WidthSpec::from(current.width as u16)));
                names.push(name.clone());
            } else {
                let ev_until = ev.width + ev.shift;
                assert!(ev_until <= cur_until);
            }

            let subreg = SubRegister::new(whole.len() as u64 - 1, ev.shift - current.shift, ev.width);

            slices.insert(name.clone(), subreg);
        }

        SubRegisterFile {
            whole_registers: whole,
            named_registers: slices,
            whole_names: names,
            alias_info: aliases,
            type_info: types,
        }
    }

    // API for Sub Reigster.
    pub fn get_subregister(&self, name: &str) -> Option<SubRegister> {
        self.named_registers.get(name).cloned()
    }

    
    // API for whole register.
    
    // Get information by id.
    pub fn get_name(&self, id: usize) -> Option<String> {
        Some(self.whole_names[id].clone())
    }

    pub fn get_width(&self, id: usize) -> Option<u64> {
        if let Some(name) = self.get_name(id) {
            if let Some(subreg) = self.named_registers.get(&name) {
                Some(subreg.width)
            } else {
                None
            }
        } else {
            None
        }
    }

    // Get information by other way. 
    pub fn get_name_by_alias(&self, alias: &String) -> Option<String> {
        for id in 0..self.whole_names.len() {
            if let Some(name) = self.get_name(id) {
                if self.alias_info.get(&name) == Some(alias) {
                    return Some(name);
                }
            }
        }
        None
    }

}
