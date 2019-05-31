// Copyright (c) 2015, The Radare Project. All rights reserved.
// See the COPYING file at the top-level directory of this distribution.
// Licensed under the BSD 3-Clause License:
// <http://opensource.org/licenses/BSD-3-Clause>
// This file may not be copied, modified, or distributed
// except according to those terms.

//! Contains the struct [`SubRegisterFile`] which extends `PhiPlacer`s
//! functionality by reads and writes to partial registers.
//! Also contains [`RegisterUsage`], and [`RegisterMap`].

mod regmap;
mod regusage;

pub use self::regmap::RegisterMap;
pub use self::regusage::RegisterUsage;

use crate::middle::ir;

use crate::middle::ssa::ssa_traits::ValueInfo;

use r2api::structs::{LCCInfo, LRegInfo};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::From;

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

pub struct RegisterIter(Box<dyn Iterator<Item = (usize, String)>>);

impl<'a> IntoIterator for &'a SubRegisterFile {
    type Item = (usize, String);
    type IntoIter = RegisterIter;
    fn into_iter(self) -> RegisterIter {
        // XXX: Avoid clones!
        RegisterIter(Box::new(self.whole_names.clone().into_iter().enumerate()))
    }
}

impl Iterator for RegisterIter {
    type Item = (usize, String);
    fn next(&mut self) -> Option<(usize, String)> {
        self.0.next()
    }
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
            events.push(SubRegister::new(
                i as u64,
                reg.offset as u64,
                reg.size as u64,
            ));
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

                whole.push(ValueInfo::new_unresolved(ir::WidthSpec::from(
                    current.width as u16,
                )));
                names.push(name.clone());
            } else {
                let ev_until = ev.width + ev.shift;
                assert!(ev_until <= cur_until);
            }

            let subreg =
                SubRegister::new(whole.len() as u64 - 1, ev.shift - current.shift, ev.width);

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

    // Get id for a register named `reg`
    pub fn register_id_by_name(&self, reg: &str) -> Option<RegisterId> {
        self.named_registers
            .get(reg)
            .map(|sr| RegisterId::from_usize(sr.base as usize))
    }

    // Get id for register by alias/role
    pub fn register_id_by_alias(&self, alias: &str) -> Option<RegisterId> {
        if let Some(rname) = self.alias_info.get(alias) {
            self.register_id_by_name(rname)
        } else {
            None
        }
    }

    // API for whole register.

    // Get information by id.
    pub fn get_name(&self, id: RegisterId) -> Option<&str> {
        self.whole_names.get(id.to_usize()).map(|s| &**s)
    }

    pub fn get_width(&self, id: RegisterId) -> Option<u64> {
        if let Some(name) = self.get_name(id) {
            if let Some(subreg) = self.named_registers.get(name) {
                Some(subreg.width)
            } else {
                None
            }
        } else {
            None
        }
    }

    // Get information by other way.
    pub fn get_name_by_alias(&self, alias: &String) -> Option<&str> {
        for id in 0..self.whole_names.len() {
            if let Some(name) = self.get_name(RegisterId::from_usize(id)) {
                if self.alias_info.get(name) == Some(alias) {
                    return Some(name);
                }
            }
        }
        None
    }

    pub fn iter_args(&self) -> RegisterIter {
        let args = &["A0", "A1", "A2", "A3", "A4", "A5"];
        let whiter: HashMap<String, usize> = self
            .whole_names
            .iter()
            .enumerate()
            .map(|(i, x)| (x.clone(), i))
            .collect();
        // XXX: Avoid clones!
        RegisterIter(Box::new(self.alias_info.clone().into_iter().filter_map(
            move |r| {
                if args.contains(&r.0.as_str()) {
                    Some((whiter[&r.1], r.1))
                } else {
                    None
                }
            },
        )))
    }

    pub fn iter_register_ids(&self) -> impl Iterator<Item = RegisterId> {
        (0..=self.whole_registers.len()).map(RegisterId::from_usize)
    }

    pub fn mem_id(&self) -> RegisterId {
        RegisterId::from_usize(self.whole_registers.len())
    }

    /// Creates a new mutable `RegisterUsage`.
    /// The returned value says "reads and clobbers everything".
    pub fn new_register_usage(&self) -> RegisterUsage {
        RegisterUsage::with_register_count(self.whole_registers.len() + 1)
    }

    /// Converts the given `LCCInfo` imported from r2 to a `RegisterUsage`.
    /// Returns `None` if the `LCCInfo` contains a register name we don't
    /// recognize.
    pub fn r2callconv_to_register_usage(
        &self,
        callconv: &LCCInfo,
        callconv_name: &str,
    ) -> Option<RegisterUsage> {
        let mut ret = self.new_register_usage();

        ret.set_all_ignored();
        for regname in callconv.args.as_ref()? {
            // bail if we don't recognize the register name
            let reg_id = self.register_id_by_name(regname)?;
            ret.set_read(reg_id);
        }
        // memory is always read
        ret.set_read(self.mem_id());

        for regname in callconv_name_to_preserved_list(callconv_name) {
            let reg_id = self
                .register_id_by_name(regname)
                .expect("unknown register in internal preserved list");
            ret.set_preserved(reg_id);
        }

        Some(ret)
    }

    /// Creates an empty `RegisterMap`.
    pub fn new_register_map<V>(&self) -> RegisterMap<V> {
        RegisterMap::with_register_count(self.whole_registers.len() + 1)
    }
}

/// Opaque identifier for a whole register in [`SubRegisterFile`]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct RegisterId(u8);

#[cfg_attr(rustfmt, rustfmt_skip)]
impl RegisterId {
    // TODO: make these private everything uses the new RegisterId API
    pub fn to_u8(&self) -> u8 { self.0 }
    pub fn from_u8(i: u8) -> Self { RegisterId(i) }
    pub fn to_usize(&self) -> usize { self.0 as usize }
    pub fn from_usize(i: usize) -> Self {
        assert!(i <= u8::max_value() as usize);
        RegisterId(i as u8)
    }
}

// TODO: if r2 ever starts keeping track of preserved registers, use that instead of this
/// For a given named calling convention, return the set of registers it
/// preserves across calls (are callee-saved).
///
/// This should only be used for imported functions that we can't analyze to
/// find a more specific calling convention.
#[cfg_attr(rustfmt, rustfmt_skip)]
fn callconv_name_to_preserved_list(cc_name: &str) -> &'static [&'static str] {
    // see https://github.com/radare/radare2/tree/master/libr/anal/d
    // for what `cc_name` can be
    match cc_name {
        // --- x86[_64] ---
        // standard for SysV-compatible systems (most modern Unixes)
        // https://github.com/hjl-tools/x86-psABI/wiki/X86-psABI
        "amd64" => &["rbx", "rsp", "rbp", "r12", "r13", "r14", "r15"],
        "cdecl" => &["ebx", "esp", "ebp", "esi", "edi"],

        // standard for Windows
        // https://en.wikipedia.org/wiki/X86_calling_conventions and https://llvm.org/viewvc/llvm-project/llvm/trunk/lib/Target/X86/X86CallingConv.td?view=markup#l1047
        "ms" => &["rbx", "rsp", "rbp", "rsi", "rdi", "r12", "r13", "r14", "r15"],
        "stdcall" => &["ebx", "ebp", "esi", "edi"],

        // --- ARM ---
        // https://developer.arm.com/docs/ihi0042/latest
        "arm32" => &["r4", "r5", "r6", "r7", "r8", "r10", "r11", "sp"],
        // https://developer.arm.com/docs/ihi0055/latest
        "arm64" => &["x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28", "fp", "sp"],

        // if we don't recognize `cc_name`, assume all registers are clobbered
        _ => &[],
    }
}
