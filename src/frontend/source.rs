//! Defines the `Source` Trait.

// Module TODO:
//  - Implement cache source
//  - Implement source for r2
//  - Implement source to read from a file
//  - Implement conversion trait `From` for r2 -> file source.

use std::collections::{HashMap, BTreeMap};
use r2pipe::structs::{FunctionInfo, LFlagInfo, LOpInfo, LRegProfile, LSectionInfo, LStringInfo};

pub trait Source: Sized {
    fn open(Option<&str>) -> Result<Self, String>;
    fn functions(&mut self) -> Vec<FunctionInfo>;
    fn instructions_at(&mut self, u64) -> Vec<LOpInfo>;
    fn register_profile(&mut self) -> LRegProfile;
    fn flags(&mut self) -> Vec<LFlagInfo>;
    fn section_map(&mut self) -> Vec<LSectionInfo>;
    fn strings(&mut self) -> Vec<LStringInfo>;

    // Non essential / functions with default implementation.

    fn function_at(&mut self, address: u64) -> Option<FunctionInfo> {
        for f in self.functions() {
            match f.offset {
                Some(off) if address == off => return Some(f),
                _ => {},
            }
        }
        None
    }

    fn function_named(&mut self, fn_name: &str) -> Option<FunctionInfo> {
        for f in self.functions() {
            match f.name {
                Some(ref name) if name == fn_name => return Some(f.clone()),
                _ => {},
            }
        }
        None
    }

    fn instructions_at_fn(&mut self, fn_name: &str) -> Option<Vec<LOpInfo>> {
        if let Some(fn_) = self.function_named(fn_name) {
            if let Some(offset) = fn_.offset {
                return Some(self.instructions_at(offset))
            }
        }
        None
    }

    fn flag_at(&mut self, address: u64) -> Option<LFlagInfo> {
        for flag in self.flags() {
            if flag.offset == address {
                return Some(flag);
            }
        }
        None
    }

    fn section_of(&mut self, address: u64) -> Option<LSectionInfo> {
        for s in self.section_map() {
            let addr = s.vaddr.expect("Invalid section");
            let size = s.size.expect("Invalid section size");
            if address >= addr && address < addr + size {
                return Some(s);
            }
        }
        None
    }
}

