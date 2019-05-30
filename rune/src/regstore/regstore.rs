use libsmt::backends::backend::SMTBackend;
use libsmt::backends::smtlib2::{SMTLib2, SMTProc};
use libsmt::logics::qf_abv;
use r2api::structs::LRegInfo;

use std::fmt::Debug;
use std::hash::Hash;

#[derive(Clone, Debug, Default)]
pub struct RegEntry {
    pub name: String,
    pub idx: usize,
    // 0 indexed
    pub start_bit: usize,
    pub end_bit: usize,
    pub is_whole: bool,
    pub alias: Option<String>,
}

impl RegEntry {
    pub fn new(name: String,
           idx: usize,
           sbit: usize,
           ebit: usize,
           is_whole: bool,
           alias: Option<String>)
           -> RegEntry {
        RegEntry {
            name: name,
            idx: idx,
            start_bit: sbit,
            end_bit: ebit,
            is_whole: is_whole,
            alias: alias,
        }
    }
}

pub trait RegStore: Clone + Debug {
    type VarRef;

    fn new(&mut LRegInfo) -> Self;

    fn get_reg_entry(&self, &str) -> RegEntry;

    fn get_reg_ref(&self, &str) -> Option<Self::VarRef>;

    fn set_reg(&mut self, &str, Self::VarRef);

    fn read(&mut self, &str, &mut SMTLib2<qf_abv::QF_ABV>) -> Self::VarRef;

    fn write(&mut self, &str, Self::VarRef) -> Option<Self::VarRef>;
}

pub trait RegStoreAPI: RegStore {
    fn get_regs(&self) -> Vec<Option<Self::VarRef>>;
}

