
use libsmt::backends::smtlib2::{SMTLib2};
use libsmt::logics::qf_abv;
use r2api::structs::LRegInfo;

use std::fmt::Debug;


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

    fn new(_: &mut LRegInfo) -> Self;

    fn get_reg_entry(&self, _: &str) -> RegEntry;

    fn get_reg_ref(&self, _: &str) -> Option<Self::VarRef>;

    fn set_reg(&mut self, _: &str, _: Self::VarRef);

    fn read(&mut self, _: &str, _: &mut SMTLib2<qf_abv::QF_ABV>) -> Self::VarRef;

    fn write(&mut self, _: &str, _: Self::VarRef) -> Option<Self::VarRef>;
}

pub trait RegStoreAPI: RegStore {
    fn get_regs(&self) -> Vec<Option<Self::VarRef>>;
}

