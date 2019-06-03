//! Utilities and other miscellaneous functions for `RuneContext`

use r2pipe::r2::R2;

use r2api::api_trait::R2Api;

use crate::context::rune_ctx::RuneContext;
use crate::context::context::{ContextAPI};

use crate::memory::memory::Memory;

use crate::memory::seg_mem::SegMem;

use crate::regstore::regstore::RegStore;
use crate::regstore::regfile::RuneRegFile;

use petgraph::graph::NodeIndex;

use libsmt::backends::smtlib2::SMTLib2;
use libsmt::logics::qf_abv;
use libsmt::logics::qf_abv::QF_ABV_Fn::BVOps;
use libsmt::theories::bitvec::OpCodes::*;


use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum ValType {
    Concrete(usize),
    Symbolic,
    Break,
    Unknown(String),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Eq, Hash)]
pub enum Key {
    Mem(usize),
    Reg(String),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SAssignment {
    pub lvalue: Key,
    pub rvalue: ValType,
}

/// Hex/Decimal to Memory address, any other string maps to Registers
///
/// Useful when input strings is to be interpretted either as a Memory Address or a register name.
pub fn to_key<T: AsRef<str>>(s: T) -> Key {
    let v = s.as_ref();
    if v.len() > 2 && &v[0..2] == "0x" {
        Key::Mem(usize::from_str_radix(&v[2..], 16).expect("Invalid number!"))
    } else if v.chars().nth(0).unwrap().is_digit(10) {
        Key::Mem(usize::from_str_radix(v, 10).expect("Invalid number!"))
    } else {
        Key::Reg(v.to_owned())
    }
}

pub fn to_valtype<T: AsRef<str>>(s: T) -> Option<ValType> {
    let v = s.as_ref();

    if v == "SYM" {
        Some(ValType::Symbolic)
    } else if let Some(val) = convert_to_u64(v) {
        Some(ValType::Concrete(val as usize))
    } else {
        None
    }
}

pub fn to_assignment<T: AsRef<str>>(s: T) -> Option<SAssignment> {
    let v = s.as_ref();
    let ops: Vec<&str> = v.split('=').collect();

    let lvalue: Key = to_key(ops[0].trim());
    if let Some(rvalue) = to_valtype(ops[1].trim()) {
        Some(SAssignment {
                lvalue: lvalue,
                rvalue: rvalue,
            })
    } else {
        None
    }
}

pub fn convert_to_u64<T: AsRef<str>>(s: T) -> Option<u64> {
    let v = s.as_ref();
    if v.len() > 2 && &v[0..2] == "0x" {
        if let Ok(val) = usize::from_str_radix(&v[2..], 16) {
            Some(val as u64)
        } else {
            None
        }
    } else if v.chars().nth(0).unwrap().is_digit(10) {
        if let Ok(val) = usize::from_str_radix(v, 10) {
            Some(val as u64)
        } else {
            None
        }
    } else {
        None
    }
}

// This function needs to be updated to support arbitrary Ctx and mem sizes
pub fn new_rune_ctx(
    ip: Option<u64>,
    syms: Option<HashMap<Key, u64>>,
    consts: Option<HashMap<Key, (u64, u64)>>,
    r2: &mut R2) -> RuneContext<SegMem, RuneRegFile> {

    let mut lreginfo = r2.reg_info().unwrap();
    let rregfile     = RuneRegFile::new(&mut lreginfo);

    let bin      = r2.bin_info().unwrap().bin.unwrap();
    let bits     = bin.bits.unwrap();
    let endian   = bin.endian.unwrap();
    let rmem = SegMem::new(bits, endian);

    let smt = SMTLib2::new(Some(qf_abv::QF_ABV));
    
    let mut ctx = RuneContext::new(ip, rmem, rregfile, smt);

    if let Some(sym_vars) = syms {
        for (sym, size) in sym_vars.iter() {
            match *sym {
                Key::Mem(addr)    => ctx.set_mem_as_sym(addr as u64, *size as usize),
                Key::Reg(ref reg) => ctx.set_reg_as_sym(reg),
            };
        }
    }

    if let Some(const_vars) = consts {
        for (key, val) in const_vars.iter() {
            match *key {
                Key::Mem(addr)    => ctx.set_mem_as_const(addr as u64, val.0, val.1 as usize),
                Key::Reg(ref reg) => ctx.set_reg_as_const(reg, val.0),
            };
        }
    }

    // Setting unset registers to zero!
    for register in &lreginfo.reg_info {
        ctx.set_reg_as_const(register.name.clone(), 0);
    }

    ctx
}

// Ideally, this should be implemented for all logics. 
// But since we are using only bitvecs, we can use this function for now I guess.
pub fn simplify_constant(ni: NodeIndex, solver: &mut SMTLib2<qf_abv::QF_ABV>) -> u64 {
    let c = match solver.get_node_info(ni) {
        &BVOps(Const(x, _)) => x,
        &BVOps(BvSub) => {
            let oper     = solver.get_operands(ni);
            let mut iter = oper.iter();

            let first    = iter.next().unwrap();
            let second   = iter.next().unwrap();

            simplify_constant(*second, solver) - simplify_constant(*first, solver)
        },
        &BVOps(BvAdd) => {
            let oper     = solver.get_operands(ni);
            let mut iter = oper.iter();

            let first    = iter.next().unwrap();
            let second   = iter.next().unwrap();

            simplify_constant(*second, solver) + simplify_constant(*first, solver)
        },
        _ => panic!("Unimplemented!"),
    };

    c
}
