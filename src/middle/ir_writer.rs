//! Convert from and to internal representation (`SSAStorage`)
//!
//! This module provides all the support needed to convert the internal representation of radeco IR
//! into human readable text format and to parse the same back to radeco IR. The text based
//! representation is inspired from (and probably similar) LLVM IR.

use std::io::Write;
use middle::ssa::ssastorage::SSAStorage;

const BLOCK_SEP:    &'static str = "{";
const CL_BLOCK_SEP: &'static str = "}";

macro_rules! concat_str {
    ($s:expr, $t:expr) => { format!("{}{}", s, t); }
}

macro_rules! concat_strln {
    ($s:expr, $t:expr) => { format!("{}\n{}", s, t); }
}

// Format constants.
macro_rules! fmtconst {
    ($c:expr) => { format!("#x{:x}", $c); }
}

// Format bb.
macro_rules! bb {
    ($bb:expr) => { format!("bb_{}():", expr); }
}

// Format an expression
macro_rules! expr {
    ($r:expr, $tp:tt = $rhs:expr) => { format!("%{}{} = {}", $r, fmttyp!($tp), $rhs); }
}

// Format a type
macro_rules! fmttyp {
    ($t: expr) => { format!(":${}", $t); }
}

macro_rules! fmtarg {
    ($v:expr, $typ:expr) => { format!("%{}{}", $v, fmtyp!($typ)); }
}


#[derive(Clone, Debug)]
pub struct IRWriter;

pub fn parse<T: AsRef<str>>(il: T) -> SSAStorage {
    unimplemented!();
}

pub fn emit_il<S, T>(fn_name: Option<S>, ssa: &SSAStorage, sink: &mut T) -> String
where S: AsRef<str>,
      T: Write 
{
    unimplemented!()
}


