//! Parses textual IL as emitted by [`ir_writer`](::middle::ir_writer).

mod lowering;
mod parser;
mod parser_util;
mod simple_ast;
#[cfg(test)]
mod test;

use crate::middle::regfile::SubRegisterFile;
use crate::middle::ssa::ssastorage::SSAStorage;

use std::sync::Arc;

/// Parses textual IL as emitted by [`ir_writer`](::middle::ir_writer).
/// The returned SSA is empty if an error occurred.
pub fn parse_il(il: &str, regfile: Arc<SubRegisterFile>) -> SSAStorage {
    let mut ret = SSAStorage::new();
    ret.regfile = regfile;
    match parser::FunctionParser::new().parse(il) {
        Ok(sast) => lowering::lower_simpleast(&mut ret, sast)
            .unwrap_or_else(|_e| radeco_err!("Error lowering IL to SSA: {:?}", _e)),
        Err(_s) => radeco_err!("Error parsing IL: {}", _s),
    }
    ret
}
