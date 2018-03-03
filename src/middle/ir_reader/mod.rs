//! Parses textual IL as emited by [`ir_writer`](::middle::ir_writer).

mod parser_util;
mod parser;
mod lowering;
mod simple_ast;

use middle::ssa::ssastorage::SSAStorage;

/// Parses textual IL as emited by [`ir_writer`](::middle::ir_writer).
/// The returned SSA is empty if an error occured.
pub fn parse_il<T: AsRef<str>>(il: T) -> SSAStorage {
    let mut ret = SSAStorage::new();
    match parser::parse_Function(il.as_ref()) {
        Ok(sast) => lowering::lower_simpleast(&mut ret, sast)
            .unwrap_or_else(|e| radeco_err!("Error lowering AST to SSA: {:?}", e)),
        Err(s) => radeco_err!("Error parsing IL: {}", s),
    }
    ret
}
