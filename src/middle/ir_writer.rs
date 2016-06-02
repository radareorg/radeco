//! Convert from and to internal representation (`SSAStorage`)
//!
//! This module provides all the support needed to convert the internal
//! representation of radeco IR
//! into human readable text format and to parse the same back to radeco IR.
//! The text based
//! representation is inspired from (and probably similar) LLVM IR.

use std::io::Write;
use std::collections::HashMap;
use std::default;
use petgraph::graph::NodeIndex;
use middle::ir::{MAddress, MOpcode};
use middle::ssa::ssastorage::{NodeData, SSAStorage};
use middle::ssa::ssa_traits::{SSA, SSAExtra, SSAMod, SSAWalk, ValueType};
use middle::ssa::cfg_traits::CFG;

const BLOCK_SEP: &'static str = "{";
const CL_BLOCK_SEP: &'static str = "}";

macro_rules! concat_str {
    ($s:expr, $t:expr) => { $s = format!("{}{}", $s, $t); }
}

macro_rules! concat_strln {
    ($s:expr, $t:expr) => { $s = format!("{}\n{}", $s, $t); }
}

// Format constants.
macro_rules! fmtconst {
    ($c:expr) => { format!("#x{:x}", $c); }
}

// Format bb.
macro_rules! bb {
    ($bb:expr) => { format!("bb_{}():", $bb); }
}

// Format an expression
macro_rules! expr {
    ($r:expr, $tp:tt = $rhs:expr) => { format!("%{}{} = {}", $r, fmttyp!($tp), $rhs); }
}

// Format a type
macro_rules! fmttyp {
    ($t: expr) => { format!(":${}", $t); }
}

macro_rules! fmt_fn {
    ($f:expr) => { format!("define-fun {}(unknown) -> unknown {}", $f, BLOCK_SEP); }
}

macro_rules! fmtarg {
    ($v:expr, $typ:expr) => { format!("%{}{}", $v, fmttyp!($typ)); }
}

macro_rules! indent {
    ($n:expr, $s:expr) => { format!("{}{}", IRWriter::fmt_indent($n), $s); }
}

// TODO before implementing this. This will also help in writing the pseudo C
// output.
// 1. Associate nodes and addresses. Currently, addresses are not stored with
// the nodes. This is
// trivial to implement as the phiplacer has the requisite information and
// this can be
//    associated in finish_block.
// 2. Write an iterator for SSAStorage that yeilds nodes "inorder" i.e. ordered
// by their
//    corresponding instruction addresses in the original binary.
// 3. Add a pass to load symbols and flags from radare2. This will produce more
// readable output in
//    both the cases.

#[derive(Clone, Debug)]
pub struct IRWriter {
    seen: HashMap<NodeIndex, u64>,
    indent: u64,
}

impl default::Default for IRWriter {
    fn default() -> IRWriter {
        IRWriter {
            seen: HashMap::new(),
            indent: 1,
        }
    }
}

impl IRWriter {
    pub fn parse<T: AsRef<str>>(il: T) -> SSAStorage {
        unimplemented!();
    }

    fn fmt_operands(&self, operands: &[NodeIndex]) -> Vec<String> {
        vec!["undef".to_owned()]
    }

    fn fmt_expression(&mut self, opcode: MOpcode, vt: ValueType, operands: Vec<String>) -> String {
        "undef".to_owned()
    }

    fn fmt_indent(by: u64) -> String {
        let mut indent = String::new();
        for i in 0..by {
            indent.push_str("    ");
        }
        indent
    }

    pub fn emit_il<T>(&mut self, fn_name: Option<String>, ssa: &SSAStorage, sink: &mut T)
        where T: Write
    {
        let mut last = None;
        let mut text_il = String::new();
        let fn_name = if fn_name.is_some() {
            fn_name.unwrap()
        } else {
            "fn_apple".to_owned()
        };

        for node in ssa.bfs_walk() {
            let intn = ssa.internal(&node);
            let line = match ssa.g[intn] {
                NodeData::Op(opcode, vt) => {
                    let operands = self.fmt_operands(ssa.get_operands(&node).as_slice());
                    indent!(self.indent, self.fmt_expression(opcode, vt, operands))
                }
                NodeData::Phi(vt, ref name) => {
                    String::new()
                }
                NodeData::Undefined(vt) => {
                    String::new()
                }
                NodeData::BasicBlock(addr) => {
                    let bbline = if let Some(ref prev_block) = last {
                        let mut jmp_statement = "JMP".to_owned();
                        let outgoing = ssa.edges_of(prev_block)
                                          .iter()
                                          .map(|x| (ssa.target_of(&x.0), x.1))
                                          .collect::<Vec<_>>();
                        // This is a conditional jump.
                        if outgoing.len() > 1 {
                            let condition = self.fmt_operands(&[ssa.selector_of(prev_block)
                                                                   .unwrap()]);
                            jmp_statement = format!("{} IF {}", jmp_statement, condition[0]);
                        }
                        for (target, edge_type) in outgoing {
                            if target == node {
                                continue;
                            }
                            jmp_statement = format!("{} {}",
                                                    jmp_statement,
                                                    self.fmt_operands(&[target])[0]);
                        }
                        concat_strln!(text_il, indent!(self.indent, jmp_statement));
                        indent!(self.indent - 1, bb!(addr))
                    } else {
                        self.indent += 1;
                        fmt_fn!(fn_name)
                    };
                    last = Some(node);
                    bbline
                }
                _ => { String::new() }
            };
            concat_strln!(text_il, line);
        }
        concat_strln!(text_il, CL_BLOCK_SEP);
        writeln!(sink, "{}", text_il);
    }
}
