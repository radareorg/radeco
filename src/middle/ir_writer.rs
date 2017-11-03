//! Convert from and to internal representation (`SSAStorage`)
//!
//! This module provides all the support needed to convert the internal
//! representation of radeco IR
//! into human readable text format and to parse the same back to radeco IR.
//! The text based
//! representation is inspired from (and probably similar) LLVM IR.

use std::collections::HashMap;
use std::default;
use petgraph::graph::NodeIndex;
use middle::ir::{MOpcode};
use middle::ssa::ssastorage::{NodeData, SSAStorage};
use middle::ssa::ssa_traits::{SSA, SSAWalk, ValueInfo};
use middle::ssa::cfg_traits::CFG;
use middle::ssa::graph_traits::Graph;

const BLOCK_SEP: &'static str = "{";
const CL_BLOCK_SEP: &'static str = "}";

#[macro_export]
macro_rules! ir_write {
    ($n: expr, $ssa: expr) => ({
        {
            use $crate::middle::ir_writer::IRWriter;

            let mut writer = IRWriter::default();
            writer.emit_il($n, $ssa, &mut io::stdout());
        }
    });
    ($n: expr, $ssa: expr, $f: expr) => ({
        {
            use $crate::middle::ir_writer::IRWriter;
            use std::fs::File;
            use std::io::prelude::*;

            let mut writer = IRWriter::default();
            let mut f = File::create($f).expect("Unable to create file");
            writer.emit_il($n, $ssa, &mut f);
        }
    });
}

#[allow(unused_macros)]
macro_rules! concat_str {
    ($s:expr, $t:expr) => { $s = format!("{}{}", $s, $t); }
}

#[allow(unused_macros)]
macro_rules! concat_strln {
    ($s:expr, $t:expr) => { $s = format!("{}\n{}", $s, $t); }
}

// Format constants.
#[allow(unused_macros)]
macro_rules! fmt_const {
    ($c:expr) => { format!("#x{:x}", $c); }
}

// Format bb.
#[allow(unused_macros)]
macro_rules! bb {
    ($bb:expr) => { format!("bb_{}():", $bb); }
}

// Format an expression
#[allow(unused_macros)]
macro_rules! expr {
    ($r:expr, $tp:tt = $rhs:expr) => { format!("%{}{} = {}", $r, fmttyp!($tp), $rhs); }
}

// Format a type
#[allow(unused_macros)]
macro_rules! fmttyp {
    ($t: expr) => { format!(":${}", $t); }
}

#[allow(unused_macros)]
macro_rules! fmt_fn {
    ($f:expr) => { format!("define-fun {}(unknown) -> unknown {}", $f, BLOCK_SEP); }
}

#[allow(unused_macros)]
macro_rules! fmtarg {
    ($v:expr, $typ:expr) => { format!("%{}{}", $v, fmttyp!($typ)); }
}

#[allow(unused_macros)]
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
    ctr: u64,
}

impl default::Default for IRWriter {
    fn default() -> IRWriter {
        IRWriter {
            seen: HashMap::new(),
            indent: 1,
            ctr: 0,
        }
    }
}

impl IRWriter {
    pub fn parse<T: AsRef<str>>(_il: T) -> SSAStorage {
        unimplemented!();
    }

    fn fmt_operands(&mut self, operands: &[NodeIndex], ssa: &SSAStorage) -> Vec<String> {
        let mut result = Vec::new();
        for operand in operands {
            match ssa.g[*operand] {
                NodeData::BasicBlock(addr) => result.push(format!("{}", addr)),
                NodeData::DynamicAction => result.push("dynamic_action".to_owned()),
                NodeData::Op(MOpcode::OpConst(ref value), _) => result.push(fmt_const!(value)),
                NodeData::Comment(_, ref named) => result.push(named.clone()),
                _ => {
                    let next = self.ctr + 1;
                    let op_i = self.seen.entry(*operand).or_insert(next);
                    if *op_i == next {
                        self.ctr += 1;
                    }
                    result.push(format!("%{}", op_i));
                }
            }
        }
        result
    }

    fn fmt_expression(&mut self,
                      ni: NodeIndex,
                      opcode: &MOpcode,
                      vt: ValueInfo,
                      mut operands: Vec<String>,
                      ssa: &SSAStorage)
                      -> String {
        let next = self.ctr + 1;
        let result_idx = self.seen.entry(ni).or_insert(next);
        if *result_idx == next {
            self.ctr += 1;
        }

        let w = vt.width().get_width().unwrap_or(64);
        let is_reference = if vt.is_reference() {
            "(*)"
        } else if vt.is_scalar() {
            ""
        } else {
            "(*?)"
        };

        // Quick hack fix
        operands.push("ERR".to_owned());
        operands.push("ERR".to_owned());
        operands.push("ERR".to_owned());

        match *opcode {
            MOpcode::OpAdd => format!("%{}: $Unknown{}{}  = {} + {}",
                                      result_idx,
                                      w,
                                      is_reference,
                                      operands[0],
                                      operands[1]),
            MOpcode::OpSub => format!("%{}: $Unknown{}{} = {} - {}",
                                      result_idx,
                                      w,
                                      is_reference,
                                      operands[0],
                                      operands[1]),
            MOpcode::OpMul => format!("%{}: $Unknown{}{} = {} * {}",
                                      result_idx,
                                      w,
                                      is_reference,
                                      operands[0],
                                      operands[1]),
            MOpcode::OpDiv => format!("%{}: $Unknown{}{} = {} / {}",
                                      result_idx,
                                      w,
                                      is_reference,
                                      operands[0],
                                      operands[1]),
            MOpcode::OpMod => format!("%{}: $Unknown{}{} = {} % {}",
                                      result_idx,
                                      w,
                                      is_reference,
                                      operands[0],
                                      operands[1]),
            MOpcode::OpAnd => format!("%{}: $Unknown{}{} = {} & {}",
                                      result_idx,
                                      w,
                                      is_reference,
                                      operands[0],
                                      operands[1]),
            MOpcode::OpOr => format!("%{}: $Unknown{}{} = {} | {}",
                                     result_idx,
                                     w,
                                     is_reference,
                                     operands[0],
                                     operands[1]),
            MOpcode::OpXor => format!("%{}: $Unknown{}{} = {} ^ {}",
                                      result_idx,
                                      w,
                                      is_reference,
                                      operands[0],
                                      operands[1]),
            MOpcode::OpNot => format!("%{}: $Unknown{}{} = !{}", result_idx, w, is_reference, operands[0]),
            MOpcode::OpCmp => format!("%{}: $Unknown{}{} = {} == {}",
                                      result_idx,
                                      w,
                                      is_reference,
                                      operands[0],
                                      operands[1]),
            MOpcode::OpGt => format!("%{}: $Unknown{}{} = {} > {}",
                                     result_idx,
                                     w,
                                     is_reference,
                                     operands[0],
                                     operands[1]),
            MOpcode::OpLt => format!("%{}: $Unknown{}{} = {} < {}",
                                     result_idx,
                                     w,
                                     is_reference,
                                     operands[0],
                                     operands[1]),
            MOpcode::OpLsl => format!("%{}: $Unknown{}{} = {} << {}",
                                      result_idx,
                                      w,
                                      is_reference,
                                      operands[0],
                                      operands[1]),
            MOpcode::OpLsr => format!("%{}: $Unknown{}{} = {} >> {}",
                                      result_idx,
                                      w,
                                      is_reference,
                                      operands[0],
                                      operands[1]),
            MOpcode::OpLoad => format!("%{}: $Unknown{}{} = Load({}, {})",
                                       result_idx,
                                       w,
                                       is_reference,
                                       operands[0],
                                       operands[1]),
            MOpcode::OpStore => format!("%{}: $Unknown{}{} = Store({}, {}, {})",
                                        result_idx,
                                        w,
                                        is_reference,
                                        operands[0],
                                        operands[1],
                                        operands[2]),
            MOpcode::OpNarrow(wd) => format!("%{}: $Unknown{}{} = Narrow{}({})",
                                             result_idx,
                                             w,
                                             is_reference,
                                             wd,
                                             operands[0]),
            MOpcode::OpSignExt(wd) => format!("%{}: $Unknown{}{} = SignExt{}({})",
                                            result_idx,
                                            w,
                                            is_reference,
                                            wd,
                                            operands[0]),
            MOpcode::OpZeroExt(wd) => format!("%{}: $Unknown{}{} = ZeroExt{}({})",
                                            result_idx,
                                            w,
                                            is_reference,
                                            wd,
                                            operands[0]),
            MOpcode::OpCall => {
                let mem = "mem".to_owned();
                format!("CALL {}({})",
                        operands[0],
                        &operands[1..]
                             .iter()
                             .enumerate()
                             .map(|i| format!("{}={}", ssa.regnames.get(i.0).unwrap_or(&mem), i.1))
                             .fold(String::new(), |acc, x| {
                                 if !acc.is_empty() {
                                     format!("{}, {}", acc, x)
                                 } else {
                                     x
                                 }
                             }))
            }
            _ => unreachable!(),
        }
    }

    fn fmt_indent(by: u64) -> String {
        let mut indent = String::new();
        for _ in 0..by {
            indent.push_str("    ");
        }
        indent
    }

    pub fn emit_il(&mut self, fn_name: Option<String>, ssa: &SSAStorage) -> String
    {
        let mut last = None;
        let mut text_il = String::new();
        let fn_name = if fn_name.is_some() {
            fn_name.unwrap()
        } else {
            "fn_apple".to_owned()
        };

        for node in ssa.inorder_walk() {
            let line = match ssa.g[node] {
                NodeData::Op(ref opcode, vt) => {
                    if let MOpcode::OpConst(_) = *opcode {
                        String::new()
                    } else {
                        let operands = self.fmt_operands(ssa.operands_of(node).as_slice(), ssa);
                        indent!(self.indent,
                                self.fmt_expression(node, opcode, vt, operands, &ssa))
                    }
                }
                NodeData::Phi(_, _) => {
                    let operands = self.fmt_operands(ssa.operands_of(node).as_slice(), ssa);
                    let next = self.ctr + 1;
                    let result_idx = self.seen.entry(node).or_insert(next);
                    if *result_idx == next {
                        self.ctr += 1;
                    }
                    let mut phi_line = format!("%{} = Phi(", result_idx);
                    for operand in operands {
                        phi_line = format!("{}{}, ", phi_line, operand);
                    }
                    let len = phi_line.len();
                    phi_line.truncate(len - 2);
                    phi_line.push_str(")");
                    indent!(self.indent, phi_line)
                }
                NodeData::Undefined(_) => {
                    "Undefined".to_owned()
                }
                NodeData::BasicBlock(addr) => {
                    let bbline = if let Some(ref prev_block) = last {
                        let mut jmp_statement = "JMP".to_owned();
                        let outgoing = ssa.outgoing_edges(*prev_block)
                                          .iter()
                                          .map(|x| 
                                            (ssa.edge_info(x.0).expect("Less-endpoints edge").target, x.1))
                                          .collect::<Vec<_>>();
                        // This is a conditional jump.
                        if ssa.is_selector(*prev_block) && outgoing.len() > 1 {
                            let condition = self.fmt_operands(&[ssa.selector_in(*prev_block)
                                                                   .unwrap()],
                                                              ssa);
                            jmp_statement = format!("{} IF {}", jmp_statement, condition[0]);
                        }

                        let mut target_string = String::new();
                        for (target, edge_type) in outgoing {
                            if target == node && (edge_type == 0 || edge_type == 2) {
                                continue;
                            }

                            let join = if edge_type == 0 {
                                " ELSE "
                            } else {
                                " "
                            };

                            target_string = format!("{}{}{}",
                                                    target_string,
                                                    join,
                                                    self.fmt_operands(&[target], &ssa)[0]);
                        }

                        if target_string.is_empty() {
                            indent!(self.indent - 1, bb!(addr))
                        } else {
                            concat_str!(jmp_statement, target_string);
                            concat_strln!(text_il, indent!(self.indent, jmp_statement));
                            indent!(self.indent - 1, bb!(addr))
                        }
                    } else {
                        self.indent += 1;
                        fmt_fn!(fn_name)
                    };
                    last = Some(node);
                    bbline
                }
                _ => {
                    String::new()
                }
            };

            if !line.is_empty() {
                concat_strln!(text_il, line);
            }
        }

        let exit_node = ssa.exit_node().expect("Incomplete CFG graph");
        let final_state = ssa.registers_in(exit_node)
                                .expect("No register state node found");
        let mem_comment = "mem".to_owned();

        for (i, reg) in ssa.operands_of(final_state).iter().enumerate() {
            let reg_assign = match ssa.g[*reg] {
                NodeData::Comment(_, _) => String::new(),
                _ => {
                    format!("{} = {}",
                            ssa.regnames.get(i).unwrap_or_else(|| {
                                assert_eq!(i, ssa.regnames.len());
                                &mem_comment
                            }),
                            self.fmt_operands(&[*reg], &ssa)[0])
                }
            };
            if !reg_assign.is_empty() {
                concat_strln!(text_il, indent!(self.indent, reg_assign));
            }
        }

        concat_strln!(text_il, CL_BLOCK_SEP);
        text_il
    }
}
