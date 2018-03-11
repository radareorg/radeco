//! Convert from and to internal representation (`SSAStorage`)
//!
//! This module provides all the support needed to convert the internal
//! representation of radeco IR
//! into human readable text format and to parse the same back to radeco IR.
//! The text based
//! representation is inspired from (and probably similar) LLVM IR.


use frontend::radeco_containers::RadecoFunction;
use middle::ir::{MOpcode};
use middle::ssa::ssastorage::{NodeData, SSAStorage};
use middle::ssa::ssa_traits::{SSA, SSAWalk, ValueInfo};
use middle::ssa::cfg_traits::CFG;
use middle::ssa::graph_traits::{EdgeInfo, Graph};


use std::collections::HashMap;
use std::default;
use petgraph::graph::NodeIndex;

const BLOCK_SEP: &'static str = "{";
const CL_BLOCK_SEP: &'static str = "}";

#[macro_export]
macro_rules! ir_write {
    ($n: expr, $ssa: expr) => ({
        {
            use $crate::middle::ir_writer::IRWriter;

            let mut writer = IRWriter::default();
            writer.emit_il($n, $ssa)
        }
    });
    ($n: expr, $ssa: expr, $f: expr) => ({
        {
            use $crate::middle::ir_writer::IRWriter;
            use std::fs::File;
            use std::io::prelude::*;

            let mut writer = IRWriter::default();
            let mut f = File::create($f).unwrap_or_else(|e| {
                radeco_err!("Unable to create file");
                radeco_err!("{:?}", e);
            });
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

macro_rules! fmt_call {
    ($fmt:expr, $operands:ident, $ssa:ident, $ni:ident) => {{
        let mem = "mem".to_owned();
        let mut operands_idx: Vec<u8> = ($ssa.sparse_operands_of($ni).iter().map(|x| x.0).collect());
        operands_idx.sort();
        format!($fmt,
            $operands[0],
            &$operands[1..]
            .iter()
            .zip(&operands_idx[1..])
            .map(|i| format!("${}={}", $ssa.regnames.get((*i.1 - 1) as usize).unwrap_or(&mem), i.0))
            .fold(String::new(), |acc, x| {
                if !acc.is_empty() {
                    format!("{}, {}", acc, x)
                } else {
                    x
                }
            }))
    }};
}

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
    fn node_idx(&mut self, node: NodeIndex) -> u64 {
        let next = self.ctr + 1;
        let node_idx = *self.seen.entry(node).or_insert(next);
        if node_idx == next {
            self.ctr += 1;
        }
        node_idx
    }

    fn fmt_operands(&mut self, operands: &[NodeIndex], ssa: &SSAStorage) -> Vec<String> {
        let mut result = Vec::new();
        for operand in operands {
            match ssa.g[*operand] {
                NodeData::BasicBlock(addr, _) => result.push(format!("{}", addr)),
                NodeData::DynamicAction => result.push("dynamic_action".to_owned()),
                NodeData::Op(MOpcode::OpConst(ref value), _) => result.push(fmt_const!(value)),
                NodeData::Op(MOpcode::OpCall, _) => {
                    println!("unchi");
                    let op_i = self.node_idx(*operand);
                    panic!("ncahrc.,uh.,r");
                    result.push(format!("%{}", op_i));
                },
                NodeData::Comment(_, ref named) => result.push(format!("{{{}}}", named.clone())),
                _ => {
                    let op_i = self.node_idx(*operand);
                    result.push(format!("%{}", op_i));
                }
            }
        }
        result
    }

    fn fmt_valueinfo(&self, vt: ValueInfo) -> String {
        let w = vt.width().get_width().unwrap_or(64);
        let is_reference = if vt.is_reference() {
            "(*)"
        } else if vt.is_scalar() {
            ""
        } else {
            "(*?)"
        };

        format!("$Unknown{}{}", w, is_reference)
    }

    fn fmt_expression(&mut self,
                      ni: NodeIndex,
                      opcode: &MOpcode,
                      vt: ValueInfo,
                      mut operands: Vec<String>,
                      ssa: &SSAStorage)
                      -> String {
        let result_idx = self.node_idx(ni);
        let vi_str = self.fmt_valueinfo(vt);

        // Quick hack fix
        operands.push("ERR".to_owned());
        operands.push("ERR".to_owned());
        operands.push("ERR".to_owned());

        let op_line = match *opcode {
            MOpcode::OpAdd => format!("%{}: {} = {} + {}",
                                      result_idx,
                                      vi_str,
                                      operands[0],
                                      operands[1]),
            MOpcode::OpSub => format!("%{}: {} = {} - {}",
                                      result_idx,
                                      vi_str,
                                      operands[0],
                                      operands[1]),
            MOpcode::OpMul => format!("%{}: {} = {} * {}",
                                      result_idx,
                                      vi_str,
                                      operands[0],
                                      operands[1]),
            MOpcode::OpDiv => format!("%{}: {} = {} / {}",
                                      result_idx,
                                      vi_str,
                                      operands[0],
                                      operands[1]),
            MOpcode::OpMod => format!("%{}: {} = {} % {}",
                                      result_idx,
                                      vi_str,
                                      operands[0],
                                      operands[1]),
            MOpcode::OpAnd => format!("%{}: {} = {} & {}",
                                      result_idx,
                                      vi_str,
                                      operands[0],
                                      operands[1]),
            MOpcode::OpOr => format!("%{}: {} = {} | {}",
                                     result_idx,
                                     vi_str,
                                     operands[0],
                                     operands[1]),
            MOpcode::OpXor => format!("%{}: {} = {} ^ {}",
                                      result_idx,
                                      vi_str,
                                      operands[0],
                                      operands[1]),
            MOpcode::OpNot => format!("%{}: {} = !{}", result_idx, vi_str, operands[0]),
            MOpcode::OpCmp => format!("%{}: {} = {} == {}",
                                      result_idx,
                                      vi_str,
                                      operands[0],
                                      operands[1]),
            MOpcode::OpGt => format!("%{}: {} = {} > {}",
                                     result_idx,
                                     vi_str,
                                     operands[0],
                                     operands[1]),
            MOpcode::OpLt => format!("%{}: {} = {} < {}",
                                     result_idx,
                                     vi_str,
                                     operands[0],
                                     operands[1]),
            MOpcode::OpLsl => format!("%{}: {} = {} << {}",
                                      result_idx,
                                      vi_str,
                                      operands[0],
                                      operands[1]),
            MOpcode::OpLsr => format!("%{}: {} = {} >> {}",
                                      result_idx,
                                      vi_str,
                                      operands[0],
                                      operands[1]),
            MOpcode::OpLoad => format!("%{}: {} = Load({}, {})",
                                       result_idx,
                                       vi_str,
                                       operands[0],
                                       operands[1]),
            MOpcode::OpStore => format!("%{}: {} = Store({}, {}, {})",
                                        result_idx,
                                        vi_str,
                                        operands[0],
                                        operands[1],
                                        operands[2]),
            MOpcode::OpNarrow(wd) => format!("%{}: {} = Narrow{}({})",
                                             result_idx,
                                             vi_str,
                                             wd,
                                             operands[0]),
            MOpcode::OpSignExt(wd) => format!("%{}: {} = SignExt{}({})",
                                              result_idx,
                                              vi_str,
                                              wd,
                                              operands[0]),
            MOpcode::OpZeroExt(wd) => format!("%{}: {} = ZeroExt{}({})",
                                              result_idx,
                                              vi_str,
                                              wd,
                                              operands[0]),
            MOpcode::OpCall => {
                if vt.is_reference() {
                    fmt_call!("CALL *({})({})", operands, ssa, ni)
                } else {
                    fmt_call!("CALL {}({})", operands, ssa, ni)
                }
            }
            _ => {
                format!("{} {:?}", opcode, operands)
            },
        };
        if let Some(addr) = ssa.address(ni) {
            format!("[@{}] {};", addr, op_line)
        } else {
            format!("{};", op_line)
        }
    }

    fn fmt_jump(&mut self, ssa: &SSAStorage, prev_blk: NodeIndex, next_blk: NodeIndex) -> String {
        if let Some(selector) = ssa.selector_in(prev_blk) {
            if let Some(cond_info_blk) = ssa.conditional_blocks(prev_blk) {
                if cond_info_blk.false_side == next_blk {
                    let ops = self.fmt_operands(&[selector, cond_info_blk.true_side], ssa);
                    format!("JMP IF {} {}", ops[0], ops[1])
                } else {
                    let ops = self.fmt_operands(&[selector, cond_info_blk.true_side, cond_info_blk.false_side], ssa);
                    format!("JMP IF {} {} ELSE {}", ops[0], ops[1], ops[2])
                }
            } else {
                radeco_err!("block with selector has no conditional successors");
                "[[invalid SSA for jump]]".to_owned()
            }
        } else {
            if let Some(blk) = ssa.unconditional_block(prev_blk) {
                if blk == next_blk {
                    String::new()
                } else {
                    let ops = self.fmt_operands(&[blk], ssa);
                    format!("JMP {}", ops[0])
                }
            } else {
                radeco_err!("block without selector has no unconditional successor");
                "[[invalid SSA for jump]]".to_owned()
            }
        }
    }

    fn fmt_indent(by: u64) -> String {
        let mut indent = String::new();
        for _ in 0..by {
            indent.push_str("    ");
        }
        indent
    }

    // TODO: expose width
    pub fn pretty_print_function_proto(rfn: &RadecoFunction) -> String {
        let mut args = rfn.bindings()
            .into_iter()
            .filter(|&x| x.btype().is_argument())
            .filter_map(|x| rfn.ssa().node_data(x.idx).map(|v| (x.idx, v.vt)).ok())
            .fold(String::new(), |acc, x| {
                if acc.is_empty() {
                    format!("{}{:?}", acc, x.1.vty)
                } else {
                    format!("{}, {:?}", acc, x.1.vty)
                }
            });
        format!("{} ({})", rfn.name, args)
    }

    pub fn emit_il(&mut self, fn_name: Option<String>, ssa: &SSAStorage) -> String {
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
                        radeco_err!("found const");
                        String::new()
                    } else {
                        let operands = self.fmt_operands(ssa.operands_of(node).as_slice(), ssa);
                        indent!(self.indent,
                                self.fmt_expression(node, opcode, vt, operands, &ssa))
                    }
                }
                NodeData::Phi(vt, _) => {
                    let operands = self.fmt_operands(ssa.operands_of(node).as_slice(), ssa);
                    let result_idx = self.node_idx(node);
                    let vi_str = self.fmt_valueinfo(vt);
                    let mut phi_line = format!("%{}: {} = Phi(", result_idx, vi_str);
                    for operand in operands {
                        phi_line = format!("{}{}, ", phi_line, operand);
                    }
                    let len = phi_line.len();
                    phi_line.truncate(len - 2);
                    phi_line.push_str(");");
                    indent!(self.indent, phi_line)
                }
                NodeData::Undefined(_) => {
                    "Undefined".to_owned()
                }
                NodeData::BasicBlock(addr, _) => {
                    let bbline = if let Some(prev_block) = last {
                        let jmp = self.fmt_jump(ssa, prev_block, node);
                        if !jmp.is_empty() {
                            concat_strln!(text_il, indent!(self.indent, jmp));
                        }
                        indent!(self.indent - 1, bb!(addr))
                    } else {
                        // first block of function, actually a "fake" block
                        // we use it as the marker for the beginning of the function
                        self.indent += 1;
                        let mut fn_line = fmt_fn!(fn_name);
                        let mut reg_list_line = indent!(self.indent - 1, "registers: ");
                        for regname in &ssa.regnames {
                            reg_list_line.push('$');
                            reg_list_line.push_str(regname);
                            reg_list_line.push(',');
                        }
                        reg_list_line.pop();
                        reg_list_line.push(';');

                        concat_strln!(fn_line, reg_list_line);
                        fn_line
                    };
                    last = Some(node);
                    bbline
                }
                ref n@_ => {
                    radeco_err!("Unknown node: {:?}", n);
                    String::new()
                }
            };

            if !line.is_empty() {
                concat_strln!(text_il, line);
            }
        }

        let exit_node = ssa.exit_node().unwrap_or_else(|| {
            radeco_err!("Incomplete CFG graph");
            ssa.invalid_action().unwrap()
        });
        let final_state = ssa.registers_in(exit_node).unwrap_or_else(|| {
            radeco_err!("No register state node found");
            ssa.invalid_value().unwrap()
        });
        let mem_comment = "mem".to_owned();

        concat_strln!(text_il, &indent!(self.indent - 1, "final-register-state:"));

        for (i, reg) in ssa.operands_of(final_state).iter().enumerate() {
            let reg_assign = match ssa.g[*reg] {
                NodeData::Comment(_, _) => String::new(),
                _ => {
                    format!("${} = {};",
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
