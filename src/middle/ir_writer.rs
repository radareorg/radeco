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
macro_rules! fmt_const {
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
    pub fn parse<T: AsRef<str>>(il: T) -> SSAStorage {
        unimplemented!();
    }

    fn fmt_operands(&self, operands: &[NodeIndex], ssa: &SSAStorage) -> Vec<String> {
        let mut result = Vec::new();
        for operand in operands {
            let op_internal = ssa.internal(operand);
            match ssa.g[op_internal] {
                NodeData::BasicBlock(addr) => result.push(format!("{}", addr)),
                NodeData::DynamicAction => result.push("dynamic_action".to_owned()),
                NodeData::Op(MOpcode::OpConst(ref value), _) => result.push(fmt_const!(value)),
                NodeData::Comment(_, ref named) => result.push(named.clone()),
                _ => {
                    result.push(format!("%{}: $unknown", self.seen.get(operand).unwrap()));
                }
            }
        }
        result
    }

    fn fmt_expression(&mut self,
                      ni: NodeIndex,
                      opcode: MOpcode,
                      vt: ValueType,
                      operands: Vec<String>)
                      -> String {
        self.ctr += 1;
        self.seen.insert(ni, self.ctr);
        match opcode {
            MOpcode::OpAdd => format!("%{} = {} + {}", self.ctr, operands[0], operands[1]),
            MOpcode::OpSub => format!("%{} = {} - {}", self.ctr, operands[0], operands[1]),
            MOpcode::OpMul => format!("%{} = {} * {}", self.ctr, operands[0], operands[1]),
            MOpcode::OpDiv => format!("%{} = {} / {}", self.ctr, operands[0], operands[1]),
            MOpcode::OpMod => format!("%{} = {} % {}", self.ctr, operands[0], operands[1]),
            MOpcode::OpAnd => format!("%{} = {} & {}", self.ctr, operands[0], operands[1]),
            MOpcode::OpOr => format!("%{} = {} | {}", self.ctr, operands[0], operands[1]),
            MOpcode::OpXor => format!("%{} = {} ^ {}", self.ctr, operands[0], operands[1]),
            MOpcode::OpNot => format!("%{} = !{}", self.ctr, operands[0]),
            MOpcode::OpCmp => format!("%{} = {} == {}", self.ctr, operands[0], operands[1]),
            MOpcode::OpGt => format!("%{} = {} > {}", self.ctr, operands[0], operands[1]),
            MOpcode::OpLt => format!("%{} = {} < {}", self.ctr, operands[0], operands[1]),
            MOpcode::OpLsl => format!("%{} = {} << {}", self.ctr, operands[0], operands[1]),
            MOpcode::OpLsr => format!("%{} = {} >> {}", self.ctr, operands[0], operands[1]),
            MOpcode::OpLoad => format!("%{} = OpLoad({})", self.ctr, operands[0]),
            MOpcode::OpStore => format!("%{} = Store({}, {})", self.ctr, operands[0], operands[1]),
            MOpcode::OpNarrow(w) => format!("%{} = Narrow{}({})", self.ctr, w, operands[0]),
            MOpcode::OpWiden(w) => format!("%{} = Widen{}({})", self.ctr, w, operands[0]),
            MOpcode::OpCall => format!("{}", operands[0]),
            _ => unreachable!(),
        }
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

        for node in ssa.inorder_walk() {
            let intn = ssa.internal(&node);
            let line = match ssa.g[intn] {
                NodeData::Op(opcode, vt) => {
                    if let MOpcode::OpConst(_) = opcode {
                        String::new()
                    } else {
                        let operands = self.fmt_operands(ssa.get_operands(&node).as_slice(), &ssa);
                        indent!(self.indent, self.fmt_expression(node, opcode, vt, operands))
                    }
                }
                NodeData::Phi(vt, ref name) => {
                    let operands = self.fmt_operands(ssa.get_operands(&node).as_slice(), &ssa);
                    self.ctr += 1;
                    self.seen.insert(node, self.ctr);
                    let mut phi_line = format!("%{} = Phi(", self.ctr);
                    for operand in operands {
                        phi_line = format!("{}{}, ", phi_line, operand);
                    }
                    let len = phi_line.len();
                    phi_line.truncate(len - 2);
                    phi_line.push_str(")");
                    indent!(self.indent, phi_line)
                }
                NodeData::Undefined(vt) => {
                    "Undefined".to_owned()
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
                                                                   .unwrap()], &ssa);
                            jmp_statement = format!("{} IF {}", jmp_statement, condition[0]);
                        }

                        let mut target_string = String::new();
                        for (target, edge_type) in outgoing {
                            if target == node {
                                continue;
                            }
                            target_string = format!("{} {}",
                                                    target_string,
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

        let exit_node = ssa.exit_node();
        let final_state = ssa.registers_at(&exit_node);

        for (i, reg) in ssa.get_operands(&final_state).iter().enumerate() {
            let rint = ssa.internal(&reg);
            let reg_assign = match ssa.g[rint] {
                NodeData::Comment(_, _) => String::new(),
                _ => {
                    format!("{} = {}", ssa.regnames[i], self.fmt_operands(&[*reg], &ssa)[0])
                }
            };
            if !reg_assign.is_empty() {
                concat_strln!(text_il, indent!(self.indent, reg_assign));
            }
        }

        concat_strln!(text_il, CL_BLOCK_SEP);
        writeln!(sink, "{}", text_il);
    }
}
