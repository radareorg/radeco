//! Convert from and to internal representation (`SSAStorage`)
//!
//! This module provides all the support needed to convert the internal
//! representation of radeco IR
//! into human readable text format and to parse the same back to radeco IR.
//! The text based
//! representation is inspired from (and probably similar) LLVM IR.

use frontend::radeco_containers::RadecoFunction;
use middle::ir::MOpcode;
use middle::ssa::cfg_traits::CFG;
use middle::ssa::ssa_traits::{SSAWalk, ValueInfo, SSA};
use middle::ssa::ssastorage::{NodeData, SSAStorage};
use middle::ssa::utils;
use petgraph::graph::NodeIndex;

use std::collections::HashMap;
use std::fmt;
use std::fmt::Write;

macro_rules! emit_list {
    ($output:expr, $list:expr, | $elem:pat | $formatter:expr) => {{
        let mut first = true;
        for $elem in $list {
            if !first {
                $output.write_str(", ")?;
            }
            $formatter;
            first = false;
        }
    }};
    ($output:expr, $list:expr, | $elem:pat | $formatter:expr,) => {
        emit_list!($output, $list, |$elem| $formatter)
    };
}

macro_rules! log_emit_err {
    ($self:ident, $($arg:tt)*) => {
        match format_args!($($arg)*) {
            msg => {
                radeco_err!("{}", msg);
                write!($self.output, "ERR{{{}}}", msg)
            }
        }
    }
}

pub fn emit_il<O: Write>(output: O, fn_name: Option<String>, ssa: &SSAStorage) -> fmt::Result {
    IRWriter::new(output, ssa).emit_il(fn_name)
}

// TODO: expose width
pub fn pretty_print_function_proto(rfn: &RadecoFunction) -> String {
    let args = rfn
        .bindings()
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

#[derive(Clone, Debug)]
struct IRWriter<'a, O: Write> {
    ssa: &'a SSAStorage,
    seen: HashMap<NodeIndex, u64>,
    ctr: u64,
    output: O,
}

impl<'a, O: Write> IRWriter<'a, O> {
    fn new(output: O, ssa: &'a SSAStorage) -> Self {
        IRWriter {
            ssa,
            seen: HashMap::new(),
            ctr: 0,
            output,
        }
    }

    fn emit_il(mut self, fn_name: Option<String>) -> fmt::Result {
        let mut last = None;
        let entry_node = entry_node_err!(self.ssa);
        let exit_node = exit_node_err!(self.ssa);
        let fn_name = fn_name.as_ref().map(|s| &**s).unwrap_or("fn_apple");

        writeln!(self.output, "define-fun {}(unknown) -> unknown {{", fn_name)?;

        let entry_regs = registers_in_err!(self.ssa, entry_node);
        self.emit_entry_regstate(entry_regs)?;

        for node in self.ssa.inorder_walk() {
            if node == entry_node {
                continue;
            }
            match self.ssa.g[node] {
                NodeData::Op(ref opcode, vt) => {
                    self.indent(2)?;
                    if let Some(address) = self.ssa.address(node) {
                        write!(self.output, "[@{}] ", address)?;
                    }
                    match opcode {
                        MOpcode::OpConst(_) => {
                            radeco_err!("found const");
                        }
                        MOpcode::OpCall => self.emit_call(node)?,
                        _ => {
                            self.emit_new_value(node, vt)?;
                            self.emit_operation(opcode, &self.ssa.operands_of(node))?;
                        }
                    };
                    writeln!(self.output, ";")?;
                }
                NodeData::Phi(vt, _) => {
                    self.indent(2)?;
                    self.emit_new_value(node, vt)?;
                    write!(self.output, "Phi(")?;
                    let operands = self.ssa.operands_of(node);
                    self.emit_operand_list(&operands)?;
                    writeln!(self.output, ");")?;
                }
                NodeData::BasicBlock(addr, sz) => {
                    if let Some(prev_block) = last {
                        // end previous block
                        self.indent(2)?;
                        self.emit_jump(prev_block)?;
                    }
                    last = Some(node);
                    self.indent(1)?;
                    writeln!(self.output, "bb_{}(sz {:#x}):", addr, sz)?;
                }
                NodeData::DynamicAction => {
                    if let Some(prev_block) = last {
                        // end previous block
                        self.indent(2)?;
                        self.emit_jump(prev_block)?;
                    } else {
                        radeco_warn!("first node was DynamicAction");
                    }
                    last = Some(node);

                    self.indent(1)?;
                    if self.ssa.exit_node() == Some(node) {
                        writeln!(self.output, "exit-node:")?;
                    } else {
                        radeco_warn!("found DynamicAction node that wasn't exit_node");
                        writeln!(self.output, "[[dynamic_action]]:")?;
                    };
                }
                ref n => {
                    log_emit_err!(self, "Unknown node: {:?}", n)?;
                }
            };
        }

        if let Some(prev_block) = last {
            if prev_block != exit_node {
                // end previous block
                self.indent(2)?;
                self.emit_jump(prev_block)?;
            }
        }

        let final_state = registers_in_err!(self.ssa, exit_node);
        self.emit_exit_regstate(final_state)?;

        writeln!(self.output, "}}")?;
        Ok(())
    }

    fn emit_entry_regstate(&mut self, entry_regstate: NodeIndex) -> fmt::Result {
        self.indent(1)?;
        writeln!(self.output, "entry-register-state:")?;
        for (reg_id, (reg_val, vt)) in utils::register_state_info(entry_regstate, self.ssa) {
            self.indent(2)?;
            self.emit_new_value(reg_val, vt)?;
            let regname = self.ssa.regfile.get_name(reg_id).unwrap_or("mem");
            writeln!(self.output, "${};", regname)?;
        }
        Ok(())
    }

    fn emit_exit_regstate(&mut self, exit_regstate: NodeIndex) -> fmt::Result {
        self.indent(1)?;
        writeln!(self.output, "final-register-state:")?;
        for (reg_id, (reg_val, _)) in utils::register_state_info(exit_regstate, self.ssa) {
            self.indent(2)?;
            let regname = self.ssa.regfile.get_name(reg_id).unwrap_or("mem");
            write!(self.output, "${} = ", regname)?;
            self.emit_operand(reg_val)?;
            writeln!(self.output, ";")?;
        }
        Ok(())
    }

    fn emit_operation(&mut self, opcode: &MOpcode, operands: &[NodeIndex]) -> fmt::Result {
        use self::MOpcode::*;
        match *opcode {
            OpAdd => self.emit_binop("+", operands),
            OpSub => self.emit_binop("-", operands),
            OpMul => self.emit_binop("*", operands),
            OpDiv => self.emit_binop("/", operands),
            OpMod => self.emit_binop("%", operands),
            OpAnd => self.emit_binop("&", operands),
            OpOr => self.emit_binop("|", operands),
            OpXor => self.emit_binop("^", operands),
            OpEq => self.emit_binop("==", operands),
            OpGt => self.emit_binop(">", operands),
            OpLt => self.emit_binop("<", operands),
            OpLsl => self.emit_binop("<<", operands),
            OpLsr => self.emit_binop(">>", operands),
            OpNot => {
                write!(self.output, "!")?;
                self.emit_opt_operand(operands.get(0).cloned())?;
                Ok(())
            }
            OpLoad => {
                write!(self.output, "Load(")?;
                self.emit_opt_operand(operands.get(0).cloned())?;
                write!(self.output, ", ")?;
                self.emit_opt_operand(operands.get(1).cloned())?;
                write!(self.output, ")")?;
                Ok(())
            }
            OpStore => {
                write!(self.output, "Store(")?;
                self.emit_opt_operand(operands.get(0).cloned())?;
                write!(self.output, ", ")?;
                self.emit_opt_operand(operands.get(1).cloned())?;
                write!(self.output, ", ")?;
                self.emit_opt_operand(operands.get(2).cloned())?;
                write!(self.output, ")")?;
                Ok(())
            }
            OpNarrow(wd) => {
                write!(self.output, "Narrow{}(", wd)?;
                self.emit_opt_operand(operands.get(0).cloned())?;
                write!(self.output, ")")?;
                Ok(())
            }
            OpSignExt(wd) => {
                write!(self.output, "SignExt{}(", wd)?;
                self.emit_opt_operand(operands.get(0).cloned())?;
                write!(self.output, ")")?;
                Ok(())
            }
            OpZeroExt(wd) => {
                write!(self.output, "ZeroExt{}(", wd)?;
                self.emit_opt_operand(operands.get(0).cloned())?;
                write!(self.output, ")")?;
                Ok(())
            }
            _ => {
                radeco_warn!("unknown opcode: {:?}", opcode);
                write!(self.output, "{}(", opcode)?;
                self.emit_operand_list(operands)?;
                write!(self.output, ")")?;
                Ok(())
            }
        }
    }

    fn emit_binop(&mut self, op: &str, operands: &[NodeIndex]) -> fmt::Result {
        self.emit_opt_operand(operands.get(0).cloned())?;
        write!(self.output, " {} ", op)?;
        self.emit_opt_operand(operands.get(1).cloned())?;
        Ok(())
    }

    fn emit_opt_operand(&mut self, opt_operand: Option<NodeIndex>) -> fmt::Result {
        match opt_operand {
            Some(operand) => self.emit_operand(operand),
            None => log_emit_err!(self, "not enough operands"),
        }
    }

    fn emit_operand_list(&mut self, operands: &[NodeIndex]) -> fmt::Result {
        emit_list!(self.output, operands, |&n| self.emit_operand(n)?);
        Ok(())
    }

    fn emit_call(&mut self, call_node: NodeIndex) -> fmt::Result {
        if let Some(call_info) = utils::call_info(call_node, self.ssa) {
            let ret_regs = utils::call_rets(call_node, self.ssa);

            write!(self.output, "(")?;
            emit_list!(self.output, ret_regs, |(idx, (ret_node, ret_vt))| {
                self.emit_new_value(ret_node, ret_vt)?;
                write!(
                    self.output,
                    "${}",
                    self.ssa.regfile.get_name(idx).unwrap_or("mem"),
                )?;
            });

            write!(self.output, ") = CALL ")?;
            if self.ssa.is_constant(call_info.target) {
                self.emit_operand(call_info.target)?;
            } else {
                write!(self.output, "(")?;
                self.emit_operand(call_info.target)?;
                write!(self.output, ")")?;
            }
            write!(self.output, "(")?;

            emit_list!(self.output, call_info.register_args, |(idx, arg)| {
                write!(
                    self.output,
                    "${}=",
                    self.ssa.regfile.get_name(idx).unwrap_or("mem"),
                )?;
                self.emit_operand(arg)?;
            });

            write!(self.output, ")")?;
        } else {
            log_emit_err!(self, "call node has no target: {:?}", call_node)?;
        }
        Ok(())
    }

    fn emit_jump(&mut self, blk: NodeIndex) -> fmt::Result {
        if let Some(successor_blk) = self.ssa.unconditional_block(blk) {
            if let Some(selector) = self.ssa.selector_in(blk) {
                // indirect jump
                if self.ssa.exit_node().map_or(false, |en| en != successor_blk) {
                    radeco_warn!("successor of block with indirect jump wasn't exit_node");
                }
                write!(self.output, "JMP TO ")?;
                self.emit_operand(selector)?;
            } else {
                if self.ssa.exit_node().map_or(false, |en| en == successor_blk) {
                    // return
                    write!(self.output, "RETURN")?;
                } else {
                    // unconditional jump
                    write!(self.output, "JMP ")?;
                    self.emit_jump_tgt(successor_blk)?;
                }
            }
        } else {
            if let Some(blk_cond_info) = self.ssa.conditional_blocks(blk) {
                // conditional jump
                if let Some(selector) = self.ssa.selector_in(blk) {
                    write!(self.output, "JMP IF ")?;
                    self.emit_operand(selector)?;
                    write!(self.output, " ")?;
                    self.emit_jump_tgt(blk_cond_info.true_side)?;
                    write!(self.output, " ELSE ")?;
                    self.emit_jump_tgt(blk_cond_info.false_side)?;
                } else {
                    log_emit_err!(
                        self,
                        "block with conditional successors has no selector: {:?} ({:?})",
                        blk,
                        self.ssa.g[blk]
                    )?;
                }
            } else {
                // non-terminating
                write!(self.output, "UNREACHABLE")?;
            }
        }
        writeln!(self.output, "")?;
        Ok(())
    }

    fn emit_jump_tgt(&mut self, tgt: NodeIndex) -> fmt::Result {
        match self.ssa.g[tgt] {
            NodeData::BasicBlock(addr, _) => write!(self.output, "{}", addr),
            NodeData::DynamicAction => write!(self.output, "{}", "dynamic_action"),
            _ => log_emit_err!(
                self,
                "invalid jump target: {:?} ({:?})",
                tgt,
                self.ssa.g[tgt]
            ),
        }
    }

    fn emit_operand(&mut self, operand: NodeIndex) -> fmt::Result {
        match self.ssa.g[operand] {
            NodeData::Op(MOpcode::OpConst(c), _) => write!(self.output, "#x{:x}", c),
            _ => {
                let idx = self.value(operand);
                write!(self.output, "%{}", idx)
            }
        }
    }

    fn emit_new_value(&mut self, node: NodeIndex, vt: ValueInfo) -> fmt::Result {
        let idx = self.value(node);
        write!(self.output, "%{}: ", idx)?;
        self.emit_valueinfo(vt)?;
        write!(self.output, " = ")?;
        Ok(())
    }

    fn value(&mut self, node: NodeIndex) -> u64 {
        use std::collections::hash_map::Entry;
        match self.seen.entry(node) {
            Entry::Occupied(o) => *o.get(),
            Entry::Vacant(v) => {
                self.ctr += 1;
                *v.insert(self.ctr)
            }
        }
    }

    fn emit_valueinfo(&mut self, vt: ValueInfo) -> fmt::Result {
        let w = vt.width().get_width().unwrap_or(64);
        let is_reference = if vt.is_reference() {
            "(*)"
        } else if vt.is_scalar() {
            ""
        } else {
            "(*?)"
        };

        write!(self.output, "$Unknown{}{}", w, is_reference)
    }

    fn indent(&mut self, by: usize) -> fmt::Result {
        for _ in 0..by {
            self.output.write_str("    ")?;
        }
        Ok(())
    }
}
