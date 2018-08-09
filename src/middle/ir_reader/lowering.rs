//! (see [`lower_simpleast`](lower_simpleast))

use super::simple_ast as sast;
use middle::ir;
use middle::ir::MOpcode as IrOpcode;
use middle::regfile::RegisterId;
use middle::ssa::cfg_traits::{CFGMod, CFG};
use middle::ssa::ssa_traits::{SSAMod, ValueInfo, SSA};
use middle::ssa::ssastorage::SSAStorage;

use std::collections::HashMap;
use std::error;
use std::fmt;
use std::fmt::Write;
use std::mem;

pub type Result<T> = ::std::result::Result<T, LoweringError>;

/// Lowers [AST](sast) into the provided [`SSAStorage`]
pub fn lower_simpleast<'a>(ssa: &'a mut SSAStorage, sfn: sast::Function) -> Result<()> {
    LowerSsa::new(ssa)?.lower_function(sfn)
}

#[derive(Debug)]
pub enum LoweringError {
    /// If an operation on the [`SSAStorage`] fails
    SsaError,
    /// If the AST was invalid somehow
    InvalidAst(String),
}

type SSABlock = <SSAStorage as CFG>::ActionRef;
type SSAValue = <SSAStorage as SSA>::ValueRef;

/// from [`ssaconstructor`](::frontend::ssaconstructor)
const FALSE_EDGE: u8 = 0;
const TRUE_EDGE: u8 = 1;
const UNCOND_EDGE: u8 = 2;

struct LowerSsa<'a> {
    ssa: &'a mut SSAStorage,
    entry_node: SSABlock,
    exit_node: SSABlock,
    blocks: HashMap<ir::MAddress, SSABlock>,
    values: HashMap<sast::ValueRef, SSAValue>,
    fw_ref_values: HashMap<sast::ValueRef, SSAValue>,
    phi_operands: Vec<(SSAValue, Vec<sast::Operand>)>,
}

impl<'a> LowerSsa<'a> {
    fn new(ssa: &'a mut SSAStorage) -> Result<Self> {
        let entry_node = if let Some(en) = ssa.entry_node() {
            en
        } else {
            let entry_node = ssa.insert_block(ir::MAddress::new(0, 0))?;
            ssa.set_entry_node(entry_node);
            entry_node
        };
        let exit_node = if let Some(en) = ssa.exit_node() {
            en
        } else {
            let exit_node = ssa.insert_dynamic()?;
            ssa.set_exit_node(exit_node);
            exit_node
        };
        Ok(LowerSsa {
            ssa,
            entry_node,
            exit_node,
            blocks: HashMap::new(),
            values: HashMap::new(),
            fw_ref_values: HashMap::new(),
            phi_operands: Vec::new(),
        })
    }

    fn lower_function(mut self, sfn: sast::Function) -> Result<()> {
        self.lower_entry_reg_state(sfn.entry_reg_state)?;

        let mut first = true;
        for sbb in sfn.basic_blocks {
            let bb = self.lower_basicblock(sbb)?;
            if first {
                self.ssa
                    .insert_control_edge(self.entry_node, bb, UNCOND_EDGE);
                first = false;
            }
        }

        if let Some(sen) = sfn.exit_node {
            self.lower_exit_node(sen)?;
        }

        self.lower_final_reg_state(sfn.final_reg_state)?;

        for (phi, sops) in mem::replace(&mut self.phi_operands, Vec::new()) {
            for sop in sops.into_iter().rev() {
                let op = self.lower_operand(sop)?;
                self.ssa.phi_use(phi, op);
            }
        }

        if !self.fw_ref_values.is_empty() {
            let undefined_values: Vec<_> = self.fw_ref_values.keys().collect();
            return Err(LoweringError::InvalidAst(format!(
                "values were used but not defined: {:?}",
                undefined_values
            )));
        }

        Ok(())
    }

    fn lower_entry_reg_state(
        &mut self,
        sregstate: Vec<(sast::NewValue, sast::PhysReg)>,
    ) -> Result<()> {
        let regstate = self.ssa.registers_in(self.entry_node)?;
        for (sast::NewValue(vr, ty), sreg) in sregstate {
            let regid = self.index_of_reg(&sreg)?;
            let val = self.ssa.insert_comment(lower_valueinfo(ty), sreg.0)?;
            self.ssa.op_use(regstate, regid.to_u8(), val);
            self.insert_new_value(vr, val)?;
        }
        Ok(())
    }

    fn lower_final_reg_state(
        &mut self,
        sregstate: Vec<(sast::PhysReg, sast::Operand)>,
    ) -> Result<()> {
        let regstate = self.ssa.registers_in(self.exit_node)?;
        for (sreg, sop) in sregstate {
            let regid = self.index_of_reg(&sreg)?;
            let op = self.lower_operand(sop)?;
            self.ssa.op_use(regstate, regid.to_u8(), op);
        }
        Ok(())
    }

    fn lower_basicblock(&mut self, sbb: sast::BasicBlock) -> Result<SSABlock> {
        let bb = self.block_at(sbb.addr)?;
        self.ssa.set_block_size(bb, sbb.size);

        for sop in sbb.ops {
            let (res, opt_op_addr) = self.lower_operation(sop)?;
            let op_addr = opt_op_addr.unwrap_or(sbb.addr);
            self.ssa.insert_into_block(res, bb, op_addr);
        }

        match sbb.term {
            sast::Terminator::Return => {
                self.ssa.insert_control_edge(bb, self.exit_node, UNCOND_EDGE);
            }
            sast::Terminator::JmpUncond(tgt) => {
                let tgt_bb = self.block_at(tgt)?;
                self.ssa.insert_control_edge(bb, tgt_bb, UNCOND_EDGE);
            }
            sast::Terminator::JmpCond(sel_sop, if_tgt, else_tgt) => {
                let sel_op = self.lower_operand(sel_sop)?;
                let if_bb = self.block_at(if_tgt)?;
                let else_bb = self.block_at(else_tgt)?;
                self.ssa.set_selector(sel_op, bb);
                self.ssa.insert_control_edge(bb, if_bb, TRUE_EDGE);
                self.ssa.insert_control_edge(bb, else_bb, FALSE_EDGE);
            }
            sast::Terminator::JmpIndirect(sel_sop) => {
                let sel_op = self.lower_operand(sel_sop)?;
                self.ssa.set_selector(sel_op, bb);
                self.ssa
                    .insert_control_edge(bb, self.exit_node, UNCOND_EDGE);
            }
            sast::Terminator::Unreachable => {
                // nothing to do
            }
        }

        Ok(bb)
    }

    fn lower_exit_node(&mut self, sen: sast::ExitNode) -> Result<()> {
        let node_addr = self.ssa.starting_address(self.exit_node)?;
        for sop in sen.ops {
            let (res, opt_op_addr) = self.lower_operation(sop)?;
            let op_addr = opt_op_addr.unwrap_or(node_addr);
            self.ssa.insert_into_block(res, self.exit_node, op_addr);
        }

        Ok(())
    }

    fn lower_operation(
        &mut self,
        sopn: sast::Operation,
    ) -> Result<(SSAValue, Option<ir::MAddress>)> {
        Ok(match sopn {
            sast::Operation::Phi(sast::NewValue(vr, ty), sops) => {
                let vi = lower_valueinfo(ty);
                let res = self.ssa.insert_phi(vi)?;
                // replacing forward refs with their values changes the order of
                // phi node operands, so we wait until forward refs have been
                // resolved before adding operands
                self.phi_operands.push((res, sops));
                self.insert_new_value(vr, res)?;
                (res, None)
            }

            sast::Operation::Assign(opt_addr, sast::NewValue(vr, ty), sexpr) => {
                let vi = lower_valueinfo(ty);
                let (opcode, sops) = match sexpr {
                    sast::Expr::Infix(sop0, sopcode, sop1) => {
                        (lower_infix_op(sopcode), vec![sop0, sop1])
                    }
                    sast::Expr::Prefix(sopcode, sop0) => (lower_prefix_op(sopcode), vec![sop0]),
                    sast::Expr::Load(sop0, sop1) => (IrOpcode::OpLoad, vec![sop0, sop1]),
                    sast::Expr::Store(sop0, sop1, sop2) => {
                        (IrOpcode::OpStore, vec![sop0, sop1, sop2])
                    }
                    sast::Expr::Resize(rst, ws, sop0) => (lower_resize_op(rst, ws), vec![sop0]),
                };
                let res = self.ssa.insert_op(opcode, vi, None)?;
                for (i, sop) in sops.into_iter().enumerate() {
                    let op = self.lower_operand(sop)?;
                    self.ssa.op_use(res, i as u8, op);
                }
                self.insert_new_value(vr, res)?;
                (res, opt_addr)
            }

            sast::Operation::Call(opt_addr, srets, tgt, sargs) => {
                let res = self.ssa.insert_op(IrOpcode::OpCall, scalar!(0), None)?;
                let tgt_op = self.lower_operand(tgt)?;
                self.ssa.op_use(res, 0, tgt_op);
                for sarg in sargs {
                    let regid = self.index_of_reg(&sarg.formal)?;
                    let op = self.lower_operand(sarg.actual)?;
                    self.ssa.op_use(res, regid.to_u8() + 1, op);
                }
                for sret in srets {
                    let regid = self.index_of_reg(&sret.reg)?;
                    let mut comment = sret.reg.0;
                    if let Some(addr) = opt_addr {
                        write!(comment, "@{}", addr).unwrap();
                    }
                    let val = self
                        .ssa
                        .insert_comment(lower_valueinfo(sret.value.1), comment)?;
                    self.ssa.op_use(val, regid.to_u8(), res);
                    self.insert_new_value(sret.value.0, val)?;
                }
                (res, opt_addr)
            }
        })
    }

    fn lower_operand(&mut self, sop: sast::Operand) -> Result<SSAValue> {
        use std::collections::hash_map::Entry;
        Ok(match sop {
            sast::Operand::ValueRef(r) => {
                if let Some(x) = self.values.get(&r).cloned() {
                    x
                } else {
                    match self.fw_ref_values.entry(r) {
                        Entry::Occupied(o) => *o.get(),
                        Entry::Vacant(v) => {
                            *v.insert(self.ssa.insert_undefined(ValueInfo::new_unresolved(
                                ir::WidthSpec::Unknown,
                            ))?)
                        }
                    }
                }
            }
            sast::Operand::Const(v) => self.ssa.insert_const(v)?,
        })
    }

    fn insert_new_value(&mut self, vr: sast::ValueRef, val: SSAValue) -> Result<()> {
        use std::collections::hash_map::Entry;
        if let Some(fw_ref_val) = self.fw_ref_values.remove(&vr) {
            self.ssa.replace_value(fw_ref_val, val);
        }

        match self.values.entry(vr) {
            Entry::Vacant(v) => {
                v.insert(val);
            }
            Entry::Occupied(o) => {
                return Err(LoweringError::InvalidAst(format!(
                    "value defined twice: {:?}",
                    o.key()
                )))
            }
        }

        Ok(())
    }

    fn block_at(&mut self, at: ir::MAddress) -> Result<SSABlock> {
        use std::collections::hash_map::Entry;
        // can't use `or_insert_with` because `ssa.insert_block` may fail
        Ok(*match self.blocks.entry(at) {
            Entry::Occupied(o) => o.into_mut(),
            Entry::Vacant(v) => v.insert(self.ssa.insert_block(at)?),
        })
    }

    fn index_of_reg(&self, sreg: &sast::PhysReg) -> Result<RegisterId> {
        if sreg.0 == "mem" {
            Ok(self.ssa.regfile.mem_id())
        } else {
            self.ssa
                .regfile
                .register_id_by_name(&sreg.0)
                .ok_or_else(|| {
                    LoweringError::InvalidAst(format!("no physical register: {:?}", sreg))
                })
        }
    }
}

fn lower_infix_op(siop: sast::InfixOp) -> IrOpcode {
    match siop {
        sast::InfixOp::Add => IrOpcode::OpAdd,
        sast::InfixOp::Sub => IrOpcode::OpSub,
        sast::InfixOp::Mul => IrOpcode::OpMul,
        sast::InfixOp::Div => IrOpcode::OpDiv,
        sast::InfixOp::Mod => IrOpcode::OpMod,
        sast::InfixOp::And => IrOpcode::OpAnd,
        sast::InfixOp::Or => IrOpcode::OpOr,
        sast::InfixOp::Xor => IrOpcode::OpXor,
        sast::InfixOp::Eq => IrOpcode::OpEq,
        sast::InfixOp::Gt => IrOpcode::OpGt,
        sast::InfixOp::Lt => IrOpcode::OpLt,
        sast::InfixOp::Lsl => IrOpcode::OpLsl,
        sast::InfixOp::Lsr => IrOpcode::OpLsr,
    }
}

fn lower_prefix_op(spop: sast::PrefixOp) -> IrOpcode {
    match spop {
        sast::PrefixOp::Not => IrOpcode::OpNot,
    }
}

fn lower_resize_op(srst: sast::ResizeType, sws: sast::WidthSpec) -> IrOpcode {
    match srst {
        sast::ResizeType::Narrow => IrOpcode::OpNarrow(sws.0),
        sast::ResizeType::SignExt => IrOpcode::OpSignExt(sws.0),
        sast::ResizeType::ZeroExt => IrOpcode::OpZeroExt(sws.0),
    }
}

fn lower_valueinfo(sty: sast::Type) -> ValueInfo {
    let ws = ir::WidthSpec::Known((sty.0).0);
    match sty.1 {
        sast::RefSpec::Scalar => ValueInfo::new_scalar(ws),
        sast::RefSpec::Reference => ValueInfo::new_reference(ws),
        sast::RefSpec::Unknown => ValueInfo::new_unresolved(ws),
    }
}

/// [`SSAStorage`][SSAStorage] methods return `Option`,
/// so we convert `None`s into [`SsaError`][LoweringError::SsaError]
impl From<::std::option::NoneError> for LoweringError {
    fn from(_: ::std::option::NoneError) -> Self {
        LoweringError::SsaError
    }
}

impl error::Error for LoweringError {
    fn description(&self) -> &str {
        match *self {
            LoweringError::SsaError => "could not perform an `SSAStorage` operation",
            LoweringError::InvalidAst(_) => "invalid ast",
        }
    }
}

impl fmt::Display for LoweringError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LoweringError::SsaError => write!(f, "could not perform an `SSAStorage` operation"),
            LoweringError::InvalidAst(ref s) => write!(f, "invalid ast: {}", s),
        }
    }
}
