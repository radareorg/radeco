//! Defines an AST to represent the textual IL. It is designed so that the parser actions in
//! [the parser] are fairly trivial and so that lowering to [`SSAStorage`] is fairly easy.
//!
//! [the parser]: ::middle::ir_reader::parser
//! [`SSAStorage`]: ::middle::ssa::ssastorage::SSAStorage

use middle::ir;
use std::fmt;

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub entry_reg_state: Vec<(NewValue, PhysReg)>,
    pub basic_blocks: Vec<BasicBlock>,
    pub exit_node: Option<ExitNode>,
    pub final_reg_state: Vec<(PhysReg, Operand)>,
}

#[derive(Debug)]
pub struct BasicBlock {
    pub addr: ir::MAddress,
    pub size: u64,
    pub ops: Vec<Operation>,
    pub term: Terminator,
}

#[derive(Debug)]
pub struct ExitNode {
    pub ops: Vec<Operation>,
}

#[derive(Debug)]
pub enum Terminator {
    Return,
    JmpUncond(ir::MAddress),
    JmpCond(Operand, ir::MAddress, ir::MAddress),
    JmpIndirect(Operand),
    Unreachable,
}

#[derive(Debug)]
pub enum Operation {
    Phi(NewValue, Vec<Operand>),
    Assign(Option<ir::MAddress>, NewValue, Expr),
    Call(Option<ir::MAddress>, Vec<CallRet>, Operand, Vec<CallArg>),
}

#[derive(Debug)]
pub struct CallRet {
    pub value: NewValue,
    pub reg: PhysReg,
}

#[derive(Debug)]
pub struct CallArg {
    pub formal: PhysReg,
    pub actual: Operand,
}

#[derive(Debug)]
pub enum Expr {
    Infix(Operand, InfixOp, Operand),
    Prefix(PrefixOp, Operand),
    Load(Operand, Operand),
    Store(Operand, Operand, Operand),
    Resize(ResizeType, WidthSpec, Operand),
}

#[derive(Debug)]
pub enum ResizeType {
    Narrow,
    SignExt,
    ZeroExt,
}

#[derive(Debug)]
pub enum Operand {
    ValueRef(ValueRef),
    Const(u64),
}

#[derive(Debug)]
pub enum PrefixOp {
    Not,
}

#[derive(Debug)]
pub enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Eq,
    Gt,
    Lt,
    Lsl,
    Lsr,
}

#[derive(Debug)]
pub struct NewValue(pub ValueRef, pub Type);

#[derive(Debug)]
pub struct Type(pub WidthSpec, pub RefSpec);

#[derive(Hash, PartialEq, Eq)]
pub struct ValueRef(pub u64);

#[derive(PartialEq, Eq)]
pub struct PhysReg(pub String);

#[derive(Debug)]
pub struct WidthSpec(pub u16);

#[derive(Debug)]
pub enum RefSpec {
    Scalar,
    Reference,
    Unknown,
}

impl fmt::Debug for ValueRef {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "%{}", self.0)
    }
}

impl fmt::Debug for PhysReg {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "${}", self.0)
    }
}
