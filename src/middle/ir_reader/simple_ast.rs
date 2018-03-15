//! Defines an AST to represent the textual IL. It is designed so that the parser actions in
//! [the parser] are fairly trivial and so that lowering to [`SSAStorage`] is fairly easy.
//!
//! [the parser]: ::middle::ir_reader::parser
//! [`SSAStorage`]: ::middle::ssa::ssastorage::SSAStorage

use middle::ir;

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub register_list: Vec<PhysReg>,
    pub basic_blocks: Vec<BasicBlock>,
    pub exit_node: Option<ExitNode>,
    pub final_reg_state: Vec<(PhysReg, Operand)>,
}

#[derive(Debug)]
pub struct BasicBlock {
    pub addr: ir::MAddress,
    pub ops: Vec<Operation>,
    pub jump: Option<Terminator>,
}

#[derive(Debug)]
pub struct ExitNode {
    pub ops: Vec<Operation>,
}

#[derive(Debug)]
pub enum Operation {
    Phi(ValueRef, Type, Vec<Operand>),
    Assign(Option<ir::MAddress>, ValueRef, Type, Expr),
    Call(Option<ir::MAddress>, Operand, Vec<CallArg>),
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
pub struct CallArg {
    pub formal: PhysReg,
    pub actual: Operand,
}

#[derive(Debug)]
pub enum Operand {
    Comment(String),
    ValueRef(ValueRef),
    Const(u64),
}

#[derive(Debug)]
pub enum Terminator {
    JmpUncond(ir::MAddress),
    JmpCond(Operand, ir::MAddress, Option<ir::MAddress>),
    JmpIndirect(Operand),
    Unreachable,
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
pub struct Type(pub WidthSpec, pub RefSpec);

#[derive(Hash, PartialEq, Eq, Debug)]
pub struct ValueRef(pub u64);

#[derive(PartialEq, Eq, Debug)]
pub struct PhysReg(pub String);

#[derive(Debug)]
pub struct WidthSpec(pub u16);

#[derive(Debug)]
pub enum RefSpec {
    Scalar,
    Reference,
    Unknown,
}
