//! Defines traits and structs that perform the actual symbolic emulation.

use crate::context::ContextError;
use crate::explorer::ExplorerError;
use crate::memory::MemoryError;

pub mod breakpt;
pub mod hook;
pub mod rune;

#[derive(Debug)]
pub enum EngineError {
    Undefined,
    IncorrectOperand,
    Explorer(ExplorerError),
    Memory(MemoryError),
    Context(ContextError),
    Unimplemented,
    Unreachable,
    InvalidOpCode,
    FetchOperand,
    Tokenizing,
    MissingLhs,
    MissingRhs,
    MissingNextInstr,
    MissingEsil,
    MissingOpInfoSize,
}

impl From<ContextError> for EngineError {
    fn from(err: ContextError) -> Self {
        match err {
            ContextError::Memory(err) => Self::Memory(err),
            _ => Self::Context(err),
        }
    }
}

impl From<ExplorerError> for EngineError {
    fn from(err: ExplorerError) -> Self {
        Self::Explorer(err)
    }
}

impl From<MemoryError> for EngineError {
    fn from(err: MemoryError) -> Self {
        Self::Memory(err)
    }
}

impl std::fmt::Display for EngineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Undefined => write!(f, "engine: undefined"),
            Self::IncorrectOperand => write!(f, "engine: incorrect operand"),
            Self::Explorer(err) => write!(f, "engine: {err}"),
            Self::Memory(err) => write!(f, "engine: {err}"),
            Self::Context(err) => write!(f, "engine: {err}"),
            Self::Unimplemented => write!(f, "engine: unimplemented"),
            Self::Unreachable => write!(f, "engine: unreachable"),
            Self::InvalidOpCode => write!(f, "engine: invalid opcode found"),
            Self::FetchOperand => write!(f, "engine: error fetching operands"),
            Self::Tokenizing => write!(f, "engine: error tokenizing"),
            Self::MissingLhs => write!(f, "engine: missing LHS operand"),
            Self::MissingRhs => write!(f, "engine: missing RHS operand"),
            Self::MissingNextInstr => write!(f, "engine: missing next instruction"),
            Self::MissingEsil => write!(f, "engine: missing ESIL"),
            Self::MissingOpInfoSize => write!(f, "engine: missing OpInfo size"),
        }
    }
}

impl std::error::Error for EngineError {}

pub type EngineResult<T> = Result<T, EngineError>;

pub trait Engine: Sized {
    fn run(&mut self) -> EngineResult<()>;
}

pub trait Configure {
    type For: Engine;
    fn configure(_: &mut Self::For) -> EngineResult<()> {
        Ok(())
    }
}
