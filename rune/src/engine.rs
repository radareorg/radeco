//! Defines traits and structs that perform the actual symbolic emulation.

use crate::explorer::ExplorerError;

pub mod breakpt;
pub mod hook;
pub mod rune;

#[derive(Clone, Copy, Debug)]
pub enum EngineError {
    Undefined,
    IncorrectOperand,
    Explorer(ExplorerError),
}

impl EngineError {
    /// Gets the string representation of the error.
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Undefined => "engine: undefined",
            Self::IncorrectOperand => "engine: incorrect operand",
            Self::Explorer(err) => err.as_str(),
        }
    }
}

impl From<ExplorerError> for EngineError {
    fn from(err: ExplorerError) -> Self {
        Self::Explorer(err)
    }
}

impl std::fmt::Display for EngineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

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
