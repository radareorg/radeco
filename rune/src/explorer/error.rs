use crate::context::ContextError;
use crate::memory::MemoryError;
use crate::regstore::RegstoreError;

/// Represents a convenience alias for the Rune Explorer [Result](std::result::Result) type.
pub type ExplorerResult<T> = std::result::Result<T, ExplorerError>;

/// Represents error variants for the Rune Explorer.
#[derive(Debug)]
pub enum ExplorerError {
    EmptyCommandQueue,
    IncompatibleCommand,
    ReadCommand,
    InvalidBranch,
    UnknownBranch,
    InvalidOpCode,
    InvalidBase16,
    Unimplemented,
    Memory(MemoryError),
    Regstore(RegstoreError),
}

impl std::fmt::Display for ExplorerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EmptyCommandQueue => write!(f, "explorer: Command queue is empty"),
            Self::IncompatibleCommand => write!(f, "explorer: Incompatible command"),
            Self::ReadCommand => write!(f, "explorer: reading command"),
            Self::InvalidBranch => write!(f, "explorer: Invalid branch type found"),
            Self::UnknownBranch => write!(
                f,
                "explorer: Do not know which branch to take. Set a default"
            ),
            Self::InvalidOpCode => write!(f, "explorer: Invalid opcode found"),
            Self::InvalidBase16 => write!(f, "explorer: invalid base16 integer"),
            Self::Unimplemented => write!(f, "explorer: unimplemented"),
            Self::Memory(err) => write!(f, "explorer: {err}"),
            Self::Regstore(err) => write!(f, "explorer: {err}"),
        }
    }
}

impl std::error::Error for ExplorerError {}

impl From<MemoryError> for ExplorerError {
    fn from(err: MemoryError) -> Self {
        Self::Memory(err)
    }
}

impl From<ContextError> for ExplorerError {
    fn from(err: ContextError) -> Self {
        match err {
            ContextError::Memory(err) => Self::Memory(err),
            ContextError::Regstore(err) => Self::Regstore(err),
            _ => Self::Unimplemented,
        }
    }
}
