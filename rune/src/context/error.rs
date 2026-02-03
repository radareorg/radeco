use crate::memory::MemoryError;
use crate::regstore::RegstoreError;
use crate::utils::UtilError;

/// Represents the error variants for the rune context module.
#[derive(Debug)]
pub enum ContextError {
    MissingEOld,
    MissingECur,
    Unsat,
    Memory(MemoryError),
    Util(UtilError),
    Regstore(RegstoreError),
    Unimplemented,
}

impl std::fmt::Display for ContextError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingECur => write!(f, "context: e_cur accessed before being set"),
            Self::MissingEOld => write!(f, "context: e_old accessed before being set"),
            Self::Unsat => write!(f, "no satisfiable solution"),
            Self::Util(err) => write!(f, "context: {err}"),
            Self::Memory(err) => write!(f, "context: {err}"),
            Self::Regstore(err) => write!(f, "context: {err}"),
            Self::Unimplemented => write!(f, "context: unimplemented"),
        }
    }
}

impl std::error::Error for ContextError {}

impl From<RegstoreError> for ContextError {
    fn from(err: RegstoreError) -> Self {
        Self::Regstore(err)
    }
}

impl From<MemoryError> for ContextError {
    fn from(err: MemoryError) -> Self {
        Self::Memory(err)
    }
}

impl From<UtilError> for ContextError {
    fn from(err: UtilError) -> Self {
        Self::Util(err)
    }
}

/// Convenience alias for the rune context module [Result](std::result::Result).
pub type ContextResult<T> = std::result::Result<T, ContextError>;
