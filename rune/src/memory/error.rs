use crate::context::ContextError;
use crate::utils::UtilError;

/// Represents error variants for the rune memory module.
#[derive(Debug)]
pub enum MemoryError {
    MissingMap,
    MissingCurrentSegment,
    MissingSolverIndex,
    ReadSizeDiv8,
    WriteSizeDiv8,
    FileOpen,
    FileCreate,
    FileRead,
    FileWrite,
    JsonDecode,
    JsonEncode,
    MissingRegInfo,
    MissingBinInfo,
    MissingBinBits,
    MissingBinEndian,
    MissingOperand,
    Unimplemented,
    R2Pipe(r2pipe::Error),
}

impl std::fmt::Display for MemoryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingMap => write!(f, "memory: missing map"),
            Self::MissingCurrentSegment => write!(f, "memory: missing current segment"),
            Self::MissingSolverIndex => write!(f, "memory: missing solver index"),
            Self::ReadSizeDiv8 => write!(f, "memory: read size is not divisible by 8"),
            Self::WriteSizeDiv8 => write!(f, "memory: write size is not divisible by 8"),
            Self::FileOpen => write!(f, "memory: error opening file"),
            Self::FileCreate => write!(f, "memory: error creating file"),
            Self::FileRead => write!(f, "memory: error reading file"),
            Self::FileWrite => write!(f, "memory: error writing file"),
            Self::JsonDecode => write!(f, "memory: error decoding JSON"),
            Self::JsonEncode => write!(f, "memory: error encoding JSON"),
            Self::MissingRegInfo => write!(f, "memory: missing register info"),
            Self::MissingBinInfo => write!(f, "memory: missing binary info"),
            Self::MissingBinBits => write!(f, "memory: missing binary bits info"),
            Self::MissingBinEndian => write!(f, "memory: missing binary endianness info"),
            Self::MissingOperand => write!(f, "memory: missing operand"),
            Self::Unimplemented => write!(f, "memory: unimplemnted"),
            Self::R2Pipe(err) => write!(f, "memory: r2pipe: {err}"),
        }
    }
}

impl std::error::Error for MemoryError {}

impl From<UtilError> for MemoryError {
    fn from(err: UtilError) -> Self {
        match err {
            UtilError::ReadSizeDiv8 => Self::ReadSizeDiv8,
            UtilError::WriteSizeDiv8 => Self::WriteSizeDiv8,
            UtilError::FileOpen => Self::FileOpen,
            UtilError::FileCreate => Self::FileCreate,
            UtilError::FileRead => Self::FileRead,
            UtilError::FileWrite => Self::FileWrite,
            UtilError::JsonDecode => Self::JsonDecode,
            UtilError::JsonEncode => Self::JsonEncode,
            UtilError::MissingRegInfo => Self::MissingRegInfo,
            UtilError::MissingBinInfo => Self::MissingBinInfo,
            UtilError::MissingBinBits => Self::MissingBinBits,
            UtilError::MissingBinEndian => Self::MissingBinEndian,
            UtilError::MissingOperand => Self::MissingOperand,
            UtilError::R2Pipe(err) => Self::R2Pipe(err),
            _ => Self::Unimplemented,
        }
    }
}

impl From<ContextError> for MemoryError {
    fn from(err: ContextError) -> Self {
        match err {
            ContextError::Memory(err) => err,
            ContextError::Util(err) => err.into(),
            _ => Self::Unimplemented,
        }
    }
}

/// Convenience alias for the run memory module [Result](std::result::Result).
pub type MemoryResult<T> = std::result::Result<T, MemoryError>;
