use crate::context::ContextError;
use crate::memory::MemoryError;

/// Represents the error variants for the rune util module.
#[derive(Debug)]
pub enum UtilError {
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
    InvalidBase16,
    InvalidInteger,
    R2Pipe(r2pipe::Error),
}

impl std::fmt::Display for UtilError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ReadSizeDiv8 => write!(f, "util: read size is not divisible by 8"),
            Self::WriteSizeDiv8 => write!(f, "util: write size is not divisible by 8"),
            Self::FileOpen => write!(f, "util: error opening file"),
            Self::FileCreate => write!(f, "util: error creating file"),
            Self::FileRead => write!(f, "util: error reading file"),
            Self::FileWrite => write!(f, "util: error writing file"),
            Self::JsonDecode => write!(f, "util: error decoding JSON"),
            Self::JsonEncode => write!(f, "util: error encoding JSON"),
            Self::MissingRegInfo => write!(f, "util: missing register info"),
            Self::MissingBinInfo => write!(f, "util: missing binary info"),
            Self::MissingBinBits => write!(f, "util: missing binary bits info"),
            Self::MissingBinEndian => write!(f, "util: missing binary endianness info"),
            Self::MissingOperand => write!(f, "util: missing operand"),
            Self::Unimplemented => write!(f, "util: unimplemnted"),
            Self::InvalidBase16 => write!(f, "util: invalid base 16 integer"),
            Self::InvalidInteger => write!(f, "util: invalid integer"),
            Self::R2Pipe(err) => write!(f, "util: r2pipe: {err}"),
        }
    }
}

impl From<r2pipe::Error> for UtilError {
    fn from(err: r2pipe::Error) -> Self {
        Self::R2Pipe(err)
    }
}

impl From<ContextError> for UtilError {
    fn from(err: ContextError) -> Self {
        match err {
            ContextError::Util(err) => err,
            ContextError::Memory(err) => err.into(),
            _ => Self::Unimplemented,
        }
    }
}
impl From<MemoryError> for UtilError {
    fn from(err: MemoryError) -> Self {
        match err {
            MemoryError::ReadSizeDiv8 => Self::ReadSizeDiv8,
            MemoryError::WriteSizeDiv8 => Self::WriteSizeDiv8,
            MemoryError::FileOpen => Self::FileOpen,
            MemoryError::FileCreate => Self::FileCreate,
            MemoryError::FileRead => Self::FileRead,
            MemoryError::FileWrite => Self::FileWrite,
            MemoryError::JsonDecode => Self::JsonDecode,
            MemoryError::JsonEncode => Self::JsonEncode,
            MemoryError::MissingRegInfo => Self::MissingRegInfo,
            MemoryError::MissingBinInfo => Self::MissingBinInfo,
            MemoryError::MissingBinBits => Self::MissingBinBits,
            MemoryError::MissingBinEndian => Self::MissingBinEndian,
            MemoryError::MissingOperand => Self::MissingOperand,
            MemoryError::R2Pipe(err) => Self::R2Pipe(err),
            _ => Self::Unimplemented,
        }
    }
}

impl std::error::Error for UtilError {}

/// Convenience alias for the rune util [Result](std::result::Result).
pub type UtilResult<T> = std::result::Result<T, UtilError>;
