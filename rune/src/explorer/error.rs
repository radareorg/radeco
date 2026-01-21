/// Represents a convenience alias for the Rune Explorer [Result](std::result::Result) type.
pub type ExplorerResult<T> = std::result::Result<T, ExplorerError>;

/// Represents error variants for the Rune Explorer.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ExplorerError {
    EmptyCommandQueue,
    IncompatibleCommand,
    InvalidBranch,
    UnknownBranch,
}

impl ExplorerError {
    /// Gets the string representation of the error.
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::EmptyCommandQueue => "explorer: Command queue is empty",
            Self::IncompatibleCommand => "explorer: Incompatible command",
            Self::InvalidBranch => "explorer: Invalid branch type found",
            Self::UnknownBranch => "explorer: Do not know which branch to take. Set a default",
        }
    }
}

impl std::fmt::Display for ExplorerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl std::error::Error for ExplorerError {}
