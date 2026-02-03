/// Represents the error variants for rune stream module.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StreamError {
    OpenR2,
    FileOpen,
    FileRead,
    JsonDecode,
}

impl StreamError {
    /// Gets the string representation of the [StreamError].
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::OpenR2 => "stream: unable to open R2",
            Self::FileOpen => "stream: error opening file",
            Self::FileRead => "stream: error reading file",
            Self::JsonDecode => "stream: error decoding JSON",
        }
    }
}

impl std::fmt::Display for StreamError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl std::error::Error for StreamError {}

/// Convenience alias for the rune stream module [Result](std::result::Result).
pub type StreamResult<T> = std::result::Result<T, StreamError>;
