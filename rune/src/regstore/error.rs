/// Represents error variants for the rune regstore.
#[derive(Debug)]
pub enum RegstoreError {
    UnknownRegister,
    UnsetRegister,
    MissingEOld,
}

impl std::fmt::Display for RegstoreError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnknownRegister => write!(f, "regstore: unknown register"),
            Self::UnsetRegister => write!(
                f,
                "regstore: unset register - Undefined Behavior. Consider setting an initial value \
                 before use!"
            ),
            Self::MissingEOld => write!(f, "regstore: attempting to access e_old before it is set"),
        }
    }
}

impl std::error::Error for RegstoreError {}

/// Convenience alias for the rune regstore [Result](std::result::Result).
pub type RegstoreResult<T> = std::result::Result<T, RegstoreError>;
