use std::fmt::Display;
use thiserror::Error;

#[derive(Debug, Error, PartialEq, Eq)]
pub enum ConflictHandlingStrategyError {
    #[error("{0} is not a supported strategy for resolving conflicting arguments - error and override / resolve are supported")]
    UnsupportedConflictHandlingStrategy(String),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ConflictHandlingStrategy {
    Error,
    Override, // instead of resolve
}

impl ConflictHandlingStrategy {
    pub fn new(
        strategy: Option<&str>,
    ) -> Result<ConflictHandlingStrategy, ConflictHandlingStrategyError> {
        match strategy {
            None | Some("error") => Ok(ConflictHandlingStrategy::Error),
            Some("override") | Some("resolve") => Ok(ConflictHandlingStrategy::Override),
            Some(s) => Err(
                ConflictHandlingStrategyError::UnsupportedConflictHandlingStrategy(s.to_string()),
            ),
        }
    }

    pub fn is_override(&self) -> bool {
        match self {
            Self::Override => true,
            _ => false,
        }
    }
}

impl Display for ConflictHandlingStrategy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ConflictHandlingStrategy::Error => "error",
                ConflictHandlingStrategy::Override => "resolve",
            }
        )
    }
}
