use thiserror::Error;

use crate::{argument::ActionError, InvalidChoice};

#[derive(Error, Debug, PartialEq, Eq)]
pub enum ArgumentError {
    #[error("{0}")]
    ActionError(ActionError),
    #[error("required argument {0} given default value {1}")]
    RequiredArgumentDefaultValueGiven(String, String),
    #[error("given default value has length {0} but expected {1} arguments")]
    InvalidDefaultValueLength(usize, String),
    #[error("single choice given, required at least 2 unique choices")]
    EmptyOrSingleChoice,
    #[error("given choice \"{0}\" value has length {1} but expected {2} arguments")]
    InvalidChoiceLength(String, usize, String),
    #[error("duplicate choice \"{0}\" found")]
    DuplicateChoice(String),
    #[error("found duplicate argument name values {0}")]
    DuplicateArgumentNameValues(String),
    #[error("no argument names given")]
    EmptyArgumentName,
    #[error("duplicate argument name {0} found")]
    DuplicateArgumentName(String),
    #[error("required is reserved for flag arguments")]
    RequiredMarkedForPositionalArgument,
    #[error("given a mix of positional arguments & flag argument names")]
    MixedArguments,
    #[error("non-required argument {0} missing  adefault value")]
    NonRequiredArgumentNotGivenDefaultValue(String),
    #[error("{0}")]
    InvalidChoice(InvalidChoice),
    #[error("start of NArgs::Range {0} is >= than end {1}")]
    InvalidRangeSize(usize, usize),
    #[error("duplicated help argument added under argument {0}")]
    DuplicateHelpArgument(String),
    #[error("duplicated help argument added under argument {0}")]
    DuplicateVersionArgument(String),
    #[error("no prefix characters were given")]
    EmptyPrefixChars,
    #[error("found unsupported prefix characters {0}")]
    IllegalPrefixChars(String),
    #[error("nargs given for action {0} which doesn't expect argument values")]
    InvalidActionForNArgs(String),
    #[error("default value given to action {0}, only allowed for append & store")]
    DefaultGivenForUnsupportedAction(String),
    #[error("choices given to unsupported action {0}, choices only supported for actions store, append, & extend")]
    ChoicesGivenForUnsupportedAction(String),
    #[error("given multiple positional argument names")]
    MultiplePositionalArgumentNames,
    #[error("required given to unsupported action")]
    RequiredGivenToUnsupportedAction,
}
