use std::fmt::Display;

use thiserror::Error;

const HELP_STRING: &str = "help";
const APPEND_CONST_STRING: &str = "append_const";
const STORE_CONST_STRING: &str = "store_const";
const STORE_STRING: &str = "store";
const VERSION_STRING: &str = "version";
const EXTEND_STRING: &str = "extend";
const APPEND_STRING: &str = "append";
const STORE_TRUE_STRING: &str = "store_true";
const STORE_FALSE_STRING: &str = "store_false";
const COUNT_STRING: &str = "count";

#[derive(Error, Debug, PartialEq, Eq)]
pub enum ActionError {
    #[error("constant given for a action that isn't'append_const' or 'store_const'")]
    MissingConstant,
    #[error("arguments with 'append_const' must be given a destination")]
    InvalidActionForConstant,
    #[error("arguments with 'append_const' must be given a destination")]
    MissingDestination,
    #[error("destination given for a non-'append_const' action")]
    InvalidActionForDestination,
    #[error("arguments with 'version' action must be given a version")]
    MissingVersion,
    #[error("version given for a non-'version' action")]
    InvalidActionForVersion,
    #[error("positional argument given non-store action {0}")]
    PositionalArgumentGivenNonStoreAction(String),
    #[error("{0} is not a supported Action")]
    InvalidAction(String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Action {
    Store(Vec<String>),
    StoreConst(Vec<String>),
    Append(Vec<String>),
    AppendConst(Vec<String>), // this is handled in argumentparser
    Count(usize),
    Help,
    Version(String),
    Extend(Vec<String>),
}

impl Action {
    pub fn new(
        action: &str,
        is_flag_argument: bool,
        constant: Option<Vec<String>>,
        version: Option<&str>,
        dest: Option<&str>,
    ) -> Result<Action, ActionError> {
        match (action, &constant) {
            (STORE_CONST_STRING, None) | (APPEND_CONST_STRING, None) => {
                Err(ActionError::MissingConstant)
            }
            (STORE_CONST_STRING, Some(_)) | (APPEND_CONST_STRING, Some(_)) | (_, None) => Ok(()),
            (_, Some(_)) => Err(ActionError::InvalidActionForConstant),
        }?;

        match (action, dest) {
            (APPEND_CONST_STRING, None) => Err(ActionError::MissingDestination),
            (APPEND_CONST_STRING, Some(_)) | (_, None) => Ok(()),
            (_, Some(_)) => Err(ActionError::InvalidActionForDestination),
        }?;

        match (action, version) {
            (VERSION_STRING, None) => Err(ActionError::MissingVersion),
            (VERSION_STRING, Some(x)) if x.is_empty() => Err(ActionError::MissingVersion),
            (_, Some(_)) => Err(ActionError::InvalidActionForVersion),
            _ => Ok(()),
        }?;

        let action = match action {
            STORE_STRING => Ok(Action::Store(vec![])),
            STORE_CONST_STRING => Ok(Action::StoreConst(constant.unwrap())),
            STORE_TRUE_STRING => Ok(Action::StoreConst(vec!["true".to_string()])),
            STORE_FALSE_STRING => Ok(Action::StoreConst(vec!["false".to_string()])),
            APPEND_STRING => Ok(Action::Append(vec![])),
            APPEND_CONST_STRING => Ok(Action::AppendConst(constant.unwrap())),
            COUNT_STRING => Ok(Action::Count(0)),
            HELP_STRING => Ok(Action::Help),
            VERSION_STRING => Ok(Action::Version(version.unwrap().to_string())),
            EXTEND_STRING => Ok(Action::Extend(vec![])),
            x => Err(ActionError::InvalidAction(x.to_string())),
        }?;

        match action {
            Action::Store(_) if !is_flag_argument => Err(
                ActionError::PositionalArgumentGivenNonStoreAction((&action).to_string()),
            ),
            action => Ok(action),
        }
    }
}

impl Display for Action {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let result = match self {
            Action::Store(_) => STORE_STRING,
            Action::StoreConst(c) => {
                if c.len() == 1 {
                    match c.first().unwrap().as_str() {
                        "true" => STORE_TRUE_STRING,
                        "false" => STORE_FALSE_STRING,
                        _ => STORE_CONST_STRING,
                    }
                } else {
                    STORE_CONST_STRING
                }
            }
            Action::Append(_) => APPEND_STRING,
            Action::AppendConst(_) => APPEND_CONST_STRING,
            Action::Count(_) => COUNT_STRING,
            Action::Help => HELP_STRING,
            Action::Version(_) => VERSION_STRING,
            Action::Extend(_) => EXTEND_STRING,
        };
        write!(f, "{}", result)
    }
}
