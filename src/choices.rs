use std::collections::HashSet;
use std::fmt::Display;

use crate::{string_vec_to_string, InvalidChoice};
use thiserror::Error;

#[derive(Error, Debug, PartialEq, Eq)]
pub enum ChoicesError {
    #[error("duplicate choice \"{0}\" found")]
    DuplicateChoice(String),
    #[error("zero or one choice given, required at least 2 unique choices")]
    InsufficientChoices,
    #[error("{0}")]
    InvalidChoice(InvalidChoice),
}

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub struct Choices(HashSet<Vec<String>>);

impl Choices {
    pub fn new(choices: &Vec<Vec<String>>) -> Result<Choices, ChoicesError> {
        if choices.len() < 2 {
            Err(ChoicesError::InsufficientChoices)
        } else {
            let mut seen_choices = HashSet::new();
            for choice in choices {
                if !seen_choices.insert(choice.clone()) {
                    return Err(ChoicesError::DuplicateChoice(string_vec_to_string(
                        &choice,
                        choice.len() != 1,
                    )));
                }
            }
            Ok(Choices(seen_choices.clone()))
        }
    }

    pub fn arg_value_in_choices(&self, raw_arg: &Vec<String>) -> Result<(), ChoicesError> {
        if self.0.contains(raw_arg) {
            Ok(())
        } else {
            let mut sorted_args = raw_arg.clone();
            sorted_args.sort();
            Err(ChoicesError::InvalidChoice(InvalidChoice(
                string_vec_to_string(&sorted_args, raw_arg.len() != 1),
                self.to_string(),
            )))
        }
    }
}

impl Display for Choices {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let choices_strs: Vec<String> = self
            .0
            .iter()
            .map(|x| string_vec_to_string(x, x.len() != 1))
            .collect();
        write!(f, "{}", string_vec_to_string(&choices_strs, true))
    }
}
