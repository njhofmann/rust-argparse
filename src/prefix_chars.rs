use std::collections::HashSet;
use thiserror::Error;

use crate::string_vec_to_string;

#[derive(Error, Debug, PartialEq, Eq)]
pub enum PrefixCharsError {
    #[error("no prefix characters were given")]
    EmptyPrefixChars,
    #[error("found unsupported prefix characters {0}")]
    IllegalPrefixChars(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PrefixChars((HashSet<char>, char));

impl Default for PrefixChars {
    fn default() -> Self {
        PrefixChars::new(Some("-")).unwrap()
    }
}

impl PrefixChars {
    pub fn default_char(&self) -> char {
        self.0 .1
    }

    pub fn new(chars: Option<&str>) -> Result<PrefixChars, PrefixCharsError> {
        match chars {
            None => Ok(PrefixChars((
                HashSet::from_iter::<Vec<char>>(vec!['-']),
                '-',
            ))),
            Some(x) => {
                let as_string = x.to_string();
                let mut chars = as_string.chars();
                let temp = HashSet::from_iter(chars.clone().into_iter());
                if temp.is_empty() {
                    Err(PrefixCharsError::EmptyPrefixChars)
                } else {
                    let mut illegal_chars: Vec<String> = temp
                        .iter()
                        .filter_map(|x| {
                            if x.is_whitespace() {
                                Some(x.to_string())
                            } else {
                                None
                            }
                        })
                        .collect();
                    illegal_chars.sort(); // for consistent tests
                    if illegal_chars.is_empty() {
                        Ok(PrefixChars((temp, chars.next().unwrap())))
                    } else {
                        Err(PrefixCharsError::IllegalPrefixChars(string_vec_to_string(
                            &illegal_chars,
                            false,
                        )))
                    }
                }
            }
        }
    }

    pub fn parse_string(&self, string: &str) -> (String, usize) {
        for char in self.0 .0.clone().into_iter() {
            let mut n_matches = 0;
            for string_char in string.chars() {
                if char == string_char {
                    n_matches += 1;
                } else {
                    break;
                }
            }
            if n_matches > 0 {
                let parsed_string = &string[n_matches..];
                return (parsed_string.to_string(), n_matches);
            }
        }
        return (string.to_string(), 0);
    }
}
