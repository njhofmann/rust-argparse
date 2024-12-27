use std::fmt::Display;

use argument::Argument;

pub mod argument;
pub mod argument_error;
mod argument_name;
pub mod argument_parser;
pub mod parse_result;
use thiserror::Error;

#[derive(Error, Debug, PartialEq, Eq)]
#[error("value \"{0}\" is not one of supported values {1}")]
pub struct InvalidChoice(String, String);

fn string_vec_to_string<T: Display>(vec: &Vec<T>, add_parans: bool) -> String {
    if vec.is_empty() {
        (if add_parans { "[]" } else { "" }).to_string()
    } else {
        let mut builder = vec[0].to_string();

        if vec.len() > 1 {
            for vec_str in vec[1..].iter() {
                builder += format!(", {}", vec_str.to_owned()).as_str();
            }
        }

        if add_parans {
            format!("[{}]", builder)
        } else {
            builder
        }
    }
}
