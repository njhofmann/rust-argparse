use crate::argument_error::ArgumentError;
use crate::argument_parser::PrefixChars;
use crate::InvalidChoice;
use crate::{argument_name::ArgumentName, string_vec_to_string};
use std::fmt::format;
use std::hash::Hash;
use std::hash::Hasher;
use std::{collections::HashSet, fmt::Display};
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
    fn new(
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

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum NArgs {
    Exact(usize),
    ZeroOrOne, // ?
    AnyNumber, // *
    OneOrMore, // +
}

impl NArgs {
    pub fn is_valid_number(&self, x: usize) -> bool {
        match self {
            NArgs::Exact(n) => n == &x,
            NArgs::ZeroOrOne => x < 2,
            NArgs::AnyNumber => true,
            NArgs::OneOrMore => x > 0,
        }
    }

    pub fn can_be_zero(&self) -> bool {
        match self {
            NArgs::Exact(x) if x != &0 => false,
            NArgs::OneOrMore => false,
            _ => true,
        }
    }

    pub fn is_variable(&self) -> bool {
        // variable means no bound on size
        match self {
            NArgs::AnyNumber | NArgs::OneOrMore => true,
            _ => false,
        }
    }
}

impl Display for NArgs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                NArgs::Exact(n) => n.to_string(),
                NArgs::ZeroOrOne => "zero or one ('?')".to_string(),
                NArgs::AnyNumber => "any number ('*')".to_string(),
                NArgs::OneOrMore => "at least one ('+')".to_string(),
            }
        )
    }
}

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub struct Choices(Vec<Vec<String>>);

impl Choices {
    fn new(choices: Vec<Vec<String>>) -> Choices {
        Choices(choices)
    }
    pub fn arg_value_in_choices(&self, raw_arg: &Vec<String>) -> Result<(), InvalidChoice> {
        if self.0.contains(raw_arg) {
            Ok(())
        } else {
            Err(InvalidChoice(
                string_vec_to_string(raw_arg, raw_arg.len() != 1),
                self.to_string(),
            ))
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

#[derive(Clone, Debug, Eq)]
pub struct Argument {
    name: ArgumentName,
    required: bool,
    help: Option<String>,
    choices: Option<Choices>,
    default: Option<Vec<String>>,
    nargs: NArgs,
    action: Action,
    metavar: Option<String>,
    dest: Option<String>, // only ever a single value
}

impl Display for Argument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut builder = format(format_args!(
            "name: {}, required: {}, nargs: {}, action: {}",
            self.display_name(),
            self.required,
            self.nargs.to_string(),
            self.action.to_string()
        ));

        if let Some(choices) = self.choices.as_ref() {
            builder += format(format_args!(", choices: {}", choices)).as_str();
        }

        if let Some(default) = self.default.as_ref() {
            let default_str = string_vec_to_string(default, true);
            builder += format(format_args!(", default: {}", default_str)).as_str();
        }

        if let Some(desp) = self.help.as_ref() {
            builder += format(format_args!(", desp: {}", desp)).as_str();
        }

        write!(f, "{}", builder)
    }
}

impl Hash for Argument {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }
}

impl PartialEq for Argument {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Argument {
    pub fn name(&self) -> &ArgumentName {
        &self.name
    }
    pub fn required(&self) -> bool {
        self.required
    }

    pub fn desp(&self) -> &Option<String> {
        &self.help
    }

    pub fn nargs(&self) -> &NArgs {
        &self.nargs
    }

    pub fn choices(&self) -> &Option<Choices> {
        &self.choices
    }

    pub fn default(&self) -> &Option<Vec<String>> {
        &self.default
    }

    pub fn new(
        name: ArgumentName,
        action: Option<&str>,
        nargs: Option<NArgs>,
        constant: Option<Vec<String>>,
        default: Option<Vec<String>>,
        choices: Option<Vec<Vec<String>>>,
        required: Option<bool>,
        help: Option<&str>,
        metavar: Option<&str>,
        dest: Option<&str>,
        version: Option<&str>,
    ) -> Result<Argument, ArgumentError> {
        let action = match action {
            None => Ok(Action::Store(vec![])),
            Some(x) => Action::new(x, name.is_flag_argument(), constant, version, dest)
                .map_err(|e| ArgumentError::ActionError(e)),
        }?;

        if !name.is_flag_argument() && required.is_some() {
            return Err(ArgumentError::RequiredMarkedForPositionalArgument);
        } else if !name.is_flag_argument() && dest.is_some() {
            return Err(ArgumentError::DestinationGivenForPositionalArgument);
        }

        let required = match (&action, required) {
            (Action::Store(_), _) => Ok(required),
            (_, None) => Ok(Some(false)),
            (_, Some(_)) => Err(ArgumentError::RequiredGivenToUnsupportedAction),
        }?;

        match (&action, &default) {
            (Action::Store(_), _) | (Action::Append(_), _) => Ok(()),
            (action, Some(_)) => Err(ArgumentError::DefaultGivenForUnsupportedAction(
                action.to_string(),
            )),
            _ => Ok(()),
        }?;

        let nargs_given = nargs.is_some();
        let nargs = match (&action, nargs) {
            (Action::StoreConst(_), Some(_))
            | (Action::AppendConst(_), Some(_))
            | (Action::Version(_), Some(_))
            | (Action::Help, Some(_))
            | (Action::Count(_), Some(_)) => {
                Err(ArgumentError::InvalidActionForNArgs(action.to_string()))
            }
            (Action::StoreConst(_), None)
            | (Action::AppendConst(_), None)
            | (Action::Version(_), None)
            | (Action::Help, None)
            | (Action::Count(_), None) => Ok(NArgs::Exact(0)), // allow nothing
            (_, Some(nargs)) => Ok(nargs),
            _ => Ok(if default.is_some() {
                NArgs::AnyNumber
            } else {
                NArgs::Exact(1)
            }),
        }?;

        let required = match (required, &default) {
            (None, None) => Ok(true),
            (Some(true), None) => Ok(true),
            (Some(false), None) => match &action {
                Action::Store(_) => Err(ArgumentError::NonRequiredArgumentNotGivenDefaultValue(
                    name.to_string(),
                )),
                _ => Ok(false),
            },
            (None, Some(_)) | (Some(false), Some(_)) => Ok(false),
            (Some(true), Some(default)) => Err(ArgumentError::RequiredArgumentDefaultValueGiven(
                name.to_string(),
                string_vec_to_string(&default, true),
            )),
        }?;

        match (&action, &choices) {
            (Action::Store(_), Some(_))
            | (Action::Append(_), Some(_))
            | (Action::Extend(_), Some(_))
            | (_, None) => Ok(()),
            (action, Some(_)) => Err(ArgumentError::ChoicesGivenForUnsupportedAction(
                action.to_string(),
            )),
        }?;

        let choices = choices
            .map(|choices| {
                if choices.is_empty() || choices.len() == 1 {
                    return Err(ArgumentError::EmptyOrSingleChoice);
                }

                let mut seen_choices: HashSet<Vec<String>> = HashSet::new();
                if nargs_given {
                    for choice in choices.iter() {
                        if !nargs.is_valid_number(choice.len()) {
                            return Err(ArgumentError::InvalidChoiceLength(
                                string_vec_to_string(choice, choice.len() != 1),
                                choice.len(),
                                nargs.to_string(),
                            ));
                        }

                        if !seen_choices.insert(choice.clone()) {
                            return Err(ArgumentError::DuplicateChoice(string_vec_to_string(
                                choice,
                                choice.len() != 1,
                            )));
                        }
                    }
                }
                Ok(Choices::new(choices))
            })
            .transpose()?;

        if default.is_some() && nargs_given {
            let default_len = default.as_ref().unwrap().len().clone();
            if !nargs.is_valid_number(default_len) {
                return Err(ArgumentError::InvalidDefaultValueLength(
                    default_len,
                    nargs.to_string(),
                ));
            }
        }

        let default = default.map(|x| {
            x.into_iter()
                .map(|y| y.to_string())
                .collect::<Vec<String>>()
        });

        if default.is_some() && choices.is_some() {
            choices
                .as_ref()
                .unwrap()
                .arg_value_in_choices(&default.as_ref().unwrap())
                .map_err(|x| ArgumentError::InvalidChoice(x))?
        };

        Ok(Argument {
            name,
            required,
            help: help.map(|x| x.to_string()),
            nargs,
            choices,
            default,
            action: action.clone(),
            metavar: metavar.map(|x| x.to_string()),
            dest: dest.map(|x| x.to_string()),
        })
    }

    pub fn with_action(&self, new_action: Action) -> Argument {
        Argument {
            name: self.name.clone(),
            required: self.required,
            help: self.help.clone(),
            nargs: self.nargs,
            choices: self.choices.clone(),
            default: self.default.clone(),
            action: new_action,
            metavar: self.metavar.clone(),
            dest: self.dest.clone(),
        }
    }

    pub fn with_name(&self, name: ArgumentName) -> Argument {
        Argument {
            name: name,
            required: self.required,
            help: self.help.clone(),
            nargs: self.nargs,
            choices: self.choices.clone(),
            default: self.default.clone(),
            action: self.action.clone(),
            metavar: self.metavar.clone(),
            dest: self.dest.clone(),
        }
    }

    pub fn with_nargs(&self, new_nargs: NArgs) -> Argument {
        Argument {
            name: self.name.clone(),
            required: self.required,
            help: self.help.clone(),
            nargs: new_nargs,
            choices: self.choices.clone(),
            default: self.default.clone(),
            action: self.action.clone(),
            metavar: self.metavar.clone(),
            dest: self.dest.clone(),
        }
    }

    pub fn arg_value_in_choices(&self, raw_arg: &Vec<String>) -> Result<(), InvalidChoice> {
        self.choices
            .as_ref()
            .map_or(Ok(()), |x| x.arg_value_in_choices(raw_arg))
    }

    pub fn action(&self) -> &Action {
        &self.action
    }

    pub fn display_name(&self) -> String {
        self.metavar
            .as_ref()
            .map_or_else(|| self.fetch_value().to_string().clone(), |x| x.clone())
    }

    pub fn flag_values(&self) -> Vec<String> {
        match self.name() {
            ArgumentName::Positional(x) => vec![x.clone()],
            ArgumentName::Flag { full, abbrev } => {
                let mut new_vec = full.clone();
                let mut new_abbrev = abbrev.clone();
                new_vec.append(&mut new_abbrev);
                new_vec
            }
        }
    }

    pub fn dest(&self) -> &Option<String> {
        &self.dest
    }

    pub fn fetch_value(&self) -> &String {
        // value that fetches argument via "arg_vec.string", not as flag
        match (&self.action, &self.dest) {
            (Action::AppendConst(_), None) => {
                panic!("this should have been checked in action init")
            }
            // if appendconst dest assigned to storing list
            (Action::AppendConst(_), Some(_)) | (_, None) => match &self.name {
                ArgumentName::Positional(x) => x,
                ArgumentName::Flag { full, abbrev } => {
                    if !full.is_empty() {
                        full.first().unwrap()
                    } else {
                        abbrev.first().unwrap()
                    }
                }
            },
            (_, Some(d)) => &d,
        }
    }

    pub fn usage_display(&self, prefix_chars: &PrefixChars) -> String {
        let mut builder = "".to_string();

        if self.name().is_flag_argument() {
            builder += "[";
        }

        let binding = prefix_chars.display_arg_name(self).to_string();
        let binding2 = self.display_name();
        builder += match (self.name.is_flag_argument(), self.nargs()) {
            (false, NArgs::ZeroOrOne) | (false, NArgs::AnyNumber) | (false, NArgs::Exact(_)) => "",
            (true, _) => &binding,
            _ => &binding2,
        };

        let fetch_value_temp = match (self.name.is_flag_argument(), self.nargs()) {
            (false, NArgs::ZeroOrOne) => "".to_string(),
            (true, _) if self.metavar.is_none() => self.display_name().to_uppercase(),
            _ => self.display_name().to_string(),
        };
        let fetch_value = fetch_value_temp.as_str();

        let zero_or_one_inner_fetch_value = match self.name.is_flag_argument() {
            true => self.display_name().to_uppercase(),
            _ => self.display_name().to_string(),
        };

        let vals = match self.nargs() {
            NArgs::Exact(x) => {
                let mut builder = "".to_string();
                for i in 0..*x as i32 {
                    if i > 0 {
                        builder += " ";
                    }
                    builder += fetch_value;
                }
                builder.to_string()
            }
            NArgs::ZeroOrOne => "[".to_string() + zero_or_one_inner_fetch_value.as_str() + "]",
            NArgs::AnyNumber => "[".to_string() + fetch_value + " ...]",
            NArgs::OneOrMore => {
                (if self.name.is_flag_argument() {
                    fetch_value.to_string() + " "
                } else {
                    "".to_string()
                }) + "["
                    + fetch_value
                    + " ...]"
            }
        };

        if !builder.is_empty() && !vals.is_empty() {
            builder += " ";
        }

        builder += vals.as_str();
        if self.name().is_flag_argument() {
            builder += "]";
        }

        builder
    }
}

#[cfg(test)]
mod test {
    use std::vec;

    use crate::{
        argument::{Action, Argument, ArgumentError, Choices, NArgs},
        argument_name::ArgumentName,
        argument_parser::PrefixChars,
        InvalidChoice,
    };

    fn default_prefix_chars() -> PrefixChars {
        PrefixChars::new(Some("-")).unwrap()
    }

    #[test]
    fn basic_argument() {
        assert_eq!(
            Argument::new(
                ArgumentName::new(vec!["foo"], &default_prefix_chars()).unwrap(),
                None,
                Some(NArgs::Exact(1)),
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None
            )
            .unwrap(),
            Argument {
                name: ArgumentName::new(vec!["foo"], &default_prefix_chars()).unwrap(),
                required: true,
                help: None,
                choices: None,
                default: None,
                nargs: NArgs::Exact(1),
                action: Action::Store(vec![]),
                metavar: None,
                dest: None
            },
        )
    }

    #[test]
    fn argument_with_help() {
        assert_eq!(
            Argument::new(
                ArgumentName::new(vec!["--foo"], &default_prefix_chars()).unwrap(),
                None,
                None,
                None,
                None,
                None,
                None,
                Some("this is an argument"),
                None,
                None,
                None
            )
            .unwrap(),
            Argument {
                name: ArgumentName::new(vec!["--foo"], &default_prefix_chars()).unwrap(),
                required: true,
                help: Some("this is an argument".to_string()),
                choices: None,
                default: None,
                nargs: NArgs::Exact(1),
                action: Action::Store(vec![]),
                metavar: None,
                dest: None
            },
        )
    }

    #[test]
    fn argument_with_choices() {
        assert_eq!(
            Argument::new(
                ArgumentName::new(vec!["foo"], &default_prefix_chars()).unwrap(),
                None,
                None,
                None,
                None,
                Some(vec![vec!["a".to_string()], vec!["c".to_string()]]),
                None,
                None,
                None,
                None,
                None
            )
            .unwrap(),
            Argument {
                name: ArgumentName::new(vec!["foo"], &default_prefix_chars()).unwrap(),
                required: true,
                action: Action::Store(vec![]),
                help: None,
                choices: Some(Choices::new(vec![
                    vec!["a".to_string()],
                    vec!["c".to_string()]
                ])),
                default: None,
                nargs: NArgs::Exact(1),
                metavar: None,
                dest: None
            },
        )
    }

    #[test]
    fn postional_argument_given_destination() {
        assert_eq!(
            Argument::new(
                ArgumentName::Positional("name".to_string()),
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some("dest"),
                None
            )
            .unwrap_err(),
            ArgumentError::DestinationGivenForPositionalArgument
        )
    }

    #[test]
    fn argument_with_default() {
        assert_eq!(
            Argument::new(
                ArgumentName::new(vec!["--foo"], &default_prefix_chars()).unwrap(),
                None,
                None,
                None,
                Some(vec!["a".to_string()]),
                None,
                None,
                None,
                None,
                None,
                None
            )
            .unwrap(),
            Argument {
                name: ArgumentName::new(vec!["--foo"], &default_prefix_chars()).unwrap(),
                action: Action::Store(vec![]),
                required: false,
                help: None,
                choices: None,
                default: Some(vec!["a".to_string()]),
                nargs: NArgs::AnyNumber,
                dest: None,
                metavar: None
            },
        )
    }

    #[test]
    fn required_argument_with_default() {
        assert_eq!(
            Argument::new(
                ArgumentName::new(vec!["--foo"], &default_prefix_chars()).unwrap(),
                None,
                None,
                None,
                Some(vec!["a".to_string()]),
                None,
                Some(true),
                None,
                None,
                None,
                None
            )
            .unwrap_err(),
            ArgumentError::RequiredArgumentDefaultValueGiven(
                ArgumentName::new(vec!["--foo"], &default_prefix_chars())
                    .unwrap()
                    .to_string(),
                "[a]".to_string()
            ),
        )
    }

    #[test]
    fn default_not_in_choices() {
        assert_eq!(
            Argument::new(
                ArgumentName::new(vec!["foo"], &default_prefix_chars()).unwrap(),
                None,
                Some(NArgs::Exact(1)),
                None,
                Some(vec!["c".to_string()]),
                Some(vec![vec!["a".to_string()], vec!["b".to_string()]]),
                None,
                None,
                None,
                None,
                None,
            )
            .unwrap_err(),
            ArgumentError::InvalidChoice(InvalidChoice("c".to_string(), "[a, b]".to_string())),
        )
    }

    #[test]
    fn default_with_invalid_length() {
        assert_eq!(
            Argument::new(
                ArgumentName::new(vec!["foo"], &default_prefix_chars()).unwrap(),
                None,
                Some(NArgs::Exact(2)),
                None,
                Some(vec!["a".to_string()]),
                None,
                None,
                None,
                None,
                None,
                None
            )
            .unwrap_err(),
            ArgumentError::InvalidDefaultValueLength(1, 2.to_string()),
        )
    }

    #[test]
    fn choices_with_invalid_length() {
        assert_eq!(
            Argument::new(
                ArgumentName::new(vec!["foo"], &default_prefix_chars()).unwrap(),
                None,
                Some(NArgs::Exact(2)),
                None,
                None,
                Some(vec![vec!["a".to_string()], vec!["b".to_string()]]),
                None,
                None,
                None,
                None,
                None
            )
            .unwrap_err(),
            ArgumentError::InvalidChoiceLength("a".to_string(), 1, 2.to_string()),
        )
    }

    #[test]
    fn duplicate_choices() {
        assert_eq!(
            Argument::new(
                ArgumentName::new(vec!["foo"], &default_prefix_chars()).unwrap(),
                None,
                Some(NArgs::Exact(1)),
                None,
                None,
                Some(vec![vec!["a".to_string()], vec!["a".to_string()]]),
                None,
                None,
                None,
                None,
                None
            )
            .unwrap_err(),
            ArgumentError::DuplicateChoice("a".to_string()),
        )
    }

    #[test]
    fn single_choice() {
        assert_eq!(
            Argument::new(
                ArgumentName::new(vec!["foo"], &default_prefix_chars()).unwrap(),
                None,
                Some(NArgs::Exact(1)),
                None,
                None,
                Some(vec![vec!["a".to_string()]]),
                None,
                None,
                None,
                None,
                None
            )
            .unwrap_err(),
            ArgumentError::EmptyOrSingleChoice,
        )
    }

    #[test]
    fn empty_choices() {
        assert_eq!(
            Argument::new(
                ArgumentName::new(vec!["foo"], &default_prefix_chars()).unwrap(),
                None,
                None,
                None,
                None,
                Some(vec![]),
                None,
                None,
                None,
                None,
                None
            )
            .unwrap_err(),
            ArgumentError::EmptyOrSingleChoice,
        )
    }
}
