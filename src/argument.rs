use crate::action::{Action, ActionError};
use crate::argument_name::ArgumentNameError;
use crate::choices::{Choices, ChoicesError};
use crate::default::ArgumentDefault;
use crate::nargs::NArgs;
use crate::prefix_chars::{PrefixChars, PrefixCharsError};
use crate::InvalidChoice;
use crate::{argument_name::ArgumentName, string_vec_to_string};
use std::fmt::format;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;
use std::iter;
use thiserror::Error;

#[derive(Error, Debug, PartialEq, Eq)]
pub enum ArgumentError {
    #[error("{0}")]
    ActionError(ActionError),
    #[error("{0}")]
    PrefixCharsError(PrefixCharsError),
    #[error("{0}")]
    InvalidChoice(InvalidChoice),
    #[error("{0}")]
    ArgumentNameError(ArgumentNameError),
    #[error("{0}")]
    ChoicesError(ChoicesError),
    #[error("default value given to argument {0} with unsupported number of arguments {1}")]
    DefaultValueForInvalidNargs(String, String),
    #[error("required argument {0} given default value {1}")]
    RequiredArgumentDefaultValueGiven(String, String),
    #[error("given default value has length {0} but expected {1} arguments")]
    InvalidDefaultValueLength(usize, String),
    #[error("given choice \"{0}\" value has length {1} but expected {2} arguments")]
    InvalidChoiceLength(String, usize, String),
    #[error("found duplicate argument name values {0}")]
    DuplicateArgumentNameValues(String),
    #[error("found duplicate argument destinations for follow duplicate argument names {0}")]
    DuplicateArgumentDestinationsForSameValue(String),
    #[error("required is reserved for flag arguments")]
    RequiredMarkedForPositionalArgument,
    #[error("non-required argument {0} missing  adefault value")]
    NonRequiredArgumentNotGivenDefaultValue(String),
    #[error("start of NArgs::Range {0} is >= than end {1}")]
    InvalidRangeSize(usize, usize),
    #[error("nargs given for action {0} which doesn't expect argument values")]
    InvalidActionForNArgs(String),
    #[error("default value given to action {0}, only allowed for append & store")]
    DefaultGivenForUnsupportedAction(String),
    #[error("choices given to unsupported action {0}, choices only supported for actions store, append, & extend")]
    ChoicesGivenForUnsupportedAction(String),
    #[error("required given to unsupported action")]
    RequiredGivenToUnsupportedAction,
    #[error("destination given for positional argument, destination is already set based off postional argument name")]
    DestinationGivenForPositionalArgument,
    #[error("can't have exactly zero arguments for store action; try store_const, store_true, or store_false actions")]
    ZeroExactNArgForStoreAction,
}

#[derive(Clone, Debug, Eq)]
pub struct Argument {
    name: ArgumentName,
    required: bool,
    help: Option<String>,
    choices: Option<Choices>,
    default: ArgumentDefault<String>,
    nargs: NArgs,
    action: Action,
    metavar: Option<String>,
    dest: Option<String>, // only ever a single value
    // for arguments that are created only to help a user given argument
    // rn only true for dest arg for append_const acions
    is_helper_arg: bool,
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

        if self.default.has_value() {
            let default_value = self.default.get_value();
            let default_str = string_vec_to_string(default_value, true);
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

    pub fn default(&self) -> &ArgumentDefault<String> {
        &self.default
    }

    pub fn new(
        name: ArgumentName,
        action: Option<&str>,
        nargs: Option<NArgs>,
        constant: Option<Vec<String>>,
        default: Option<ArgumentDefault<String>>,
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
                .map_err(ArgumentError::ActionError),
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

        let default = default.unwrap_or(ArgumentDefault::None);

        match (&action, &default) {
            (Action::Store(_), _) | (Action::Append(_), _) => Ok(()),
            (action, ArgumentDefault::Value(_)) => Err(
                ArgumentError::DefaultGivenForUnsupportedAction(action.to_string()),
            ),
            _ => Ok(()),
        }?;

        let nargs_given = nargs.is_some();
        let nargs = match (&action, nargs) {
            (Action::Store(_), Some(NArgs::Exact(0))) => {
                Err(ArgumentError::ZeroExactNArgForStoreAction)
            }
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
            _ => Ok(if default.has_value() {
                NArgs::AnyNumber
            } else {
                NArgs::Exact(1)
            }),
        }?;

        let required = match (required, &default) {
            (None, ArgumentDefault::None) => Ok(false),
            (Some(true), ArgumentDefault::None) => Ok(true),
            // (Some(false), None) => match &action {
            //     Action::Store(_) => Err(ArgumentError::NonRequiredArgumentNotGivenDefaultValue(
            //         name.to_string(),
            //     )),
            //     _ => Ok(false),
            // },
            (Some(true), ArgumentDefault::Suppress) => panic!(),
            (None, ArgumentDefault::Value(_))
            | (Some(false), _)
            | (_, ArgumentDefault::Suppress) => Ok(false),
            (Some(true), ArgumentDefault::Value(default)) => {
                Err(ArgumentError::RequiredArgumentDefaultValueGiven(
                    name.to_string(),
                    string_vec_to_string(&default, true),
                ))
            }
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
                let parsed_choices = Choices::new(&choices).map_err(ArgumentError::ChoicesError)?;
                if nargs_given {
                    for choice in choices.into_iter() {
                        if !nargs.is_valid_number(choice.len()) {
                            return Err(ArgumentError::InvalidChoiceLength(
                                string_vec_to_string(&choice.clone(), choice.len() != 1),
                                choice.len(),
                                nargs.to_string(),
                            ));
                        }
                    }
                }
                Ok(parsed_choices)
            })
            .transpose()?;

        if default.has_value() && nargs_given {
            let default_len = default.get_value().len().clone();
            if !nargs.is_valid_number(default_len) {
                return Err(ArgumentError::InvalidDefaultValueLength(
                    default_len,
                    nargs.to_string(),
                ));
            }
        }

        if default.has_value() && choices.is_some() {
            choices
                .as_ref()
                .unwrap()
                .arg_value_in_choices(default.get_value())
                .map_err(ArgumentError::ChoicesError)?
        };

        match (default.clone(), nargs) {
            (ArgumentDefault::Value(_), NArgs::AnyNumber)
            | (ArgumentDefault::Value(_), NArgs::OneOrMore) => Ok(()),
            (ArgumentDefault::Value(v), n) => {
                if n.is_valid_number(v.len()) {
                    Ok(())
                } else {
                    Err(ArgumentError::DefaultValueForInvalidNargs(
                        name.to_string(),
                        n.to_string(),
                    ))
                }
            }
            _ => Ok(()),
        }?;

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
            is_helper_arg: false,
        })
    }

    pub fn with_action(&self, action: Action) -> Argument {
        let mut new = self.clone();
        new.action = action;
        new
    }

    pub fn with_name(&self, name: ArgumentName) -> Argument {
        let mut new = self.clone();
        new.name = name;
        new
    }

    pub fn with_nargs(&self, nargs: NArgs) -> Argument {
        let mut new = self.clone();
        new.nargs = nargs;
        new
    }

    pub fn arg_value_in_choices(&self, raw_arg: &Vec<String>) -> Result<(), ChoicesError> {
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

    pub fn flag_values(&self) -> Vec<(usize, &String)> {
        match self.name() {
            ArgumentName::Positional(x) => vec![(0, &x)],
            ArgumentName::Flag(map) => {
                let mut new_vec = Vec::new();
                for (prefix_size, arg_names) in map {
                    for arg_name in arg_names.into_iter() {
                        new_vec.push((prefix_size.clone(), arg_name));
                    }
                }

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
                ArgumentName::Flag(map) => {
                    let mut prefix_counts: Vec<&usize> = map.keys().into_iter().collect();
                    prefix_counts.sort();
                    let largest_prefix = prefix_counts.last().unwrap();
                    let fetch_value: &String = map.get(&largest_prefix).unwrap().first().unwrap();
                    fetch_value
                }
            },
            (_, Some(d)) => &d,
        }
    }

    pub fn display_arg_name(&self, prefix_chars: &PrefixChars) -> String {
        match self.name() {
            ArgumentName::Positional(x) => x.clone(),
            ArgumentName::Flag(map) => {
                let default: String = prefix_chars.default_char().to_string();
                let mut prefix_counts: Vec<&usize> = map.keys().into_iter().collect();
                prefix_counts.sort();
                //let prefix = default.
                let prefix_size = prefix_counts.first().unwrap();
                let name: String = map.get(prefix_size).unwrap().first().unwrap().clone();
                let prefix: String = iter::repeat(default).take(**prefix_size).collect();
                prefix + &name
            }
        }
    }

    pub fn usage_display(&self, prefix_chars: &PrefixChars) -> String {
        let mut builder = "".to_string();

        if self.name().is_flag_argument() {
            builder += "[";
        }

        let binding = self.display_arg_name(prefix_chars).to_string();
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

    pub fn set_as_helper_arg(&self) -> Argument {
        let mut new_argument = self.clone();
        new_argument.is_helper_arg = true;
        new_argument
    }

    pub fn is_helper_arg(&self) -> bool {
        self.is_helper_arg
    }

    pub fn name_overlap(&self, other: &Argument) -> Vec<(usize, String, Option<(usize, String)>)> {
        match (self.name(), other.name()) {
            (ArgumentName::Positional(x), ArgumentName::Positional(y)) => {
                if x == y {
                    vec![(0, x.clone(), None)]
                } else {
                    vec![]
                }
            }
            (ArgumentName::Positional(_), ArgumentName::Flag { .. }) => other.name_overlap(self),
            (ArgumentName::Flag(map), ArgumentName::Positional(x)) => {
                if map.into_iter().any(|(_, v)| v.contains(x)) {
                    vec![(0, x.clone(), None)]
                } else {
                    vec![]
                }
            }
            (ArgumentName::Flag(map1), ArgumentName::Flag(map2)) => {
                // --foo & ---foo, diff destinations --> not conflict
                // --foo & ---foo, same destinations --> conflict
                // --foo & --foo --> conflict
                let mut conflicts: Vec<(usize, String, Option<(usize, String)>)> = Vec::new();
                for (n_prefixes_2, v2) in map2.into_iter() {
                    for (n_prefixes_1, v1) in map1.into_iter() {
                        for name in v2.into_iter() {
                            if v1.contains(name) {
                                if n_prefixes_1 == n_prefixes_2 {
                                    conflicts.push((*n_prefixes_1, name.clone(), None))
                                } else if self.fetch_value() == other.fetch_value() {
                                    conflicts.push((
                                        *n_prefixes_1,
                                        name.clone(),
                                        Some((*n_prefixes_2, self.fetch_value().to_string())),
                                    ))
                                }
                            }
                        }
                    }
                }
                conflicts
            }
        }
    }
}

#[cfg(test)]
mod test {
    use std::vec;

    use crate::{
        argument::{Action, Argument, ArgumentError, Choices, NArgs},
        argument_name::ArgumentName,
        choices::ChoicesError,
        default::ArgumentDefault,
        prefix_chars::PrefixChars,
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
                default: ArgumentDefault::None,
                nargs: NArgs::Exact(1),
                action: Action::Store(vec![]),
                metavar: None,
                dest: None,
                is_helper_arg: false
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
                default: ArgumentDefault::None,
                nargs: NArgs::Exact(1),
                action: Action::Store(vec![]),
                metavar: None,
                dest: None,
                is_helper_arg: false
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
                choices: Some(
                    Choices::new(&vec![vec!["a".to_string()], vec!["c".to_string()]]).unwrap()
                ),
                default: ArgumentDefault::None,
                nargs: NArgs::Exact(1),
                metavar: None,
                dest: None,
                is_helper_arg: false
            },
        )
    }

    #[test]
    fn store_argument_with_zeroexact_nargs() {
        assert_eq!(
            Argument::new(
                ArgumentName::Positional("foo".to_string()),
                None,
                Some(NArgs::Exact(0)),
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None
            )
            .unwrap_err(),
            ArgumentError::ZeroExactNArgForStoreAction
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
                Some(ArgumentDefault::Value(vec!["a".to_string()])),
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
                default: ArgumentDefault::Value(vec!["a".to_string()]),
                nargs: NArgs::AnyNumber,
                dest: None,
                metavar: None,
                is_helper_arg: false,
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
                Some(ArgumentDefault::Value(vec!["a".to_string()])),
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
                Some(ArgumentDefault::Value(vec!["c".to_string()])),
                Some(vec![vec!["a".to_string()], vec!["b".to_string()]]),
                None,
                None,
                None,
                None,
                None,
            )
            .unwrap_err(),
            ArgumentError::ChoicesError(ChoicesError::InvalidChoice(InvalidChoice(
                "c".to_string(),
                "[a, b]".to_string()
            ))),
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
                Some(ArgumentDefault::Value(vec!["a".to_string()])),
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
            ArgumentError::ChoicesError(ChoicesError::DuplicateChoice("a".to_string())),
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
            ArgumentError::ChoicesError(ChoicesError::InsufficientChoices),
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
            ArgumentError::ChoicesError(ChoicesError::InsufficientChoices),
        )
    }
}
