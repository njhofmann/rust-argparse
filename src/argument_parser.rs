use std::{
    collections::{HashMap, HashSet},
    env,
    fmt::Display,
};

use crate::{
    action::Action,
    argument::{Argument, ArgumentError},
    argument_name::{ArgumentName, ArgumentNameError},
    builder::{AddArgumentError, ArgumentAdder, ArgumentBuilder},
    choices::ChoicesError,
    conclict_handling_strategy::{ConflictHandlingStrategy, ConflictHandlingStrategyError},
    default::ArgumentDefault,
    nargs::NArgs,
    parse_result::Namespace,
    prefix_chars::{PrefixChars, PrefixCharsError},
    string_vec_to_string, InvalidChoice,
};
use thiserror::Error;

// if this pseudo-arg is given, everything after is a positional argument
const ONLY_POSITIONAL_ARGS: &str = "--";

// errors occuring during ArgumentParser setup
#[derive(Error, Debug, PartialEq, Eq)]
pub enum ArgumentParserError {
    #[error("{0}")]
    PrefixCharsError(PrefixCharsError),
    #[error("{0}")]
    InvalidChoice(InvalidChoice),
    #[error("{0}")]
    AddArgumentError(AddArgumentError),
    #[error("{0}")]
    ConflictHandlingStrategyError(ConflictHandlingStrategyError),
}

// errors that occur when parsing raw argument values
#[derive(Error, Debug, PartialEq, Eq)]
pub enum ParsingError {
    #[error("{0}")]
    ArgumentGroupError(ArgumentGroupError),
    #[error("{0}")]
    AddArgumentError(AddArgumentError),
    #[error("argument {0} expected {1} values, but found {2}")]
    IncorrectValueCount(String, NArgs, usize),
    #[error("missing values for required flag aruments {0}")]
    MissingRequiredFlagArguments(String),
    #[error("invalid flag argument {0} found")]
    InvalidFlagArgument(String),
    #[error("duplicate flag argument {0} found")]
    DuplicateFlagArgument(String),
    #[error("found multiple arguments {1} for abbreviated argument key \"{0}\"")]
    AmbiguousAbbreviatedArguments(String, String),
    #[error("{0}")]
    ChoicesError(ChoicesError),
    #[error("no arguments were found to process remaining raw arguments {0}")]
    UnprocessedRawArguments(String),
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum SubparserError {
    #[error("a subparser with the name \"{0}\" has already been registered")]
    DuplicateNameInParser(String),
    #[error("subparser identifier \"{0}\" has been registered for multiple subparsers across multiple subparser managers")]
    DupblicateNameAcrossParsers(String),
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum ArgumentGroupError {
    #[error("{0}")]
    ConflictHandlingStrategyError(ConflictHandlingStrategyError),
    #[error("multiple arguments from mutually exclusive group were found")]
    DuplicateMutuallyExclusiveGroupArguments,
    #[error("missing required argument group {0}")]
    MissingRequiredArgumentGroup(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ArgumentGroupBuilder {
    title: Option<String>,
    description: Option<String>,
    suppress_missing_attributes: Option<bool>,
    conflict_handler: Option<String>,
}

impl ArgumentGroupBuilder {
    pub fn new() -> Self {
        ArgumentGroupBuilder {
            title: None,
            description: None,
            suppress_missing_attributes: None,
            conflict_handler: None,
        }
    }

    pub fn build(&self) -> Result<ArgumentGroup, ArgumentGroupError> {
        ArgumentGroup::new(
            self.title.as_deref(),
            self.description.as_deref(),
            self.suppress_missing_attributes,
            self.conflict_handler.as_deref(),
        )
    }

    pub fn with_title(&self, title: &str) -> Self {
        let mut new = self.clone();
        new.title = Some(title.to_string());
        new
    }

    pub fn with_description(&self, desp: &str) -> Self {
        let mut new = self.clone();
        new.description = Some(desp.to_string());
        new
    }

    pub fn with_suppress_missing_attributes(&self, suppress: bool) -> Self {
        let mut new = self.clone();
        new.suppress_missing_attributes = Some(suppress);
        new
    }

    pub fn with_conflict_handler(&self, handler: &str) -> Self {
        let mut new = self.clone();
        new.conflict_handler = Some(handler.to_string());
        new
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ArgumentGroup {
    title: Option<String>,
    description: Option<String>,
    required: bool,
    mutually_exclusive: bool,
    seen: bool,
    parser: ArgumentParser,
}

impl ArgumentGroup {
    pub fn new(
        title: Option<&str>,
        description: Option<&str>,
        suppress_missing_attributes: Option<bool>,
        conflict_handler: Option<&str>,
    ) -> Result<ArgumentGroup, ArgumentGroupError> {
        let conflict_handler = ConflictHandlingStrategy::new(conflict_handler)
            .map_err(ArgumentGroupError::ConflictHandlingStrategyError)?
            .to_string();
        Ok(ArgumentGroup {
            title: title.map(|x| x.to_string()),
            description: description.map(|x| x.to_string()),
            required: false,
            mutually_exclusive: false,
            seen: false,
            parser: ArgumentParser::new(
                None,
                None,
                None,
                None,
                None,
                None,
                suppress_missing_attributes,
                Some(&conflict_handler),
                Some(false),
                None,
            )
            .unwrap(),
        })
    }

    pub fn add_argument<T: ToString>(
        &self,
        name: Vec<&str>,
        action: Option<&str>,
        nargs: Option<NArgs>,
        constant: Option<Vec<T>>,
        default: Option<ArgumentDefault<T>>,
        choices: Option<Vec<Vec<T>>>,
        required: Option<bool>,
        help: Option<&str>,
        metavar: Option<&str>,
        dest: Option<&str>,
        version: Option<&str>,
    ) -> Result<Self, AddArgumentError> {
        let mut new_group = self.clone();
        new_group.parser = self.parser.add_argument(
            name, action, nargs, constant, default, choices, required, help, metavar, dest, version,
        )?;
        Ok(new_group)
    }

    pub fn has_been_seen(&mut self) -> Result<(), ArgumentGroupError> {
        // TODO is this bad form? use bool instead?
        if !self.mutually_exclusive {
            panic!(
                "seen checks are argument groups should only be done on mutually exclusive groups"
            )
        } else if self.seen {
            Err(ArgumentGroupError::DuplicateMutuallyExclusiveGroupArguments)
        } else {
            self.seen = true;
            Ok(())
        }
    }

    pub fn suppress_missing_attributes(&self) -> bool {
        self.parser.suppress_missing_attributes
    }
}

impl Default for ArgumentGroup {
    fn default() -> Self {
        ArgumentGroup::new(None, None, None, None).unwrap()
    }
}

impl Display for ArgumentGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let arg_name_strs: Vec<String> = self
            .parser
            .arguments()
            .into_iter()
            .map(|x| x.name().to_string())
            .collect();
        let display_str = string_vec_to_string(&arg_name_strs, true);
        write!(f, "{}", display_str)
    }
}

impl ArgumentAdder for ArgumentGroup {
    fn add_argument<T: ToString>(
        &self,
        name: Vec<&str>,
        action: Option<&str>,
        nargs: Option<NArgs>,
        constant: Option<Vec<T>>,
        default: Option<ArgumentDefault<T>>,
        choices: Option<Vec<Vec<T>>>,
        required: Option<bool>,
        help: Option<&str>,
        metavar: Option<&str>,
        dest: Option<&str>,
        version: Option<&str>,
    ) -> Result<Self, AddArgumentError> {
        let mut new_group = self.clone();
        new_group.parser = self.parser.add_argument(
            name, action, nargs, constant, default, choices, required, help, metavar, dest, version,
        )?;
        Ok(new_group)
    }
    fn get_current_arg_builder(&self) -> Option<ArgumentBuilder> {
        self.parser.arg_builder.clone()
    }

    fn set_current_arg_builder(&self, new_arg_builder: ArgumentBuilder) -> Self {
        let mut new = self.clone();
        new.parser.arg_builder = Some(new_arg_builder);
        new
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MutuallyExclusiveGroupBuilder {
    arg_group_builder: ArgumentGroupBuilder,
    required: Option<bool>,
}

impl MutuallyExclusiveGroupBuilder {
    pub fn new() -> Self {
        MutuallyExclusiveGroupBuilder {
            arg_group_builder: ArgumentGroupBuilder::new(),
            required: None,
        }
    }

    pub fn build(&self) -> Result<MutuallyExclusiveGroup, ArgumentGroupError> {
        MutuallyExclusiveGroup::new(
            self.arg_group_builder.title.as_deref(),
            self.arg_group_builder.description.as_deref(),
            self.arg_group_builder.suppress_missing_attributes,
            self.arg_group_builder.conflict_handler.as_deref(),
            self.required,
        )
    }

    pub fn with_title(&self, title: &str) -> Self {
        let mut new = self.clone();
        new.arg_group_builder = new.arg_group_builder.with_title(title);
        new
    }

    pub fn with_description(&self, desp: &str) -> Self {
        let mut new = self.clone();
        new.arg_group_builder = new.arg_group_builder.with_description(desp);
        new
    }

    pub fn with_suppress_missing_attributes(&self, suppress: bool) -> Self {
        let mut new = self.clone();
        new.arg_group_builder = new
            .arg_group_builder
            .with_suppress_missing_attributes(suppress);
        new
    }

    pub fn with_conflict_handler(&self, handler: &str) -> Self {
        let mut new = self.clone();
        new.arg_group_builder = new.arg_group_builder.with_conflict_handler(handler);
        new
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MutuallyExclusiveGroup(ArgumentGroup);

impl Default for MutuallyExclusiveGroup {
    fn default() -> Self {
        Self::new(None, None, None, None, None).unwrap()
    }
}

impl ArgumentAdder for MutuallyExclusiveGroup {
    fn add_argument<T: ToString>(
        &self,
        name: Vec<&str>,
        action: Option<&str>,
        nargs: Option<NArgs>,
        constant: Option<Vec<T>>,
        default: Option<ArgumentDefault<T>>,
        choices: Option<Vec<Vec<T>>>,
        required: Option<bool>,
        help: Option<&str>,
        metavar: Option<&str>,
        dest: Option<&str>,
        version: Option<&str>,
    ) -> Result<Self, AddArgumentError> {
        let mut new_group = self.clone();
        new_group.0 = self.0.add_argument(
            name, action, nargs, constant, default, choices, required, help, metavar, dest, version,
        )?;
        Ok(new_group)
    }

    fn get_current_arg_builder(&self) -> Option<ArgumentBuilder> {
        self.0.parser.arg_builder.clone()
    }

    fn set_current_arg_builder(&self, new_arg_builder: ArgumentBuilder) -> Self {
        let mut new = self.clone();
        new.0.parser.arg_builder = Some(new_arg_builder);
        new
    }
}

impl MutuallyExclusiveGroup {
    pub fn with_required(&self, required: bool) -> Self {
        let mut new = self.clone();
        new.0.required = required;
        new
    }

    pub fn new(
        title: Option<&str>,
        description: Option<&str>,
        suppress_missing_attributes: Option<bool>,
        conflict_handler: Option<&str>,
        required: Option<bool>,
    ) -> Result<MutuallyExclusiveGroup, ArgumentGroupError> {
        let conflict_handler = ConflictHandlingStrategy::new(conflict_handler)
            .map_err(ArgumentGroupError::ConflictHandlingStrategyError)?
            .to_string();
        Ok(MutuallyExclusiveGroup(ArgumentGroup {
            title: title.map(|x| x.to_string()),
            description: description.map(|x| x.to_string()),
            required: required.unwrap_or(false),
            mutually_exclusive: true,
            seen: false,
            parser: ArgumentParser::new(
                None,
                None,
                None,
                None,
                None,
                None,
                Some(suppress_missing_attributes.unwrap_or(false)),
                Some(&conflict_handler),
                Some(false),
                None,
            )
            .unwrap(),
        }))
    }
}

impl Display for MutuallyExclusiveGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.to_string())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SubparserManager {
    title: Option<String>,
    description: Option<String>,
    prog: Option<String>,
    help: Option<String>,
    required: bool,
    subparser_name_idx_mapping: HashMap<String, usize>,
    subparsers: Vec<ArgumentParser>,
}

impl SubparserManager {
    pub fn new(
        title: Option<&str>,
        description: Option<&str>,
        prog: Option<&str>,
        help: Option<&str>,
        required: Option<bool>,
    ) -> SubparserManager {
        // TODO action, dest, metavar
        // TODO unsupported: parser_class
        SubparserManager {
            title: title.map_or_else(
                || description.map(|_| "subcommands".to_string()),
                |x| Some(x.to_string()),
            ),
            description: description.map(|x| x.to_string()),
            prog: prog.map(|x| x.to_string()),
            help: help.map(|x| x.to_string()),
            required: required.unwrap_or(false),
            subparser_name_idx_mapping: HashMap::new(),
            subparsers: Vec::new(),
        }
    }

    pub fn add_parser(
        &self,
        name: &str,
        parser: ArgumentParser,
        aliases: Option<Vec<&str>>,
    ) -> Result<SubparserManager, SubparserError> {
        let mut new_manager = self.clone();
        new_manager.subparsers.push(parser);
        let idx = new_manager.subparsers.len() - 1;

        if new_manager.subparser_name_idx_mapping.contains_key(name) {
            Err(SubparserError::DuplicateNameInParser(name.to_string()))
        } else {
            new_manager
                .subparser_name_idx_mapping
                .insert(name.to_string(), idx);
            if aliases.is_some() {
                for alias in aliases.unwrap() {
                    if new_manager.subparser_name_idx_mapping.contains_key(name) {
                        return Err(SubparserError::DuplicateNameInParser(name.to_string()));
                    }
                    new_manager
                        .subparser_name_idx_mapping
                        .insert(alias.to_string(), idx);
                }
            }
            Ok(new_manager)
        }
    }

    fn get_subparser(&self, name: String) -> Option<&ArgumentParser> {
        self.subparser_name_idx_mapping
            .get(&name)
            .map(|idx| &self.subparsers[*idx])
    }

    fn names(&self) -> Vec<&String> {
        self.subparser_name_idx_mapping.keys().into_iter().collect()
    }

    fn check_name_overlap(&self, other_manager: &SubparserManager) -> Result<(), SubparserError> {
        for name in self.names().into_iter() {
            for other_name in other_manager.names().into_iter() {
                if name == other_name {
                    return Err(SubparserError::DupblicateNameAcrossParsers(
                        name.to_string(),
                    ));
                }
            }
        }
        Ok(())
    }
}

impl Default for SubparserManager {
    fn default() -> Self {
        SubparserManager::new(None, None, None, None, None)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ArgumentParserBuilder {
    prog: Option<String>,
    usage: Option<String>, // TODO program usage from arguments
    description: Option<String>,
    epilog: Option<String>,
    parents: Option<Vec<ArgumentParser>>,
    prefix_chars: Option<String>,
    suppress_missing_attributes: Option<bool>,
    conflict_handler: Option<String>,
    add_help: Option<bool>,
    allow_abbrev: Option<bool>,
}

impl ArgumentParserBuilder {
    pub fn new() -> Self {
        Self {
            prog: None,
            usage: None,
            description: None,
            epilog: None,
            parents: None,
            prefix_chars: None,
            suppress_missing_attributes: None,
            conflict_handler: None,
            add_help: None,
            allow_abbrev: None,
        }
    }

    pub fn build(&mut self) -> Result<ArgumentParser, ArgumentParserError> {
        ArgumentParser::new(
            self.prog.as_deref(),
            self.usage.as_deref(),
            self.description.as_deref(),
            self.epilog.as_deref(),
            self.parents.clone(),
            self.prefix_chars.as_deref(),
            self.suppress_missing_attributes,
            self.conflict_handler.as_deref(),
            self.add_help,
            self.allow_abbrev,
        )
    }

    pub fn with_prog(&mut self, prog: &str) -> &mut Self {
        self.prog = Some(prog.to_string());
        self
    }

    pub fn with_usage(&mut self, usage: &str) -> &mut Self {
        self.usage = Some(usage.to_string());
        self
    }

    pub fn with_description(&mut self, desp: &str) -> &mut Self {
        self.description = Some(desp.to_string());
        self
    }

    pub fn with_epilog(&mut self, epilog: &str) -> &mut Self {
        self.epilog = Some(epilog.to_string());
        self
    }

    pub fn with_parents(&mut self, parents: Vec<ArgumentParser>) -> &mut Self {
        if !parents.is_empty() {
            if self.parents.is_some() {
                self.parents.as_mut().unwrap().append(&mut parents.clone());
            } else {
                self.parents = Some(parents.to_vec())
            }
        }
        self
    }

    pub fn with_parent(&mut self, parent: ArgumentParser) -> &mut Self {
        if self.parents.is_some() {
            self.parents.as_mut().unwrap().push(parent);
        } else {
            self.parents = Some(vec![parent]);
        }
        self
    }

    pub fn with_prefix_chars(&mut self, chars: &str) -> &mut Self {
        self.prefix_chars = Some(if let Some(old_chars) = self.prefix_chars.as_ref() {
            old_chars.clone() + chars
        } else {
            chars.to_string()
        });
        self
    }

    pub fn with_suppress_missing_attributes(&mut self, suppress: bool) -> &mut Self {
        self.suppress_missing_attributes = Some(suppress);
        self
    }

    pub fn with_conflict_handler(&mut self, handler: &str) -> &mut Self {
        self.conflict_handler = Some(handler.to_string());
        self
    }

    pub fn with_help(&mut self, add: bool) -> &mut Self {
        self.add_help = Some(add);
        self
    }

    pub fn with_allow_abbrev(&mut self, allow: bool) -> &mut Self {
        self.allow_abbrev = Some(allow);
        self
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ArgumentParser {
    positional_args: Vec<Argument>,
    flag_args: Vec<Argument>,
    // (n_prefixes, argument name) --> (index in list, is flag or positional argument)
    arg_name_mapping: HashMap<(usize, String), (usize, bool)>,
    usage: Option<String>,
    desp: Option<String>,
    prog: String,
    epilog: Option<String>,
    prefix_chars: PrefixChars,
    suppress_missing_attributes: bool,
    allow_abbrev: bool,
    help_arg_added: bool,
    version_arg_added: bool,
    conflict_handler: ConflictHandlingStrategy,
    subparser_managers: Option<Vec<SubparserManager>>,
    argument_groups: Vec<ArgumentGroup>, // TODO make these optional?
    arg_name_to_arg_group: HashMap<ArgumentName, usize>,
    arg_builder: Option<ArgumentBuilder>,
}

impl Display for ArgumentParser {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut builder = if let Some(desp) = self.desp.as_ref() {
            format!("{}\n\n", desp)
        } else {
            "".to_string()
        };

        builder += self.format_usage().as_str();

        if self.positional_args.len() > 0 {
            builder += "\npositional arguments:";
            for argument in self.positional_args.iter() {
                builder += format!("\n- {}", argument).as_str();
            }
        }

        if self.flag_args.len() > 0 {
            builder += "\nflag arguments:";
            for argument in self.flag_args.iter() {
                builder += format!("\n- {}", argument).as_str();
            }
        }

        if let Some(epilog) = self.epilog.as_ref() {
            builder += format!("\n{}", epilog).as_str()
        }

        write!(f, "{}", builder)
    }
}

impl Default for ArgumentParser {
    fn default() -> Self {
        ArgumentParser::new(None, None, None, None, None, None, None, None, None, None).unwrap()
    }
}

impl ArgumentAdder for ArgumentParser {
    fn add_argument<T: ToString>(
        &self,
        name: Vec<&str>,
        action: Option<&str>,
        nargs: Option<NArgs>,
        constant: Option<Vec<T>>,
        default: Option<ArgumentDefault<T>>,
        choices: Option<Vec<Vec<T>>>,
        required: Option<bool>,
        help: Option<&str>,
        metavar: Option<&str>,
        dest: Option<&str>,
        version: Option<&str>,
    ) -> Result<ArgumentParser, AddArgumentError> {
        let arg_name: ArgumentName = ArgumentName::new(name, &self.prefix_chars)
            .map_err(|e| AddArgumentError::ArgumentError(ArgumentError::ArgumentNameError(e)))?
            .clone();

        // this shouldn't happen often
        let constant = constant.map(|x| x.into_iter().map(|y| y.to_string()).collect());
        let default: Option<ArgumentDefault<String>> = default.map(|v| match v {
            ArgumentDefault::None => ArgumentDefault::None,
            ArgumentDefault::Suppress => ArgumentDefault::Suppress,
            ArgumentDefault::Value(vec) => {
                ArgumentDefault::Value(vec.into_iter().map(|x| x.to_string()).collect())
            }
        });
        let choices = choices.map(|inner| {
            inner
                .into_iter()
                .map(|choice| choice.into_iter().map(|x| x.to_string()).collect())
                .collect()
        });

        let new_argument = Argument::new(
            arg_name, action, nargs, constant, default, choices, required, help, metavar, dest,
            version,
        )
        .map_err(AddArgumentError::ArgumentError)?;

        let (new_flag_args, new_positional_args, new_arg_name_mapping) = self
            .check_dup_arg_names(&new_argument)
            .map_err(AddArgumentError::ArgumentError)?;

        let mut new_parser = self.clone();
        new_parser.positional_args = new_positional_args;
        new_parser.flag_args = new_flag_args;
        new_parser.arg_name_mapping = new_arg_name_mapping;
        new_parser.store_argument(new_argument, dest)
    }

    fn get_current_arg_builder(&self) -> Option<ArgumentBuilder> {
        self.arg_builder.clone()
    }

    fn set_current_arg_builder(&self, new_arg_builder: ArgumentBuilder) -> Self {
        let mut new = self.clone();
        new.arg_builder = Some(new_arg_builder);
        new
    }
}

impl ArgumentParser {
    pub fn new(
        prog: Option<&str>,
        usage: Option<&str>, // TODO program usage from arguments
        description: Option<&str>,
        epilog: Option<&str>,
        parents: Option<Vec<ArgumentParser>>,
        prefix_chars: Option<&str>,
        suppress_missing_attributes: Option<bool>,
        conflict_handler: Option<&str>,
        add_help: Option<bool>,
        allow_abbrev: Option<bool>,
    ) -> Result<ArgumentParser, ArgumentParserError> {
        let mut parser = ArgumentParser {
            positional_args: Vec::new(),
            flag_args: Vec::new(),
            arg_name_mapping: HashMap::new(),
            usage: usage.map(|x| x.to_string()),
            desp: description.map(|x| x.to_string()),
            prog: prog.map_or_else(
                || {
                    env::args()
                        .collect::<Vec<String>>()
                        .first()
                        .unwrap()
                        .clone()
                },
                |x| x.to_string(),
            ),
            epilog: epilog.map(|x| x.to_string()),
            prefix_chars: PrefixChars::new(prefix_chars)
                .map_err(ArgumentParserError::PrefixCharsError)?,
            suppress_missing_attributes: suppress_missing_attributes.unwrap_or(false),
            allow_abbrev: allow_abbrev.unwrap_or(true),
            conflict_handler: ConflictHandlingStrategy::new(conflict_handler)
                .map_err(ArgumentParserError::ConflictHandlingStrategyError)?,
            help_arg_added: false,
            version_arg_added: false,
            subparser_managers: None,
            argument_groups: vec![],
            arg_name_to_arg_group: HashMap::new(),
            arg_builder: None,
        };

        if add_help.unwrap_or(true) && !parser.help_arg_added {
            parser = parser
                .add_argument::<&str>(
                    vec!["-h", "--help"],
                    Some("help"),
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                )
                .unwrap();
        }

        // do this after setting up the parser
        if let Some(parents) = parents {
            for parent_parser in parents.iter() {
                parser = parser.add_parser_arguments(parent_parser)?;
            }
        }

        Ok(parser)
    }

    fn add_parser_arguments(
        &self,
        other_parser: &ArgumentParser,
    ) -> Result<Self, ArgumentParserError> {
        let mut parser = self.clone();
        for argument in other_parser.arguments() {
            // this is a pruned down version of add_argument
            (
                parser.flag_args,
                parser.positional_args,
                parser.arg_name_mapping,
            ) = parser.check_dup_arg_names(&argument).map_err(|e| {
                ArgumentParserError::AddArgumentError(AddArgumentError::ArgumentError(e))
            })?;
            let cloned_arg = argument.clone();
            let temp_dest = cloned_arg.dest().clone();
            let dest: Option<&str> = temp_dest.as_deref();
            parser = parser
                .store_argument(cloned_arg, dest)
                .map_err(ArgumentParserError::AddArgumentError)?;
        }
        Ok(parser)
    }

    fn check_dup_arg_names(
        &self,
        arg: &Argument,
    ) -> Result<
        (
            Vec<Argument>,
            Vec<Argument>,
            HashMap<(usize, String), (usize, bool)>,
        ),
        ArgumentError,
    > {
        let handle_overlap = |argument: &Argument| -> Option<Result<Argument, ArgumentError>> {
            let mut process_argument = Some(Ok(argument.clone()));
            let mut overlap_on_name: Vec<String> = Vec::new();
            let mut overlap_on_dest: Vec<(String, String, String)> = Vec::new();
            for (n_prefixes, name, conflicting_dest) in argument.name_overlap(arg).into_iter() {
                let prepended_name = self.prefix_chars.prepend_prefixes(n_prefixes, &name);
                match conflicting_dest {
                    None => overlap_on_name.push(prepended_name),
                    Some((other_n_prefixes, dest)) => {
                        let other_prepended_name =
                            self.prefix_chars.prepend_prefixes(other_n_prefixes, &name);
                        overlap_on_dest.push((prepended_name, other_prepended_name, dest))
                    }
                }
            }

            if !overlap_on_name.is_empty() {
                process_argument = match self.conflict_handler {
                    ConflictHandlingStrategy::Error => {
                        overlap_on_name.sort();
                        Some(Err(ArgumentError::DuplicateArgumentNameValues(
                            string_vec_to_string(&overlap_on_name, true),
                        )))
                    }
                    ConflictHandlingStrategy::Override => {
                        if overlap_on_name.len() == argument.name().num_of_identifiers() {
                            None
                        } else {
                            // if posn would always have overlap of 1 & be caught above
                            debug_assert!(argument.name().is_flag_argument());
                            let mut existing_name_map = match &argument.name() {
                                ArgumentName::Flag(map) => map.clone(),
                                _ => panic!("argument with posn should have been removed above"),
                            };

                            for overlap_name in overlap_on_name {
                                let (raw_name, other_n_prefixes) =
                                    self.prefix_chars.parse_raw_name_string(&overlap_name);
                                existing_name_map = existing_name_map
                                    .into_iter()
                                    .map(|(n_prefixes, names)| {
                                        let mut new_names = names.clone();
                                        new_names.retain(|x| {
                                            x != &raw_name && other_n_prefixes != n_prefixes
                                        });
                                        (n_prefixes, new_names)
                                    })
                                    .collect();
                            }

                            Some(Ok(
                                argument.with_name(ArgumentName::Flag(existing_name_map.clone()))
                            ))
                        }
                    }
                }
            }

            // TODO can conflcit_handler here somehwo
            if (process_argument.is_some() && process_argument.as_ref().unwrap().is_ok())
                && !overlap_on_dest.is_empty()
            {
                overlap_on_dest.sort();
                let overlap_on_dest_strings = overlap_on_dest
                    .into_iter()
                    .map(|(name1, name2, dest)| {
                        "[".to_string() + &name1 + ", " + &name2 + ", " + &dest + "]"
                    })
                    .collect::<Vec<String>>();
                return Some(Err(
                    ArgumentError::DuplicateArgumentDestinationsForSameValue(string_vec_to_string(
                        &overlap_on_dest_strings,
                        true,
                    )),
                ));
            }

            return process_argument;
        };

        let new_flag_arguments = self
            .flag_args
            .iter()
            .filter_map(handle_overlap)
            .collect::<Result<Vec<Argument>, ArgumentError>>()?;
        let new_positional_arguments = self
            .positional_args
            .iter()
            .filter_map(handle_overlap)
            .collect::<Result<Vec<Argument>, ArgumentError>>()?;
        // TODO reduce complexity here?
        let new_arg_name_mapping = if &new_flag_arguments.len() < &self.flag_args.len()
            || &new_positional_arguments.len() < &self.positional_args.len()
        {
            self.init_arg_idx_mappings(&new_flag_arguments, &new_positional_arguments)
        } else {
            self.arg_name_mapping.clone()
        };

        Ok((
            new_flag_arguments,
            new_positional_arguments,
            new_arg_name_mapping,
        ))
    }

    fn get_subparser(&self, name: String) -> Option<&ArgumentParser> {
        if self.subparser_managers.is_none() {
            None
        } else {
            let possible_parsers: Vec<&ArgumentParser> = self
                .subparser_managers
                .as_ref()
                .unwrap()
                .into_iter()
                .filter_map(|manager| manager.get_subparser(name.to_string()))
                .collect();
            match possible_parsers.len() {
                0 => None,
                1 => Some(&possible_parsers.first().unwrap()),
                _ => panic!("duplicate subparser names should've been caught when adding them"),
            }
        }
    }

    fn parse_flag_argument_name(
        &self,
        argument_name: &String,
        n_prefixes: usize,
    ) -> Result<(Vec<Argument>, Option<String>), ParsingError> {
        debug_assert!(n_prefixes > 0); // 0 sometimes reservered as signifier for positional argument
                                       // check for values in form of "key=value"
        let is_short_flag = n_prefixes == 1;
        let mut optional_value: Option<String> = None;
        let split: &str = &argument_name.clone();
        let partitions: Vec<&str> = split.split("=").collect();
        let argument_name = if partitions.len() > 1 {
            optional_value = Some(string_vec_to_string(&partitions[1..].to_vec(), false));
            &partitions.first().unwrap().to_string()
        } else {
            argument_name
        };

        // if allow abbrevated flags is true, check for possible matches
        let location = if self.allow_abbrev {
            let possible_locations: Vec<((&usize, &String), &(usize, bool))> = self
                .arg_name_mapping
                .iter()
                .filter_map(|((n_arg_prefixes, arg), v)| {
                    if arg.len() > 1
                        && n_prefixes == *n_arg_prefixes
                        && arg.starts_with(argument_name)
                    {
                        Some(((n_arg_prefixes, arg), v))
                    } else {
                        None
                    }
                })
                .collect();

            match possible_locations.len() {
                0 => Ok(None),
                1 => Ok(Some(possible_locations.first().unwrap().to_owned().1)),
                _ => {
                    let mut conflicting_arg_names: Vec<String> = possible_locations
                        .into_iter()
                        .map(|((n_prefixes, arg_name), _)| {
                            self.prefix_chars.prepend_prefixes(*n_prefixes, arg_name)
                        })
                        .collect();
                    conflicting_arg_names.sort();
                    Err(ParsingError::AmbiguousAbbreviatedArguments(
                        argument_name.clone(),
                        string_vec_to_string(&conflicting_arg_names, true),
                    ))
                }
            }
        } else {
            Ok(None)
        }?;

        let location = match location {
            Some(x) => Ok(Some(x)),
            None => {
                let location = self
                    .arg_name_mapping
                    .get(&(n_prefixes, argument_name.clone()));

                if location.is_some() {
                    Ok(location)
                } else if is_short_flag && argument_name.len() > 1 {
                    Ok(None)
                } else {
                    Err(ParsingError::InvalidFlagArgument(argument_name.clone()))
                }
            }
        }?;

        if location.is_none() {
            debug_assert!(is_short_flag);
            debug_assert!(argument_name.len() > 2); // must be multiple short flag or short flag & value
            let possible_args = &argument_name[..argument_name.len() - 1];
            let possible_optional_value = &argument_name[argument_name.len() - 1..];
            let mut arguments = Vec::new();

            for arg in possible_args.chars() {
                let arg = arg.to_string();
                let &(idx, is_flag_arg) = self
                    .arg_name_mapping
                    .get(&(n_prefixes, arg.clone()))
                    .ok_or(ParsingError::InvalidFlagArgument(arg.clone()))?;
                debug_assert!(is_flag_arg);
                arguments.push(self.flag_args[idx].clone());
            }

            let optional_value_location = self
                .arg_name_mapping
                .get(&(n_prefixes, possible_optional_value.to_string()));
            if let Some(&(idx, is_flag_arg)) = optional_value_location {
                debug_assert!(is_flag_arg);
                let last_arg = arguments.last().unwrap();
                if !last_arg.nargs().can_be_zero() {
                    // TODO should
                    return Err(ParsingError::IncorrectValueCount(
                        last_arg.name().to_string(),
                        last_arg.nargs().clone(),
                        0,
                    ));
                }
                arguments.push(self.flag_args[idx].clone())
            } else if optional_value.is_none() {
                let last_arg = arguments.last().unwrap();
                if !last_arg.nargs().is_valid_number(1) {
                    return Err(ParsingError::IncorrectValueCount(
                        last_arg.name().to_string(),
                        last_arg.nargs().clone(),
                        1,
                    ));
                }
                optional_value = Some(possible_optional_value.to_string());
            } else {
                panic!("optional value already filled")
            }

            for argument in arguments.clone()[..arguments.len() - 1].into_iter() {
                if !argument.nargs().can_be_zero() {
                    return Err(ParsingError::IncorrectValueCount(
                        argument.name().to_string(),
                        argument.nargs().clone(),
                        0,
                    ));
                }
            }
            Ok((arguments, optional_value)) // todo need to return optional value
        } else {
            let &(idx, is_flag_arg) = location.unwrap();
            debug_assert!(is_flag_arg);
            let argument = self.flag_args[idx].clone();

            if optional_value.is_some() && !argument.nargs().is_valid_number(1) {
                return Err(ParsingError::IncorrectValueCount(
                    argument.name().to_string(),
                    argument.nargs().clone(),
                    1,
                ));
            }
            let arguments = vec![argument];
            Ok((arguments, optional_value))
        }
    }

    fn store_argument(
        &self,
        argument: Argument,
        dest: Option<&str>,
    ) -> Result<ArgumentParser, AddArgumentError> {
        let (help_arg_added, version_arg_added) = match argument.action() {
            Action::Help => {
                if self.help_arg_added && !self.conflict_handler.is_override() {
                    Err(AddArgumentError::DuplicateHelpArgument(
                        argument.name().to_string(),
                    ))
                } else {
                    Ok((true, self.version_arg_added))
                }
            }
            Action::Version(_) => {
                if self.version_arg_added && !self.conflict_handler.is_override() {
                    Err(AddArgumentError::DuplicateVersionArgument(
                        argument.name().to_string(),
                    ))
                } else {
                    Ok((self.help_arg_added, true))
                }
            }
            _ => Ok((self.help_arg_added, self.version_arg_added)),
        }?;

        let mut new_flag_args = self.flag_args.clone();
        let mut new_positional_args = self.positional_args.clone();
        let new_arg_location = if argument.name().is_flag_argument() {
            new_flag_args.push(argument.clone());
            Ok((new_flag_args.len() - 1, true))
        } else {
            new_positional_args.push(argument.clone());
            Ok((new_positional_args.len() - 1, false))
        }?;

        let mut new_arg_name_mapping = self.arg_name_mapping.clone();
        for (n_prefixes, flag) in argument.flag_values() {
            new_arg_name_mapping.insert((n_prefixes, flag.clone()), new_arg_location);
        }

        let mut new_parser = ArgumentParser {
            positional_args: new_positional_args,
            flag_args: new_flag_args,
            arg_name_mapping: new_arg_name_mapping,
            usage: self.usage.clone(),
            desp: self.desp.clone(),
            prog: self.prog.clone(),
            epilog: self.epilog.clone(),
            prefix_chars: self.prefix_chars.clone(),
            suppress_missing_attributes: self.suppress_missing_attributes,
            allow_abbrev: self.allow_abbrev, // TODO correct default
            conflict_handler: self.conflict_handler,
            help_arg_added,
            version_arg_added,
            subparser_managers: self.subparser_managers.clone(),
            argument_groups: self.argument_groups.clone(),
            arg_name_to_arg_group: self.arg_name_to_arg_group.clone(),
            arg_builder: self.arg_builder.clone(),
        };

        if let Action::AppendConst(_) = argument.action() {
            if dest.is_none() {
                todo!()
            }
            let dest = dest.unwrap().to_string();
            let mut dest_arg_map = HashMap::new();
            dest_arg_map.insert(2 as usize, vec![dest.clone()]);
            let new_dest_argument = Argument::new(
                ArgumentName::Flag(dest_arg_map),
                Some("extend"),
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
            )
            .unwrap()
            .set_as_helper_arg();
            let existing_dest_arg = self.parse_flag_argument_name(&dest, 2);
            if existing_dest_arg.is_err() {
                // TODO check against specific error
                // if is_some() is already in
                new_parser = new_parser.store_argument(new_dest_argument, None)?;
            } else {
                // TODO assert this
                let existing_dest_arg: Argument =
                    existing_dest_arg.unwrap().0.first().unwrap().clone();
                match (existing_dest_arg.action(), new_dest_argument.action()) {
                    (Action::Extend(_), Action::Extend(_)) => Ok(()),
                    _ => Err(AddArgumentError::ArgumentError(
                        ArgumentError::ArgumentNameError(ArgumentNameError::DuplicateArgumentName(
                            dest,
                        )),
                    )),
                }?
            }
        };

        Ok(new_parser)
    }

    fn init_arg_idx_mappings(
        &self,
        flag_args: &Vec<Argument>,
        positional_args: &Vec<Argument>,
    ) -> HashMap<(usize, String), (usize, bool)> {
        let mut new_arg_idx_mapping: HashMap<(usize, String), (usize, bool)> = HashMap::new();

        for (idx, arg) in flag_args.iter().enumerate() {
            for (prefix_count, arg_name) in arg.flag_values() {
                new_arg_idx_mapping.insert((prefix_count, arg_name.clone()), (idx, true));
            }
        }

        for (idx, arg) in positional_args.iter().enumerate() {
            let (prefix_count, arg_name) = arg.flag_values().first().unwrap().clone();
            debug_assert!(prefix_count == 0);
            new_arg_idx_mapping.insert((prefix_count, arg_name.clone()), (idx, false));
        }

        new_arg_idx_mapping
    }

    pub fn arguments(&self) -> Vec<&Argument> {
        let mut arguments = Vec::new();
        for argument in self.positional_args.iter() {
            arguments.push(argument)
        }
        for argument in self.flag_args.iter() {
            if !argument.is_helper_arg() {
                // only return arguments that were "given" by user
                arguments.push(argument)
            }
        }
        arguments
    }

    fn set_values_of_other(&mut self, other: ArgumentParser) {
        self.positional_args = other.positional_args;
        self.flag_args = other.flag_args;
        self.arg_name_mapping = other.arg_name_mapping;
        self.usage = other.usage;
        self.desp = other.desp;
        self.prog = other.prog;
        self.epilog = other.epilog;
        self.prefix_chars = other.prefix_chars;
        self.suppress_missing_attributes = other.suppress_missing_attributes;
        self.allow_abbrev = other.allow_abbrev;
        self.help_arg_added = other.help_arg_added;
        self.version_arg_added = other.version_arg_added;
        self.conflict_handler = other.conflict_handler;
        self.subparser_managers = other.subparser_managers;
        self.argument_groups = other.argument_groups;
        self.arg_name_to_arg_group = other.arg_name_to_arg_group;
        self.arg_builder = other.arg_builder;
    }

    fn parse_raw_args_with_arg_builder(
        &mut self,
        raw_args: Option<Vec<String>>,
        keep_or_error_unknown_args: bool,
    ) -> Result<(Namespace, Vec<String>), ParsingError> {
        // add last argument builder if there is one
        if self.arg_builder.is_some() {
            let new = self
                .add_argument_builder()
                .map_err(ParsingError::AddArgumentError)?; // TODO should this behere
            self.set_values_of_other(new);
        }

        self.parse_raw_args_without_arg_builder(raw_args, keep_or_error_unknown_args)
    }

    fn parse_raw_args_without_arg_builder(
        &self,
        raw_args: Option<Vec<String>>,
        keep_or_error_unknown_args: bool,
    ) -> Result<(Namespace, Vec<String>), ParsingError> {
        let mut unknown_args: Vec<String> = Vec::new(); // only used if keep_or_error_unknown_args is true
        let raw_args = raw_args.unwrap_or(env::args().collect());
        let mut processed_arguments: HashSet<Argument> = HashSet::new();
        let mut idx = 0;
        let mut cur_posn_argument_idx: Option<usize> = self.positional_args.first().map(|_| 0);
        let mut processed_posn_args: Vec<Argument> = Vec::new();
        let mut arg_and_raw_arg_range: Vec<(Argument, ArgumentValue)> = Vec::new();
        let mut var_posn_found = false;
        let mut last_arg_was_flag = true;
        let mut set_next_posn_group_idx = false;
        let mut all_remaining_args_positional = false;
        let mut subparser_parsing: Option<(Namespace, Vec<String>)> = None;
        let mut argument_groups = self.argument_groups.clone(); // clone this so method remains functional & parser can be reused
                                                                // staring idx of the last posn arg group
        let mut last_posn_arg_group_idx: Option<usize> = None; // tracks indices in arg_and_raw_arg_range
                                                               // contine to parse so long as posn arguments are left or raw args haven't been looked at (may have flags)

        #[derive(Clone, Debug)]
        enum ArgumentValue {
            FromArg(String),
            AfterArg(usize, usize),
        }

        let in_seen_mutually_exclusive_group = |arg: &Argument,
                                                arg_groups: &mut Vec<ArgumentGroup>|
         -> Result<(), ArgumentGroupError> {
            match self.arg_name_to_arg_group.get(arg.name()) {
                None => Ok(()),
                Some(idx) => {
                    let arg_group = arg_groups.get(*idx).unwrap();
                    let mut new_arg_group = arg_group.clone();
                    if !new_arg_group.mutually_exclusive {
                        Ok(())
                    } else {
                        let result = new_arg_group.has_been_seen();
                        arg_groups[*idx] = new_arg_group;
                        result
                    }
                }
            }
        };

        while idx < raw_args.len()
            || (cur_posn_argument_idx.is_some_and(|idx| idx < self.positional_args.len()))
        {
            let cur_raw_arg = if idx >= raw_args.len() {
                None
            } else {
                Some(raw_args[idx].as_str())
            };

            let (flag_raw_arg, n_prefixes) = if cur_raw_arg.is_some() {
                let cur_raw_arg = cur_raw_arg.unwrap();

                if let Some(subparser) = self.get_subparser(cur_raw_arg.to_string()) {
                    // TODO run subparser on remaining args, concat results at end
                    let remaining_args: Vec<String> = raw_args[idx + 1..].to_vec();
                    subparser_parsing = Some(subparser.parse_raw_args_without_arg_builder(
                        Some(remaining_args),
                        keep_or_error_unknown_args,
                    )?);
                    break;
                }

                if cur_raw_arg == ONLY_POSITIONAL_ARGS {
                    if !all_remaining_args_positional {
                        all_remaining_args_positional = true;
                        idx += 1; // move past "--"
                        continue;
                    }
                    (None, None)
                } else {
                    let (flag_raw_arg, n_prefixes) =
                        self.prefix_chars.parse_raw_name_string(cur_raw_arg);
                    (Some(flag_raw_arg), Some(n_prefixes))
                }
            } else {
                (None, None)
            };

            let (found_arguments, argument_value) = if !all_remaining_args_positional
                && cur_raw_arg.is_some()
                && n_prefixes.unwrap() > 0
            {
                let flag_raw_arg = flag_raw_arg.unwrap();
                let n_prefixes = n_prefixes.unwrap();

                if n_prefixes == 0 {
                    panic!("found non-flag argument")
                }

                // either returns
                // - single flag argument (long or short)
                // - single flag argument & single length value (--key=value)
                // - multiple short flag arguments & maybe a single length value
                let parsed_result: Result<(Vec<Argument>, Option<String>), ParsingError> =
                    self.parse_flag_argument_name(&flag_raw_arg, n_prefixes);

                if keep_or_error_unknown_args
                    && parsed_result == Err(ParsingError::InvalidFlagArgument(flag_raw_arg.clone()))
                {
                    unknown_args.push(cur_raw_arg.unwrap().to_string());
                    idx += 1; // move past flag
                    continue;
                }

                let (mut found_args, optional_value) = parsed_result?;

                found_args = found_args
                    .into_iter()
                    .map(|found_arg| {
                        if processed_arguments.contains(&found_arg) {
                            // TODO what does process_args doa again, why replace found_arg already in?
                            processed_arguments.get(&found_arg).unwrap().clone()
                        } else {
                            found_arg.clone()
                        }
                    })
                    .collect();
                // TODO handle multiple argments

                let argument_value = if optional_value.is_some() {
                    Some(ArgumentValue::FromArg(optional_value.unwrap()))
                } else {
                    None
                };

                last_arg_was_flag = true;
                idx += 1; // move past flag
                Ok((found_args.clone(), argument_value))
            } else if cur_posn_argument_idx.is_none() {
                let unprocessed_args = &raw_args[idx..].to_vec();
                if keep_or_error_unknown_args {
                    unknown_args.append(&mut unprocessed_args.clone());
                    break;
                } else {
                    return Err(ParsingError::UnprocessedRawArguments(string_vec_to_string(
                        &raw_args[idx..].to_vec(),
                        false,
                    )));
                }
            } else {
                if last_arg_was_flag {
                    set_next_posn_group_idx = true;
                    last_arg_was_flag = false;
                }

                let found_argument = self.positional_args[cur_posn_argument_idx.unwrap()].clone();
                cur_posn_argument_idx = if cur_posn_argument_idx
                    .is_some_and(|x| (x + 1) >= self.positional_args.len())
                {
                    None
                } else {
                    Some(cur_posn_argument_idx.unwrap() + 1)
                };
                processed_posn_args.push(found_argument.clone());
                Ok((vec![found_argument], None))
            }?;

            let last_arg_idx = found_arguments.len() - 1;
            for (i, found_argument) in found_arguments.into_iter().enumerate() {
                // check if mutually exclusive group
                in_seen_mutually_exclusive_group(&found_argument, &mut argument_groups)
                    .map_err(ParsingError::ArgumentGroupError)?;

                // only use arg value on last found arg
                let argument_value = if !(i == last_arg_idx && argument_value.is_some()) {
                    let end_or_new_flag_arg = |idx: &usize| {
                        if idx >= &raw_args.len() {
                            return true;
                        }
                        let (_, n_prefixes) = self
                            .prefix_chars
                            .parse_raw_name_string(&raw_args[idx.clone()]);
                        return !all_remaining_args_positional && n_prefixes > 0;
                        // TODO redo this check
                    };

                    let mut start_idx = idx;
                    let mut found_value_count = 0;
                    let mut n_missing_args = 0;
                    let mut end_idx: Option<usize> = match found_argument.nargs() {
                        NArgs::Exact(n) => {
                            let mut actual_value_count = 0;
                            let mut return_val: Option<usize> = None;
                            while idx < start_idx + n {
                                if end_or_new_flag_arg(&idx) {
                                    found_value_count = actual_value_count;
                                    n_missing_args = n - found_value_count;
                                    return_val = None;
                                    break;
                                } else {
                                    actual_value_count += 1;
                                    idx += 1;
                                    return_val = Some(idx);
                                }
                            }
                            return_val
                        }
                        NArgs::ZeroOrOne => {
                            if !end_or_new_flag_arg(&idx) {
                                idx += 1;
                            }
                            Some(idx)
                        }
                        NArgs::AnyNumber => {
                            // TODO reduce this
                            if !found_argument.name().is_flag_argument() {
                                if !var_posn_found {
                                    var_posn_found = true;
                                    while !end_or_new_flag_arg(&idx) {
                                        idx += 1
                                    }
                                }
                            } else {
                                while !end_or_new_flag_arg(&idx) {
                                    idx += 1
                                }
                            }
                            Some(idx)
                        }
                        NArgs::OneOrMore => {
                            idx += 1;
                            if end_or_new_flag_arg(&idx) {
                                found_value_count = 0;
                                n_missing_args = 1;
                                None
                            } else {
                                while !end_or_new_flag_arg(&idx) {
                                    idx += 1
                                }
                                Some(idx)
                            }
                        }
                    };

                    if end_idx.is_none()
                        && found_argument.name().is_flag_argument()
                        && found_argument.nargs() != &NArgs::Exact(0)
                    {
                        return Err(ParsingError::IncorrectValueCount(
                            found_argument.name().to_string(),
                            *found_argument.nargs(),
                            found_value_count,
                        ));
                    }

                    if end_idx.is_none()
                        && found_argument.nargs() == &NArgs::Exact(0)
                        && found_value_count == 0
                    {
                        end_idx = Some(start_idx);
                    }

                    // TODO handle this more properly
                    if end_idx.is_none() {
                        // TODO redo these assignments
                        let mut last_recorded_arg = None;

                        let start_group_idx =
                            match (last_posn_arg_group_idx, arg_and_raw_arg_range.is_empty()) {
                                (None, false) => {
                                    // is this right?
                                    return Err(ParsingError::IncorrectValueCount(
                                        found_argument.name().to_string(),
                                        found_argument.nargs().clone(),
                                        found_value_count,
                                    ));
                                } // nothing added yet
                                (Some(n), false) => n, // args added, some are posns
                                (None, true) => {
                                    // posn args not added
                                    return Err(ParsingError::IncorrectValueCount(
                                        found_argument.name().to_string(),
                                        found_argument.nargs().clone(),
                                        found_value_count,
                                    ));
                                }
                                (Some(_), true) => panic!("this shouldn't be possible"), // can't have smth in the former but not the latter
                            };

                        let mut group_shifts = Vec::new();
                        let perm_n_missing_args = n_missing_args;
                        for (relative_idx, (parsed_argument, argument_value)) in
                            arg_and_raw_arg_range[start_group_idx..]
                                .iter()
                                .enumerate()
                                .rev()
                        {
                            let (group_start_idx, group_end_idx) = match argument_value {
                            ArgumentValue::FromArg(_) => panic!("FromArg should only be set on flag arguments which shouldn't be here present in this loop"),
                            ArgumentValue::AfterArg(group_start_idx, group_end_idx) => (group_start_idx, group_end_idx),
                        };

                            // TODO fix ordering iteration
                            if parsed_argument.name().is_flag_argument() {
                                continue;
                            }

                            if last_recorded_arg.is_none() {
                                last_recorded_arg = Some(parsed_argument.clone());
                            }

                            let n_excess_args = *group_end_idx as i32
                                - *group_start_idx as i32
                                - parsed_argument.nargs().min_n_required_args() as i32;

                            debug_assert!(n_excess_args >= 0);

                            let n_args_to_shift = if n_missing_args == n_excess_args as usize {
                                n_missing_args = 0;
                                n_excess_args as usize
                            } else if n_missing_args < n_excess_args as usize {
                                let temp = n_missing_args;
                                n_missing_args = 0;
                                temp
                            } else {
                                n_missing_args -= n_excess_args as usize;
                                n_excess_args as usize
                            };

                            // track n excess args each process parsed arg can contribute
                            if n_args_to_shift > 0 {
                                // TODO make shift start_idx again
                                group_shifts
                                    .push((start_group_idx + relative_idx, n_args_to_shift));
                            }

                            if n_missing_args == 0 {
                                break;
                            }
                        }

                        if n_missing_args > 0 {
                            return Err(ParsingError::IncorrectValueCount(
                                found_argument.name().to_string(),
                                found_argument.nargs().clone(),
                                found_value_count,
                            ));
                        }

                        for (abs_idx, shift) in group_shifts.into_iter().rev() {
                            for (rel_idx, (arg, argument_value)) in
                                arg_and_raw_arg_range.clone()[abs_idx..].iter().enumerate()
                            {
                                let (start, end) = match argument_value {
                                ArgumentValue::FromArg(_) => panic!("FromArg should only be set on flag arguments which shouldn't be here present in this loop"),
                                ArgumentValue::AfterArg(group_start_idx, group_end_idx) => (group_start_idx, group_end_idx),
                            };

                                // only take items from first item in group, shift remaining one's down
                                if arg.name().is_flag_argument() {
                                    // TODO address this, shouldn't need to have this check here
                                    continue;
                                }

                                let new_start = if rel_idx == 0 { *start } else { *start - shift };
                                let new_end = end - shift;
                                let last_recorded_arg = last_recorded_arg.clone().unwrap();

                                if arg == &last_recorded_arg {
                                    start_idx = new_end;
                                }

                                arg_and_raw_arg_range[abs_idx + rel_idx] =
                                    (arg.clone(), ArgumentValue::AfterArg(new_start, new_end));
                            }
                        }

                        end_idx = Some(start_idx + perm_n_missing_args + found_value_count);
                    }

                    if end_idx.is_none() {
                        panic!("this should be set by now")
                    }

                    ArgumentValue::AfterArg(start_idx, end_idx.unwrap())
                } else {
                    argument_value.clone().unwrap()
                };

                arg_and_raw_arg_range.push((found_argument, argument_value));
            }

            // this needs to be set after adding above found arg
            if set_next_posn_group_idx {
                set_next_posn_group_idx = false;
                last_posn_arg_group_idx = Some(arg_and_raw_arg_range.len() - 1);
            }
        }

        let mut seen_arguments: HashSet<Argument> = HashSet::new();
        for (argument, value) in arg_and_raw_arg_range {
            let mut arg_val_vec = match value {
                ArgumentValue::FromArg(val) => vec![val],
                ArgumentValue::AfterArg(start, end) => raw_args[start..end].to_vec(),
            };
            let new_argument = if argument.name().is_flag_argument() {
                let argument = processed_arguments
                    .get(&argument)
                    .unwrap_or_else(|| &argument)
                    .clone();
                match argument.action() {
                    Action::Help => {
                        println!("{}", self.format_help());
                        std::process::exit(0)
                    }
                    Action::Version(v) => {
                        println!("{}", v);
                        std::process::exit(0)
                    }
                    Action::Store(_) => {
                        if seen_arguments.contains(&argument) {
                            Err(ParsingError::DuplicateFlagArgument(
                                argument.name().to_string(),
                            ))
                        } else {
                            seen_arguments.contains(&argument); // mark as found
                            argument
                                .arg_value_in_choices(&arg_val_vec)
                                .map_err(ParsingError::ChoicesError)?;
                            Ok(argument.with_action(Action::Store(arg_val_vec)))
                        }
                    }
                    Action::StoreConst(c) => {
                        if seen_arguments.contains(&argument) {
                            Err(ParsingError::DuplicateFlagArgument(
                                argument.name().to_string(),
                            ))
                        } else {
                            Ok(argument.with_action(Action::Store(c.clone())))
                        }
                    }
                    Action::Append(v) => {
                        argument
                            .arg_value_in_choices(&arg_val_vec)
                            .map_err(ParsingError::ChoicesError)?;
                        let mut new_v = v.clone();
                        if argument.default().has_value() {
                            new_v.append(&mut argument.default().get_value().clone())
                        }
                        new_v.append(&mut arg_val_vec);
                        Ok(argument.with_action(Action::Append(new_v)))
                    }
                    Action::AppendConst(c) => {
                        let vec_key = argument
                            .dest()
                            .as_ref()
                            .expect("this should have been checked before");
                        let parsed_argument = self.parse_flag_argument_name(&vec_key, 2)?;

                        let mut storing_argument = if parsed_argument.1.is_some() {
                            panic!("optional argument value found for append const dest argument")
                        } else if parsed_argument.0.len() > 1 {
                            panic!("multiple arguments found for append const dest argument")
                        } else {
                            parsed_argument.0.first().unwrap().clone()
                        };

                        if processed_arguments.contains(&storing_argument) {
                            storing_argument =
                                processed_arguments.get(&storing_argument).unwrap().clone();
                        }
                        let new_action = match storing_argument.action() {
                            Action::Extend(v) => {
                                let mut new_v = v.clone();
                                new_v.append(&mut c.clone());
                                Action::Extend(new_v)
                            }
                            _ => panic!("storing argument should have extend action"),
                        };
                        Ok(storing_argument.with_action(new_action))
                    }
                    Action::Count(x) => Ok(argument.with_action(Action::Count(x + 1))),
                    Action::Extend(v) => {
                        argument
                            .arg_value_in_choices(&arg_val_vec)
                            .map_err(ParsingError::ChoicesError)?;
                        let mut new_v = v.clone();
                        new_v.append(&mut arg_val_vec);
                        Ok(argument.with_action(Action::Extend(new_v)))
                    }
                }?
            } else {
                argument.with_action(match argument.action() {
                    Action::Store(_) => {
                        argument
                            .arg_value_in_choices(&arg_val_vec)
                            .map_err(ParsingError::ChoicesError)?;

                        let arg_val = if (argument.nargs() == &NArgs::AnyNumber
                            || argument.nargs() == &NArgs::ZeroOrOne)
                            && arg_val_vec.is_empty()
                            && argument.default().has_value()
                        {
                            argument.default().get_value().clone()
                        } else {
                            arg_val_vec
                        };

                        Ok(Action::Store(arg_val))
                    }
                    _ => panic!("given unsupported action to positional argument"),
                }?)
            };

            seen_arguments.insert(new_argument.clone());
            if !processed_arguments.insert(new_argument.clone()) {
                processed_arguments.replace(new_argument.clone());
            }
        }

        // check for missing required argument groups (mutually exclusive only)
        for arg_group in &argument_groups {
            if arg_group.required && !arg_group.seen {
                return Err(ParsingError::ArgumentGroupError(
                    ArgumentGroupError::MissingRequiredArgumentGroup(arg_group.to_string()),
                ));
            }
        }

        // defaults for missing flag arguments / rquired missing args
        let mut missing_required_flag_args = Vec::new();
        for arg in self.flag_args.iter() {
            if !processed_arguments.contains(arg) {
                if arg.required() {
                    missing_required_flag_args.push(arg)
                } else {
                    let arg_group_suppress =
                        self.arg_name_to_arg_group.get(arg.name()).map_or_else(
                            || false,
                            |idx| {
                                argument_groups.get(*idx).map_or_else(
                                    || false,
                                    |group| group.suppress_missing_attributes(),
                                )
                            },
                        );

                    // if default value for this argument should be suppressed acccording according to global, group, or argument level suppress bool
                    let non_default_value = !(self.suppress_missing_attributes
                        || arg_group_suppress
                        || arg.default().is_suppress());
                    if non_default_value {
                        let default_value = match arg.default() {
                            ArgumentDefault::None => vec![],
                            ArgumentDefault::Suppress => panic!("this arm shouldn't be selected"),
                            ArgumentDefault::Value(vec) => vec.clone(),
                        };
                        // TODO panic or debug assert here for non-store action? default used in other actions at all?
                        if let Action::Store(_) = arg.action() {
                            processed_arguments
                                .insert(arg.with_action(Action::Store(default_value)));
                        }
                    }
                }
            }
        }

        if !missing_required_flag_args.is_empty() {
            let name_vec = missing_required_flag_args
                .iter()
                .map(|x| x.name())
                .collect();
            let str_vec = string_vec_to_string(&name_vec, true);
            Err(ParsingError::MissingRequiredFlagArguments(str_vec))
        } else {
            let result_namespace = Namespace::new(HashMap::from_iter(
                processed_arguments.iter().map(|argument| {
                    (
                        argument.fetch_value().clone(),
                        match &argument.action() {
                            Action::Count(n) => argument
                                .with_action(Action::Store(vec![n.to_string()]))
                                .with_nargs(NArgs::Exact(1)),
                            Action::Extend(v)
                            | Action::Append(v)
                            | Action::StoreConst(v)
                            | Action::Store(v) => argument.with_nargs(NArgs::Exact(v.len())),
                            _ => panic!("these shouldn't be here"),
                        },
                    )
                }),
            ));

            let (final_namespace, final_unknown_args) =
                if let Some((subparser_namespace, subparser_unknown_args)) = subparser_parsing {
                    let mut new_unknown_args = unknown_args.clone();
                    let mut new_subparser_unknown_args = subparser_unknown_args.clone();
                    new_unknown_args.append(&mut new_subparser_unknown_args);
                    (
                        result_namespace.extend(subparser_namespace),
                        new_unknown_args,
                    )
                } else {
                    (result_namespace, unknown_args)
                };

            Ok((final_namespace, final_unknown_args))
        }
    }

    pub fn parse_args(&mut self, raw_args: Option<Vec<String>>) -> Result<Namespace, ParsingError> {
        let (namespace, remaining_args) = self.parse_raw_args_with_arg_builder(raw_args, false)?;
        debug_assert!(remaining_args.is_empty());
        Ok(namespace)
    }

    pub fn parse_known_args(
        &mut self,
        raw_args: Option<Vec<String>>,
    ) -> Result<(Namespace, Vec<String>), ParsingError> {
        self.parse_raw_args_with_arg_builder(raw_args, true)
    }

    pub fn add_subparsers(
        &self,
        subparser_managers: Vec<SubparserManager>,
    ) -> Result<ArgumentParser, SubparserError> {
        // TODO not supported: parser_class
        // TODO support: action, required, metavar, help,
        let mut new_parser = self.clone();
        // TODO check for naming conflicts
        new_parser.subparser_managers = Some(match new_parser.subparser_managers {
            Some(v) => {
                let mut new_v = v.clone();
                new_v.extend(subparser_managers);
                new_v
            }
            None => subparser_managers,
        });

        for (i, manager) in new_parser
            .subparser_managers
            .clone()
            .unwrap()
            .into_iter()
            .enumerate()
        {
            for (k, other_manager) in new_parser
                .subparser_managers
                .clone()
                .unwrap()
                .into_iter()
                .enumerate()
            {
                if i != k {
                    manager.check_name_overlap(&other_manager)?;
                }
            }
        }

        Ok(new_parser)
    }

    pub fn add_argument_group(&self, group: ArgumentGroup) -> Result<Self, ArgumentParserError> {
        let group = if group.get_current_arg_builder().is_some() {
            group
                .add_argument_builder()
                .map_err(|e| ArgumentParserError::AddArgumentError(e))?
        } else {
            group
        };
        let mut new_parser = self.clone();
        new_parser = new_parser.add_parser_arguments(&group.parser)?;

        new_parser.argument_groups.push(group.clone());
        let group_idx = new_parser.argument_groups.len() - 1;
        for argument in group.parser.arguments() {
            new_parser
                .arg_name_to_arg_group
                .insert(argument.name().clone(), group_idx);
        }

        Ok(new_parser)
    }

    pub fn add_mutually_exclusive_group(
        &self,
        group: MutuallyExclusiveGroup,
    ) -> Result<Self, ArgumentParserError> {
        self.add_argument_group(group.0)
    }

    pub fn print_usage(&self) {
        println!("{}", self.format_usage())
    }

    pub fn print_help(&self) {
        todo!()
    }

    pub fn format_usage(&self) -> String {
        self.usage.clone().unwrap_or_else(|| {
            let mut builder = self.prog.clone();

            for flag_arg in &self.flag_args {
                builder += " ";
                builder += flag_arg.usage_display(&self.prefix_chars).as_str();
            }

            for posn_arg in &self.positional_args {
                builder += " ";
                builder += posn_arg.usage_display(&self.prefix_chars).as_str();
            }

            builder
        })
    }

    pub fn format_help(&self) -> String {
        todo!()
    }
}

#[cfg(test)]
mod test {

    use crate::{
        argument::ArgumentError,
        argument_parser::{AddArgumentError, ArgumentParserError},
        builder::ArgumentAdder,
        conclict_handling_strategy::ConflictHandlingStrategyError,
        prefix_chars::PrefixCharsError,
    };

    use super::ArgumentParser;

    #[test]
    fn display_no_desp() {
        assert_eq!(
            ArgumentParser::new(
                Some("only prog"),
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some(false),
                None
            )
            .unwrap()
            .to_string(),
            "only prog".to_string()
        )
    }

    #[test]
    fn display_with_desp() {
        assert_eq!(
            ArgumentParser::new(
                Some("program"),
                None,
                Some("this parses arguments"),
                None,
                None,
                None,
                None,
                None,
                Some(false),
                None
            )
            .unwrap()
            .to_string(),
            "this parses arguments\n\nprogram".to_string()
        )
    }

    #[test]
    fn no_prefix_chars() {
        assert_eq!(
            ArgumentParser::new(
                None,
                None,
                None,
                None,
                None,
                Some(""),
                None,
                None,
                None,
                None
            )
            .unwrap_err(),
            ArgumentParserError::PrefixCharsError(PrefixCharsError::EmptyPrefixChars)
        )
    }
    #[test]
    fn unsupported_prefix_chars() {
        assert_eq!(
            ArgumentParser::new(
                None,
                None,
                None,
                None,
                None,
                Some("\n\t"),
                None,
                None,
                None,
                None
            )
            .unwrap_err(),
            ArgumentParserError::PrefixCharsError(PrefixCharsError::IllegalPrefixChars(
                "\t, \n".to_string()
            ))
        )
    }

    #[test]
    fn unsupported_override_value() {
        assert_eq!(
            ArgumentParser::new(
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some("foo"),
                None,
                None
            )
            .unwrap_err(),
            ArgumentParserError::ConflictHandlingStrategyError(
                ConflictHandlingStrategyError::UnsupportedConflictHandlingStrategy(
                    "foo".to_string()
                )
            )
        )
    }

    #[test]
    fn same_arg_name_diff_n_prefixes_diff_dest() {
        let namespace = ArgumentParser::default()
            .add_argument::<&str>(
                vec!["--foo"],
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some("foo1"),
                None,
            )
            .unwrap()
            .add_argument::<&str>(
                vec!["---foo"],
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some("foo2"),
                None,
            )
            .unwrap()
            .parse_args(Some(vec![
                "---foo".to_string(),
                "a".to_string(),
                "--foo".to_string(),
                "b".to_string(),
            ]))
            .unwrap();
        assert_eq!(
            namespace.get_one_value::<String>("foo1").unwrap(),
            "b".to_string()
        );
        assert_eq!(
            namespace.get_one_value::<String>("foo2").unwrap(),
            "a".to_string()
        );
    }

    #[test]
    fn same_arg_name_same_n_prefixes_diff_dest() {
        let error = ArgumentParser::default()
            .add_argument::<&str>(
                vec!["--foo"],
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some("foo1"),
                None,
            )
            .unwrap()
            .add_argument::<&str>(
                vec!["--foo"],
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some("foo2"),
                None,
            )
            .unwrap_err();
        assert_eq!(
            error,
            AddArgumentError::ArgumentError(ArgumentError::DuplicateArgumentNameValues(
                "[--foo]".to_string()
            ))
        )
    }

    #[test]
    fn same_arg_name_diff_n_prefixes_same_dest() {
        let error = ArgumentParser::default()
            .add_argument::<&str>(
                vec!["--foo"],
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some("foo1"),
                None,
            )
            .unwrap()
            .add_argument::<&str>(
                vec!["---foo"],
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some("foo1"),
                None,
            )
            .unwrap_err();
        assert_eq!(
            error,
            AddArgumentError::ArgumentError(
                ArgumentError::DuplicateArgumentDestinationsForSameValue(
                    "[[--foo, ---foo, foo1]]".to_string()
                )
            )
        )
    }

    mod parent_parser_tests {
        use crate::{
            argument::ArgumentError,
            argument_parser::{AddArgumentError, ArgumentParser, ArgumentParserError},
            builder::ArgumentAdder,
        };

        #[test]
        fn test_non_conflicting_arg() {
            let parent = ArgumentParser::new(
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some(true),
                None,
            )
            .unwrap();
            let child = ArgumentParser::new(
                None,
                None,
                None,
                None,
                Some(vec![parent]),
                None,
                None,
                None,
                Some(false),
                None,
            );
            assert!(child.is_ok())
        }

        #[test]
        fn test_non_conflicting_arg_non_help() {
            let parent = ArgumentParser::new(
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some(false),
                None,
            )
            .unwrap()
            .add_argument::<&str>(
                vec!["--foo"],
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
            )
            .unwrap();
            let child = ArgumentParser::new(
                None,
                None,
                None,
                None,
                Some(vec![parent]),
                None,
                None,
                None,
                None,
                None,
            );
            assert!(child.is_ok());
            let namespace = child
                .unwrap()
                .parse_args(Some(vec!["--foo".to_string(), "bar".to_string()]))
                .unwrap();
            assert_eq!(
                namespace.get_one_value::<String>("foo").unwrap(),
                "bar".to_string()
            );
        }

        #[test]
        fn test_conflicting_arg() {
            let parent = ArgumentParser::new(
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some(true),
                None,
            )
            .unwrap();
            let child = ArgumentParser::new(
                None,
                None,
                None,
                None,
                Some(vec![parent]),
                None,
                None,
                None,
                Some(true),
                None,
            );
            assert_eq!(
                child.unwrap_err(),
                ArgumentParserError::AddArgumentError(AddArgumentError::ArgumentError(
                    ArgumentError::DuplicateArgumentNameValues("[--help, -h]".to_string())
                ))
            )
        }

        #[test]
        fn test_conflicting_arg_with_override() {
            let parent = ArgumentParser::new(
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some(true),
                None,
            )
            .unwrap();
            let child = ArgumentParser::new(
                None,
                None,
                None,
                None,
                Some(vec![parent]),
                None,
                None,
                Some("override"),
                Some(true),
                None,
            );
            assert!(child.is_ok());
        }
    }
}
