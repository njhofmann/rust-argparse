use std::{
    collections::{HashMap, HashSet},
    env,
    fmt::Display,
    iter, option,
};

use crate::{
    argument::{Action, Argument, NArgs},
    argument_error::ArgumentError,
    argument_name::ArgumentName,
    parse_result::Namespace,
    string_vec_to_string, InvalidChoice,
};
use thiserror::Error;

#[derive(Error, Debug, PartialEq, Eq)]
pub enum ParserError {
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
    InvalidChoice(InvalidChoice),
    #[error("no arguments were found to process remaining raw arguments {0}")]
    UnprocessedRawArguments(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PrefixChars((HashSet<char>, char));

impl PrefixChars {
    pub fn new(chars: Option<&str>) -> Result<PrefixChars, ArgumentError> {
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
                    Err(ArgumentError::EmptyPrefixChars)
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
                        Err(ArgumentError::IllegalPrefixChars(string_vec_to_string(
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

    pub fn display_arg_name(&self, argument: &Argument) -> String {
        match argument.name() {
            ArgumentName::Positional(x) => x.clone(),
            ArgumentName::Flag(map) => {
                let default: String = self.0 .1.to_string();
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
}

impl Default for PrefixChars {
    fn default() -> Self {
        PrefixChars::new(Some("-")).unwrap()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ConflictHandlingStrategy {
    Error,
    Override, // instead of resolve
}

impl ConflictHandlingStrategy {
    fn new(strategy: Option<&str>) -> Result<ConflictHandlingStrategy, ArgumentError> {
        match strategy {
            None | Some("error") => Ok(ConflictHandlingStrategy::Error),
            Some("override") | Some("resolve") => Ok(ConflictHandlingStrategy::Override),
            Some(s) => Err(ArgumentError::UnsupportedConflictHandlingStrategy(
                s.to_string(),
            )),
        }
    }

    fn is_override(&self) -> bool {
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

#[derive(Debug, PartialEq, Eq)]
pub struct ArgumentParser {
    positional_args: Vec<Argument>,
    flag_args: Vec<Argument>,
    arg_name_mapping: HashMap<String, (usize, bool)>, // vec idx & flag bool
    usage: Option<String>,
    desp: Option<String>,
    prog: String,
    epilog: Option<String>,
    prefix_chars: PrefixChars,
    argument_default: Option<String>,
    allow_abbrev_mapping: Option<HashMap<String, (usize, bool)>>,
    help_arg_added: bool,
    version_arg_added: bool,
    conflict_handler: ConflictHandlingStrategy,
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

impl ArgumentParser {
    pub fn new(
        prog: Option<&str>,
        usage: Option<&str>, // TODO program usage from arguments
        description: Option<&str>,
        epilog: Option<&str>,
        parents: Option<Vec<ArgumentParser>>,
        prefix_chars: Option<&str>,
        argument_default: Option<Vec<&str>>, // TODO set default arg if given
        conflict_handler: Option<&str>,
        add_help: Option<bool>,
        allow_abbrev: Option<bool>,
    ) -> Result<ArgumentParser, ArgumentError> {
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
            prefix_chars: PrefixChars::new(prefix_chars)?,
            argument_default: argument_default
                .map(|x| x.into_iter().map(|y| y.to_string()).collect()),
            allow_abbrev_mapping: match allow_abbrev.unwrap_or(true) {
                true => Some(HashMap::new()),
                false => None,
            },
            conflict_handler: ConflictHandlingStrategy::new(conflict_handler)?,
            help_arg_added: false,
            version_arg_added: false,
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
                for argument in parent_parser.arguments() {
                    // this is a pruned down version of add_argument
                    (
                        parser.flag_args,
                        parser.positional_args,
                        parser.arg_name_mapping,
                        parser.allow_abbrev_mapping,
                    ) = parser.check_for_duplicate_arg_names(argument.name())?;
                    let cloned_arg = argument.clone();
                    let temp_dest = cloned_arg.dest().clone();
                    let dest: Option<&str> = temp_dest.as_deref();
                    parser = parser.store_argument(cloned_arg, dest)?;
                }
            }
        }
        Ok(parser)
    }

    fn check_for_duplicate_arg_names(
        &self,
        arg_name: &ArgumentName,
    ) -> Result<
        (
            Vec<Argument>,
            Vec<Argument>,
            HashMap<String, (usize, bool)>,
            Option<HashMap<String, (usize, bool)>>,
        ),
        ArgumentError,
    > {
        let handle_overlap = |argument: &Argument| -> Option<Result<Argument, ArgumentError>> {
            let mut overlap = argument.name().overlap(arg_name);
            if overlap.is_empty() {
                Some(Ok(argument.clone()))
            } else {
                match self.conflict_handler {
                    ConflictHandlingStrategy::Error => {
                        overlap.sort();
                        Some(Err(ArgumentError::DuplicateArgumentNameValues(
                            string_vec_to_string(&overlap, true),
                        )))
                    }
                    ConflictHandlingStrategy::Override => {
                        if overlap.len() == argument.name().num_of_identifiers() {
                            None
                        } else {
                            // if posn would always have overlap of 1 & be caught above
                            debug_assert!(argument.name().is_flag_argument());
                            let mut existing_name_map = match &argument.name() {
                                ArgumentName::Flag(map) => map.clone(),
                                _ => panic!("argument with posn should have been removed above"),
                            };

                            for overlap_name in overlap {
                                existing_name_map = existing_name_map
                                    .into_iter()
                                    .map(|(k, v)| {
                                        let mut new_v = v.clone();
                                        new_v.retain(|x| x != &overlap_name);
                                        (k, new_v)
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
        };

        let new_flag_arguments = self
            .flag_args
            .iter()
            .filter_map(|argument| handle_overlap(argument))
            .collect::<Result<Vec<Argument>, ArgumentError>>()?;
        let new_positional_arguments = self
            .positional_args
            .iter()
            .filter_map(|argument| handle_overlap(argument))
            .collect::<Result<Vec<Argument>, ArgumentError>>()?;

        let (new_arg_name_mapping, new_allow_abbrev_mapping) = if &new_flag_arguments.len()
            < &self.flag_args.len()
            || &new_positional_arguments.len() < &self.positional_args.len()
        {
            self.init_arg_idx_mappings(&new_flag_arguments, &new_positional_arguments)
        } else {
            (
                self.arg_name_mapping.clone(),
                self.allow_abbrev_mapping.clone(),
            )
        };
        Ok((
            new_flag_arguments,
            new_positional_arguments,
            new_arg_name_mapping,
            new_allow_abbrev_mapping,
        ))
    }

    fn parse_flag_argument_name(
        &self,
        argument_name: &String,
        is_short_flag: bool,
    ) -> Result<(Vec<Argument>, Option<String>), ParserError> {
        // check for values in form of "key=value"
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
        let location = if let Some(allow_abbrev_mapping) = self.allow_abbrev_mapping.as_ref() {
            let possible_locations: Vec<(&String, &(usize, bool))> = allow_abbrev_mapping
                .iter()
                .filter_map(|(k, v)| {
                    if k.starts_with(argument_name) {
                        Some((k, v))
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
                        .map(|(k, _)| k.clone())
                        .collect();
                    conflicting_arg_names.sort();
                    Err(ParserError::AmbiguousAbbreviatedArguments(
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
                let location = self.arg_name_mapping.get(argument_name.as_str());

                if location.is_some() {
                    Ok(location)
                } else if is_short_flag {
                    // will try to parse multiple short options in form of "-abc [value]"
                    Ok(None)
                } else {
                    Err(ParserError::InvalidFlagArgument(argument_name.clone()))
                }
            }
        }?;

        if location.is_none() {
            debug_assert!(argument_name.len() > 2); // must be multiple short flag or short flag & value
            let possible_args = &argument_name[..argument_name.len() - 1];
            let possible_optional_value = &argument_name[argument_name.len() - 1..];
            let mut arguments = Vec::new();
            for arg in possible_args.chars() {
                let arg = arg.to_string();
                println!("{:?}", self.arg_name_mapping);
                let &(idx, is_flag_arg) = self
                    .arg_name_mapping
                    .get(arg.as_str())
                    .ok_or(ParserError::InvalidFlagArgument(arg.clone()))?;
                debug_assert!(is_flag_arg);
                arguments.push(self.flag_args[idx].clone());
            }

            let optional_value_location = self.arg_name_mapping.get(possible_optional_value);
            if let Some(&(idx, is_flag_arg)) = optional_value_location {
                debug_assert!(is_flag_arg);
                let last_arg = arguments.last().unwrap();
                if !last_arg.nargs().can_be_zero() {
                    // TODO should
                    return Err(ParserError::IncorrectValueCount(
                        last_arg.name().to_string(),
                        last_arg.nargs().clone(),
                        0,
                    ));
                }
                arguments.push(self.flag_args[idx].clone())
            } else if optional_value.is_none() {
                let last_arg = arguments.last().unwrap();
                if !last_arg.nargs().is_valid_number(1) {
                    return Err(ParserError::IncorrectValueCount(
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
                    return Err(ParserError::IncorrectValueCount(
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
                return Err(ParserError::IncorrectValueCount(
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
    ) -> Result<ArgumentParser, ArgumentError> {
        let (help_arg_added, version_arg_added) = match argument.action() {
            Action::Help => {
                if self.help_arg_added && !self.conflict_handler.is_override() {
                    Err(ArgumentError::DuplicateHelpArgument(
                        argument.name().to_string(),
                    ))
                } else {
                    Ok((true, self.version_arg_added))
                }
            }
            Action::Version(_) => {
                if self.version_arg_added && !self.conflict_handler.is_override() {
                    Err(ArgumentError::DuplicateVersionArgument(
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
        for flag in argument.flag_values() {
            new_arg_name_mapping.insert(flag, new_arg_location);
        }

        let new_allow_abbrev_mapping = self.add_arg_to_abbrev_mapping(
            &argument,
            new_arg_location.0,
            &self.allow_abbrev_mapping,
        );

        let mut new_parser = ArgumentParser {
            positional_args: new_positional_args,
            flag_args: new_flag_args,
            arg_name_mapping: new_arg_name_mapping,
            usage: self.usage.clone(),
            desp: self.desp.clone(),
            prog: self.prog.clone(),
            epilog: self.epilog.clone(),
            prefix_chars: self.prefix_chars.clone(),
            argument_default: self.argument_default.clone(),
            allow_abbrev_mapping: new_allow_abbrev_mapping, // TODO correct default
            conflict_handler: self.conflict_handler,
            help_arg_added: help_arg_added,
            version_arg_added: version_arg_added,
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
            let existing_dest_arg = self.parse_flag_argument_name(&dest, false);
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
                    _ => Err(ArgumentError::DuplicateArgumentName(dest)),
                }?
            }
        };

        Ok(new_parser)
    }

    fn add_arg_to_abbrev_mapping(
        &self,
        new_arg: &Argument,
        idx: usize,
        allow_abbrev_mapping: &Option<HashMap<String, (usize, bool)>>,
    ) -> Option<HashMap<String, (usize, bool)>> {
        allow_abbrev_mapping.as_ref().map(|x| {
            let mut new_x = x.clone();
            match new_arg.name() {
                ArgumentName::Flag(map) => {
                    for (_, v) in map.into_iter() {
                        for argument in v.into_iter() {
                            if argument.len() > 1 {
                                new_x.insert(argument.clone(), (idx, true));
                            }
                        }
                    }
                    new_x
                }
                _ => x.clone(), // applies only to options
            }
        })
    }

    fn init_arg_idx_mappings(
        &self,
        flag_args: &Vec<Argument>,
        positional_args: &Vec<Argument>,
    ) -> (
        HashMap<String, (usize, bool)>,
        Option<HashMap<String, (usize, bool)>>,
    ) {
        let mut new_arg_idx_mapping: HashMap<String, (usize, bool)> = HashMap::new();
        let mut new_allow_abbrev_mapping =
            self.allow_abbrev_mapping.as_ref().map(|_| HashMap::new());
        for (idx, arg) in flag_args.iter().enumerate() {
            for arg_name in arg.flag_values() {
                new_arg_idx_mapping.insert(arg_name.clone(), (idx, true));
                new_allow_abbrev_mapping = new_allow_abbrev_mapping.map(|mut x| {
                    x.insert(arg_name.clone(), (idx, true));
                    x
                });
            }
        }
        for (idx, arg) in flag_args.iter().enumerate() {
            for arg_name in arg.flag_values() {
                new_arg_idx_mapping.insert(arg_name.clone(), (idx, true));
                new_allow_abbrev_mapping =
                    self.add_arg_to_abbrev_mapping(arg, idx, &new_allow_abbrev_mapping);
            }
        }
        for (idx, arg) in positional_args.iter().enumerate() {
            new_arg_idx_mapping.insert(arg.flag_values().first().unwrap().clone(), (idx, false));
        }
        (new_arg_idx_mapping, new_allow_abbrev_mapping)
    }

    pub fn add_argument<T: ToString>(
        &self,
        name: Vec<&str>,
        action: Option<&str>,
        nargs: Option<NArgs>,
        constant: Option<Vec<T>>,
        default: Option<Vec<T>>,
        choices: Option<Vec<Vec<T>>>,
        required: Option<bool>,
        help: Option<&str>,
        metavar: Option<&str>,
        dest: Option<&str>,
        version: Option<&str>,
    ) -> Result<ArgumentParser, ArgumentError> {
        let arg_name: ArgumentName = ArgumentName::new(name, &self.prefix_chars)?.clone();
        let (new_flag_args, new_positional_args, new_arg_name_mapping, new_allow_abbrev_mapping) =
            self.check_for_duplicate_arg_names(&arg_name)?;
        // this shouldn't happen often
        let constant = constant.map(|x| x.into_iter().map(|y| y.to_string()).collect());
        let default = default.map(|x| x.into_iter().map(|y| y.to_string()).collect());
        let choices = choices.map(|inner| {
            inner
                .into_iter()
                .map(|choice| choice.into_iter().map(|x| x.to_string()).collect())
                .collect()
        });

        let new_argument = Argument::new(
            arg_name, action, nargs, constant, default, choices, required, help, metavar, dest,
            version,
        )?;

        let new_parser = ArgumentParser {
            positional_args: new_positional_args,
            flag_args: new_flag_args,
            arg_name_mapping: new_arg_name_mapping,

            usage: self.usage.clone(),
            desp: self.desp.clone(),
            prog: self.prog.clone(),
            epilog: self.epilog.clone(),
            prefix_chars: self.prefix_chars.clone(),
            argument_default: self.argument_default.clone(),
            allow_abbrev_mapping: new_allow_abbrev_mapping, // TODO correct default
            conflict_handler: self.conflict_handler,
            help_arg_added: self.help_arg_added,
            version_arg_added: self.version_arg_added,
        };
        new_parser.store_argument(new_argument, dest)
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

    fn parse_raw_args(
        &self,
        raw_args: Option<Vec<String>>,
        keep_or_error_unknown_args: bool,
    ) -> Result<(Namespace, Vec<String>), ParserError> {
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
        // staring idx of the last posn arg group
        let mut last_posn_arg_group_idx: Option<usize> = None; // tracks indices in arg_and_raw_arg_range
                                                               // contine to parse so long as posn arguments are left or raw args haven't been looked at (may have flags)

        #[derive(Clone, Debug)]
        enum ArgumentValue {
            FromArg(String),
            AfterArgument(usize, usize),
        }

        while idx < raw_args.len()
            || (cur_posn_argument_idx.is_some_and(|idx| idx < self.positional_args.len()))
        {
            let cur_raw_arg = if idx >= raw_args.len() {
                None
            } else {
                Some(raw_args[idx].as_str())
            };

            let (found_arguments, argument_value) = if cur_raw_arg.is_some()
                && self.prefix_chars.parse_string(cur_raw_arg.unwrap()).1 > 0
            // TODO redo this check
            {
                let (flag_raw_arg, n_prefixes) =
                    self.prefix_chars.parse_string(&cur_raw_arg.unwrap());
                if n_prefixes == 0 {
                    panic!("found non-flag argument")
                };
                let flag_raw_arg = flag_raw_arg.to_string();

                // either returns
                // - single flag argument (long or short)
                // - single flag argument & single length value (--key=value)
                // - multiple short flag arguments & maybe a single length value
                let parsed_result: Result<(Vec<Argument>, Option<String>), ParserError> =
                    self.parse_flag_argument_name(&flag_raw_arg, n_prefixes == 1);

                if keep_or_error_unknown_args
                    && parsed_result == Err(ParserError::InvalidFlagArgument(flag_raw_arg.clone()))
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
                    return Err(ParserError::UnprocessedRawArguments(string_vec_to_string(
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
                // only use arg value on last found arg
                let argument_value = if !(i == last_arg_idx && argument_value.is_some()) {
                    let end_or_new_flag_arg = |idx: &usize| {
                        idx >= &raw_args.len()
                            || self.prefix_chars.parse_string(&raw_args[idx.clone()]).1 > 0
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
                        return Err(ParserError::IncorrectValueCount(
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
                                    return Err(ParserError::IncorrectValueCount(
                                        found_argument.name().to_string(),
                                        found_argument.nargs().clone(),
                                        found_value_count,
                                    ));
                                } // nothing added yet
                                (Some(n), false) => n, // args added, some are posns
                                (None, true) => {
                                    // posn args not added
                                    return Err(ParserError::IncorrectValueCount(
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
                            ArgumentValue::AfterArgument(group_start_idx, group_end_idx) => (group_start_idx, group_end_idx),
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
                            return Err(ParserError::IncorrectValueCount(
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
                                ArgumentValue::AfterArgument(group_start_idx, group_end_idx) => (group_start_idx, group_end_idx),
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

                                arg_and_raw_arg_range[abs_idx + rel_idx] = (
                                    arg.clone(),
                                    ArgumentValue::AfterArgument(new_start, new_end),
                                );
                            }
                        }

                        end_idx = Some(start_idx + perm_n_missing_args + found_value_count);
                    }

                    if end_idx.is_none() {
                        panic!("this should be set by now")
                    }

                    ArgumentValue::AfterArgument(start_idx, end_idx.unwrap())
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
                ArgumentValue::AfterArgument(start, end) => raw_args[start..end].to_vec(),
            };
            let new_argument = if argument.name().is_flag_argument() {
                let argument = if processed_arguments.contains(&argument) {
                    processed_arguments.get(&argument).unwrap().clone()
                } else {
                    argument
                };
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
                            Err(ParserError::DuplicateFlagArgument(
                                argument.name().to_string(),
                            ))
                        } else {
                            seen_arguments.contains(&argument); // mark as found
                            argument
                                .arg_value_in_choices(&arg_val_vec)
                                .map_err(ParserError::InvalidChoice)?;
                            Ok(argument.with_action(Action::Store(arg_val_vec)))
                        }
                    }
                    Action::StoreConst(c) => {
                        if seen_arguments.contains(&argument) {
                            Err(ParserError::DuplicateFlagArgument(
                                argument.name().to_string(),
                            ))
                        } else {
                            Ok(argument.with_action(Action::Store(c.clone())))
                        }
                    }
                    Action::Append(v) => {
                        argument
                            .arg_value_in_choices(&arg_val_vec)
                            .map_err(ParserError::InvalidChoice)?;
                        let mut new_v = v.clone();
                        if let Some(default) = argument.default() {
                            new_v.append(&mut default.clone())
                        }
                        new_v.append(&mut arg_val_vec);
                        Ok(argument.with_action(Action::Append(new_v)))
                    }
                    Action::AppendConst(c) => {
                        let vec_key = argument
                            .dest()
                            .as_ref()
                            .expect("this should have been checked before");
                        let parsed_argument = self.parse_flag_argument_name(&vec_key, false)?;

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
                            .map_err(ParserError::InvalidChoice)?;
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
                            .map_err(ParserError::InvalidChoice)?;

                        let arg_val = if (argument.nargs() == &NArgs::AnyNumber
                            || argument.nargs() == &NArgs::ZeroOrOne)
                            && arg_val_vec.is_empty()
                            && argument.default().is_some()
                        {
                            argument.default().as_ref().unwrap().clone()
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

        // defaults for missing flag arguments / rquired missing args
        let mut missing_required_flag_args = Vec::new();
        for arg in self.flag_args.iter() {
            if !processed_arguments.contains(arg) {
                if arg.required() {
                    missing_required_flag_args.push(arg)
                } else {
                    match arg.action() {
                        Action::Store(_) => {
                            processed_arguments.insert(
                                arg.with_action(Action::Store(
                                    arg.default()
                                        .clone()
                                        .expect("this should be guaranteed to have a default set"),
                                )),
                            );
                        }
                        _ => (),
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
            Err(ParserError::MissingRequiredFlagArguments(str_vec))
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
            Ok((result_namespace, unknown_args))
        }
    }

    pub fn parse_args(&self, raw_args: Option<Vec<String>>) -> Result<Namespace, ParserError> {
        let (namespace, remaining_args) = self.parse_raw_args(raw_args, false)?;
        debug_assert!(remaining_args.is_empty());
        Ok(namespace)
    }

    pub fn parse_known_args(
        &self,
        raw_args: Option<Vec<String>>,
    ) -> Result<(Namespace, Vec<String>), ParserError> {
        self.parse_raw_args(raw_args, true)
    }

    pub fn add_subparsers() {}

    pub fn add_argument_group() {
        todo!()
    }

    pub fn add_mutually_exclusive_group() {
        todo!()
    }

    pub fn get_default(&self, dest: &str) -> &Option<String> {
        todo!()
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
    use std::{fmt::Display, str::FromStr};

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
    fn default_usage() {
        let parser = ArgumentParser::new(
            Some("test_parser"),
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
        .add_argument::<&str>(
            vec!["-g"],
            None,
            Some(NArgs::Exact(2)),
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
        .add_argument::<&str>(
            vec!["--bug", "-b"],
            None,
            Some(NArgs::AnyNumber),
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
        .add_argument::<&str>(
            vec!["--bag"],
            None,
            Some(NArgs::ZeroOrOne),
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
        .add_argument::<&str>(
            vec!["goo"],
            None,
            Some(NArgs::OneOrMore),
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
        .add_argument::<&str>(
            vec!["--hyt"],
            None,
            Some(NArgs::OneOrMore),
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
        .add_argument::<&str>(
            vec!["boo"],
            None,
            Some(NArgs::ZeroOrOne),
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
        .add_argument::<&str>(
            vec!["gaf"],
            None,
            Some(NArgs::Exact(2)),
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
        .add_argument::<&str>(
            vec!["opy"],
            None,
            Some(NArgs::AnyNumber),
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
        .add_argument::<&str>(
            vec!["--got"],
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            Some("tog"),
            None,
            None,
        )
        .unwrap();
        assert_eq!(
            parser.format_usage(),
            "test_parser [-h] [-g G G] [-b [BUG ...]] [--bag [BAG]] [--hyt HYT [HYT ...]] [--got tog] goo [goo ...] [boo] gaf gaf [opy ...]".to_string()
        )
    }

    #[test]
    fn parser_with_help() {
        assert_eq!(
            ArgumentParser::new(
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some(true),
                None
            )
            .unwrap(),
            ArgumentParser::new(
                None,
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
                None
            )
            .unwrap()
        )
    }

    fn default_arg_parser() -> ArgumentParser {
        ArgumentParser::new(
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
    }

    use crate::{
        argument::{Argument, NArgs},
        argument_error::ArgumentError,
        argument_name::ArgumentName,
        argument_parser::{ParserError, PrefixChars},
        parse_result::Namespace,
    };

    #[test]
    fn add_non_required_positional_argument() {
        assert_eq!(
            default_arg_parser()
                .add_argument::<&str>(
                    vec!["foo"],
                    None,
                    None,
                    None,
                    None,
                    None,
                    Some(false),
                    None,
                    None,
                    None,
                    None
                )
                .unwrap_err(),
            ArgumentError::RequiredMarkedForPositionalArgument
        )
    }

    #[test]
    fn add_non_required_flag_argument_with_default() {
        assert_eq!(
            default_arg_parser()
                .add_argument::<&str>(
                    vec!["--foo"],
                    None,
                    Some(NArgs::AnyNumber),
                    None,
                    None,
                    None,
                    Some(false),
                    None,
                    None,
                    None,
                    None
                )
                .unwrap_err(),
            ArgumentError::NonRequiredArgumentNotGivenDefaultValue(
                ArgumentName::new(vec!["--foo"], &PrefixChars::default())
                    .unwrap()
                    .to_string()
            )
        )
    }

    #[test]
    fn add_required_flag_argument_with_default() {
        assert_eq!(
            default_arg_parser()
                .add_argument(
                    vec!["--foo"],
                    None,
                    None,
                    None,
                    Some(vec!["bar"]),
                    None,
                    Some(true),
                    None,
                    None,
                    None,
                    None
                )
                .unwrap_err(),
            ArgumentError::RequiredArgumentDefaultValueGiven(
                ArgumentName::new(vec!["--foo"], &PrefixChars::default())
                    .unwrap()
                    .to_string(),
                "[bar]".to_string()
            ),
        )
    }

    #[test]
    fn duplicate_flag_argument_names() {
        assert_eq!(
            default_arg_parser()
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
                    None
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
                    None
                )
                .unwrap_err(),
            ArgumentError::DuplicateArgumentNameValues("[foo]".to_string())
        )
    }

    #[test]
    fn duplicate_positional_argument_names_error() {
        assert_eq!(
            default_arg_parser()
                .add_argument::<&str>(
                    vec!["foo"],
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None
                )
                .unwrap()
                .add_argument::<&str>(
                    vec!["foo"],
                    None,
                    None,
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
            ArgumentError::DuplicateArgumentNameValues("[foo]".to_string())
        )
    }

    #[test]
    fn duplicate_positional_argument_names_override() {
        let parser = ArgumentParser::new(
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            Some("override"),
            None,
            None,
        )
        .unwrap()
        .add_argument::<&str>(
            vec!["foo"],
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
        assert!(parser
            .add_argument::<&str>(
                vec!["foo"],
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None
            )
            .is_ok())
    }

    #[test]
    fn add_float_arg() {
        let expected_arg = Argument::new(
            ArgumentName::new(vec!["-b", "--bar"], &PrefixChars::default()).unwrap(),
            None,
            Some(NArgs::AnyNumber),
            None,
            Some(vec![5.6.to_string(), 1.2.to_string()]),
            Some(vec![
                vec![5.6.to_string(), 1.2.to_string()],
                vec![3.4.to_string(), 11.9.to_string()],
            ]),
            Some(false),
            None,
            None,
            None,
            None,
        )
        .unwrap();
        let parser = default_arg_parser()
            .add_argument::<f64>(
                vec!["-b", "--bar"],
                None,
                Some(NArgs::AnyNumber),
                None,
                Some(vec![5.6, 1.2]),
                Some(vec![vec![5.6, 1.2], vec![3.4, 11.9]]),
                None,
                None,
                None,
                None,
                None,
            )
            .unwrap();
        argument_in_parser(parser, expected_arg)
    }

    #[test]
    fn add_int_arg() {
        let expected_arg = Argument::new(
            ArgumentName::new(vec!["-b", "--bar"], &PrefixChars::default()).unwrap(),
            None,
            Some(NArgs::AnyNumber),
            None,
            Some(vec![5.to_string(), 1.to_string()]),
            Some(vec![
                vec![5.to_string(), 1.to_string()],
                vec![3.to_string(), 1.to_string()],
            ]),
            Some(false),
            None,
            None,
            None,
            None,
        )
        .unwrap();
        let parser = default_arg_parser()
            .add_argument::<i64>(
                vec!["-b", "--bar"],
                None,
                Some(NArgs::AnyNumber),
                None,
                Some(vec![5, 1]),
                Some(vec![vec![5, 1], vec![3, 1]]),
                None,
                None,
                None,
                None,
                None,
            )
            .unwrap();
        argument_in_parser(parser, expected_arg)
    }

    fn argument_in_parser(parser: ArgumentParser, expected_arg: Argument) {
        let mut found = false;
        for argument in parser.arguments().iter() {
            if argument == &&expected_arg {
                found = true;
                break;
            }
        }
        assert!(found)
    }

    #[test]
    fn add_bool_arg() {
        let expected_arg = Argument::new(
            ArgumentName::new(vec!["-b", "--bar"], &PrefixChars::default()).unwrap(),
            None,
            Some(NArgs::AnyNumber),
            None,
            Some(vec![false.to_string(), true.to_string()]),
            Some(vec![
                vec![true.to_string(), false.to_string()],
                vec![false.to_string(), true.to_string()],
            ]),
            Some(false),
            None,
            None,
            None,
            None,
        )
        .unwrap();
        let parser = default_arg_parser()
            .add_argument::<bool>(
                vec!["-b", "--bar"],
                None,
                Some(NArgs::AnyNumber),
                None,
                Some(vec![false, true]),
                Some(vec![vec![true, false], vec![false, true]]),
                None,
                None,
                None,
                None,
                None,
            )
            .unwrap();
        argument_in_parser(parser, expected_arg)
    }

    #[test]
    fn add_enum_arg() {
        enum Test {
            One,
            Two,
            Three,
        }

        impl Display for Test {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(
                    f,
                    "{}",
                    match self {
                        Test::One => "1",
                        Test::Two => "2",
                        Test::Three => "3",
                    }
                )
            }
        }

        impl FromStr for Test {
            type Err = String;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    "1" => Ok(Test::One),
                    "2" => Ok(Test::Two),
                    "3" => Ok(Test::Three),
                    _ => Err("invalid test".to_string()),
                }
            }
        }

        let expected_arg = Argument::new(
            ArgumentName::new(vec!["-b", "--bar"], &PrefixChars::default()).unwrap(),
            None,
            None,
            None,
            Some(vec![Test::One.to_string(), Test::One.to_string()]),
            Some(vec![
                vec![Test::One.to_string(), Test::One.to_string()],
                vec![Test::Two.to_string(), Test::Two.to_string()],
            ]),
            Some(false),
            None,
            None,
            None,
            None,
        )
        .unwrap();
        let parser = default_arg_parser()
            .add_argument::<Test>(
                vec!["-b", "--bar"],
                None,
                Some(NArgs::AnyNumber),
                None,
                Some(vec![Test::One, Test::One]),
                Some(vec![vec![Test::One, Test::One], vec![Test::Two, Test::Two]]),
                None,
                None,
                None,
                None,
                None,
            )
            .unwrap();
        argument_in_parser(parser, expected_arg)
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
            ArgumentError::EmptyPrefixChars
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
            ArgumentError::IllegalPrefixChars("\t, \n".to_string())
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
            ArgumentError::UnsupportedConflictHandlingStrategy("foo".to_string())
        )
    }

    #[test]
    fn add_duplicate_argument_name_with_overlap_error() {
        let parser = ArgumentParser::new(
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            Some("error"),
            Some(false),
            None,
        )
        .unwrap()
        .add_argument::<&str>(
            vec!["--foo", "-h"],
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
        assert_eq!(
            parser
                .parse_args(Some(vec!["-h".to_string(), "a".to_string()]))
                .unwrap()
                .get_one_value::<String>("foo")
                .unwrap(),
            "a"
        );
        assert_eq!(
            parser
                .parse_args(Some(vec!["--foo".to_string(), "a".to_string()]))
                .unwrap()
                .get_one_value::<String>("foo")
                .unwrap(),
            "a"
        );
        assert_eq!(
            parser
                .add_argument::<&str>(
                    vec!["--h"],
                    None,
                    None,
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
            ArgumentError::DuplicateArgumentNameValues("[h]".to_string())
        )
    }

    mod parent_parser_tests {
        use crate::{argument_error::ArgumentError, argument_parser::ArgumentParser};

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
            let child = child.unwrap();
            let namespace = child
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
                ArgumentError::DuplicateArgumentNameValues("[h, help]".to_string())
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

    mod extraneous_parsing {
        use std::string::ParseError;

        use crate::argument_parser::ParserError;

        use super::*;
        #[test]
        fn long_option_value_with_argument() {
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
                    None,
                    None,
                )
                .unwrap()
                .parse_args(Some(vec!["--foo=bar".to_string()]))
                .unwrap();
            assert_eq!(
                namespace.get_one_value::<String>("foo").unwrap(),
                "bar".to_string()
            )
        }

        #[test]
        fn long_option_value_with_multiple_nargs() {
            let parser = ArgumentParser::default()
                .add_argument::<&str>(
                    vec!["--foo"],
                    None,
                    Some(NArgs::Exact(2)),
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
                .parse_args(Some(vec!["--foo=bar".to_string()]));
            assert_eq!(
                parser.unwrap_err(),
                ParserError::IncorrectValueCount("[--foo]".to_string(), NArgs::Exact(2), 1)
            );
        }
    }

    #[test]
    fn longer_flag_args() {
        let namespace = ArgumentParser::default()
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
                None,
                None,
            )
            .unwrap()
            .parse_args(Some(vec!["---foo".to_string(), "bar".to_string()]))
            .unwrap();
        assert_eq!(
            namespace.get_one_value::<String>("foo").unwrap(),
            "bar".to_string()
        );
    }

    #[test]
    fn multiple_short_flags() {
        let namespace = ArgumentParser::default()
            .add_argument::<&str>(
                vec!["-a"],
                Some("store_true"),
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
            .add_argument::<&str>(
                vec!["-b"],
                Some("store_true"),
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
            .add_argument::<&str>(
                vec!["-c"],
                Some("store_true"),
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
            .parse_args(Some(vec!["-abc".to_string()]))
            .unwrap();
        assert_eq!(namespace.get_one_value::<bool>("a").unwrap(), true);
        assert_eq!(namespace.get_one_value::<bool>("b").unwrap(), true);
        assert_eq!(namespace.get_one_value::<bool>("c").unwrap(), true);
    }

    #[test]
    fn multiple_short_flags_with_optional_value() {
        let namespace = ArgumentParser::default()
            .add_argument::<&str>(
                vec!["-a"],
                Some("store_true"),
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
            .add_argument::<&str>(
                vec!["-b"],
                Some("store_true"),
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
            .add_argument::<&str>(
                vec!["-c"],
                None,
                Some(NArgs::Exact(1)),
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
            .parse_args(Some(vec!["-abcD".to_string()]))
            .unwrap();
        assert_eq!(namespace.get_one_value::<bool>("a").unwrap(), true);
        assert_eq!(namespace.get_one_value::<bool>("b").unwrap(), true);
        assert_eq!(
            namespace.get_one_value::<String>("c").unwrap(),
            "D".to_string()
        );
    }

    #[test]
    fn multiple_short_flags_invalid_flag() {
        let parser = ArgumentParser::default()
            .add_argument::<&str>(
                vec!["-a"],
                Some("store_true"),
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
            .add_argument::<&str>(
                vec!["-b"],
                Some("store_true"),
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
            .parse_args(Some(vec!["-acb".to_string()]))
            .unwrap_err();
        assert_eq!(parser, ParserError::InvalidFlagArgument("c".to_string()));
    }

    #[test]
    fn multiple_short_flags_with_optional_arg_invalid_nargs() {
        let parser = ArgumentParser::default()
            .add_argument::<&str>(
                vec!["-a"],
                Some("store_true"),
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
            .add_argument::<&str>(
                vec!["-b"],
                Some("store_true"),
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
            .parse_args(Some(vec!["-abC".to_string()]))
            .unwrap_err();
        assert_eq!(
            parser,
            ParserError::IncorrectValueCount("[-b]".to_string(), NArgs::Exact(0), 1)
        );
    }

    #[test]
    fn multiple_short_flags_invalid_nargs() {
        let parser = ArgumentParser::default()
            .add_argument::<&str>(
                vec!["-a"],
                None,
                Some(NArgs::Exact(2)),
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
            .parse_args(Some(vec!["-a".to_string()]))
            .unwrap_err();
        assert_eq!(
            parser,
            ParserError::IncorrectValueCount("[-a]".to_string(), NArgs::Exact(2), 0)
        );
    }
}
