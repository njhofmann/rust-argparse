use std::{
    collections::{HashMap, HashSet},
    env,
    fmt::Display,
    hash::Hash,
};

use crate::{
    argument::{self, Action, Argument, NArgs},
    argument_error::ArgumentError,
    argument_name::ArgumentName,
    parse_result::Namespace,
    string_vec_to_string, InvalidChoice, FLAG_ARG_ABBREV_LEN, FLAG_ARG_LEN,
};
use thiserror::Error;

const ILLEGAL_PREFIX_CHARS: [char; 3] = [' ', '\n', '\t']; // TODO fill out illegal chars

#[derive(Error, Debug, PartialEq, Eq)]
pub enum ParserError {
    #[error("missing positional arguments {0}")]
    MissingPositionArguments(String),
    #[error("argument {0} expected {1} values, but found {2}")]
    IncorrectValueCount(String, NArgs, usize),
    #[error("missing values for required flag aruments {0}")]
    MissingRequiredFlagArguments(String),
    #[error("invalid flag argument {0} found")]
    InvalidFlagArgument(String),
    #[error("duplicate flag argument {0} found")]
    DuplicateFlagArgument(String),
    #[error(
        "expected flag argument starting with '-' or '--' but found raw value \"{0}\" instead"
    )]
    ExpectedFlagArgument(String), // TODO is this needed
    #[error("found multiple arguments {1} for abbreviated argument key \"{0}\"")]
    ConflictingArguments(String, String),
    #[error("{0}")]
    InvalidChoice(InvalidChoice),
    #[error("no arguments were found to process remaining raw arguments {0}")]
    UnprocessedRawArguments(String)
}

#[derive(Debug, PartialEq)]
pub enum PrefixCharOutcomes {
    LONG,
    ABBREV,
    NONE,
}

impl PrefixCharOutcomes {
    fn is_flag(&self) -> bool {
        match self {
            PrefixCharOutcomes::NONE => false,
            _ => true,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PrefixChars(HashSet<char>);

impl PrefixChars {
    pub fn new(chars: Option<&str>) -> Result<PrefixChars, ArgumentError> {
        match chars {
            None => Ok(PrefixChars(HashSet::from_iter::<Vec<char>>(vec!['-']))),
            Some(x) => {
                let temp = HashSet::from_iter(x.to_string().chars().into_iter());
                if temp.is_empty() {
                    Err(ArgumentError::EmptyPrefixChars)
                } else {
                    let illegal_chars: Vec<String> = ILLEGAL_PREFIX_CHARS
                        .iter()
                        .filter_map(|x| {
                            if temp.contains(x) {
                                Some(x.to_string())
                            } else {
                                None
                            }
                        })
                        .collect();

                    if illegal_chars.is_empty() {
                        Ok(PrefixChars(temp))
                    } else {
                        Err(ArgumentError::IllegalPrefixChars(string_vec_to_string(
                            &illegal_chars,
                            false,
                        ))) // TODO change this error
                    }
                }
            }
        }
    }

    pub fn parse_string(&self, string: &str) -> PrefixCharOutcomes {
        if string.len() < 2 {
            PrefixCharOutcomes::NONE
        } else {
            let mut chars = string.chars();
            let a = chars.next().unwrap();
            let b = chars.next().unwrap();
            if !self.0.contains(&a) {
                PrefixCharOutcomes::NONE
            } else if a == b {
                if string.len() > 2 {
                    // TODO is ++ or -- w/ no text valid?
                    PrefixCharOutcomes::LONG
                } else {
                    PrefixCharOutcomes::NONE
                }
            } else {
                PrefixCharOutcomes::ABBREV
            }
        }
    }
}

impl Default for PrefixChars {
    fn default() -> Self {
        PrefixChars::new(Some("-")).unwrap()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ArgumentParser {
    positional_args: Vec<Argument>,
    flag_args: Vec<Argument>,
    arg_name_mapping: HashMap<String, (usize, bool)>, // vec idx & flag bool
    desp: Option<String>,
    prog: String,
    epilog: Option<String>,
    prefix_chars: PrefixChars,
    argument_default: Option<String>,
    allow_abbrev_mapping: Option<HashMap<String, (usize, bool)>>,
    help_arg_added: bool,
    version_arg_added: bool,
}

impl Display for ArgumentParser {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut builder = if let Some(desp) = self.desp.as_ref() {
            format!("ArgumentParser - {}:", desp)
        } else {
            "ArgumentParser:".to_string()
        };

        if self.positional_args.len() > 0 {
            builder += "\nPositional Arguments:";
            for argument in self.positional_args.iter() {
                builder += format!("\n- {}", argument).as_str();
            }
        }

        if self.flag_args.len() > 0 {
            builder += "\nFlag Arguments:";
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
        ArgumentParser::new(None, None, None, None, None, None, None, None, None).unwrap()
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
        add_help: Option<bool>,
        allow_abbrev: Option<bool>,
    ) -> Result<ArgumentParser, ArgumentError> {
        let mut parser = ArgumentParser {
            positional_args: Vec::new(),
            flag_args: Vec::new(),
            arg_name_mapping: HashMap::new(),
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
            help_arg_added: false,
            version_arg_added: false,
        };

        if let Some(parents) = parents {
            for parent_parser in parents.iter() {
                for argument in parent_parser.arguments() {
                    parser = parser.store_argument(argument.clone(), None)?;
                }
            }
        }

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

        Ok(parser)
    }

    fn check_for_duplicate_arg_names(&self, arg_name: &ArgumentName) -> Result<(), ArgumentError> {
        for flag_arg in self.flag_args.iter() {
            let overlap = flag_arg.name().overlap(arg_name);
            if !overlap.is_empty() {
                return Err(ArgumentError::DuplicateArgumentNameValues(
                    string_vec_to_string(&overlap, true),
                ));
            }
        }

        for positional_arg in self.positional_args.iter() {
            let overlap = positional_arg.name().overlap(&arg_name);
            if !overlap.is_empty() {
                return Err(ArgumentError::DuplicateArgumentNameValues(
                    string_vec_to_string(&overlap, true),
                ));
            }
        }

        Ok(())
    }

    fn get_flag_argument(&self, argument_name: &String) -> Result<Argument, ParserError> {
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
                    let conflicting_arg_names: Vec<String> = possible_locations
                        .into_iter()
                        .map(|(k, _)| k.clone())
                        .collect();
                    Err(ParserError::ConflictingArguments(
                        argument_name.clone(),
                        string_vec_to_string(&conflicting_arg_names, true),
                    ))
                }
            }
        } else {
            Ok(None)
        }?;

        let &(idx, is_flag_arg) = match location {
            Some(x) => Ok(x),
            None => self
                .arg_name_mapping
                .get(argument_name.as_str())
                .ok_or(ParserError::InvalidFlagArgument(argument_name.clone())),
        }?;

        debug_assert!(is_flag_arg);
        Ok(self.flag_args[idx].clone())
    }

    pub fn store_argument(
        mut self,
        argument: Argument,
        dest: Option<&str>,
    ) -> Result<ArgumentParser, ArgumentError> {
        let (help_arg_added, version_arg_added) = match argument.action() {
            Action::Help => {
                if self.help_arg_added {
                    Err(ArgumentError::DuplicateHelpArgument(
                        argument.name().to_string(),
                    ))
                } else {
                    Ok((true, self.version_arg_added))
                }
            }
            Action::Version(_) => {
                if self.version_arg_added {
                    Err(ArgumentError::DuplicateVersionArgument(
                        argument.name().to_string(),
                    ))
                } else {
                    Ok((self.help_arg_added, true))
                }
            }
            _ => Ok((self.help_arg_added, self.version_arg_added)),
        }?;

        let new_arg_location = if argument.name().is_flag_argument() {
            self.flag_args.push(argument.clone());
            Ok((self.flag_args.len() - 1, true))
        } else {
            self.positional_args.push(argument.clone());
            Ok((self.positional_args.len() - 1, false))
        }?;

        for flag in argument.flag_values() {
            self.arg_name_mapping.insert(flag, new_arg_location);
        }

        self.allow_abbrev_mapping = self.allow_abbrev_mapping.map(|x| {
            let mut new_x = x.clone();
            match argument.name() {
                ArgumentName::Positional(_) => x,
                ArgumentName::Flag { full, abbrev } => {
                    for argument in full {
                        new_x.insert(argument.clone(), new_arg_location.clone());
                    }
                    new_x
                }
            }
        });

        if let Action::AppendConst(_) = argument.action() {
            let new_dest_argument = Argument::new(
                ArgumentName::Flag {
                    full: vec![dest.unwrap().to_string()],
                    abbrev: vec![],
                },
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
            .unwrap();
            if !self
                .arg_name_mapping
                .contains_key(new_dest_argument.fetch_value())
            {
                self.flag_args.push(new_dest_argument.clone());
                self.arg_name_mapping.insert(
                    new_dest_argument.fetch_value().clone(),
                    (self.flag_args.len() - 1, true),
                );
            }
        };

        Ok(ArgumentParser {
            positional_args: self.positional_args,
            flag_args: self.flag_args,
            arg_name_mapping: self.arg_name_mapping,
            desp: self.desp,
            prog: self.prog,
            epilog: self.epilog,
            prefix_chars: self.prefix_chars,
            argument_default: self.argument_default,
            allow_abbrev_mapping: self.allow_abbrev_mapping, // TODO correct default
            help_arg_added: help_arg_added,
            version_arg_added: version_arg_added,
        })
    }

    pub fn add_argument<T: ToString>(
        mut self,
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
        self.check_for_duplicate_arg_names(&arg_name)?;
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
        self.store_argument(new_argument, dest)
    }

    fn divide_raw_positional_args(
        &self,
        all_nargs: Vec<&NArgs>,
        args: &Vec<String>,
    ) -> Result<Vec<usize>, ParserError> {
        let mut indices = vec![0];
        for narg in all_nargs.into_iter() {
            match narg {
                NArgs::Range(_, _) => todo!(),
                NArgs::Exact(x) => {
                    let new_index = indices.last().unwrap() + x;
                    if new_index <= args.len() {
                        indices.push(new_index)
                    } else {
                        panic!() // TODO not enough values
                    }
                }
                NArgs::AtLeastOne => todo!(),
                _ => panic!("unsupported nargs for positional arguments"),
            }
        }
        Ok(indices)
    }

    pub fn arguments(&self) -> Vec<&Argument> {
        let mut arguments = Vec::new();
        for argument in self.positional_args.iter() {
            arguments.push(argument)
        }
        for argument in self.flag_args.iter() {
            arguments.push(argument)
        }
        arguments
    }

    pub fn parse_args(&self, raw_args: Option<Vec<String>>) -> Result<Namespace, ParserError> {
        let raw_args = raw_args.unwrap_or(env::args().collect());
        let mut processed_arguments: HashSet<Argument> = HashSet::new();
        let mut idx = 0;
        let mut cur_posn_argument_idx: Option<usize> = self.positional_args.first().map(|_| 0);
        let mut processed_posn_args: Vec<Argument> = Vec::new();
        let mut arg_and_raw_arg_range: Vec<(Argument, (usize, usize))> = Vec::new();
        let mut last_arg_group_idx: Option<usize> = None;  // tracks indices in arg_and_raw_arg_range
        // contine to parse so long as posn arguments are left or raw args haven't been looked at (may have flags)
        while idx < raw_args.len()
            || (cur_posn_argument_idx.is_some_and(|idx| idx < self.positional_args.len()))
        {           
            let cur_raw_arg = if idx >= raw_args.len()  {
                // TODO fix this, cover case of not enough argsfor remaining 
                // error only 
                None
            } else {
                Some(raw_args[idx].as_str())
            };
            //  
            let found_argument = if cur_raw_arg.is_some() && self.prefix_chars.parse_string(cur_raw_arg.unwrap()).is_flag() {
                let cur_raw_arg = match self.prefix_chars.parse_string(&cur_raw_arg.unwrap()) {
                    PrefixCharOutcomes::LONG => Ok(cur_raw_arg.unwrap()[FLAG_ARG_LEN..].to_string()),
                    PrefixCharOutcomes::ABBREV => {
                        Ok(cur_raw_arg.unwrap()[FLAG_ARG_ABBREV_LEN..].to_string())
                    }
                    _ => panic!("found non-flag argument")
                }?;

                let mut found_arg = self.get_flag_argument(&cur_raw_arg)?;
                if processed_arguments.contains(&found_arg) {
                    found_arg = processed_arguments.get(&found_arg).unwrap().clone();
                }
                
                last_arg_group_idx = Some(last_arg_group_idx.map_or(0, |x| x + 1));
                idx += 1; // move past flag
                Ok(found_arg.clone())
            } else if cur_posn_argument_idx.is_none() {
                Err(ParserError::UnprocessedRawArguments(string_vec_to_string(&raw_args[idx..].to_vec(), false)))
            } else {
                if last_arg_group_idx.is_none() { // TODO this is never 
                    last_arg_group_idx = Some(last_arg_group_idx.map_or(0, |x| x + 1)) 
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
                Ok(found_argument)
            }?;

            let end_or_new_flag_arg = |idx: &usize| {
                idx >= &raw_args.len()
                    || self
                        .prefix_chars
                        .parse_string(&raw_args[idx.clone()])
                        .is_flag()
            };

            let mut start_idx = idx;
            let mut found_value_count= 0;
            let mut n_missing_args = 0;

            let mut end_idx: Option<usize> = match found_argument.nargs() {
                NArgs::Range(_, _) => panic!(), // TODO support this later
                NArgs::Exact(n) => {
                    let mut actual_value_count = 0;
                    let mut return_val: Option<usize> = None;
                    while idx < start_idx + n {
                        // - 1 as idx starts on first argument
                        if end_or_new_flag_arg(&idx) {
                            // TODO if out check prior variable args exist, shift all the way down IF no flags
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
                    // TODO attempt shift here?
                    if !end_or_new_flag_arg(&idx) {
                        idx += 1;
                    }
                    Some(idx)
                }
                NArgs::AnyNumber => {
                    while !end_or_new_flag_arg(&idx) {
                        idx += 1
                    }
                    Some(idx)
                }
                NArgs::AtLeastOne => {
                    idx += 1;
                    if end_or_new_flag_arg(&idx) {
                        // TODO attempt shift here
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
            
            if end_idx.is_none() {
                start_idx -= n_missing_args;
                end_idx = Some(start_idx + n_missing_args + found_value_count);
                // push back start_idx
                // if variable arg val was found since flag arg, attempt to pull X args
                // push back rest of args
                let start_group_idx = match (last_arg_group_idx, arg_and_raw_arg_range.is_empty()) {
                    (None, true) => 0,
                    (None, false) => return Err(ParserError::IncorrectValueCount(
                        found_argument.name().to_string(),
                        found_argument.nargs().clone(),
                        found_value_count,
                    )), // TODO raise not enough args here 
                    (Some(n), false) => n,
                    (Some(_), true) => panic!("flag group found but no arg groups recorded"),
                };

                let mut group_shifts = Vec::new();
                for (relative_idx, (parsed_argument, (group_start_idx, group_end_idx))) in arg_and_raw_arg_range[start_group_idx..].iter().enumerate().rev() {
                    // debug_assert!(match parsed_argument.nargs() {
                    //     NArgs::Exact(_) | NArgs::AtLeastOne => true, // only expect these type of nargs here
                    //     _ => also
                    // });

                    // debug_assert!(!parsed_argument.name().is_flag_argument());
                    let n_excess_args = *group_end_idx as i32 - *group_start_idx as i32 - match parsed_argument.nargs() {
                        NArgs::Range(_, _) => todo!(),
                        NArgs::Exact(n) => *n as i32,
                        NArgs::ZeroOrOne | NArgs::AnyNumber => 0,
                        NArgs::AtLeastOne => 1,
                    };

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
                        group_shifts.push((start_group_idx + relative_idx, n_args_to_shift));
                    }

                    if n_missing_args == 0 {
                        break;
                    }
                }

                if n_missing_args > 0 {
                    panic!() // TODO not enough excess args to pull
                }

                // TODO downshift each group
                for (abs_idx, shift ) in group_shifts.into_iter().rev() {
                    for (rel_idx, (arg, (start, end))) in arg_and_raw_arg_range.clone()[abs_idx..].iter().enumerate() {
                        // only take items from first item in group, shift remaining one's down
                        // TODO abs_idx is wrong variable to write to
                        arg_and_raw_arg_range[abs_idx + rel_idx] = (arg.clone(), (if rel_idx == 0 { *start } else { *start - shift}, end - shift));
                    }
                }
            }

            // TODO ordering adjustments
            // - "zero or one": if values left take one, else none (like at end)
            //     - if followed by exact, keep 1
            // - if out of values, if last one variable and has some left try to take some (if key needs it)
            //     - if "zero or one", take value from it
            //     - if key needs it, variable after variable will be empty

            // TODO rerun this at end / everytime boundaries are iolvated
            // let actual_value_count = idx - start_val_idx;
            // if !found_argument.nargs().is_valid_number(actual_value_count) {
            //     return Err(ParserError::IncorrectValueCount(
            //         found_argument.name().to_string(),
            //         found_argument.nargs().clone(),
            //         actual_value_count,
            //     ));
            // }

            arg_and_raw_arg_range.push((found_argument, (start_idx, end_idx.unwrap())))
        }

        let mut seen_arguments: HashSet<Argument> = HashSet::new(); // TODO needed? or can just use arg_vals
        for (argument, (start_idx, end_idx)) in arg_and_raw_arg_range {
            let mut arg_val_vec = raw_args[start_idx..end_idx].to_vec();
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
                        let mut storing_argument = self.get_flag_argument(&vec_key)?;
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
                            _ => panic!(),
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
                // TODO turn if else into match w/ is_flag_arg
                // TODO process actions, depending on arg type
                argument.with_action(match argument.action() {
                    Action::Store(_) => {
                        argument
                            .arg_value_in_choices(&arg_val_vec)
                            .map_err(ParserError::InvalidChoice)?;
                        Ok(Action::Store(arg_val_vec))
                    }
                    _ => panic!(), // TODO what else to support for posn arguments
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
            Ok(Namespace::new(HashMap::from_iter(
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
            )))
        }
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
        todo!()
    }

    pub fn print_help(&self) {
        todo!()
    }

    pub fn format_usage(&self) -> String {
        todo!()
    }

    pub fn format_help(&self) -> String {
        todo!()
    }

    pub fn parse_known_args(&self) -> Result<(Namespace, Vec<String>), ParserError> {
        todo!()
    }

    pub fn parse_intermixed_args(&self) -> Result<Namespace, ParserError> {
        todo!()
    }

    pub fn parse_known_intermixed_args(&self) -> Result<(Namespace, Vec<String>), ParserError> {
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
            ArgumentParser::new(None, None, None, None, None, None, None, Some(false), None)
                .unwrap()
                .to_string(),
            "ArgumentParser:".to_string()
        )
    }

    #[test]
    fn display_with_desp() {
        assert_eq!(
            ArgumentParser::new(
                None,
                None,
                Some("this parses arguments"),
                None,
                None,
                None,
                None,
                Some(false),
                None
            )
            .unwrap()
            .to_string(),
            "ArgumentParser - this parses arguments:".to_string()
        )
    }

    #[test]
    fn parser_with_help() {
        assert_eq!(
            ArgumentParser::new(None, None, None, None, None, None, None, Some(true), None)
                .unwrap(),
            ArgumentParser::new(None, None, None, None, None, None, None, Some(false), None)
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
        ArgumentParser::new(None, None, None, None, None, None, None, Some(false), None).unwrap()
    }

    use crate::{
        argument::{self, Argument, NArgs},
        argument_error::ArgumentError,
        argument_name::ArgumentName,
        argument_parser::PrefixChars,
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
    fn duplicate_positional_argument_names() {
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
            ArgumentParser::new(None, None, None, None, None, Some(""), None, None, None)
                .unwrap_err(),
            ArgumentError::EmptyPrefixChars
        )
    }
    #[test]
    fn unsupported_prefix_chars() {
        assert_eq!(
            ArgumentParser::new(None, None, None, None, None, Some("\n\t"), None, None, None)
                .unwrap_err(),
            ArgumentError::IllegalPrefixChars("\n, \t".to_string())
        )
    }
}
