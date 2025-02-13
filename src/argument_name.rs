use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;

use crate::prefix_chars;
use prefix_chars::PrefixChars;
use thiserror::Error;

use crate::string_vec_to_string;

#[derive(Error, Debug, PartialEq, Eq)]
pub enum ArgumentNameError {
    #[error("no argument names given")]
    EmptyArgumentName,
    #[error("given a mix of positional arguments & flag argument names")]
    MixedArguments,
    #[error("given multiple positional argument names")]
    MultiplePositionalArgumentNames,
    #[error("duplicate argument name {0} found")]
    DuplicateArgumentName(String),
}

#[derive(Clone, Debug, Eq)]
pub enum ArgumentName {
    Positional(String),
    Flag(HashMap<usize, Vec<String>>), // num of prexies to identifiers given with that prefix
}

impl ArgumentName {
    pub fn new(
        raw_arg_names: Vec<&str>,
        prefix_chars: &PrefixChars,
    ) -> Result<ArgumentName, ArgumentNameError> {
        if raw_arg_names.is_empty() {
            Err(ArgumentNameError::EmptyArgumentName)
        } else if raw_arg_names.len() == 1 {
            let raw_arg_name = raw_arg_names.first().unwrap();
            let (parsed_arg_name, n_prefixes) = prefix_chars.parse_raw_name_string(raw_arg_name);
            if n_prefixes == 0 {
                Ok(ArgumentName::Positional(parsed_arg_name.to_string()))
            } else {
                let mut map = HashMap::new();
                map.insert(n_prefixes, vec![parsed_arg_name.to_string()]);
                Ok(ArgumentName::Flag(map))
            }
        } else {
            // TODO multiple posn arguments error vs mixed arguments err
            let mut map: HashMap<usize, Vec<String>> = HashMap::new();
            let mut posn_arg_found = false;
            for raw_arg_name in raw_arg_names.into_iter() {
                let (parsed_arg_name, n_prefixes) =
                    prefix_chars.parse_raw_name_string(raw_arg_name);
                if n_prefixes == 0 {
                    if !map.is_empty() {
                        return Err(ArgumentNameError::MixedArguments);
                    } else if posn_arg_found {
                        return Err(ArgumentNameError::MultiplePositionalArgumentNames);
                    } else {
                        posn_arg_found = true;
                    }
                } else {
                    if posn_arg_found {
                        return Err(ArgumentNameError::MixedArguments);
                    }
                    if map
                        .clone()
                        .into_iter()
                        .any(|(_, v)| v.contains(&parsed_arg_name.to_string()))
                    {
                        return Err(ArgumentNameError::DuplicateArgumentName(
                            parsed_arg_name.to_string(),
                        ));
                    }

                    let mut temp_map = map.clone();
                    if !temp_map.contains_key(&n_prefixes) {
                        temp_map.insert(n_prefixes, Vec::new());
                    }
                    temp_map
                        .get_mut(&n_prefixes)
                        .unwrap()
                        .push(parsed_arg_name.to_string());
                    map = temp_map;
                }
            }

            Ok(ArgumentName::Flag(map.clone()))
        }
    }

    pub fn contains_name(&self, n_prefixes: usize, name: &String) -> bool {
        // --foo & ---foo, diff destinations --> not conflict
        // --foo & ---foo, same destinations --> conflict
        // --foo & --foo --> conflict
        match self {
            ArgumentName::Positional(x) => x == name,
            ArgumentName::Flag(map) => map
                .into_iter()
                .any(|(map_n_prefixes, v)| v.contains(name) && *map_n_prefixes == n_prefixes),
        }
    }

    pub fn is_flag_argument(&self) -> bool {
        match self {
            ArgumentName::Flag { .. } => true,
            _ => false,
        }
    }

    pub fn is_positional_argument(&self) -> bool {
        !self.is_flag_argument()
    }

    pub fn is_abbrev_argument(&self) -> bool {
        match self {
            ArgumentName::Flag(map)
                if map.len() == 1 && map.contains_key(&1) && map.get(&1).unwrap().len() > 0 =>
            {
                true
            }
            _ => false,
        }
    }

    pub fn get_short_flags(&self) -> Vec<String> {
        match self {
            ArgumentName::Flag(map) => map.get(&1).map_or(Vec::new(), |x| x.clone()),
            _ => panic!("tried to retrieve flags from positional argument"),
        }
    }

    pub fn names(&self) -> Vec<String> {
        match self {
            ArgumentName::Positional(x) => vec![x.clone()],
            ArgumentName::Flag(map) => {
                let mut all_names: Vec<String> = Vec::new();
                for (k, v) in map.into_iter() {
                    if *k > (1 as usize) {
                        all_names.extend(v.clone());
                    }
                }
                all_names
            }
        }
    }

    pub fn num_of_identifiers(&self) -> usize {
        match self {
            ArgumentName::Positional(..) => 1,
            ArgumentName::Flag(map) => map.into_iter().map(|(_, v)| v.len()).sum(),
        }
    }
}

impl Display for ArgumentName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let builder = match self {
            ArgumentName::Positional(x) => x.clone(),
            ArgumentName::Flag(map) => {
                let mut full: Vec<String> = Vec::new();
                for (k, v) in map.into_iter() {
                    let prepend = "-".repeat(*k);
                    let view: Vec<String> = v.iter().map(|x| prepend.clone() + x).collect();
                    full.extend(view);
                }
                full.sort();
                string_vec_to_string(&full, true).clone()
            }
        };
        write!(f, "{}", builder)
    }
}

impl Hash for ArgumentName {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // TODO do referenes wrk here
        match self {
            ArgumentName::Positional(x) => (&vec![x.clone()]).hash(state),
            // TODO is this ok
            ArgumentName::Flag(map) => {
                for (k, v) in map.into_iter() {
                    k.hash(state);
                    v.hash(state);
                }
            }
        };
    }
}

impl PartialEq for ArgumentName {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Positional(l0), Self::Positional(r0)) => l0 == r0,
            (Self::Flag(map_a), Self::Flag(map_b)) => map_a == map_b,
            _ => false,
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use crate::{
        argument_name::{ArgumentName, ArgumentNameError},
        prefix_chars::PrefixChars,
    };

    #[test]
    fn empty_argument() {
        assert_eq!(
            ArgumentName::new(vec![], &PrefixChars::default()).unwrap_err(),
            ArgumentNameError::EmptyArgumentName
        )
    }

    #[test]
    fn create_positional_arguments() {
        assert_eq!(
            ArgumentName::new(vec!["foo"], &PrefixChars::default()).unwrap(),
            ArgumentName::Positional("foo".to_string())
        )
    }

    #[test]
    fn create_flag_arguments() {
        let mut result: HashMap<usize, Vec<String>> = HashMap::new();
        result.insert(1, vec!["f".to_string()]);
        result.insert(2, vec!["foo".to_string()]);
        assert_eq!(
            ArgumentName::new(vec!["--foo", "-f"], &PrefixChars::default()).unwrap(),
            ArgumentName::Flag(result)
        )
    }

    #[test]
    fn display_positional_arguments() {
        assert_eq!(
            ArgumentName::new(vec!["foo"], &PrefixChars::default())
                .unwrap()
                .to_string(),
            "foo".to_string()
        )
    }

    #[test]
    fn display_flag_arguments() {
        assert_eq!(
            ArgumentName::new(vec!["--foo", "-f", "-bar", "-z"], &PrefixChars::default())
                .unwrap()
                .to_string(),
            "[--foo, -bar, -f, -z]".to_string()
        )
    }

    #[test]
    fn duplicate_flag_arguments() {
        assert_eq!(
            ArgumentName::new(vec!["--foo", "-f", "--foo"], &PrefixChars::default()).unwrap_err(),
            ArgumentNameError::DuplicateArgumentName("foo".to_string())
        )
    }

    #[test]
    fn flag_and_positional_arguments_mixed() {
        assert_eq!(
            ArgumentName::new(vec!["--foo", "bar"], &PrefixChars::default()).unwrap_err(),
            ArgumentNameError::MixedArguments
        )
    }

    #[test]
    fn flag_arguments_contains() {
        assert!(
            ArgumentName::new(vec!["--foo", "-f"], &PrefixChars::default())
                .unwrap()
                .contains_name(2, &"foo".to_string())
        )
    }

    #[test]
    fn flag_arguments_does_not_contain() {
        assert!(
            !ArgumentName::new(vec!["--foo", "-f"], &PrefixChars::default())
                .unwrap()
                .contains_name(1, &"bar".to_string())
        )
    }

    #[test]
    fn positional_arguments_contains() {
        assert!(ArgumentName::new(vec!["foo"], &PrefixChars::default())
            .unwrap()
            .contains_name(0, &"foo".to_string()))
    }

    #[test]
    fn positional_arguments_does_not_contain() {
        assert!(!ArgumentName::new(vec!["foo"], &PrefixChars::default())
            .unwrap()
            .contains_name(0, &"flop".to_string()))
    }

    #[test]
    fn multiple_prefix_chars() {
        let arg_name = ArgumentName::new(
            vec!["++foo", "-f", "@g"],
            &PrefixChars::new(Some("+-@")).unwrap(),
        )
        .unwrap();
        assert!(arg_name.contains_name(2, &"foo".to_string()));
        assert!(arg_name.contains_name(1, &"f".to_string()));
        assert!(arg_name.contains_name(1, &"g".to_string()));
    }

    #[test]
    fn multiple_positional_argument() {
        assert_eq!(
            ArgumentName::new(vec!["foo", "gor"], &PrefixChars::default()).unwrap_err(),
            ArgumentNameError::MultiplePositionalArgumentNames
        )
    }
}
