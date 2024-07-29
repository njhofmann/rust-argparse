use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;

use crate::argument_error::ArgumentError;
use crate::argument_parser::PrefixCharOutcomes;
use crate::argument_parser::PrefixChars;
use crate::string_vec_to_string;
use crate::FLAG_ARG_ABBREV_LEN;
use crate::FLAG_ARG_LEN;

#[derive(Clone, Debug, Eq)]
pub enum ArgumentName {
    Positional(String),
    Flag {
        full: Vec<String>,
        abbrev: Vec<String>,
    },
}

impl ArgumentName {
    pub fn new(
        raw_arg_names: Vec<&str>,
        prefix_chars: &PrefixChars,
    ) -> Result<ArgumentName, ArgumentError> {
        if raw_arg_names.is_empty() {
            Err(ArgumentError::EmptyArgumentName)
        } else if raw_arg_names.len() == 1 {
            let raw_arg_name = raw_arg_names.first().unwrap();
            match prefix_chars.parse_string(raw_arg_name) {
                PrefixCharOutcomes::LONG => Ok(ArgumentName::Flag {
                    full: vec![raw_arg_name[FLAG_ARG_LEN..].to_string()],
                    abbrev: vec![],
                }),
                PrefixCharOutcomes::ABBREV => Ok(ArgumentName::Flag {
                    full: vec![],
                    abbrev: vec![raw_arg_name[FLAG_ARG_ABBREV_LEN..].to_string()],
                }),
                PrefixCharOutcomes::NONE => Ok(ArgumentName::Positional(raw_arg_name.to_string())),
            }
        } else {
            // TODO multiple posn arguments error vs mixed arguments err
            let mut names = vec![];
            let mut abbreviations = vec![];
            let mut posn_arg_found = false;
            for raw_arg_name in raw_arg_names.into_iter() {
                match prefix_chars.parse_string(raw_arg_name) {
                    PrefixCharOutcomes::LONG => {
                        if posn_arg_found {
                            return Err(ArgumentError::MixedArguments);
                        }
                        let arg_name = raw_arg_name[FLAG_ARG_LEN..].to_string();
                        if names.contains(&arg_name) || abbreviations.contains(&arg_name) {
                            return Err(ArgumentError::DuplicateArgumentName(arg_name));
                        }
                        names.push(arg_name)
                    }
                    PrefixCharOutcomes::ABBREV => {
                        if posn_arg_found {
                            return Err(ArgumentError::MixedArguments);
                        }
                        let arg_name = raw_arg_name[FLAG_ARG_ABBREV_LEN..].to_string();
                        if names.contains(&arg_name) || abbreviations.contains(&arg_name) {
                            return Err(ArgumentError::DuplicateArgumentName(arg_name.clone()));
                        }
                        abbreviations.push(arg_name)
                    }
                    PrefixCharOutcomes::NONE => {
                        if !names.is_empty() || !abbreviations.is_empty() {
                            return Err(ArgumentError::MixedArguments);
                        } else if posn_arg_found {
                            return Err(ArgumentError::MultiplePositionalArgumentNames);
                        } else {
                            posn_arg_found = true;
                        }
                    }
                }
            }

            debug_assert!(!names.is_empty());
            Ok(ArgumentName::Flag {
                full: names,
                abbrev: abbreviations,
            })
        }
    }

    pub fn contains(&self, name: &String) -> bool {
        match self {
            ArgumentName::Positional(x) => x == name,
            ArgumentName::Flag { full, abbrev } => full.contains(name) || abbrev.contains(name),
        }
    }

    pub fn is_flag_argument(&self) -> bool {
        match self {
            ArgumentName::Flag { .. } => true,
            _ => false,
        }
    }

    pub fn is_abbrev_argument(&self) -> bool {
        match self {
            ArgumentName::Flag { full, abbrev } if full.is_empty() && !abbrev.is_empty() => true,
            _ => false,
        }
    }

    pub fn overlap(&self, other: &ArgumentName) -> Vec<String> {
        match (self, other) {
            (ArgumentName::Positional(x), ArgumentName::Positional(y)) => {
                if x == y {
                    vec![x.clone()]
                } else {
                    vec![]
                }
            }
            (ArgumentName::Positional(_), ArgumentName::Flag { .. }) => other.overlap(self),
            (ArgumentName::Flag { full, abbrev }, ArgumentName::Positional(x)) => {
                if full.contains(x) || abbrev.contains(x) {
                    vec![x.clone()]
                } else {
                    vec![]
                }
            }
            (
                ArgumentName::Flag { .. },
                ArgumentName::Flag {
                    full: full2,
                    abbrev: abbrev2,
                },
            ) => {
                let mut conflicts: Vec<String> = full2
                    .iter()
                    .filter(|x| self.contains(x))
                    .map(|x| x.clone())
                    .collect();
                for a in abbrev2.iter() {
                    if self.contains(a) {
                        conflicts.push(a.clone());
                    }
                }
                conflicts
            }
        }
    }

    pub fn names(&self) -> Vec<String> {
        match self {
            ArgumentName::Positional(x) => vec![x.clone()],
            ArgumentName::Flag { full, .. } => full.clone(),
        }
    }

    pub fn num_of_identifiers(&self) -> usize {
        match self {
            ArgumentName::Positional(..) => 1,
            ArgumentName::Flag { full, abbrev } => full.len() + abbrev.len(),
        }
    }
}

impl Display for ArgumentName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let builder = match self {
            ArgumentName::Positional(x) => x.clone(),
            ArgumentName::Flag { full, abbrev } => {
                let mut full: Vec<String> = full.iter().map(|x| "--".to_string() + x).collect();
                full.sort();
                let mut abbrev: Vec<String> = abbrev.iter().map(|x| "-".to_string() + x).collect();
                abbrev.sort();
                full.append(&mut abbrev);
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
            ArgumentName::Flag { full, abbrev } => {
                full.hash(state);
                abbrev.hash(state)
            }
        };
    }
}

impl PartialEq for ArgumentName {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Positional(l0), Self::Positional(r0)) => l0 == r0,
            (
                Self::Flag {
                    full: l_full,
                    abbrev: l_abbrev,
                },
                Self::Flag {
                    full: r_full,
                    abbrev: r_abbrev,
                },
            ) => l_full == r_full && l_abbrev == r_abbrev,
            _ => false,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        argument_error::ArgumentError, argument_name::ArgumentName, argument_parser::PrefixChars,
    };

    #[test]
    fn empty_argument() {
        assert_eq!(
            ArgumentName::new(vec![], &PrefixChars::default()).unwrap_err(),
            ArgumentError::EmptyArgumentName
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
        assert_eq!(
            ArgumentName::new(vec!["--foo", "-f"], &PrefixChars::default()).unwrap(),
            ArgumentName::Flag {
                full: vec!["foo".to_string()],
                abbrev: vec!["f".to_string()]
            }
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
            ArgumentError::DuplicateArgumentName("foo".to_string())
        )
    }

    #[test]
    fn flag_and_positional_arguments_mixed() {
        assert_eq!(
            ArgumentName::new(vec!["--foo", "bar"], &PrefixChars::default()).unwrap_err(),
            ArgumentError::MixedArguments
        )
    }

    #[test]
    fn flag_arguments_contains() {
        assert!(
            ArgumentName::new(vec!["--foo", "-f"], &PrefixChars::default())
                .unwrap()
                .contains(&"foo".to_string())
        )
    }

    #[test]
    fn flag_arguments_does_not_contain() {
        assert!(
            !ArgumentName::new(vec!["--foo", "-f"], &PrefixChars::default())
                .unwrap()
                .contains(&"bar".to_string())
        )
    }

    #[test]
    fn positional_arguments_contains() {
        assert!(ArgumentName::new(vec!["foo"], &PrefixChars::default())
            .unwrap()
            .contains(&"foo".to_string()))
    }

    #[test]
    fn positional_arguments_does_not_contain() {
        assert!(!ArgumentName::new(vec!["foo"], &PrefixChars::default())
            .unwrap()
            .contains(&"flop".to_string()))
    }

    #[test]
    fn flag_argument_overlap() {
        let a = ArgumentName::new(vec!["--foo", "-f", "--fooo"], &PrefixChars::default()).unwrap();
        let b = ArgumentName::new(vec!["--flop", "-f", "--flob"], &PrefixChars::default()).unwrap();
        assert_eq!(a.overlap(&b), b.overlap(&a));
        assert_eq!(a.overlap(&b), vec!["f".to_string()])
    }

    #[test]
    fn flag_argument_no_overlap() {
        let a = ArgumentName::new(vec!["--foo", "-f", "--fooo"], &PrefixChars::default()).unwrap();
        let b =
            ArgumentName::new(vec!["--flop", "-fp", "--flob"], &PrefixChars::default()).unwrap();
        assert_eq!(a.overlap(&b), b.overlap(&a));
        assert!(a.overlap(&b).is_empty())
    }

    #[test]
    fn positional_argument_overlap() {
        let a = ArgumentName::new(vec!["f"], &PrefixChars::default()).unwrap();
        let b = ArgumentName::new(vec!["f"], &PrefixChars::default()).unwrap();
        assert_eq!(a.overlap(&b), b.overlap(&a));
        assert_eq!(a.overlap(&b), vec!["f".to_string()])
    }

    #[test]
    fn positional_argument_no_overlap() {
        let a = ArgumentName::new(vec!["foo"], &PrefixChars::default()).unwrap();
        let b = ArgumentName::new(vec!["flop"], &PrefixChars::default()).unwrap();
        assert_eq!(a.overlap(&b), b.overlap(&a));
        assert!(a.overlap(&b).is_empty())
    }

    #[test]
    fn mixed_argument_overlap() {
        let a = ArgumentName::new(vec!["--foo", "-f", "--fooo"], &PrefixChars::default()).unwrap();
        let b = ArgumentName::new(vec!["f"], &PrefixChars::default()).unwrap();
        assert_eq!(a.overlap(&b), b.overlap(&a));
        assert_eq!(a.overlap(&b), vec!["f".to_string()])
    }

    #[test]
    fn mixed_argument_no_overlap() {
        let a = ArgumentName::new(vec!["--foo", "-fp", "--fooo"], &PrefixChars::default()).unwrap();
        let b = ArgumentName::new(vec!["flop"], &PrefixChars::default()).unwrap();
        assert_eq!(a.overlap(&b), b.overlap(&a));
        assert!(a.overlap(&b).is_empty())
    }

    #[test]
    fn multiple_prefix_chars() {
        let arg_name = ArgumentName::new(
            vec!["++foo", "-f", "@g"],
            &PrefixChars::new(Some("+-@")).unwrap(),
        )
        .unwrap();
        assert!(arg_name.contains(&"foo".to_string()));
        assert!(arg_name.contains(&"f".to_string()));
        assert!(arg_name.contains(&"g".to_string()));
    }

    #[test]
    fn multiple_positional_argument() {
        assert_eq!(
            ArgumentName::new(vec!["foo", "gor"], &PrefixChars::default()).unwrap_err(),
            ArgumentError::MultiplePositionalArgumentNames
        )
    }
}
