use std::{any::type_name, collections::HashMap, fmt::Debug, ops::Index, str::FromStr};

use thiserror::Error;

use crate::{action::Action, Argument};

#[derive(Error, Debug, PartialEq, Eq)]
pub enum RetrievalError {
    #[error("{0} is not a key added to this argument parser")]
    InvalidKey(String),
    #[error("argument {0} has {1} associated values, tried retrieving {2}")]
    IncorrectValueCount(String, usize, usize),
    #[error("failed to convert argument value \"{0}\" under argument \"{1}\" to type {2}")]
    ConversionError(String, String, String),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Namespace(HashMap<String, Argument>);

impl Index<&str> for Namespace {
    type Output = Vec<String>;

    fn index(&self, index: &str) -> &Self::Output {
        self.get_value(index).unwrap()
    }
}

impl Namespace {
    pub fn new<'a>(found_values: HashMap<String, Argument>) -> Namespace {
        Namespace(found_values)
    }

    // TODO make this not public only in module
    pub fn extend(&self, other: Namespace) -> Namespace {
        let mut new_namespace = self.clone();
        for (k, v) in other.0.into_iter() {
            new_namespace.0.insert(k, v);
        }
        new_namespace
    }

    pub fn contains(&self, arg: &str) -> bool {
        self.0.get(arg).map_or(false, |_| true)
    }

    fn get_value(&self, arg: &str) -> Result<&Vec<String>, RetrievalError> {
        let found_argument = self
            .0
            .get(&arg.to_string())
            .ok_or(RetrievalError::InvalidKey(arg.to_string()))?;
        match found_argument.action() {
            Action::Store(v) | Action::Append(v) | Action::Extend(v) => Ok(v),
            // help & version should exit before namespace is even created
            // count & store_const converted into store
            // count converted into store
            // append_const filtered into single store
            _ => panic!("action type that shouldn't have been passed was passed"),
        }
    }

    pub fn get<T: FromStr>(&self, arg: &str) -> Result<Vec<T>, RetrievalError>
    where
        <T as FromStr>::Err: Debug,
    {
        Ok(self
            .get_value(arg)?
            .into_iter()
            .map(|x| {
                T::from_str(&x).map_err(|_| {
                    RetrievalError::ConversionError(
                        x.clone(),
                        arg.to_string(),
                        type_name::<T>().to_string(),
                    )
                })
            })
            .collect::<Result<Vec<T>, _>>()?)
    }

    fn get_x_arg_value<T: FromStr>(
        &self,
        arg: &str,
        expected_n_args: usize,
    ) -> Result<Vec<T>, RetrievalError>
    where
        <T as FromStr>::Err: Debug,
    {
        let vals = self.get(arg)?;
        if vals.len() == expected_n_args {
            Ok(vals)
        } else {
            Err(RetrievalError::IncorrectValueCount(
                arg.to_string(),
                expected_n_args,
                vals.len(),
            ))
        }
    }

    pub fn get_one_value<T: FromStr + Clone>(&self, arg: &str) -> Result<T, RetrievalError>
    where
        <T as FromStr>::Err: Debug,
    {
        let vals = self.get_x_arg_value::<T>(arg, 1)?;
        Ok(vals.get(0).unwrap().clone())
    }

    pub fn get_two_value<T: FromStr + Clone>(&self, arg: &str) -> Result<(T, T), RetrievalError>
    where
        <T as FromStr>::Err: Debug,
    {
        let vals = self.get_x_arg_value::<T>(arg, 2)?;
        Ok((vals.get(0).unwrap().clone(), vals.get(1).unwrap().clone()))
    }

    pub fn get_three_value<T: FromStr + Clone>(
        &self,
        arg: &str,
    ) -> Result<(T, T, T), RetrievalError>
    where
        <T as FromStr>::Err: Debug,
    {
        let vals = self.get_x_arg_value::<T>(arg, 3)?;
        Ok((
            vals.get(0).unwrap().clone(),
            vals.get(1).unwrap().clone(),
            vals.get(2).unwrap().clone(),
        ))
    }

    pub fn get_four_value<T: FromStr + Clone>(
        &self,
        arg: &str,
    ) -> Result<(T, T, T, T), RetrievalError>
    where
        <T as FromStr>::Err: Debug,
    {
        let vals = self.get_x_arg_value::<T>(arg, 4)?;
        Ok((
            vals.get(0).unwrap().clone(),
            vals.get(1).unwrap().clone(),
            vals.get(2).unwrap().clone(),
            vals.get(3).unwrap().clone(),
        ))
    }

    pub fn get_five_value<T: FromStr + Clone>(
        &self,
        arg: &str,
    ) -> Result<(T, T, T, T, T), RetrievalError>
    where
        <T as FromStr>::Err: Debug,
    {
        let vals = self.get_x_arg_value::<T>(arg, 5)?;
        Ok((
            vals.get(0).unwrap().clone(),
            vals.get(1).unwrap().clone(),
            vals.get(2).unwrap().clone(),
            vals.get(3).unwrap().clone(),
            vals.get(4).unwrap().clone(),
        ))
    }
}

#[cfg(test)]
mod test {

    use crate::{
        argument_parser::{ArgumentParser, ParsingError},
        nargs::NArgs,
    };

    use super::Namespace;

    fn setup_parser<'a>() -> ArgumentParser {
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
        .unwrap()
        .add_argument::<&str>(
            vec!["bar"],
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
        .add_argument::<&str>(
            vec!["boo"],
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
            vec!["--foo1"],
            None,
            Some(NArgs::Exact(1)),
            None,
            None,
            None,
            Some(true),
            None,
            None,
            None,
            None,
        )
        .unwrap()
        .add_argument::<&str>(
            vec!["--foo2"],
            None,
            Some(NArgs::Exact(2)),
            None,
            Some(vec!["1", "2"]),
            Some(vec![vec!["one", "two"], vec!["1", "2"]]),
            Some(false),
            None,
            None,
            None,
            None,
        )
        .unwrap()
        .add_argument::<&str>(
            vec!["--foo3"],
            None,
            Some(NArgs::Exact(3)),
            None,
            None,
            None,
            Some(true),
            None,
            None,
            None,
            None,
        )
        .unwrap()
        .add_argument::<&str>(
            vec!["--foo4"],
            None,
            Some(NArgs::Exact(4)),
            None,
            None,
            None,
            Some(true),
            None,
            None,
            None,
            None,
        )
        .unwrap()
        .add_argument::<&str>(
            vec!["--foo5"],
            None,
            Some(NArgs::Exact(5)),
            None,
            None,
            None,
            Some(true),
            None,
            None,
            None,
            None,
        )
        .unwrap()
        .add_argument::<i32>(
            vec!["--foo6"],
            None,
            Some(NArgs::Exact(1)),
            None,
            Some(vec![1]),
            Some(vec![vec![1], vec![2]]),
            Some(false),
            None,
            None,
            None,
            None,
        )
        .unwrap()
        .add_argument::<bool>(
            vec!["--foo7"],
            None,
            Some(NArgs::Exact(1)),
            None,
            Some(vec![true]),
            None,
            Some(false),
            None,
            None,
            None,
            None,
        )
        .unwrap()
    }

    fn setup_parser_error(raw_args: Vec<&str>) -> ParsingError {
        get_parsed_results(raw_args).unwrap_err()
    }

    fn setup_input<'a>() -> Namespace {
        let raw_args = vec![
            "val1", "val2", "val3", "val4", "--foo1", "one", "--foo3", "one", "two", "three",
            "--foo4", "one", "two", "three", "four", "--foo5", "one", "two", "three", "four",
            "five", "--foo6", "1", "--foo7", "true",
        ];
        get_parsed_results(raw_args).unwrap()
    }

    fn get_parsed_results(raw_args: Vec<&str>) -> Result<Namespace, ParsingError> {
        let raw_args = raw_args.into_iter().map(|x| x.to_string()).collect();
        setup_parser().parse_args(Some(raw_args))
    }

    mod parsing {
        use crate::{
            argument_parser::ParsingError, choices::ChoicesError, nargs::NArgs, parse_result::test::setup_parser_error, InvalidChoice
        };

        #[test]
        fn display() {}

        #[test]
        fn incorrect_arg_value_length() {
            assert_eq!(
                setup_parser_error(vec![
                    "val1", "val2", "val3", "val4", "--foo1", "one", "--foo3", "one", "two",
                    "three", "--foo4", "one", "two", "three", "four", "--foo5", "one", "two",
                    "three", "four",
                ]),
                ParsingError::IncorrectValueCount("[--foo5]".to_string(), NArgs::Exact(5), 4)
            )
        }

        #[test]
        fn incorrect_positional_arguments_value_count_out_of_bounds() {
            assert_eq!(
                setup_parser_error(vec!["val1", "val2", "val3"]),
                ParsingError::IncorrectValueCount("boo".to_string(), NArgs::Exact(2), 1)
            )
        }

        #[test]
        fn incorrect_positional_arguments_value_count_flag_encountered() {
            assert_eq!(
                setup_parser_error(vec![
                    "val1", "val2", "val3", "--foo1", "one", "--foo3", "one", "two", "three",
                    "--foo4", "one", "two", "three", "four", "--foo5", "one", "two", "three",
                    "four",
                ]),
                ParsingError::IncorrectValueCount("boo".to_string(), NArgs::Exact(2), 1)
            )
        }

        #[test]
        fn missing_required_flag_arguments() {
            assert_eq!(
                setup_parser_error(vec![
                    "val1", "val2", "val3", "val4", "--foo4", "one", "two", "three", "four",
                    "--foo5", "one", "two", "three", "four", "five",
                ]),
                ParsingError::MissingRequiredFlagArguments("[[--foo1], [--foo3]]".to_string())
            )
        }

        #[test]
        fn invalid_flag_argument() {
            assert_eq!(
                setup_parser_error(vec![
                    "val1", "val2", "val3", "val4", "--foo0", "one", "--foo3", "one", "two",
                    "three", "--foo4", "one", "two", "three", "four", "--foo5", "one", "two",
                    "three", "four", "five"
                ]),
                ParsingError::InvalidFlagArgument("foo0".to_string(),)
            )
        }

        #[test]
        fn duplicate_flag_argument() {
            assert_eq!(
                setup_parser_error(vec![
                    "val1", "val2", "val3", "val4", "--foo1", "one", "--foo1", "two", "--foo3",
                    "one", "two", "three", "--foo4", "one", "two", "three", "four", "--foo5",
                    "one", "two", "three", "four", "five"
                ]),
                ParsingError::DuplicateFlagArgument("[--foo1]".to_string())
            )
        }

        #[test]
        fn invalid_choice() {
            assert_eq!(
                setup_parser_error(vec![
                    "val1", "val2", "val3", "val4", "--foo1", "one", "--foo2", "3", "4", "--foo3",
                    "one", "two", "three", "--foo4", "one", "two", "three", "four", "--foo5",
                    "one", "two", "three", "four", "five"
                ]),
                ParsingError::ChoicesError(ChoicesError::InvalidChoice(InvalidChoice(
                    "[3, 4]".to_string(),
                    "[[1, 2], [one, two]]".to_string()
                )))
            )
        }
    }

    #[test]
    fn invalid_key() {}
    mod retrieval {
        use std::{any::type_name, str::FromStr};

        use crate::parse_result::{test::setup_input, RetrievalError};
        #[test]
        fn does_not_contain() {
            assert!(!setup_input().contains("fake"))
        }

        #[test]
        fn contains() {
            assert!(setup_input().contains("foo"))
        }

        #[test]
        fn index() {
            assert_eq!(setup_input()["foo"], vec!["val1"])
        }

        #[test]
        #[should_panic]
        fn failed_index() {
            let _ = &(setup_input()["foooo"]);
        }

        #[test]
        fn get_two_string_from_two_valued_integer() {
            assert_eq!(
                setup_input().get_two_value::<String>("boo").unwrap(),
                ("val3".to_string(), "val4".to_string())
            )
        }

        #[test]
        fn get_one_string() {
            assert_eq!(
                setup_input().get_one_value::<String>("foo1").unwrap(),
                "one".to_string()
            )
        }

        #[test]
        fn get_default() {
            assert_eq!(
                setup_input().get_two_value::<String>("foo2").unwrap(),
                ("1".to_string(), "2".to_string())
            )
        }

        #[test]
        fn get_three_string() {
            assert_eq!(
                setup_input().get_three_value::<String>("foo3").unwrap(),
                ("one".to_string(), "two".to_string(), "three".to_string())
            )
        }

        #[test]
        fn get_four_string() {
            assert_eq!(
                setup_input().get_four_value::<String>("foo4").unwrap(),
                (
                    "one".to_string(),
                    "two".to_string(),
                    "three".to_string(),
                    "four".to_string()
                )
            )
        }

        #[test]
        fn get_five_string() {
            assert_eq!(
                setup_input().get_five_value::<String>("foo5").unwrap(),
                (
                    "one".to_string(),
                    "two".to_string(),
                    "three".to_string(),
                    "four".to_string(),
                    "five".to_string()
                )
            )
        }

        #[test]
        fn bad_key() {
            assert_eq!(
                setup_input().get_four_value::<String>("foo8").unwrap_err(),
                RetrievalError::InvalidKey("foo8".to_string())
            )
        }

        #[test]
        fn incorrect_value_count() {
            assert_eq!(
                setup_input().get_four_value::<String>("foo5").unwrap_err(),
                RetrievalError::IncorrectValueCount("foo5".to_string(), 4, 5)
            )
        }

        #[test]
        fn get_one_int() {
            assert_eq!(setup_input().get_one_value::<i32>("foo6").unwrap(), 1)
        }

        #[test]
        fn get_one_float() {
            assert_eq!(setup_input().get_one_value::<f64>("foo6").unwrap(), 1.0)
        }

        #[test]
        fn get_one_bool() {
            assert_eq!(setup_input().get_one_value::<bool>("foo7").unwrap(), true)
        }

        #[test]
        fn get_enum() {
            #[derive(PartialEq, Debug, Clone)]
            enum Test {
                One,
                Two,
                Three,
            }

            impl FromStr for Test {
                type Err = String;

                fn from_str(s: &str) -> Result<Self, Self::Err> {
                    match s {
                        "one" => Ok(Test::One),
                        "two" => Ok(Test::Two),
                        "three" => Ok(Test::Three),
                        _ => Err("invalid conversion".to_string()),
                    }
                }
            }
            assert_eq!(
                setup_input().get_one_value::<Test>("foo1").unwrap(),
                Test::One
            )
        }

        #[test]
        fn failed_conversion() {
            assert_eq!(
                setup_input().get_one_value::<bool>("foo6").unwrap_err(),
                RetrievalError::ConversionError(
                    "1".to_string(),
                    "foo6".to_string(),
                    type_name::<bool>().to_string()
                )
            )
        }
    }

    #[test]
    fn duplicate_argument_names_override_with_overlap() {
        let mut parser = ArgumentParser::new(
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            Some("override"),
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
        parser = parser
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
                None,
            )
            .unwrap();
        let namespace = parser
            .parse_args(Some(vec![
                "--h".to_string(),
                "a".to_string(),
                "--foo".to_string(),
                "b".to_string(),
            ]))
            .unwrap();
        assert_eq!(
            namespace.get_one_value::<String>("h").unwrap(),
            "a".to_string()
        );
        assert_eq!(
            namespace.get_one_value::<String>("foo").unwrap(),
            "b".to_string()
        );
    }

    #[test]
    fn duplicate_argument_names_override_total_replacement_one_arg() {
        let mut parser = ArgumentParser::new(
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            Some("override"),
            Some(false),
            None,
        )
        .unwrap()
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
        .unwrap();
        assert_eq!(
            parser
                .parse_args(Some(vec![
                    "--foo".to_string(),
                    "a".to_string(),
                    "a".to_string()
                ]))
                .unwrap()
                .get_two_value::<String>("foo")
                .unwrap(),
            ("a".to_string(), "a".to_string())
        );

        assert_eq!(
            parser
                .parse_args(Some(vec!["--foo".to_string(), "a".to_string()]))
                .unwrap_err(),
            ParsingError::IncorrectValueCount("[--foo]".to_string(), NArgs::Exact(2), 1)
        );
        parser = parser
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
        assert_eq!(
            parser
                .parse_args(Some(vec!["--foo".to_string(), "a".to_string()]))
                .unwrap()
                .get_one_value::<String>("foo")
                .unwrap(),
            "a".to_string()
        );
        assert_eq!(
            parser
                .parse_args(Some(vec![
                    "--foo".to_string(),
                    "a".to_string(),
                    "a".to_string()
                ]))
                .unwrap_err(),
            ParsingError::UnprocessedRawArguments("a".to_string())
        );
    }

    #[test]
    fn duplicate_argument_names_override_total_replacement_mult_args() {
        let mut parser = ArgumentParser::new(
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            Some("override"),
            Some(false),
            None,
        )
        .unwrap()
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
        .add_argument::<&str>(
            vec!["--bar"],
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
                .parse_args(Some(vec![
                    "--foo".to_string(),
                    "a".to_string(),
                    "a".to_string(),
                    "--bar".to_string(),
                    "a".to_string()
                ]))
                .unwrap()
                .get_two_value::<String>("foo")
                .unwrap(),
            ("a".to_string(), "a".to_string())
        );

        assert_eq!(
            parser
                .parse_args(Some(vec![
                    "--foo".to_string(),
                    "a".to_string(),
                    "--bar".to_string(),
                    "a".to_string()
                ]))
                .unwrap_err(),
            ParsingError::IncorrectValueCount("[--foo]".to_string(), NArgs::Exact(2), 1)
        );
        parser = parser
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
        assert_eq!(
            parser
                .parse_args(Some(vec![
                    "--bar".to_string(),
                    "a".to_string(),
                    "--foo".to_string(),
                    "a".to_string()
                ]))
                .unwrap()
                .get_one_value::<String>("foo")
                .unwrap(),
            "a".to_string()
        );
        assert_eq!(
            parser
                .parse_args(Some(vec![
                    "--bar".to_string(),
                    "a".to_string(),
                    "--foo".to_string(),
                    "a".to_string(),
                    "a".to_string()
                ]))
                .unwrap_err(),
            ParsingError::UnprocessedRawArguments("a".to_string())
        );
    }
}
