mod add_argument {
    use std::{fmt::Display, str::FromStr};

    use py_arg_parse::{
        argument::{Argument, ArgumentError},
        argument_name::ArgumentName,
        argument_parser::{AddArgumentError, ArgumentParser},
        default::ArgumentDefault,
        nargs::NArgs,
        parse_result::RetrievalError,
        prefix_chars::PrefixChars,
    };

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

    #[test]
    fn missing_non_required_argument_no_value_() {
        let parser = ArgumentParser::default()
            .add_argument::<&str>(
                vec!["--foo"],
                None,
                None,
                None,
                None,
                None,
                Some(false),
                None,
                None,
                None,
                None,
            )
            .unwrap();
        let namespace = parser.parse_args(Some(vec![])).unwrap();
        assert!(namespace.get::<String>("foo").unwrap().is_empty())
    }

    #[test]
    fn missing_non_required_argument_no_value_global_suppress() {
        let parser = ArgumentParser::new(
            None,
            None,
            None,
            None,
            None,
            None,
            Some(true),
            None,
            None,
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
            Some(false),
            None,
            None,
            None,
            None,
        )
        .unwrap();
        let namespace = parser.parse_args(Some(vec![])).unwrap();
        assert_eq!(
            namespace.get::<String>("foo").unwrap_err(),
            RetrievalError::InvalidKey("foo".to_string())
        );
    }

    #[test]
    fn missing_non_required_argument_no_value_global_suppress_override() {
        let parser = ArgumentParser::new(
            None,
            None,
            None,
            None,
            None,
            None,
            Some(true),
            None,
            None,
            None,
        )
        .unwrap()
        .add_argument::<&str>(
            vec!["--foo"],
            None,
            None,
            None,
            Some(ArgumentDefault::Value(vec!["fool"])),
            None,
            Some(false),
            None,
            None,
            None,
            None,
        )
        .unwrap();
        let namespace = parser.parse_args(Some(vec![])).unwrap();
        assert_eq!(
            namespace.get::<String>("foo").unwrap_err(),
            RetrievalError::InvalidKey("foo".to_string())
        );
    }

    #[test]
    fn missing_non_required_argument_no_value_local_suppress() {
        let parser = ArgumentParser::default()
            .add_argument::<&str>(
                vec!["--foo"],
                None,
                None,
                None,
                Some(ArgumentDefault::Suppress),
                None,
                Some(false),
                None,
                None,
                None,
                None,
            )
            .unwrap();
        let namespace = parser.parse_args(Some(vec![])).unwrap();
        assert_eq!(
            namespace.get::<String>("foo").unwrap_err(),
            RetrievalError::InvalidKey("foo".to_string())
        );
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
            AddArgumentError::ArgumentError(ArgumentError::DuplicateArgumentNameValues(
                "[h]".to_string()
            ))
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
            AddArgumentError::ArgumentError(ArgumentError::RequiredMarkedForPositionalArgument)
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
                    Some(ArgumentDefault::Value(vec!["bar"])),
                    None,
                    Some(true),
                    None,
                    None,
                    None,
                    None
                )
                .unwrap_err(),
            AddArgumentError::ArgumentError(ArgumentError::RequiredArgumentDefaultValueGiven(
                ArgumentName::new(vec!["--foo"], &PrefixChars::default())
                    .unwrap()
                    .to_string(),
                "[bar]".to_string()
            )),
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
            AddArgumentError::ArgumentError(ArgumentError::DuplicateArgumentNameValues(
                "[foo]".to_string()
            ))
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
            AddArgumentError::ArgumentError(ArgumentError::DuplicateArgumentNameValues(
                "[foo]".to_string()
            ))
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
            Some(ArgumentDefault::Value(vec![
                5.6.to_string(),
                1.2.to_string(),
            ])),
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
                Some(ArgumentDefault::Value(vec![5.6, 1.2])),
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
            Some(ArgumentDefault::Value(vec![5.to_string(), 1.to_string()])),
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
                Some(ArgumentDefault::Value(vec![5, 1])),
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
            Some(ArgumentDefault::Value(vec![
                false.to_string(),
                true.to_string(),
            ])),
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
                Some(ArgumentDefault::Value(vec![false, true])),
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
            Some(ArgumentDefault::Value(vec![
                Test::One.to_string(),
                Test::One.to_string(),
            ])),
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
                Some(ArgumentDefault::Value(vec![Test::One, Test::One])),
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
}
