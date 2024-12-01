#[cfg(test)]
mod actions {
    use py_arg_parse::argument_parser::ArgumentParser;
    use py_arg_parse::parse_result::RetrievalError;

    mod parse_known_args {
        use py_arg_parse::{argument::NArgs, argument_parser::ParserError};

        use super::*;

        #[test]
        fn test() {
            let parser = ArgumentParser::default()
                .add_argument::<&str>(
                    vec!["--foo"],
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
                .unwrap();
            assert_eq!(
                parser
                    .parse_args(Some(vec![
                        "--foo".to_string(),
                        "--badger".to_string(),
                        "BAR".to_string(),
                        "spam".to_string()
                    ]))
                    .unwrap_err(),
                ParserError::InvalidFlagArgument("badger".to_string())
            );
            let (expected_namespace, unknown_args) = parser
                .parse_known_args(Some(vec![
                    "--foo".to_string(),
                    "--badger".to_string(),
                    "BAR".to_string(),
                    "spam".to_string(),
                ]))
                .unwrap();
            assert!(expected_namespace.contains("foo"));
            assert_eq!(
                unknown_args,
                vec!["--badger".to_string(), "spam".to_string()]
            )
        }

        #[test]
        fn var_posn_then_var_flag_then_var_posn() {
            let parser = ArgumentParser::default()
                .add_argument::<&str>(
                    vec!["a"],
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
                    vec!["--foo"],
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
                    vec!["b"],
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
                .unwrap();
            let (expected_namespace, unknown_args) = parser
                .parse_known_args(Some(vec![
                    "1".to_string(),
                    "2".to_string(),
                    "3".to_string(),
                    "--foo".to_string(),
                    "4".to_string(),
                    "5".to_string(),
                    "6".to_string(),
                    "--c".to_string(),
                ]))
                .unwrap();
            assert_eq!(
                expected_namespace.get_three_value::<i32>("a").unwrap(),
                (1, 2, 3)
            );
            assert_eq!(
                expected_namespace.get_three_value::<i32>("foo").unwrap(),
                (4, 5, 6)
            );
            assert!(expected_namespace.get::<i32>("b").unwrap().is_empty());
            assert_eq!(unknown_args, vec!["--c"])
        }

        #[test]
        fn var_posn_then_fixed_flag_then_var_posn() {
            let parser = ArgumentParser::default()
                .add_argument::<&str>(
                    vec!["a"],
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
                    vec!["b"],
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
                .unwrap();
            let (expected_namespace, unknown_args) = parser
                .parse_known_args(Some(vec![
                    "1".to_string(),
                    "2".to_string(),
                    "3".to_string(),
                    "--foo".to_string(),
                    "4".to_string(),
                    "5".to_string(),
                    "6".to_string(),
                ]))
                .unwrap();
            assert_eq!(
                expected_namespace.get_three_value::<i32>("a").unwrap(),
                (1, 2, 3)
            );
            assert_eq!(
                expected_namespace.get_two_value::<i32>("foo").unwrap(),
                (4, 5)
            );
            assert!(expected_namespace.get::<i32>("b").unwrap().is_empty());
            assert_eq!(unknown_args, vec!["6"])
        }

        #[test]
        fn var_posn_then_var_flag_then_one_or_more_posn() {
            let parser = ArgumentParser::default()
                .add_argument::<&str>(
                    vec!["a"],
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
                    vec!["--foo"],
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
                    vec!["b"],
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
                .unwrap();
            let (expected_namespace, unknown_args) = parser
                .parse_known_args(Some(vec![
                    "1".to_string(),
                    "--foo".to_string(),
                    "2".to_string(),
                ]))
                .unwrap();
            assert!(expected_namespace.get::<i32>("a").unwrap().is_empty());
            assert_eq!(expected_namespace.get_one_value::<i32>("b").unwrap(), 1);
            assert_eq!(expected_namespace.get_one_value::<i32>("foo").unwrap(), 2);
            assert!(unknown_args.is_empty())
        }
    }

    mod store {
        use py_arg_parse::{argument::NArgs, argument_parser::ParserError};

        use super::*;

        #[test]
        fn any_number_posn_arg_and_zero_or_one_posn_arg_no_values_have_default() {
            let namespace = ArgumentParser::default()
                .add_argument::<&str>(
                    vec!["a"],
                    None,
                    Some(NArgs::AnyNumber),
                    None,
                    Some(vec!["a"]),
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                )
                .unwrap()
                .add_argument::<&str>(
                    vec!["b"],
                    None,
                    Some(NArgs::AnyNumber),
                    None,
                    Some(vec!["b"]),
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                )
                .unwrap()
                .add_argument::<&str>(
                    vec!["c"],
                    None,
                    Some(NArgs::ZeroOrOne),
                    None,
                    Some(vec!["c"]),
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                )
                .unwrap()
                .parse_args(Some(vec![
                    "d".to_string(),
                    "e".to_string(),
                    "f".to_string(),
                ]))
                .unwrap();
            assert_eq!(
                namespace.get_three_value::<String>("a").unwrap(),
                ("d".to_string(), "e".to_string(), "f".to_string())
            );
            assert_eq!(
                namespace.get_one_value::<String>("b").unwrap(),
                "b".to_string()
            );
            assert_eq!(
                namespace.get_one_value::<String>("c").unwrap(),
                ("c".to_string())
            )
        }

        #[test]
        fn store() {
            let namespace = ArgumentParser::default()
                .add_argument::<&str>(
                    vec!["--foo", "-f"],
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
                .parse_args(Some(vec!["-f".to_string(), "test".to_string()]))
                .unwrap();
            assert_eq!(
                namespace.get_one_value::<String>("foo").unwrap(),
                "test".to_string()
            )
        }

        #[test]
        fn diff_dest() {
            let namespace = ArgumentParser::default()
                .add_argument::<&str>(
                    vec!["--foo", "-f"],
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    Some("boo"),
                    None,
                )
                .unwrap()
                .parse_args(Some(vec!["-f".to_string(), "test".to_string()]))
                .unwrap();
            assert_eq!(
                namespace.get_one_value::<String>("boo").unwrap(),
                "test".to_string()
            );
            assert_eq!(
                namespace.get_one_value::<String>("foo").unwrap_err(),
                RetrievalError::InvalidKey("foo".to_string())
            )
        }

        #[test]
        fn allow_abbrev() {
            let namespace = ArgumentParser::default()
                .add_argument::<&str>(
                    vec!["--foo", "-f"],
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
                .parse_args(Some(vec!["--fo".to_string(), "test".to_string()]))
                .unwrap();
            assert_eq!(
                namespace.get_one_value::<String>("foo").unwrap(),
                "test".to_string()
            );
        }

        #[test]
        fn allow_abbrev_conflicting_result() {
            let parser = ArgumentParser::default()
                .add_argument::<&str>(
                    vec!["--beam"],
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
                    vec!["--beast"],
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
                    .parse_args(Some(vec!["--bea".to_string(), "test".to_string()]))
                    .unwrap_err(),
                ParserError::AmbiguousAbbreviatedArguments(
                    "bea".to_string(),
                    "[beam, beast]".to_string()
                )
            );
        }

        #[test]
        fn allow_abbrev_argument_override_enabled() {
            let mut parser = ArgumentParser::new(
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
                vec!["--beam"],
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
                vec!["--beast"],
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
                        "--beast".to_string(),
                        "test".to_string(),
                        "--beam".to_string(),
                        "test".to_string()
                    ]))
                    .unwrap()
                    .get_one_value::<String>("beast")
                    .unwrap(),
                "test".to_string()
            );
            parser = parser
                .add_argument::<&str>(
                    vec!["--beast", "-b"],
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
                        "--beast".to_string(),
                        "test".to_string(),
                        "--beam".to_string(),
                        "test".to_string()
                    ]))
                    .unwrap()
                    .get_one_value::<String>("beast")
                    .unwrap(),
                "test".to_string()
            );
        }

        #[test]
        fn allow_abbrev_false() {
            let parser = ArgumentParser::new(
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some(false),
            )
            .unwrap()
            .add_argument::<&str>(
                vec!["--beam"],
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
                vec!["--beast"],
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
                    .parse_args(Some(vec!["--bea".to_string(), "test".to_string()]))
                    .unwrap_err(),
                ParserError::InvalidFlagArgument("bea".to_string())
            );
        }

        #[test]
        fn duplicate() {
            assert_eq!(
                ArgumentParser::default()
                    .add_argument::<&str>(
                        vec!["--foo", "-f"],
                        None,
                        None,
                        None,
                        None,
                        None,
                        None,
                        None,
                        None,
                        Some("boo"),
                        None,
                    )
                    .unwrap()
                    .parse_args(Some(vec![
                        "-f".to_string(),
                        "test".to_string(),
                        "-f".to_string(),
                        "young".to_string()
                    ]))
                    .unwrap_err(),
                ParserError::DuplicateFlagArgument("[--foo, -f]".to_string())
            );
        }
    }

    mod append {}

    mod extend {
        use super::*;

        #[test]
        fn extend() {
            let namespace = ArgumentParser::default()
                .add_argument::<&str>(
                    vec!["--foo", "-f"],
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
                .parse_args(Some(vec![
                    "--foo".to_string(),
                    "test".to_string(),
                    "--foo".to_string(),
                    "blah".to_string(),
                    "--foo".to_string(),
                    "moo".to_string(),
                ]))
                .unwrap();
            assert_eq!(
                namespace.get_three_value::<String>("foo").unwrap(),
                ("test".to_string(), "blah".to_string(), "moo".to_string())
            )
        }
    }

    mod store_const {
        use super::*;
        #[test]
        fn store_const() {
            let namespace = ArgumentParser::default()
                .add_argument::<&str>(
                    vec!["--foo", "-f"],
                    Some("store_const"),
                    None,
                    Some(vec!["test"]),
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                )
                .unwrap()
                .parse_args(Some(vec!["--foo".to_string()]))
                .unwrap();
            assert_eq!(
                namespace.get_one_value::<String>("foo").unwrap(),
                "test".to_string()
            )
        }
    }

    mod append_const {
        use super::*;

        #[test]
        fn append_const() {
            let namespace = ArgumentParser::default()
                .add_argument::<&str>(
                    vec!["--foo", "-f"],
                    Some("append_const"),
                    None,
                    Some(vec!["test"]),
                    None,
                    None,
                    None,
                    None,
                    None,
                    Some("maud"),
                    None,
                )
                .unwrap()
                .add_argument::<&str>(
                    vec!["--boo", "-b"],
                    Some("append_const"),
                    None,
                    Some(vec!["poppy"]),
                    None,
                    None,
                    None,
                    None,
                    None,
                    Some("maud"),
                    None,
                )
                .unwrap()
                .add_argument::<&str>(
                    vec!["--moo", "-m"],
                    Some("append_const"),
                    None,
                    Some(vec!["sugar"]),
                    None,
                    None,
                    None,
                    None,
                    None,
                    Some("maud"),
                    None,
                )
                .unwrap()
                .parse_args(Some(vec![
                    "-f".to_string(),
                    "-m".to_string(),
                    "-b".to_string(),
                ]))
                .unwrap();
            assert_eq!(
                namespace.get_three_value::<String>("maud").unwrap(),
                ("test".to_string(), "sugar".to_string(), "poppy".to_string())
            )
        }
    }

    mod count {
        use super::*;

        #[test]
        fn count() {
            let namespace = ArgumentParser::default()
                .add_argument::<&str>(
                    vec!["--foo", "-f"],
                    Some("count"),
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
                .parse_args(Some(vec![
                    "-f".to_string(),
                    "--foo".to_string(),
                    "-f".to_string(),
                ]))
                .unwrap();
            assert_eq!(namespace.get_one_value::<usize>("foo").unwrap(), 3)
        }
    }
}
