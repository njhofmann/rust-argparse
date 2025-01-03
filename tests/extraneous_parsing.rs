mod extraneous_parsing {

    mod long_optional_value {
        use py_arg_parse::{
            argument_parser::{ArgumentParser, ParsingError},
            nargs::NArgs,
        };
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
                ParsingError::IncorrectValueCount("[--foo]".to_string(), NArgs::Exact(2), 1)
            );
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

        mod short_option {
            use py_arg_parse::{
                argument_parser::{ArgumentParser, ParsingError},
                nargs::NArgs,
            };
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
                assert_eq!(parser, ParsingError::InvalidFlagArgument("c".to_string()));
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
                    ParsingError::IncorrectValueCount("[-b]".to_string(), NArgs::Exact(0), 1)
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
                    ParsingError::IncorrectValueCount("[-a]".to_string(), NArgs::Exact(2), 0)
                );
            }

            #[test]
            fn all_remaining_args_positional() {
                let parser = ArgumentParser::default()
                    .add_argument::<&str>(
                        vec!["a"],
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
                    .unwrap();
                let namespace_err = parser.parse_args(Some(vec!["-g".to_string()])).unwrap_err();
                assert_eq!(
                    namespace_err,
                    ParsingError::InvalidFlagArgument("g".to_string())
                );
                let namespace = parser
                    .parse_args(Some(vec!["--".to_string(), "-g".to_string()]))
                    .unwrap();
                assert_eq!(
                    namespace.get_one_value::<String>("a").unwrap(),
                    "-g".to_string()
                );
            }

            #[test]
            fn all_remaining_args_positional_with_flag() {
                let namespace = ArgumentParser::default()
                    .add_argument::<&str>(
                        vec!["a"],
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
                    .add_argument::<&str>(
                        vec!["-b"],
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
                    .parse_args(Some(vec![
                        "-b".to_string(),
                        "f".to_string(),
                        "--".to_string(),
                        "-g".to_string(),
                    ]))
                    .unwrap();
                assert_eq!(
                    namespace.get_one_value::<String>("a").unwrap(),
                    "-g".to_string()
                );
                assert_eq!(
                    namespace.get_one_value::<String>("b").unwrap(),
                    "f".to_string()
                );
            }
        }
    }
}
