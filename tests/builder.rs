mod builder {
    // test equivalency between builder & add_argument with parse_args
    use py_arg_parse::{
        argument_parser::{ArgumentGroupError, ArgumentParser},
        builder::ArgumentAdder,
        default::ArgumentDefault,
    };

    mod arg_parser_builder {
        use py_arg_parse::argument_parser::{ArgumentParser, ArgumentParserBuilder};

        #[test]
        fn with_prog() {
            assert_eq!(
                ArgumentParserBuilder::new()
                    .with_prog("program")
                    .build()
                    .unwrap(),
                ArgumentParser::new(
                    Some("program"),
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
        fn with_usage() {
            assert_eq!(
                ArgumentParserBuilder::new()
                    .with_usage("usage")
                    .build()
                    .unwrap(),
                ArgumentParser::new(
                    None,
                    Some("usage"),
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
            )
        }

        #[test]
        fn with_description() {
            assert_eq!(
                ArgumentParserBuilder::new()
                    .with_description("this parses")
                    .build()
                    .unwrap(),
                ArgumentParser::new(
                    None,
                    None,
                    Some("this parses"),
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                )
                .unwrap()
            )
        }

 
        #[test]
        fn with_epilog() {
            assert_eq!(
                ArgumentParserBuilder::new()
                    .with_epilog("the end")
                    .build()
                    .unwrap(),
                ArgumentParser::new(
                    None,
                    None,
                    None,
                    Some("the end"),
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                )
                .unwrap()
            )
        }

        #[test]
        fn with_parents() {
            todo!()
        }
        #[test]
        fn with_parent() {
            todo!()
        }

        #[test]
        fn with_prefix_chars() {
            assert_eq!(
                ArgumentParserBuilder::new()
                    .with_prefix_chars("+")
                    .with_prefix_chars("-@")
                    .build()
                    .unwrap(),
                ArgumentParser::new(
                    None,
                    None,
                    None,
                    None,
                    None,
                    Some("+-@"),
                    None,
                    None,
                    None,
                    None,
                )
                .unwrap()
            )
        }
        #[test]
        fn with_suppress_missing_attributes_true() {
            assert_eq!(
                ArgumentParserBuilder::new()
                    .with_suppress_missing_attributes(true)
                    .build()
                    .unwrap(),
                ArgumentParser::new(
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
            )
        }

        #[test]
        fn with_suppress_missing_attributes_false() {
            assert_eq!(
                ArgumentParserBuilder::new()
                    .with_suppress_missing_attributes(false)
                    .build()
                    .unwrap(),
                ArgumentParser::new(
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    Some(false),
                    None,
                    None,
                    None,
                )
                .unwrap()
            )
        }


        #[test]
        fn with_conflict_handler_error() {
            assert_eq!(
                ArgumentParserBuilder::new()
                    .with_conflict_handler("error")
                    .build()
                    .unwrap(),
                ArgumentParser::new(
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    Some("error"),
                    None,
                    None,
                )
                .unwrap()
            )
        }

        #[test]
        fn with_conflict_handler_override() {
            assert_eq!(
                ArgumentParserBuilder::new()
                    .with_conflict_handler("override")
                    .build()
                    .unwrap(),
                ArgumentParser::new(
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
            )
        }

        #[test]
        fn with_conflict_handler_bad_value() {
            assert!(
                ArgumentParserBuilder::new()
                    .with_conflict_handler("jkfjafid")
                    .build()
                   .is_err()
                )
        }

        #[test]
        fn with_help_true() {
            assert_eq!(
                ArgumentParserBuilder::new()
                    .with_help(true)
                    .build()
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
                    Some(true),
                    None,
                )
                .unwrap()
            )
        }

        #[test]
        fn with_help_false() {
            assert_eq!(
                ArgumentParserBuilder::new()
                    .with_help(true)
                    .build()
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
                    Some(true),
                    None,
                )
                .unwrap()
            )
        }

        #[test]
        fn with_allow_abbrev_true() {
            assert_eq!(
                ArgumentParserBuilder::new()
                    .with_allow_abbrev(true)
                    .build()
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
                    None,
                    Some(true)
                )
                .unwrap()
            )
        }

        #[test]
        fn with_allow_abbrev_false() {
            assert_eq!(
                ArgumentParserBuilder::new()
                    .with_allow_abbrev(false)
                    .build()
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
                    None,
                    Some(false)
                )
                .unwrap()
            )
        }
    }

    #[test]
    fn single_storage_argument() {
        let left = ArgumentParser::default()
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
            .parse_args(Some(vec!["--foo".to_string(), "bar".to_string()]))
            .unwrap();
        let right = ArgumentParser::default()
            .with_name(vec!["--foo"])
            .unwrap()
            .parse_args(Some(vec!["--foo".to_string(), "bar".to_string()]))
            .unwrap();
        assert_eq!(left, right)
    }

    #[test]
    fn append_const() {
        todo!()
    }

    #[test]
    fn metavar() {
        todo!()
    }

    #[test]
    fn default() {
        todo!()
    }

    #[test]
    fn mutually_exclusive_group_builder() {
        todo!()
    }

    #[test]
    fn argument_group_builder() {
        todo!()
    }
}
