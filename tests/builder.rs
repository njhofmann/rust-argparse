mod builder {
    // test equivalency between builder & add_argument with parse_args
    use py_arg_parse::{
        argument_parser::{ArgumentGroup, ArgumentParser, MutuallyExclusiveGroup},
        builder::ArgumentAdder,
        default::ArgumentDefault,
    };

    mod arg_parser_builder {
        use py_arg_parse::{
            argument_parser::{ArgumentParser, ArgumentParserBuilder},
            builder::ArgumentAdder,
        };

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
            let parent1 = ArgumentParser::new(
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
            let parent2 = ArgumentParser::new(
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
                vec!["--boo"],
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
                ArgumentParserBuilder::new()
                    .with_parents(vec![parent1.clone(), parent2.clone()])
                    .build()
                    .unwrap(),
                ArgumentParser::new(
                    None,
                    None,
                    None,
                    None,
                    Some(vec![parent1, parent2]),
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
        fn with_parent() {
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
            assert_eq!(
                ArgumentParserBuilder::new()
                    .with_parent(parent.clone())
                    .build()
                    .unwrap(),
                ArgumentParser::new(
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
                )
                .unwrap()
            )
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
            assert!(ArgumentParserBuilder::new()
                .with_conflict_handler("jkfjafid")
                .build()
                .is_err())
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
        let left = ArgumentParser::default()
            .add_argument::<&str>(
                vec!["--foo"],
                Some("append_const"),
                None,
                Some(vec!["a"]),
                None,
                None,
                None,
                None,
                None,
                Some("dest"),
                None,
            )
            .unwrap()
            .add_argument::<&str>(
                vec!["--bar"],
                Some("append_const"),
                None,
                Some(vec!["b"]),
                None,
                None,
                None,
                None,
                None,
                Some("dest"),
                None,
            )
            .unwrap()
            .parse_args(Some(vec!["--foo".to_string(), "--bar".to_string()]))
            .unwrap();
        let right = ArgumentParser::default()
            .with_name(vec!["--foo"])
            .unwrap()
            .with_action("append_const")
            .with_constant(vec!["a"])
            .with_dest("dest")
            .with_name(vec!["--bar"])
            .unwrap()
            .with_action("append_const")
            .with_constant(vec!["b"])
            .with_dest("dest")
            .parse_args(Some(vec!["--foo".to_string(), "--bar".to_string()]))
            .unwrap();
        assert_eq!(left, right)
    }

    #[test]
    fn metavar() {
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
                Some("new_display"),
                None,
                None,
            )
            .unwrap()
            .parse_args(Some(vec!["--foo".to_string(), "bar".to_string()]))
            .unwrap();
        let right = ArgumentParser::default()
            .with_name(vec!["--foo"])
            .unwrap()
            .with_metavar("new_display")
            .parse_args(Some(vec!["--foo".to_string(), "bar".to_string()]))
            .unwrap();
        assert_eq!(left, right)
    }

    #[test]
    fn default() {
        let left = ArgumentParser::default()
            .add_argument::<&str>(
                vec!["--foo"],
                None,
                None,
                None,
                Some(ArgumentDefault::Value(vec!["blah"])),
                None,
                None,
                None,
                None,
                None,
                None,
            )
            .unwrap()
            .parse_args(Some(vec![]))
            .unwrap();
        let right = ArgumentParser::default()
            .with_name(vec!["--foo"])
            .unwrap()
            .with_default(ArgumentDefault::Value(vec!["blah"]))
            .parse_args(Some(vec![]))
            .unwrap();
        assert_eq!(left, right)
    }

    #[test]
    fn mutually_exclusive_group_builder() {
        let left = ArgumentParser::default()
            .add_mutually_exclusive_group(
                MutuallyExclusiveGroup::default()
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
                    .add_argument::<&str>(
                        vec!["--gar"],
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
                    .unwrap(),
            )
            .unwrap()
            .parse_args(Some(vec!["--foo".to_string(), "bar".to_string()]))
            .unwrap();
        let right = ArgumentParser::default()
            .add_mutually_exclusive_group(
                MutuallyExclusiveGroup::default()
                    .with_name(vec!["--foo"])
                    .unwrap()
                    .with_name(vec!["--gar"])
                    .unwrap(),
            )
            .unwrap()
            .parse_args(Some(vec!["--foo".to_string(), "bar".to_string()]))
            .unwrap();
        assert_eq!(left, right)
    }

    #[test]
    fn argument_group_builder() {
        let left = ArgumentParser::default()
            .add_argument_group(
                ArgumentGroup::default()
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
                    .add_argument::<&str>(
                        vec!["--gar"],
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
                    .unwrap(),
            )
            .unwrap()
            .parse_args(Some(vec![
                "--foo".to_string(),
                "bar".to_string(),
                "--gar".to_string(),
                "por".to_string(),
            ]))
            .unwrap();
        let right = ArgumentParser::default()
            .add_argument_group(
                ArgumentGroup::default()
                    .with_name(vec!["--foo"])
                    .unwrap()
                    .with_name(vec!["--gar"])
                    .unwrap(),
            )
            .unwrap()
            .parse_args(Some(vec![
                "--foo".to_string(),
                "bar".to_string(),
                "--gar".to_string(),
                "por".to_string(),
            ]))
            .unwrap();
        assert_eq!(left, right)
    }
}
