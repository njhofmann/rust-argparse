mod argument_groups {
    mod mutually_exclusive {
        use py_arg_parse::argument_parser::{
            ArgumentGroupError, ArgumentParser, MutuallyExclusiveGroup, ParsingError,
        };

        #[test]
        fn mutually_exclusive_group_working() {
            let group = MutuallyExclusiveGroup::default()
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
            let parser = ArgumentParser::default()
                .add_mutually_exclusive_group(group)
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
            let namespace = parser
                .parse_args(Some(vec![
                    "--boo".to_string(),
                    "7".to_string(),
                    "--foo".to_string(),
                    "5".to_string(),
                ]))
                .unwrap();
            assert_eq!(namespace.get_one_value::<usize>("foo").unwrap(), 5);
            assert_eq!(namespace.get_one_value::<usize>("boo").unwrap(), 7);
            assert!(!namespace.contains("bar"))
        }

        #[test]
        fn mutually_exclusive_group_violated() {
            let group = MutuallyExclusiveGroup::default()
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
            let parser = ArgumentParser::default()
                .add_mutually_exclusive_group(group)
                .unwrap();
            let err = parser
                .parse_args(Some(vec![
                    "--foo".to_string(),
                    "5".to_string(),
                    "--bar".to_string(),
                    "6".to_string(),
                ]))
                .unwrap_err();
            assert_eq!(
                err,
                ParsingError::ArgumentGroupError(
                    ArgumentGroupError::DuplicateMutuallyExclusiveGroupArguments
                )
            )
        }
    }
}
