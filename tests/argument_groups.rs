mod argument_groups {
    mod mutually_exclusive {
        use py_arg_parse::{argument_parser::{
            ArgumentGroupError, ArgumentParser, MutuallyExclusiveGroup, ParsingError,
        }, default::ArgumentDefault};

        #[test]
        fn working_group() {
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
                    Some(ArgumentDefault::Value(vec!["a", "b", "c"])),
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
            // other mutually exclusive resort to default value
            assert_eq!(namespace.get_three_value::<String>("bar").unwrap(), ("a".to_string(), "b".to_string(), "c".to_string())); 
        }

        #[test]
        fn working_group_with_local_suppress() {
            let group = MutuallyExclusiveGroup::default()
                .add_argument::<&str>(
                    vec!["--foo"],
                    None,
                    None,
                    None,
                    Some(ArgumentDefault::Suppress),
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
                    "--bar".to_string(),
                    "5".to_string(),
                ]))
                .unwrap();
            assert!(!namespace.contains("foo"));
            assert_eq!(namespace.get_one_value::<usize>("boo").unwrap(), 7);
            // other mutually exclusive resort to default value
            assert_eq!(namespace.get_one_value::<usize>("bar").unwrap(),5); 
        }

        #[test]
        fn working_group_with_global_suppress() {
            let group = MutuallyExclusiveGroup::default()
                .add_argument::<&str>(
                    vec!["--foo"],
                    None,
                    None,
                    None,
                    Some(ArgumentDefault::Suppress),
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
                None,
            )
            .unwrap()
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
                    "--bar".to_string(),
                    "5".to_string(),
                ]))
                .unwrap();
            assert!(!namespace.contains("foo"));
            assert_eq!(namespace.get_one_value::<usize>("boo").unwrap(), 7);
            // other mutually exclusive resort to default value
            assert_eq!(namespace.get_one_value::<usize>("bar").unwrap(),5); 
        }


        #[test]
        fn same_group_multiple_arguments() {
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

        #[test]
        fn missing_required() { 
            let group = MutuallyExclusiveGroup::new(None, None, None, Some(true))
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
                .add_mutually_exclusive_group(group.clone())
                .unwrap();
            let err = parser
                .parse_args(Some(vec![
                    ]))
                .unwrap_err();
            
            assert_eq!(
                err,
                ParsingError::ArgumentGroupError(
                    ArgumentGroupError::MissingRequiredArgumentGroup(group.to_string())
                )
            )
        }


        #[test]
        fn missing_non_required() { 
            let group = MutuallyExclusiveGroup::default()
            .add_argument::<&str>(
                vec!["--foo"],
                None,
                None,
                None,
                Some(ArgumentDefault::Value(vec!["d", "g", "f"])),
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
        let namespace = ArgumentParser::default()
            .add_mutually_exclusive_group(group.clone()).unwrap()
            .parse_args(Some(vec![
                ]))
            .unwrap();
        
        assert!(namespace.get::<String>("bar").unwrap().is_empty());
        assert_eq!(namespace.get_three_value::<String>("foo").unwrap(), ("d".to_string(), "g".to_string(), "f".to_string())); 
        }
        #[test]
        fn missing_non_required_with_local_suppress() { 
            let group = MutuallyExclusiveGroup::default()
            .add_argument::<&str>(
                vec!["--foo"],
                None,
                None,
                None,
                Some(ArgumentDefault::Value(vec!["d", "g", "f"])),
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
                Some(ArgumentDefault::Suppress),
                None,
                None,
                None,
                None,
                None,
                None,
            )
            .unwrap();
        let namespace = ArgumentParser::default()
            .add_mutually_exclusive_group(group.clone()).unwrap()
            .parse_args(Some(vec![
                ]))
            .unwrap();
        
        assert!(!namespace.contains("bar"));
        assert_eq!(namespace.get_three_value::<String>("foo").unwrap(), ("d".to_string(), "g".to_string(), "f".to_string())); 
        }

        #[test]
        fn missing_non_required_with_global_suppress() { 
            let group = MutuallyExclusiveGroup::default()
            .add_argument::<&str>(
                vec!["--foo"],
                None,
                None,
                None,
                Some(ArgumentDefault::Value(vec!["d", "g", "f"])),
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
        let namespace = ArgumentParser::new(
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
            .add_mutually_exclusive_group(group.clone()).unwrap()
            .parse_args(Some(vec![
                ]))
            .unwrap();
        
        assert!(!namespace.contains("bar"));
        assert!(!namespace.contains("foo"));
        }

        
    }


    
}
