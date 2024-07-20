#[cfg(test)]
mod actions {
    use py_arg_parse::argument_parser::ArgumentParser;
    use py_arg_parse::parse_result::RetrievalError;

    mod store {
        use py_arg_parse::{argument_error::ArgumentError, argument_parser::ParserError};

        use super::*;

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
            let mut parser =  ArgumentParser::new(
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
                    .parse_args(Some(vec!["--beast".to_string(), "test".to_string(), "--beam".to_string(), "test".to_string()]))
                    .unwrap().get_one_value::<String>("beast").unwrap(),
                "test".to_string()
            );
            parser = parser           .add_argument::<&str>(
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
                .parse_args(Some(vec!["--beast".to_string(), "test".to_string(), "--beam".to_string(), "test".to_string()]))
                .unwrap().get_one_value::<String>("beast").unwrap(),
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
