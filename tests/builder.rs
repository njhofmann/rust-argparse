mod builder {
    // test equivalency between builder & add_argument with parse_args
    use py_arg_parse::{
        argument_parser::{ArgumentGroupError, ArgumentParser},
        builder::ArgumentAdder,
        default::ArgumentDefault,
    };

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
    fn argument_group_builder() {
        todo!()
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
}
