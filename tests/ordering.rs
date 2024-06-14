#[cfg(test)]
mod ordering {
    use py_arg_parse::argument::NArgs;
    use py_arg_parse::argument_parser::ArgumentParser;
    use py_arg_parse::parse_result::Namespace;

    #[test]
    fn diff_add_arg_ordering_same_result() {
        let parser_a = ArgumentParser::default()
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
            .add_argument::<&str>(
                vec!["boo"],
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

        let parser_b = ArgumentParser::default()
            .add_argument::<&str>(
                vec!["boo"],
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
            .unwrap();
        let raw_args = Some(vec![
            "--foo".to_string(),
            "test".to_string(),
            "blah".to_string(),
        ]);
        assert_eq!(
            parser_a.parse_args(raw_args.clone()).unwrap(),
            parser_b.parse_args(raw_args.clone()).unwrap()
        );

        let raw_args = Some(vec![
            "blah".to_string(),
            "--foo".to_string(),
            "test".to_string(),
        ]);
        assert_eq!(
            parser_a.parse_args(raw_args.clone()).unwrap(),
            parser_b.parse_args(raw_args.clone()).unwrap()
        );
    }

    fn init_two_posn_arg_parser(a: NArgs, b: NArgs, c: Option<NArgs>, raw_args: Vec<&str>) -> Namespace {
         let mut parser = ArgumentParser::default()
        .add_argument::<&str>(
            vec!["a"],
            None,
            Some(a),
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
            Some(b),
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

        if let Some(nargs) = c {
            parser = parser.add_argument::<&str>(
                vec!["c"],
                None,
                Some(nargs),
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
        }
        parser.parse_args(Some(raw_args.into_iter().map(|x| x.to_string()).collect()))
        .unwrap()
    }

    #[test]
    fn var_posn_then_exact_posn() {
        let namespace = init_two_posn_arg_parser(NArgs::AnyNumber, NArgs::Exact(1), None, vec!["a", "b", "c", "d", "e"]);
        assert_eq!(namespace.get::<String>("a").unwrap(), vec!["a".to_string(), "b".to_string(), "c".to_string(), "d".to_string()]);
        assert_eq!(namespace.get::<String>("b").unwrap(), vec!["e".to_string()]);
    }

    #[test]
    fn var_posn_then_var_posn() {
        let namespace = init_two_posn_arg_parser(NArgs::AnyNumber, NArgs::AnyNumber, None, vec!["a", "b", "c", "d", "e"]);
        assert_eq!(namespace.get::<String>("a").unwrap(), vec!["a".to_string(), "b".to_string(), "c".to_string(), "d".to_string(), 'e'.to_string()]);
        assert!(namespace.get::<String>("b").unwrap().is_empty());
    }

    #[test]
    fn exact_posn_then_var_posn() {
        let namespace = init_two_posn_arg_parser(NArgs::Exact(1), NArgs::AnyNumber, None, vec!["a", "b", "c", "d", "e"]);
        assert_eq!(namespace.get::<String>("a").unwrap(), vec!["a".to_string()]);
        assert_eq!(namespace.get::<String>("b").unwrap(), vec!["b".to_string(), "c".to_string(), "d".to_string(), "e".to_string()]);
    }

    #[test]
    fn var_posn_then_exact_posn_then_var_posn() {
        let namespace = init_two_posn_arg_parser(NArgs::AnyNumber, NArgs::Exact(1), Some(NArgs::AnyNumber), vec!["a", "b", "c", "d", "e"]);
        assert_eq!(namespace.get::<String>("a").unwrap(), vec!["a".to_string(), "b".to_string(), "c".to_string(), "d".to_string()]);
        assert_eq!(namespace.get::<String>("b").unwrap(), vec!["e".to_string()]);
        assert!(namespace.get::<String>("c").unwrap().is_empty());
    }

    #[test]
    fn exact_posn_then_var_posn_then_exact_posn() {
        let namespace = init_two_posn_arg_parser(NArgs::Exact(1), NArgs::AnyNumber, Some(NArgs::Exact(1)), vec!["a", "b", "c", "d", "e"]);
        assert_eq!(namespace.get::<String>("a").unwrap(), vec!["a".to_string()]);
        assert_eq!(namespace.get::<String>("b").unwrap(), vec!["b".to_string(), "c".to_string(), "d".to_string()]);
        assert_eq!(namespace.get::<String>("c").unwrap(), vec!["e".to_string()]);
    }

    #[test]
    fn var_posn_then_var_posn_then_exact_posn() {
        let namespace = init_two_posn_arg_parser(NArgs::AnyNumber, NArgs::AnyNumber, Some(NArgs::Exact(1)), vec!["a", "b", "c", "d", "e"]);
        assert_eq!(namespace.get::<String>("a").unwrap(), vec!["a".to_string(), "b".to_string(), "c".to_string(), "d".to_string()]);
        assert!(namespace.get::<String>("b").unwrap().is_empty());
        assert_eq!(namespace.get::<String>("c").unwrap(), vec!["e".to_string()]);
    }

    #[test]
    fn exact_posn_then_exact_posn_then_var_posn() {
        let namespace = init_two_posn_arg_parser(NArgs::Exact(1), NArgs::Exact(1), Some(NArgs::AnyNumber), vec!["a", "b", "c", "d", "e"]);
        assert_eq!(namespace.get::<String>("a").unwrap(), vec!["a".to_string()]);
        assert_eq!(namespace.get::<String>("b").unwrap(), vec!["b".to_string()]);
        assert_eq!(namespace.get::<String>("c").unwrap(), vec!["c".to_string(), "d".to_string(), "e".to_string()]);
    }

    #[test]
    fn var_posn_then_exact_posn_then_exact_posn() {
        let namespace = init_two_posn_arg_parser(NArgs::AnyNumber, NArgs::Exact(1), Some(NArgs::Exact(1)), vec!["a", "b", "c", "d", "e"]);
        assert_eq!(namespace.get::<String>("a").unwrap(), vec!["a".to_string(), "b".to_string(), "c".to_string(), ]);
        assert_eq!(namespace.get::<String>("b").unwrap(), vec!["d".to_string()]);
        assert_eq!(namespace.get::<String>("c").unwrap(), vec!["e".to_string()]);
    }

    #[test]
    fn exact_posn_then_var_posn_then_var_posn() {
        let namespace = init_two_posn_arg_parser(NArgs::AnyNumber, NArgs::Exact(1), Some(NArgs::Exact(1)), vec!["a", "b", "c", "d", "e"]);
        assert_eq!(namespace.get::<String>("a").unwrap(), vec!["a".to_string()]);
        assert_eq!(namespace.get::<String>("b").unwrap(), vec!["b".to_string(), "c".to_string(), "d".to_string(), "e".to_string()]);
        assert!(namespace.get::<String>("c").unwrap().is_empty());
    }

    #[test]
    fn var_flag_then_var_posn() {
        todo!()
    }

    #[test]
    fn var_posn_then_var_flag() {
        todo!()
    }
}
