#[cfg(test)]
mod ordering {
    use py_arg_parse::argument_parser::ArgumentParser;
    use py_arg_parse::builder::ArgumentAdder;
    use py_arg_parse::nargs::NArgs;
    use py_arg_parse::parse_result::Namespace;
    #[test]
    fn diff_add_arg_ordering_same_result() {
        let mut parser_a = ArgumentParser::default()
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

        let mut parser_b = ArgumentParser::default()
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

    fn init_test_arg_parser(nargs_vec: Vec<(NArgs, bool)>, raw_args: Vec<&str>) -> Namespace {
        let mut parser = ArgumentParser::default();
        for (i, (nargs, is_flag)) in nargs_vec.into_iter().enumerate() {
            let mut name = i.to_string();
            if is_flag {
                name = "--".to_string() + name.as_str();
            }
            parser = parser
                .add_argument::<&str>(
                    vec![name.as_str()],
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

        parser
            .parse_args(Some(raw_args.into_iter().map(|x| x.to_string()).collect()))
            .unwrap()
    }

    #[test]
    fn var_posn_exact_posn() {
        let namespace = init_test_arg_parser(
            vec![(NArgs::AnyNumber, false), (NArgs::Exact(1), false)],
            vec!["a", "b", "c", "d", "e"],
        );
        assert_eq!(
            namespace.get::<String>("0").unwrap(),
            vec![
                "a".to_string(),
                "b".to_string(),
                "c".to_string(),
                "d".to_string()
            ]
        );
        assert_eq!(namespace.get::<String>("1").unwrap(), vec!["e".to_string()]);
    }

    #[test]
    fn var_posn_var_posn() {
        let namespace = init_test_arg_parser(
            vec![(NArgs::AnyNumber, false), (NArgs::AnyNumber, false)],
            vec!["a", "b", "c", "d", "e"],
        );
        assert_eq!(
            namespace.get::<String>("0").unwrap(),
            vec![
                "a".to_string(),
                "b".to_string(),
                "c".to_string(),
                "d".to_string(),
                'e'.to_string()
            ]
        );
        assert!(namespace.get::<String>("1").unwrap().is_empty());
    }

    #[test]
    fn exact_posn_var_posn() {
        let namespace = init_test_arg_parser(
            vec![(NArgs::Exact(1), false), (NArgs::AnyNumber, false)],
            vec!["a", "b", "c", "d", "e"],
        );
        assert_eq!(namespace.get::<String>("0").unwrap(), vec!["a".to_string()]);
        assert_eq!(
            namespace.get::<String>("1").unwrap(),
            vec![
                "b".to_string(),
                "c".to_string(),
                "d".to_string(),
                "e".to_string()
            ]
        );
    }

    #[test]
    fn var_posn_exact_posn_exact_posn() {
        let namespace = init_test_arg_parser(
            vec![
                (NArgs::AnyNumber, false),
                (NArgs::Exact(1), false),
                (NArgs::Exact(1), false),
            ],
            vec!["a", "b", "c", "d", "e"],
        );
        assert_eq!(
            namespace.get::<String>("0").unwrap(),
            vec!["a".to_string(), "b".to_string(), "c".to_string(),]
        );
        assert_eq!(namespace.get::<String>("1").unwrap(), vec!["d".to_string()]);
        assert_eq!(namespace.get::<String>("2").unwrap(), vec!["e".to_string()]);
    }

    #[test]
    fn var_posn_exact_posn_var_posn() {
        let namespace = init_test_arg_parser(
            vec![
                (NArgs::AnyNumber, false),
                (NArgs::Exact(1), false),
                (NArgs::AnyNumber, false),
            ],
            vec!["a", "b", "c", "d", "e"],
        );
        assert_eq!(
            namespace.get::<String>("0").unwrap(),
            vec![
                "a".to_string(),
                "b".to_string(),
                "c".to_string(),
                "d".to_string()
            ]
        );
        assert_eq!(namespace.get::<String>("1").unwrap(), vec!["e".to_string()]);
        assert!(namespace.get::<String>("2").unwrap().is_empty());
    }

    #[test]
    fn exact_posn_var_posn_exact_posn() {
        let namespace = init_test_arg_parser(
            vec![
                (NArgs::Exact(1), false),
                (NArgs::AnyNumber, false),
                (NArgs::Exact(1), false),
            ],
            vec!["a", "b", "c", "d", "e"],
        );
        assert_eq!(namespace.get::<String>("0").unwrap(), vec!["a".to_string()]);
        assert_eq!(
            namespace.get::<String>("1").unwrap(),
            vec!["b".to_string(), "c".to_string(), "d".to_string()]
        );
        assert_eq!(namespace.get::<String>("2").unwrap(), vec!["e".to_string()]);
    }

    #[test]
    fn exact_posn_zero_or_one_posn_exact_posn_three_values() {
        let namespace = init_test_arg_parser(
            vec![
                (NArgs::Exact(1), false),
                (NArgs::ZeroOrOne, false),
                (NArgs::Exact(1), false),
            ],
            vec!["a", "b", "c"],
        );
        assert_eq!(namespace.get::<String>("0").unwrap(), vec!["a".to_string()]);
        assert_eq!(namespace.get::<String>("1").unwrap(), vec!["b".to_string()]);
        assert_eq!(namespace.get::<String>("2").unwrap(), vec!["c".to_string()]);
    }

    #[test]
    fn zero_or_one_posn_zero_or_one_posn_exact_posn_three_values() {
        let namespace = init_test_arg_parser(
            vec![
                (NArgs::ZeroOrOne, false),
                (NArgs::ZeroOrOne, false),
                (NArgs::Exact(2), false),
            ],
            vec!["a", "b", "c"],
        );
        assert_eq!(namespace.get::<String>("0").unwrap(), vec!["a".to_string()]);
        assert!(namespace.get::<String>("1").unwrap().is_empty());
        assert_eq!(
            namespace.get::<String>("2").unwrap(),
            vec!["b".to_string(), "c".to_string()]
        );
    }

    #[test]
    fn zero_or_one_posn_variable_posn_exact_posn_three_values() {
        let namespace = init_test_arg_parser(
            vec![
                (NArgs::ZeroOrOne, false),
                (NArgs::AnyNumber, false),
                (NArgs::Exact(2), false),
            ],
            vec!["a", "b", "c"],
        );
        assert_eq!(namespace.get::<String>("0").unwrap(), vec!["a".to_string()]);
        assert!(namespace.get::<String>("1").unwrap().is_empty());
        assert_eq!(
            namespace.get::<String>("2").unwrap(),
            vec!["b".to_string(), "c".to_string()]
        );
    }

    #[test]
    fn exact_posn_zero_or_one_posn_exact_posn_two_values() {
        let namespace = init_test_arg_parser(
            vec![
                (NArgs::Exact(1), false),
                (NArgs::ZeroOrOne, false),
                (NArgs::Exact(1), false),
            ],
            vec!["a", "b"],
        );
        assert_eq!(namespace.get::<String>("0").unwrap(), vec!["a".to_string()]);
        assert!(namespace.get::<String>("1").unwrap().is_empty());
        assert_eq!(namespace.get::<String>("2").unwrap(), vec!["b".to_string()]);
    }

    #[test]
    fn var_posn_var_posn_exact_posn() {
        let namespace = init_test_arg_parser(
            vec![
                (NArgs::AnyNumber, false),
                (NArgs::AnyNumber, false),
                (NArgs::Exact(1), false),
            ],
            vec!["a", "b", "c", "d", "e"],
        );
        assert_eq!(
            namespace.get::<String>("0").unwrap(),
            vec![
                "a".to_string(),
                "b".to_string(),
                "c".to_string(),
                "d".to_string()
            ]
        );
        assert!(namespace.get::<String>("1").unwrap().is_empty());
        assert_eq!(namespace.get::<String>("2").unwrap(), vec!["e".to_string()]);
    }

    #[test]
    fn exact_posn_exact_posn_var_posn() {
        let namespace = init_test_arg_parser(
            vec![
                (NArgs::Exact(1), false),
                (NArgs::Exact(1), false),
                (NArgs::AnyNumber, false),
            ],
            vec!["a", "b", "c", "d", "e"],
        );
        assert_eq!(namespace.get::<String>("0").unwrap(), vec!["a".to_string()]);
        assert_eq!(namespace.get::<String>("1").unwrap(), vec!["b".to_string()]);
        assert_eq!(
            namespace.get::<String>("2").unwrap(),
            vec!["c".to_string(), "d".to_string(), "e".to_string()]
        );
    }

    #[test]
    fn exact_posn_var_posn_var_posn() {
        let namespace = init_test_arg_parser(
            vec![
                (NArgs::Exact(2), false),
                (NArgs::AnyNumber, false),
                (NArgs::AnyNumber, false),
            ],
            vec!["a", "b", "c", "d", "e"],
        );
        assert_eq!(
            namespace.get::<String>("0").unwrap(),
            vec!["a".to_string(), "b".to_string()]
        );
        assert_eq!(
            namespace.get::<String>("1").unwrap(),
            vec!["c".to_string(), "d".to_string(), "e".to_string()]
        );
        assert!(namespace.get::<String>("2").unwrap().is_empty());
    }

    #[test]
    fn at_least_one_posn_exact_posn() {
        let namespace = init_test_arg_parser(
            vec![(NArgs::OneOrMore, false), (NArgs::Exact(1), false)],
            vec!["a", "b", "c", "d", "e"],
        );
        assert_eq!(
            namespace.get::<String>("0").unwrap(),
            vec![
                "a".to_string(),
                "b".to_string(),
                "c".to_string(),
                "d".to_string()
            ]
        );
        assert_eq!(namespace.get::<String>("1").unwrap(), vec!["e".to_string()]);
    }

    #[test]
    fn at_least_one_posn_at_least_one_posn_posn() {
        let namespace = init_test_arg_parser(
            vec![(NArgs::OneOrMore, false), (NArgs::OneOrMore, false)],
            vec!["a", "b", "c", "d", "e"],
        );
        assert_eq!(
            namespace.get::<String>("0").unwrap(),
            vec![
                "a".to_string(),
                "b".to_string(),
                "c".to_string(),
                "d".to_string()
            ]
        );
        assert_eq!(namespace.get::<String>("1").unwrap(), vec!["e".to_string()]);
    }

    #[test]
    fn exact_posn_zero_or_one_posn_leftover() {
        let namespace = init_test_arg_parser(
            vec![(NArgs::Exact(4), false), (NArgs::ZeroOrOne, false)],
            vec!["a", "b", "c", "d", "e"],
        );
        assert_eq!(
            namespace.get::<String>("0").unwrap(),
            vec![
                "a".to_string(),
                "b".to_string(),
                "c".to_string(),
                "d".to_string()
            ]
        );
        assert_eq!(namespace.get::<String>("1").unwrap(), vec!["e".to_string()]);
    }

    #[test]
    fn exact_posn_zero_or_one_posn_no_leftover() {
        let namespace = init_test_arg_parser(
            vec![(NArgs::Exact(5), false), (NArgs::ZeroOrOne, false)],
            vec!["a", "b", "c", "d", "e"],
        );
        assert_eq!(
            namespace.get::<String>("0").unwrap(),
            vec![
                "a".to_string(),
                "b".to_string(),
                "c".to_string(),
                "d".to_string(),
                "e".to_string()
            ]
        );
        assert!(namespace.get::<String>("1").unwrap().is_empty());
    }

    #[test]
    fn exact_posn_at_least_one_posn() {
        let namespace = init_test_arg_parser(
            vec![(NArgs::Exact(1), false), (NArgs::OneOrMore, false)],
            vec!["a", "b", "c", "d", "e"],
        );
        assert_eq!(namespace.get::<String>("0").unwrap(), vec!["a".to_string()]);
        assert_eq!(
            namespace.get::<String>("1").unwrap(),
            vec![
                "b".to_string(),
                "c".to_string(),
                "d".to_string(),
                "e".to_string()
            ]
        );
    }

    #[test]
    fn exact_posn_one_or_more_posn_exact_posn() {
        let namespace = init_test_arg_parser(
            vec![
                (NArgs::Exact(1), false),
                (NArgs::OneOrMore, false),
                (NArgs::Exact(1), false),
            ],
            vec!["a", "b", "c", "d", "e"],
        );
        assert_eq!(namespace.get::<String>("0").unwrap(), vec!["a".to_string()]);
        assert_eq!(
            namespace.get::<String>("1").unwrap(),
            vec!["b".to_string(), "c".to_string(), "d".to_string()]
        );
        assert_eq!(namespace.get::<String>("2").unwrap(), vec!["e".to_string()]);
    }

    #[test]
    fn exact_posn_one_or_more_posn_one_or_more_posn() {
        let namespace = init_test_arg_parser(
            vec![
                (NArgs::Exact(1), false),
                (NArgs::OneOrMore, false),
                (NArgs::OneOrMore, false),
            ],
            vec!["a", "b", "c", "d", "e"],
        );
        assert_eq!(namespace.get::<String>("0").unwrap(), vec!["a".to_string()]);
        assert_eq!(
            namespace.get::<String>("1").unwrap(),
            vec!["b".to_string(), "c".to_string(), "d".to_string()]
        );
        assert_eq!(namespace.get::<String>("2").unwrap(), vec!["e".to_string()]);
    }

    #[test]
    fn exact_posn_zero_or_one_posn_one_or_more_posn() {
        let namespace = init_test_arg_parser(
            vec![
                (NArgs::Exact(2), false),
                (NArgs::ZeroOrOne, false),
                (NArgs::OneOrMore, false),
            ],
            vec!["a", "b", "c", "d", "e"],
        );
        assert_eq!(
            namespace.get::<String>("0").unwrap(),
            vec!["a".to_string(), "b".to_string()]
        );
        assert_eq!(namespace.get::<String>("1").unwrap(), vec!["c".to_string()]);
        assert_eq!(
            namespace.get::<String>("2").unwrap(),
            vec!["d".to_string(), "e".to_string()]
        );
    }

    #[test]
    fn var_flag_var_posn() {
        let namespace = init_test_arg_parser(
            vec![(NArgs::AnyNumber, true), (NArgs::AnyNumber, false)],
            vec!["--0", "a", "b", "c", "d", "e"],
        );
        assert_eq!(
            namespace.get::<String>("0").unwrap(),
            vec![
                "a".to_string(),
                "b".to_string(),
                "c".to_string(),
                "d".to_string(),
                "e".to_string()
            ]
        );
        assert!(namespace.get::<String>("1").unwrap().is_empty());
    }

    // #[test] // TODO error this out
    // fn var_flag_one_or_more_posn() {
    //     let namespace = init_test_arg_parser(
    //         vec![(NArgs::AnyNumber, true), (NArgs::OneOrMore, false)],
    //         vec!["--0", "a", "b"],
    //     );
    //     assert_eq!(namespace.get::<String>("0").unwrap(), vec!["a".to_string()]);
    //     assert_eq!(namespace.get::<String>("1").unwrap(), vec!["b".to_string()]);
    // }

    #[test]
    fn var_posn_var_flag_one_or_more_posn() {
        let namespace = init_test_arg_parser(
            vec![
                (NArgs::AnyNumber, false),
                (NArgs::AnyNumber, true),
                (NArgs::OneOrMore, false),
            ],
            vec!["a", "--1", "b", "c"],
        );
        // actual: {0: [a], 1: [b], 2: [c]}
        // expected: {0: [], 1: [b, c], 2: [a]}
        assert!(namespace.get::<String>("0").unwrap().is_empty());
        assert_eq!(
            namespace.get::<String>("1").unwrap(),
            vec!["b".to_string(), "c".to_string()]
        );
        assert_eq!(namespace.get::<String>("2").unwrap(), vec!["a".to_string()]);
    }

    // #[test]
    // fn one_or_more_posn_var_flag_one_or_more_posn() {
    //     let namespace = init_test_arg_parser(
    //         vec![(NArgs::OneOrMore, false), (NArgs::AnyNumber, true), (NArgs::OneOrMore, false)],
    //         vec!["a", "--1", "b", "c"],
    //     );
    //     // actual: {0: [a], 1: [b], 2: [c]}
    //     // expected: {0: [], 1: [b, c], 2: [a]}
    //     assert_eq!(
    //         namespace.get::<String>("0").unwrap(),
    //         vec!["a".to_string()]
    //     );
    //     assert_eq!(
    //         namespace.get::<String>("1").unwrap(),
    //         vec!["b".to_string(),]
    //     );
    //     assert_eq!(
    //         namespace.get::<String>("2").unwrap(),
    //         vec!["a".to_string()]
    //     );
    // }

    #[test]
    fn var_flag_exact_posn() {}

    #[test]
    fn var_posn_var_flag_posn_first() {
        let namespace = init_test_arg_parser(
            vec![(NArgs::AnyNumber, false), (NArgs::AnyNumber, true)],
            vec!["a", "--1", "b", "c"],
        );
        assert_eq!(namespace.get::<String>("0").unwrap(), vec!["a".to_string()]);
        assert_eq!(
            namespace.get::<String>("1").unwrap(),
            vec!["b".to_string(), "c".to_string()]
        );
    }

    #[test]
    fn var_posn_var_flag_flag_first() {
        let namespace = init_test_arg_parser(
            vec![(NArgs::AnyNumber, false), (NArgs::AnyNumber, true)],
            vec!["--1", "a", "b", "c"],
        );
        assert!(namespace.get::<String>("0").unwrap().is_empty());
        assert_eq!(
            namespace.get::<String>("1").unwrap(),
            vec!["a".to_string(), "b".to_string(), "c".to_string()]
        );
    }

    #[test]
    fn var_posn_var_flag_exact_posn() {
        let namespace = init_test_arg_parser(
            vec![
                (NArgs::AnyNumber, false),
                (NArgs::AnyNumber, true),
                (NArgs::Exact(1), false),
            ],
            vec!["a", "--1", "b", "c"],
        );
        assert!(namespace.get::<String>("0").unwrap().is_empty());
        assert_eq!(
            namespace.get::<String>("1").unwrap(),
            vec!["b".to_string(), "c".to_string()]
        );
        assert_eq!(namespace.get::<String>("2").unwrap(), vec!["a".to_string()]);
    }

    #[test]
    fn var_posn_var_flag_some_after_flag() {
        let namespace = init_test_arg_parser(
            vec![(NArgs::AnyNumber, false), (NArgs::AnyNumber, true)],
            vec!["a", "b", "c", "--1", "d", "e"],
        );
        assert_eq!(
            namespace.get::<String>("0").unwrap(),
            vec!["a".to_string(), "b".to_string(), "c".to_string()]
        );
        assert_eq!(
            namespace.get::<String>("1").unwrap(),
            vec!["d".to_string(), "e".to_string()]
        );
    }

    #[test]
    fn var_posn_exact_flag_var_posn() {
        let namespace = init_test_arg_parser(
            vec![
                (NArgs::AnyNumber, false),
                (NArgs::Exact(1), true),
                (NArgs::AnyNumber, false),
            ],
            vec!["a", "b", "c", "--1", "d"],
        );
        assert_eq!(
            namespace.get::<String>("0").unwrap(),
            vec!["a".to_string(), "b".to_string(), "c".to_string()]
        );
        assert_eq!(namespace.get::<String>("1").unwrap(), vec!["d".to_string()]);
        assert!(namespace.get::<String>("2").unwrap().is_empty())
    }

    #[test]
    fn var_posn_exact_flag_exact_posn() {
        let namespace = init_test_arg_parser(
            vec![
                (NArgs::AnyNumber, false),
                (NArgs::Exact(1), true),
                (NArgs::Exact(1), false),
            ],
            vec!["a", "b", "c", "--1", "d"],
        );
        assert_eq!(
            namespace.get::<String>("0").unwrap(),
            vec!["a".to_string(), "b".to_string()]
        );
        assert_eq!(namespace.get::<String>("1").unwrap(), vec!["d".to_string()]);
        assert_eq!(namespace.get::<String>("2").unwrap(), vec!["c".to_string()]);
    }

    #[test]
    fn var_posn_var_flag_none_after_flag() {
        let namespace = init_test_arg_parser(
            vec![(NArgs::AnyNumber, false), (NArgs::AnyNumber, true)],
            vec!["a", "b", "c", "--1"],
        );
        assert_eq!(
            namespace.get::<String>("0").unwrap(),
            vec!["a".to_string(), "b".to_string(), "c".to_string()]
        );
        assert!(namespace.get::<String>("1").unwrap().is_empty());
    }

    #[test]
    fn zero_or_one_flag_one() {
        let namespace: Namespace =
            init_test_arg_parser(vec![(NArgs::ZeroOrOne, true)], vec!["--0", "a"]);
        assert_eq!(namespace.get::<String>("0").unwrap(), vec!["a".to_string()]);
    }

    #[test]
    fn zero_or_one_flag_none() {
        let namespace = init_test_arg_parser(vec![(NArgs::ZeroOrOne, true)], vec!["--0"]);
        assert!(namespace.get::<String>("0").unwrap().is_empty());
    }

    #[test]
    fn zero_or_one_flag_as_one_exact_posn() {
        let namespace = init_test_arg_parser(
            vec![(NArgs::ZeroOrOne, true), (NArgs::Exact(2), false)],
            vec!["--0", "a", "b", "c"],
        );
        assert_eq!(namespace.get::<String>("0").unwrap(), vec!["a".to_string()]);
        assert_eq!(
            namespace.get::<String>("1").unwrap(),
            vec!["b".to_string(), "c".to_string()]
        );
    }

    #[test]
    fn zero_or_one_flag_as_zero_exact_posn() {
        let namespace = init_test_arg_parser(
            vec![(NArgs::ZeroOrOne, true), (NArgs::Exact(3), false)],
            vec!["--0", "a", "b", "c", "d"],
        );
        assert_eq!(namespace.get::<String>("0").unwrap(), vec!["a".to_string()]);
        assert_eq!(
            namespace.get::<String>("1").unwrap(),
            vec!["b".to_string(), "c".to_string(), "d".to_string(),]
        );
    }
}
