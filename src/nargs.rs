use std::fmt::Display;

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum NArgs {
    Exact(usize),
    ZeroOrOne, // ?
    AnyNumber, // *
    OneOrMore, // +
}

impl NArgs {
    pub fn is_valid_number(&self, x: usize) -> bool {
        match self {
            NArgs::Exact(n) => n == &x,
            NArgs::ZeroOrOne => x < 2,
            NArgs::AnyNumber => true,
            NArgs::OneOrMore => x > 0,
        }
    }

    pub fn can_be_zero(&self) -> bool {
        match self {
            NArgs::Exact(x) if x != &0 => false,
            NArgs::OneOrMore => false,
            _ => true,
        }
    }

    pub fn is_variable(&self) -> bool {
        // variable means no bound on size
        match self {
            NArgs::AnyNumber | NArgs::OneOrMore => true,
            _ => false,
        }
    }

    pub fn min_n_required_args(&self) -> usize {
        match self {
            NArgs::Exact(n) => n.clone(),
            NArgs::OneOrMore => 1,
            _ => 0,
        }
    }
}

impl Display for NArgs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                NArgs::Exact(n) => n.to_string(),
                NArgs::ZeroOrOne => "zero or one ('?')".to_string(),
                NArgs::AnyNumber => "any number ('*')".to_string(),
                NArgs::OneOrMore => "at least one ('+')".to_string(),
            }
        )
    }
}
