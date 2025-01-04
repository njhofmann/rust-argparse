use std::fmt::Display;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum ArgumentDefault<T: ToString> {
    None,
    Suppress,
    Value(Vec<T>),
}

impl<T: Display> ArgumentDefault<T> {
    pub fn has_value(&self) -> bool {
        match self {
            ArgumentDefault::Value(_) => true,
            _ => false,
        }
    }

    pub fn get_value(&self) -> &Vec<T> {
        match self {
            ArgumentDefault::Value(v) => &v,
            _ => panic!(),
        }
    }

    pub fn is_suppress(&self) -> bool {
        match self {
            ArgumentDefault::Suppress => true,
            _ => false,
        }
    }
}
