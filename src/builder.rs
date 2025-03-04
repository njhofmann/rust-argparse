use thiserror::Error;

use crate::{argument::ArgumentError, default::ArgumentDefault, nargs::NArgs};
use std::fmt::Display;
#[derive(Debug, PartialEq, Eq, Clone)]

pub struct ArgumentBuilder {
    // keep everything but name optional
    // leaves checks upon argument construction
    name: Vec<String>,
    required: Option<bool>,
    help: Option<String>,
    choices: Option<Vec<Vec<String>>>,
    default: Option<ArgumentDefault<String>>,
    nargs: Option<NArgs>,
    action: Option<String>,
    metavar: Option<String>,
    dest: Option<String>,
    constant: Option<Vec<String>>,
    version: Option<String>,
}

impl Display for ArgumentBuilder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl ArgumentBuilder {
    fn new(name: Vec<&str>) -> ArgumentBuilder {
        ArgumentBuilder {
            name: name.into_iter().map(|x| x.to_string()).collect(),
            required: None,
            help: None,
            choices: None,
            default: None,
            nargs: None,
            action: None,
            metavar: None,
            dest: None,
            constant: None,
            version: None,
        }
    }

    pub fn with_required(&self) -> Self {
        let mut new = self.clone();
        new.required = Some(true);
        new
    }

    pub fn with_nargs(&self, nargs: NArgs) -> Self {
        let mut new = self.clone();
        new.nargs = Some(nargs);
        new
    }

    pub fn with_help(&self, help: &str) -> Self {
        let mut new = self.clone();
        new.help = Some(help.to_string());
        new
    }

    pub fn with_choices<T: ToString>(&self, choices: Vec<Vec<T>>) -> Self {
        let mut new = self.clone();
        new.choices = Some(
            choices
                .iter()
                .map(|x| x.iter().map(|y| y.to_string()).collect())
                .collect(),
        );
        new
    }

    pub fn with_default<T: ToString>(&self, default: ArgumentDefault<T>) -> Self {
        let mut new = self.clone();
        new.default = Some(match default {
            ArgumentDefault::None => ArgumentDefault::None,
            ArgumentDefault::Suppress => ArgumentDefault::Suppress,
            ArgumentDefault::Value(items) => {
                ArgumentDefault::Value(items.iter().map(|x| x.to_string()).collect())
            }
        });
        new
    }

    pub fn with_action(&self, action: &str) -> Self {
        let mut new = self.clone();
        new.action = Some(action.to_string());
        new
    }

    pub fn with_metavar(&self, metavar: &str) -> Self {
        let mut new = self.clone();
        new.metavar = Some(metavar.to_string());
        new
    }

    pub fn with_dest(&self, dest: &str) -> Self {
        let mut new = self.clone();
        new.dest = Some(dest.to_string());
        new
    }

    pub fn with_version(&self, version: &str) -> Self {
        let mut new = self.clone();
        new.version = Some(version.to_string());
        new
    }

    pub fn with_constant<T: ToString>(&self, constant: Vec<T>) -> Self {
        let mut new = self.clone();
        new.constant = Some(constant.iter().map(|x| x.to_string()).collect());
        new
    }
}

// errors occuring when adding an argument
#[derive(Error, Debug, PartialEq, Eq)]
pub enum AddArgumentError {
    #[error("{0}")]
    ArgumentError(ArgumentError),
    #[error("duplicated help argument added under argument {0}")]
    DuplicateHelpArgument(String),
    #[error("duplicated help argument added under argument {0}")]
    DuplicateVersionArgument(String),
}

pub trait ArgumentAdder {
    fn add_argument<T: ToString>(
        &self,
        name: Vec<&str>,
        action: Option<&str>,
        nargs: Option<NArgs>,
        constant: Option<Vec<T>>,
        default: Option<ArgumentDefault<T>>,
        choices: Option<Vec<Vec<T>>>,
        required: Option<bool>,
        help: Option<&str>,
        metavar: Option<&str>,
        dest: Option<&str>,
        version: Option<&str>,
    ) -> Result<Self, AddArgumentError>
    where
        Self: Sized;

    fn get_current_arg_builder(&self) -> Option<ArgumentBuilder>;

    fn set_current_arg_builder(&self, new_arg_builder: ArgumentBuilder) -> Self;

    fn add_argument_builder(&self) -> Result<Self, AddArgumentError>
    where
        Self: Sized,
    {
        let arg_builder = self.get_current_arg_builder().unwrap();
        let name = arg_builder.name.iter().map(|x| x.as_str()).collect();
        let action = arg_builder.action.as_deref();
        let nargs = arg_builder.nargs;
        let constant = arg_builder.constant;
        let default = arg_builder.default;
        let choices = arg_builder.choices;
        let required = arg_builder.required;
        let help: Option<&str> = arg_builder.help.as_deref();
        let metavar: Option<&str> = arg_builder.metavar.as_deref();
        let dest: Option<&str> = arg_builder.dest.as_deref();
        let version: Option<&str> = arg_builder.version.as_deref();
        self.add_argument(
            name, action, nargs, constant, default, choices, required, help, metavar, dest, version,
        )
    }

    fn with_name(&self, name: Vec<&str>) -> Result<Self, AddArgumentError>
    where
        Self: Sized,
    {
        let new_builder = ArgumentBuilder::new(name);
        match &self.get_current_arg_builder() {
            None => Ok(self.set_current_arg_builder(new_builder)),
            Some(_) => Ok(self
                .add_argument_builder()?
                .set_current_arg_builder(new_builder)),
        }
    }

    fn with_required(&self) -> Self
    where
        Self: Sized,
    {
        self.set_current_arg_builder(self.get_current_arg_builder().unwrap().with_required())
    }

    fn with_nargs(&self, nargs: NArgs) -> Self
    where
        Self: Sized,
    {
        self.set_current_arg_builder(self.get_current_arg_builder().unwrap().with_nargs(nargs))
    }

    fn with_help(&self, help: &str) -> Self
    where
        Self: Sized,
    {
        self.set_current_arg_builder(self.get_current_arg_builder().unwrap().with_help(help))
    }

    fn with_choices<T: ToString>(&self, choices: Vec<Vec<T>>) -> Self
    where
        Self: Sized,
    {
        self.set_current_arg_builder(
            self.get_current_arg_builder()
                .unwrap()
                .with_choices(choices),
        )
    }

    fn with_action(&self, action: &str) -> Self
    where
        Self: Sized,
    {
        self.set_current_arg_builder(self.get_current_arg_builder().unwrap().with_action(action))
    }

    fn with_constant<T: ToString>(&self, constant: Vec<T>) -> Self
    where
        Self: Sized,
    {
        self.set_current_arg_builder(
            self.get_current_arg_builder()
                .unwrap()
                .with_constant(constant),
        )
    }

    fn with_metavar(&self, metavar: &str) -> Self
    where
        Self: Sized,
    {
        self.set_current_arg_builder(
            self.get_current_arg_builder()
                .unwrap()
                .with_metavar(metavar),
        )
    }

    fn with_dest(&self, dest: &str) -> Self
    where
        Self: Sized,
    {
        self.set_current_arg_builder(self.get_current_arg_builder().unwrap().with_dest(dest))
    }

    fn with_version(&self, version: &str) -> Self
    where
        Self: Sized,
    {
        self.set_current_arg_builder(
            self.get_current_arg_builder()
                .unwrap()
                .with_version(version),
        )
    }

    fn with_default<T: ToString>(&self, default: ArgumentDefault<T>) -> Self
    where
        Self: Sized,
    {
        self.set_current_arg_builder(
            self.get_current_arg_builder()
                .unwrap()
                .with_default(default),
        )
    }
}
