use super::expression::{Expression, Function};
use super::flatten::{mutate, ItemOrCollection};
use super::{Component, Node, Text};
use crate::functions::display::RenderContext;
use crate::parser::{self, RawArgGroup, RawClause};
use crate::Data;
use crate::TimeFrequency;
use anyhow::{anyhow, Error, Result};
use chrono::NaiveDate;
use lazy_static::lazy_static;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::str::FromStr;

lazy_static! {
    pub static ref COMMANDS: HashMap<&'static str, &'static str> = {
        [
            ("fig", "Number"),
            ("name", "Name"),
            ("description", "Description"),
            ("span", "Time Span"),
            ("prev", "Previous Time Span"),
            ("change", "Change"),
            ("avg_freq", "Figure per Frequency"),
            ("col", "Column"),
            ("row", "Row"),
            ("table", "Table"),
        ]
        .into_iter()
        .collect()
    };
    pub static ref FUNCTIONS: HashSet<&'static str> = ["table"].iter().cloned().collect();
    static ref FREQUENCIES: HashSet<&'static str> = {
        ["Daily", "Weekly", "Monthly", "Quarterly", "Yearly"]
            .iter()
            .cloned()
            .collect()
    };
    static ref DISPLAY_TYPES: HashSet<&'static str> =
        ["Words", "Numbers",].iter().cloned().collect();
    pub static ref DATA_NAMES: HashMap<String, String> = Data::read_names().unwrap();
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Arg {
    Command(&'static str),
    Function(&'static str),
    TimeFrequency(TimeFrequency),
    DataName(&'static str),
    Date(NaiveDate),
    RenderContext(RenderContext),
}

impl Arg {
    /// Matches a name keyword to an enum variant and tries to create that variant from `s`
    pub fn from_labelled_str(name: &str, s: &str) -> Result<Self> {
        let arg = match name {
            "command" => Arg::try_new_command(s),
            "function" => Arg::try_new_function(s),
            "frequency" => Arg::try_new_time_frequency(s),
            "name" => Arg::try_new_data_name(s),
            "date" => Arg::try_new_date(s),
            "display" => Arg::try_new_render_context(s),
            _ => return Err(anyhow!("Unknown arg type {name}")),
        };
        arg.ok_or_else(|| anyhow!("Named arg of type {name} is unknown: {s}"))
    }

    fn try_new_command(s: &str) -> Option<Self> {
        COMMANDS
            .keys()
            .find(|command| **command == s)
            .and_then(|command| Some(Arg::Command(command)))
    }
    fn try_new_function(s: &str) -> Option<Self> {
        FUNCTIONS
            .get(s)
            .and_then(|function| Some(Arg::Function(function)))
    }
    fn try_new_time_frequency(s: &str) -> Option<Self> {
        s.parse::<TimeFrequency>()
            .and_then(|frequency| Ok(Arg::TimeFrequency(frequency)))
            .ok()
    }
    fn try_new_data_name(s: &str) -> Option<Self> {
        DATA_NAMES
            .keys()
            .find(|name| *name == &s.to_string())
            .and_then(|name| Some(Arg::DataName(name.as_str())))
    }
    fn try_new_date(s: &str) -> Option<Self> {
        parser::parse_date(s)
            .and_then(|date| Ok(Arg::Date(date)))
            .ok()
    }
    fn try_new_render_context(s: &str) -> Option<Self> {
        s.parse::<RenderContext>()
            .and_then(|render_context| Ok(Arg::RenderContext(render_context)))
            .ok()
    }
}

impl FromStr for Arg {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        for f in [
            Arg::try_new_command,
            Arg::try_new_function,
            Arg::try_new_time_frequency,
            Arg::try_new_data_name,
            Arg::try_new_date,
            Arg::try_new_render_context,
        ] {
            if let Some(arg) = f(s) {
                return Ok(arg);
            }
        }
        Err(anyhow!("Unnamed arg is unknown: {s}"))
    }
}

impl Display for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(text) = match self {
            Arg::Command(command) => COMMANDS
                .get(command)
                .and_then(|command| Some(command.to_string())),
            Arg::Function(function) => Some(function.to_string()),
            Arg::TimeFrequency(frequency) => Some(frequency.to_string()),
            Arg::DataName(name) => DATA_NAMES
                .get(&name.to_string())
                .and_then(|name| Some(name.to_owned())),
            Arg::Date(date) => Some(date.format("%Y-%m-%d").to_string()),
            Arg::RenderContext(rc) => Some(rc.to_string()),
        } {
            write!(f, "{}", text)
        } else {
            panic!("Failed to Display Arg");
        }
    }
}

pub enum ArgGroup<'a> {
    Arg(Arg),
    Collection(Vec<Arg>),
    NamedCollection(&'a str, Vec<Arg>),
}

impl<'a> TryFrom<RawArgGroup<'a>> for ArgGroup<'a> {
    type Error = Error;

    fn try_from(raw: RawArgGroup<'a>) -> Result<Self, Self::Error> {
        match raw {
            RawArgGroup::Arg(arg) => Ok(ArgGroup::Arg(arg.parse::<Arg>()?)),
            RawArgGroup::LabelledArg(label, arg) => {
                Ok(ArgGroup::Arg(Arg::from_labelled_str(label, arg)?))
            }
            RawArgGroup::Collection(args) => Ok(ArgGroup::Collection(
                args.into_iter()
                    .map(|arg| arg.parse::<Arg>())
                    .collect::<Result<Vec<_>>>()?,
            )),
            RawArgGroup::NamedCollection(name, args) => Ok(ArgGroup::NamedCollection(
                name,
                args.into_iter()
                    .map(|arg| arg.parse::<Arg>())
                    .collect::<Result<Vec<_>>>()?,
            )),
        }
    }
}

impl ItemOrCollection for ArgGroup<'_> {
    type Item = Arg;

    fn items(&self) -> &[Self::Item] {
        match self {
            ArgGroup::Collection(collection) | ArgGroup::NamedCollection(_, collection) => {
                collection
            }
            ArgGroup::Arg(arg) => std::slice::from_ref(arg),
        }
    }
}

pub enum Clause<'a> {
    Function(Vec<ArgGroup<'a>>),
    NamedFunction(&'a str, Vec<ArgGroup<'a>>),
    Block(Vec<ArgGroup<'a>>, Vec<Clause<'a>>),
    Text(&'a str),
}

impl Clause<'_> {
    pub fn to_component(self) -> Result<Box<dyn Component>> {
        match self {
            Clause::Function(groups) => Ok(Box::new(Function::from_groups(groups)?)),
            Clause::NamedFunction(name, groups) => {
                Ok(Box::new(Function::from_labelled_groups(name, groups)?))
            }
            Clause::Block(groups, clauses) => {
                let exprs = mutate(ItemOrCollection::collection(groups))
                    .into_iter()
                    .map(|group| {
                        group.into_iter().fold(Expression::new(), |mut expr, arg| {
                            expr.fill_arg(arg);
                            expr
                        })
                    })
                    .collect::<Vec<Expression>>();

                let base_node = clauses.into_iter().try_fold(
                    Node::new(Expression::new()),
                    |mut node, clause| -> Result<Node> {
                        node.add_child(clause.to_component()?);
                        Ok(node)
                    },
                )?;

                let node = exprs.into_iter().fold(
                    Node::new(Expression::new()),
                    |mut parent, child_expr| {
                        parent.add_child(Box::new(base_node.with_expr(child_expr)));
                        parent
                    },
                );

                Ok(Box::new(node))
            }
            Clause::Text(text) => Ok(Box::new(Text(text.to_string()))),
        }
    }
}

impl<'a> TryFrom<RawClause<'a>> for Clause<'a> {
    type Error = Error;

    fn try_from(raw: RawClause<'a>) -> Result<Self, Self::Error> {
        match raw {
            RawClause::Function(arg_groups) => Ok(Clause::Function(
                arg_groups
                    .into_iter()
                    .map(|group| ArgGroup::try_from(group))
                    .collect::<Result<_>>()?,
            )),
            RawClause::NamedFunction(name, arg_groups) => Ok(Clause::NamedFunction(
                name,
                arg_groups
                    .into_iter()
                    .map(|group| ArgGroup::try_from(group))
                    .collect::<Result<_>>()?,
            )),
            RawClause::Block(arg_groups, clauses) => Ok(Clause::Block(
                arg_groups
                    .into_iter()
                    .map(|group| ArgGroup::try_from(group))
                    .collect::<Result<_>>()?,
                clauses
                    .into_iter()
                    .map(|clause| Clause::try_from(clause))
                    .collect::<Result<_>>()?,
            )),
            RawClause::Text(text) => Ok(Clause::Text(text)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arg() {
        assert_eq!("fig".parse::<Arg>().unwrap(), Arg::Command("fig"));
    }
}
