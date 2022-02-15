use anyhow::{anyhow, Error, Result};
use chrono::NaiveDate;
use lazy_static::lazy_static;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::ops::{Add, AddAssign};
use std::rc::Rc;

use crate::functions::table::Table;
use crate::functions::{AvgFreq, Change, Fig};
use crate::{Data, DisplayType, TimeFrequency};

use super::tree::Component;

lazy_static! {
    static ref COMMANDS: HashSet<&'static str> = {
        [
            "fig",
            "name",
            "description",
            "span",
            "prev",
            "change",
            "avg_freq",
            "col",
            "row",
            "table",
        ]
        .iter()
        .cloned()
        .collect()
    };
    static ref FREQUENCIES: HashSet<&'static str> = {
        ["Daily", "Weekly", "Monthly", "Quarterly", "Yearly"]
            .iter()
            .cloned()
            .collect()
    };
    static ref DISPLAY_TYPES: HashSet<&'static str> = {
        [
            "rounded",
            "describedrounded",
            "percentage",
            "describedpercentage",
        ]
        .iter()
        .cloned()
        .collect()
    };
    static ref DATA_NAMES: HashSet<String> = Data::read_names().unwrap().iter().cloned().collect();
}

pub struct CommandFreq {
    pub data: Rc<Data>,
    pub frequency: TimeFrequency,
    pub display_type: Option<DisplayType>,
}

pub struct CommandDisplay {
    pub data: Rc<Data>,
    pub display_type: Option<DisplayType>,
}

pub enum Command {
    Fig(CommandDisplay),
    Name(Rc<Data>),
    Description(Rc<Data>),
    Span(Rc<Data>),
    Prev(CommandFreq),
    Change(CommandFreq),
    AvgFreq(CommandFreq),
    Col(Vec<Command>),
}

impl TryFrom<Command> for String {
    type Error = Error;

    fn try_from(value: Command) -> Result<Self, Self::Error> {
        match value {
            Command::Fig(command) => Ok(Fig::try_from(command)?.to_string()),
            Command::Name(data) => Ok(data.long_name.clone()),
            Command::Description(data) => Ok(match &data.description {
                Some(description) => description.clone(),
                None => "-".to_string(),
            }),
            Command::Span(data) => Ok(data.span.to_string()),
            Command::Prev(command) => Ok((&command.data.span - command.frequency).to_string()),
            Command::Change(command) => Ok(Change::try_from(command)?.to_string()),
            Command::AvgFreq(command) => Ok(AvgFreq::try_from(command)?.to_string()),
            Command::Col(_commands) => {
                todo!()
            }
        }
    }
}

/// Provides variables that can be added to [Expression]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpressionVariable {
    Command(String),
    TimeFrequency(TimeFrequency),
    Data(Rc<Data>),
    DisplayType(DisplayType),
}

impl ExpressionVariable {
    pub fn try_new(s: &str, date: &NaiveDate) -> Result<Self> {
        if COMMANDS.contains(s) {
            Ok(ExpressionVariable::Command(s.to_string()))
        } else if FREQUENCIES.contains(s) {
            Ok(ExpressionVariable::TimeFrequency(
                s.parse::<TimeFrequency>()?,
            ))
        } else if DISPLAY_TYPES.contains(s) {
            Ok(ExpressionVariable::DisplayType(s.parse::<DisplayType>()?))
        } else if DATA_NAMES.contains(&s.to_string()) {
            Ok(ExpressionVariable::Data(Rc::new(Data::read(
                &s.to_string(),
                date,
            )?)))
        } else {
            return Err(anyhow!("Unknown key word: {}", s));
        }
    }
}

impl AddAssign<ExpressionVariable> for Expression {
    fn add_assign(&mut self, rhs: ExpressionVariable) {
        match rhs {
            ExpressionVariable::Command(command) => self.set_command(command),
            ExpressionVariable::TimeFrequency(frequency) => self.set_frequency(frequency),
            ExpressionVariable::Data(data) => self.set_data(&data),
            ExpressionVariable::DisplayType(display_type) => self.set_display_type(display_type),
        }
    }
}

impl Add<ExpressionVariable> for Expression {
    type Output = Self;

    fn add(self, rhs: ExpressionVariable) -> Self::Output {
        let mut expr = self;
        expr += rhs;
        expr
    }
}

impl Display for ExpressionVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ExpressionVariable::Command(command) => command.fmt(f),
            ExpressionVariable::TimeFrequency(frequency) => frequency.fmt(f),
            ExpressionVariable::Data(data) => write!(f, "{}", data.long_name),
            ExpressionVariable::DisplayType(display_type) => display_type.fmt(f),
        }
    }
}

/// The entire context for an expression.  
/// Contains a collection of command names, and optional values of [TimeFrequency], [Data], and [DisplayType].  
///
/// Values can be filled with either the methods `set_command`, `set_frequency`, `set_data`, `set_display_type`,
///  or by using either the [Add] or [AddAssign] ops with a [ExpressionVariable].
///
/// Convertible to [Command] when required values are filled for the relevant command.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expression {
    pub command: Option<String>,
    pub frequency: Option<TimeFrequency>,
    pub data: Option<Rc<Data>>,
    pub display_type: Option<DisplayType>,
}

impl Expression {
    /// Initializes an empty context.
    pub fn new() -> Self {
        Expression {
            command: None,
            frequency: None,
            data: None,
            display_type: None,
        }
    }
    /// Set the expression's [TimeFrequency].
    pub fn set_frequency(&mut self, frequency: TimeFrequency) {
        self.frequency = Some(frequency);
    }
    /// Set the expression's [Data].
    pub fn set_data(&mut self, data: &Rc<Data>) {
        self.data = Some(Rc::clone(&data));
    }
    /// Set the expression's [DisplayType].
    pub fn set_display_type(&mut self, display_type: DisplayType) {
        self.display_type = Some(display_type);
    }
    /// Set the expression's command name
    pub fn set_command(&mut self, command: String) {
        self.command = Some(command);
    }
}

impl Component for Expression {
    fn render(&mut self, ctx: &Expression) -> Result<String> {
        if let None = self.command {
            if let Some(v) = &ctx.command {
                self.set_command(v.clone())
            }
        }
        if let None = self.frequency {
            if let Some(v) = ctx.frequency {
                self.set_frequency(v)
            }
        }
        if let None = self.data {
            if let Some(v) = &ctx.data {
                self.set_data(&v)
            }
        }
        if let None = self.display_type {
            if let Some(v) = ctx.display_type {
                self.set_display_type(v)
            }
        }

        String::try_from(Command::try_from(&*self)?)
    }
}

impl From<Vec<ExpressionVariable>> for Expression {
    fn from(vars: Vec<ExpressionVariable>) -> Self {
        let mut expr = Expression::new();
        for var in vars {
            expr += var;
        }
        expr
    }
}

impl TryFrom<&Expression> for Command {
    type Error = Error;

    /// Tries to convert [Expression] into [Command] by matching a list of commands.
    fn try_from(value: &Expression) -> Result<Self, Self::Error> {
        if let Some(command) = &value.command {
            match command.as_str() {
                "fig" => Ok(Command::Fig(CommandDisplay::try_from(&*value)?)),
                "name" => Ok(Command::Name(Rc::<Data>::try_from(&*value)?)),
                "description" => Ok(Command::Description(Rc::<Data>::try_from(&*value)?)),
                "span" => Ok(Command::Span(Rc::<Data>::try_from(&*value)?)),
                "prev" => Ok(Command::Prev(CommandFreq::try_from(&*value)?)),
                "change" => Ok(Command::Change(CommandFreq::try_from(&*value)?)),
                "avg_freq" => Ok(Command::AvgFreq(CommandFreq::try_from(&*value)?)),
                command => Err(anyhow!("Unrecognised command: {}", command)),
            }
        } else {
            Err(anyhow!("No command detected for {:#?}", value))
        }
    }
}

impl TryFrom<&Expression> for CommandFreq {
    type Error = Error;

    fn try_from(value: &Expression) -> Result<Self, Self::Error> {
        match value.frequency {
            Some(frequency) => match &value.data {
                Some(data) => Ok(CommandFreq {
                    data: Rc::clone(data),
                    frequency,
                    display_type: value.display_type,
                }),
                None => Err(anyhow!("Data required but not provided for {:#?}", value)),
            },
            None => Err(anyhow!(
                "TimeFrequency required but not provided for {:#?}",
                value
            )),
        }
    }
}

impl TryFrom<&Expression> for CommandDisplay {
    type Error = Error;

    fn try_from(value: &Expression) -> Result<Self, Self::Error> {
        match &value.data {
            Some(data) => Ok(CommandDisplay {
                data: Rc::clone(data),
                display_type: value.display_type,
            }),
            None => Err(anyhow!("Data required but not provided for {:#?}", value)),
        }
    }
}

impl TryFrom<&Expression> for Rc<Data> {
    type Error = Error;

    fn try_from(value: &Expression) -> Result<Self, Self::Error> {
        match &value.data {
            Some(data) => Ok(Rc::clone(&data)),
            None => Err(anyhow!("Data required but not provided for {:#?}", value)),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExpressionNamedCollection {
    command: String,
    collections: HashMap<String, Vec<ExpressionVariable>>,
    ctx: Expression,
}

impl ExpressionNamedCollection {
    pub fn new(
        command: String,
        collections: HashMap<String, Vec<ExpressionVariable>>,
        ctx: Expression,
    ) -> Self {
        ExpressionNamedCollection {
            command,
            collections,
            ctx,
        }
    }
    /// Set the expression's commmand name
    pub fn set_command(&mut self, command: String) {
        self.command = command
    }

    pub fn set_ctx(&mut self, ctx: Expression) {
        self.ctx = ctx;
    }
}

impl Component for ExpressionNamedCollection {
    fn render(&mut self, ctx: &Expression) -> Result<String> {
        self.set_ctx(ctx.clone());
        String::try_from(Table::try_from(&*self)?)
    }
}

impl<'a> TryFrom<&'a ExpressionNamedCollection> for Table<'a> {
    type Error = Error;

    fn try_from(value: &'a ExpressionNamedCollection) -> Result<Self, Self::Error> {
        if let Some(rows) = value.collections.get("rows") {
            if let Some(cols) = value.collections.get("cols") {
                Ok(Table::new(rows.to_vec(), cols.clone(), &value.ctx))
            } else {
                Err(anyhow!("Table missing cols"))
            }
        } else {
            Err(anyhow!("Table missing rows"))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::NaiveDate;

    #[test]
    fn test_expression() {
        let mut expr = Expression::new();

        expr.set_frequency(TimeFrequency::Weekly);
        expr.set_data(&Rc::new(
            Data::read(&"cat_purrs".to_string(), &NaiveDate::from_ymd(2022, 2, 4)).unwrap(),
        ));
        expr.set_command("change".to_string());

        let result = String::try_from(Command::try_from(&expr).unwrap()).unwrap();

        assert_eq!(result, "25.0%".to_string());
    }

    #[test]
    fn test_expression_variable() {
        let mut expr = Expression::new();
        let date = NaiveDate::from_ymd(2022, 2, 4);
        for var in ["Weekly", "change", "cat_purrs", "describedpercentage"] {
            expr += ExpressionVariable::try_new(var, &date).unwrap()
        }
        let result = String::try_from(Command::try_from(&expr).unwrap()).unwrap();

        assert_eq!(result, "up 25.0%".to_string());
    }
}
