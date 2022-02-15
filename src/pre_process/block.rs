use anyhow::{anyhow, Error, Result};
use chrono::NaiveDate;
use lazy_static::lazy_static;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::ops::{Add, AddAssign};
use std::rc::Rc;

use crate::functions::table::Table;
use crate::functions::{AvgFreq, Change, Fig};
use crate::parser;
use crate::{Data, RenderContext, TimeFrequency};

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
    static ref DISPLAY_TYPES: HashSet<&'static str> =
        ["Words", "Numbers",].iter().cloned().collect();
    static ref DATA_NAMES: HashSet<String> = Data::read_names().unwrap().iter().cloned().collect();
}

pub struct CommandFreq {
    pub data: Rc<Data>,
    pub frequency: TimeFrequency,
    pub display_type: Option<RenderContext>,
}

pub struct CommandDisplay {
    pub data: Rc<Data>,
    pub display_type: Option<RenderContext>,
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
    DataName(String),
    Date(NaiveDate),
    RenderContext(RenderContext),
}

impl ExpressionVariable {
    pub fn try_new(s: &str) -> Result<Self> {
        if COMMANDS.contains(s) {
            Ok(ExpressionVariable::Command(s.to_string()))
        } else if FREQUENCIES.contains(s) {
            Ok(ExpressionVariable::TimeFrequency(
                s.parse::<TimeFrequency>()?,
            ))
        } else if DISPLAY_TYPES.contains(s) {
            Ok(ExpressionVariable::RenderContext(
                s.parse::<RenderContext>()?,
            ))
        } else if DATA_NAMES.contains(&s.to_string()) {
            Ok(ExpressionVariable::DataName(s.to_string()))
        } else {
            if let Ok(date) = parser::parse_date(s) {
                Ok(ExpressionVariable::Date(date))
            } else {
                return Err(anyhow!("Unknown key word: {}", s));
            }
        }
    }
}

impl AddAssign<ExpressionVariable> for Expression {
    fn add_assign(&mut self, rhs: ExpressionVariable) {
        match rhs {
            ExpressionVariable::Command(command) => self.set_command(command),
            ExpressionVariable::TimeFrequency(frequency) => self.set_frequency(frequency),
            ExpressionVariable::DataName(data_name) => self.set_data_name(data_name),
            ExpressionVariable::RenderContext(display_type) => self.set_display_type(display_type),
            ExpressionVariable::Date(date) => self.set_date(date),
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
            ExpressionVariable::DataName(data_name) => data_name.fmt(f),
            ExpressionVariable::RenderContext(display_type) => display_type.fmt(f),
            ExpressionVariable::Date(date) => date.format("%Y-%m-%d").fmt(f),
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
    pub data_name: Option<String>,
    pub display_type: Option<RenderContext>,
    pub date: Option<NaiveDate>,
}

impl Expression {
    /// Initializes an empty context.
    pub fn new() -> Self {
        Expression {
            command: None,
            frequency: None,
            data_name: None,
            display_type: None,
            date: None,
        }
    }
    /// Set the expression's [TimeFrequency].
    pub fn set_frequency(&mut self, frequency: TimeFrequency) {
        self.frequency = Some(frequency);
    }
    /// Set the expression's [Data].
    pub fn set_data_name(&mut self, data_name: String) {
        self.data_name = Some(data_name);
    }
    /// Set the expression's [DisplayType].
    pub fn set_display_type(&mut self, display_type: RenderContext) {
        self.display_type = Some(display_type);
    }
    /// Set the expression's command name
    pub fn set_command(&mut self, command: String) {
        self.command = Some(command);
    }
    /// Set the expression's date
    pub fn set_date(&mut self, date: NaiveDate) {
        self.date = Some(date);
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
        if let None = self.data_name {
            if let Some(v) = &ctx.data_name {
                self.set_data_name(v.clone())
            }
        }
        if let None = self.date {
            if let Some(v) = ctx.date {
                self.set_date(v)
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
            Some(frequency) => match &value.data_name {
                Some(data_name) => match &value.date {
                    Some(date) => Ok(CommandFreq {
                        data: Rc::new(Data::read(data_name, date)?),
                        frequency,
                        display_type: value.display_type,
                    }),
                    None => Err(anyhow!(
                        "Date required for data but not provided for {:#?}",
                        value
                    )),
                },
                None => Err(anyhow!(
                    "Data name required but not provided for {:#?}",
                    value
                )),
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
        match &value.data_name {
            Some(data_name) => match &value.date {
                Some(date) => Ok(CommandDisplay {
                    data: Rc::new(Data::read(data_name, date)?),
                    display_type: value.display_type,
                }),
                None => Err(anyhow!(
                    "Date required for data but not provided for {:#?}",
                    value
                )),
            },
            None => Err(anyhow!("Data required but not provided for {:#?}", value)),
        }
    }
}

impl TryFrom<&Expression> for Rc<Data> {
    type Error = Error;

    fn try_from(value: &Expression) -> Result<Self, Self::Error> {
        match &value.data_name {
            Some(data_name) => match &value.date {
                Some(date) => Ok(Rc::new(Data::read(data_name, date)?)),
                None => Err(anyhow!(
                    "Date required for data but not provided for {:#?}",
                    value
                )),
            },
            None => Err(anyhow!("Data required but not provided for {:#?}", value)),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
        expr.set_data_name("cat_purrs".to_string());
        expr.set_date(NaiveDate::from_ymd(2022, 2, 4));
        expr.set_command("change".to_string());

        let result = String::try_from(Command::try_from(&expr).unwrap()).unwrap();

        assert_eq!(result, "25.0%".to_string());
    }

    #[test]
    fn test_expression_variable() {
        let mut expr = Expression::new();
        let date = NaiveDate::from_ymd(2022, 2, 4);
        expr.set_date(date);
        for var in ["Weekly", "change", "cat_purrs", "Words"] {
            expr += ExpressionVariable::try_new(var).unwrap()
        }
        let result = String::try_from(Command::try_from(&expr).unwrap()).unwrap();

        assert_eq!(result, "up 25.0%".to_string());
    }
}
