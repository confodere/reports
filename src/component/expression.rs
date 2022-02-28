use super::{arg::Arg, Component};
use crate::functions::{AvgFreq, Change, Fig};
use crate::{Data, RenderContext, TimeFrequency};
use anyhow::{anyhow, Error, Result};
use chrono::NaiveDate;

pub struct CommandFreq {
    pub data: Data,
    pub frequency: TimeFrequency,
    pub display_type: RenderContext,
}

pub struct CommandDisplay {
    pub data: Data,
    pub display_type: RenderContext,
}

impl CommandDisplay {
    pub fn try_new(data_name: &str, date: NaiveDate, display_type: RenderContext) -> Result<Self> {
        Ok(CommandDisplay {
            data: Data::read(&data_name.to_string(), &date)?,
            display_type,
        })
    }

    pub fn to_command_freq(&self, frequency: TimeFrequency) -> CommandFreq {
        CommandFreq {
            data: self.data.clone(),
            frequency,
            display_type: self.display_type,
        }
    }
}

pub enum Command {
    Fig(CommandDisplay),
    Name(Data),
    Description(Data),
    Span(Data),
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

    pub fn fill_blank(&mut self, other: Expression) {
        self.command = self.command.take().or(other.command);
        self.frequency = self.frequency.take().or(other.frequency);
        self.data_name = self.data_name.take().or(other.data_name);
        self.date = self.date.take().or(other.date);
        self.display_type = self.display_type.take().or(other.display_type);
    }

    pub fn fill_arg(&mut self, arg: Arg) {
        match arg {
            Arg::Command(s) => self.command = self.command.take().or(Some(s.to_string())),
            Arg::TimeFrequency(frequency) => {
                self.frequency = self.frequency.take().or(Some(frequency))
            }
            Arg::DataName(s) => self.data_name = self.data_name.take().or(Some(s.to_string())),
            Arg::Date(date) => self.date = self.date.take().or(Some(date)),
            Arg::RenderContext(rc) => self.display_type = self.display_type.take().or(Some(rc)),
            _ => panic!("{arg:?} cannot be filled into an Expression"),
        }
    }
}

impl Component for Expression {
    fn render(&self, ctx: &Expression) -> Result<String> {
        let mut expr = self.clone();
        expr.fill_blank(ctx.to_owned());

        String::try_from(Command::try_from(&expr)?)
    }
}

impl From<Vec<Arg>> for Expression {
    fn from(args: Vec<Arg>) -> Self {
        args.into_iter().fold(Expression::new(), |mut expr, arg| {
            expr.fill_arg(arg);
            expr
        })
    }
}

impl TryFrom<&Expression> for Command {
    type Error = Error;

    /// Tries to convert [Expression] into [Command] by matching a list of commands.
    fn try_from(value: &Expression) -> Result<Self, Self::Error> {
        if let Some(command) = &value.command {
            match command.as_str() {
                "fig" => Ok(Command::Fig(CommandDisplay::try_from(&*value)?)),
                "name" => Ok(Command::Name(Data::try_from(&*value)?)),
                "description" => Ok(Command::Description(Data::try_from(&*value)?)),
                "span" => Ok(Command::Span(Data::try_from(&*value)?)),
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

    fn try_from(expr: &Expression) -> Result<Self, Self::Error> {
        let command_display = CommandDisplay::try_from(expr)?;
        let frequency = expr
            .frequency
            .ok_or_else(|| anyhow!("TimeFrequency required but not provided for {:#?}", expr))?;

        Ok(command_display.to_command_freq(frequency))
    }
}

impl TryFrom<&Expression> for CommandDisplay {
    type Error = Error;

    fn try_from(expr: &Expression) -> Result<Self, Self::Error> {
        let data = Data::try_from(expr)?;

        let display_type = expr
            .display_type
            .ok_or_else(|| anyhow!("Render Context required but not provided for {:#?}", expr))?;

        Ok(CommandDisplay { data, display_type })
    }
}

impl TryFrom<&Expression> for Data {
    type Error = Error;

    fn try_from(expr: &Expression) -> Result<Self, Self::Error> {
        let date = expr
            .date
            .ok_or_else(|| anyhow!("Date required but not provided for {:#?}", expr))?;
        let data_name = expr
            .data_name
            .clone()
            .ok_or_else(|| anyhow!("Data name required but not provided for {:#?}", expr))?;

        let data = Data::read(&data_name, &date)?;

        Ok(data)
    }
}
