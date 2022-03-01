pub mod display;
pub mod table;

use self::display::FloatFormatter;
use crate::component::expression::{CommandDisplay, CommandFreq};
use crate::time_span::{TimeFrequency, TimeSpan, TimeSpanIter};
use crate::{Metric, Point};
use anyhow::{anyhow, Error, Result};
use display::RenderContext;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::{self, Display};

pub trait Figure {
    fn fig(&self) -> f64;
    fn display_type(&self) -> RenderContext;

    fn from_inside(
        points: HashMap<TimeSpan, Point>,
        span: TimeSpan,
        frequency: TimeFrequency,
        depth: i32,
    ) -> Result<Vec<f64>> {
        TimeSpanIter::new(span, frequency, depth)
            .map(|span| {
                if let Some(point) = points.get(&span) {
                    Ok(point.fig())
                } else {
                    return Err(anyhow!(
                        "Couldn't find datapoint from span: {:#?} in {:#?}",
                        span,
                        points
                    ));
                }
            })
            .collect::<Result<Vec<_>>>()
    }
}

pub struct ShowFigure<F>(pub F);

impl<T: Figure> Display for ShowFigure<&T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0.display_type() {
            RenderContext::Words => FloatFormatter::new(self.0.fig()).fmt(f),
            RenderContext::Numbers => FloatFormatter::new(self.0.fig()).fmt(f),
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Change {
    old: f64,
    new: f64,
    span: TimeSpan,
    frequency: TimeFrequency,
    display_type: RenderContext,
}

impl Figure for Change {
    fn fig(&self) -> f64 {
        (self.new - self.old) / self.old
    }

    fn display_type(&self) -> RenderContext {
        self.display_type
    }
}

impl TryFrom<CommandFreq> for Change {
    type Error = Error;

    /// Tries to convert CommandFreq into Change
    /// Defaults display_type to DisplayType::Percentage if None
    fn try_from(value: CommandFreq) -> Result<Self, Self::Error> {
        let points = value.data.read_points(
            (&value.data.span - value.frequency).start(),
            value.data.span.end(),
        )?;
        let vals = Change::from_inside(points, value.data.span, value.frequency, 2)?;

        Ok(Change {
            old: vals[1],
            new: vals[0],
            span: value.data.span,
            frequency: value.frequency,
            display_type: value.display_type,
        })
    }
}

impl Display for Change {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let description: Option<Box<dyn Fn((f64, String)) -> String>> =
            Some(Box::new(|(num, num_print)| {
                let description = if num > 0.0 { "up" } else { "down" };
                let num_print = if let Some(val) = num_print.strip_prefix("-") {
                    val
                } else {
                    num_print.as_str()
                };
                format!("{description} {num_print}")
            }));

        let formatter = match self.display_type() {
            RenderContext::Numbers => FloatFormatter::new_all(true, &None, Some(1), self.fig()),
            RenderContext::Words => {
                FloatFormatter::new_all(true, &description, Some(1), self.fig())
            }
        };
        formatter.fmt(f)
    }
}

impl<'a> Change {
    pub fn from(metric: &Metric) -> Result<Change> {
        let datapoints = metric
            .data
            .read_points(
                (&metric.data.span - metric.frequency).start(),
                metric.data.span.end(),
            )
            .expect("Couldn't read underlying datapoint");

        let vals = Change::from_inside(datapoints, metric.data.span, metric.frequency, 2)?;

        Ok(Change {
            old: vals[1],
            new: vals[0],
            span: metric.data.span,
            frequency: metric.frequency,
            display_type: RenderContext::Numbers,
        })
    }
}

#[derive(Serialize, Deserialize)]
pub struct AvgFreq {
    fig: f64,
    span: TimeSpan,
    frequency: TimeFrequency,
    display_type: RenderContext,
}

impl Figure for AvgFreq {
    fn fig(&self) -> f64 {
        self.fig * (self.span.clone() / self.frequency.clone())
    }

    fn display_type(&self) -> RenderContext {
        self.display_type
    }
}

impl TryFrom<CommandFreq> for AvgFreq {
    type Error = Error;

    /// Tries to convert CommandFreq into AvgFreq
    /// Defaults display_type to DisplayType::Rounded if None
    fn try_from(value: CommandFreq) -> Result<Self, Self::Error> {
        let point = value.data.read_point()?;
        Ok(AvgFreq {
            fig: point.fig(),
            span: value.data.span,
            frequency: value.frequency,
            display_type: value.display_type,
        })
    }
}

impl Display for AvgFreq {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.display_type() {
            RenderContext::Words => {
                let freq = match self.frequency {
                    TimeFrequency::Yearly => "year",
                    TimeFrequency::Quarterly => "quarter",
                    TimeFrequency::Monthly => "month",
                    TimeFrequency::Weekly => "week",
                    TimeFrequency::Daily => "day",
                };
                write!(
                    f,
                    "{} per {}",
                    FloatFormatter::new_precision(self.fig(), 1).to_string(),
                    freq
                )
            }
            RenderContext::Numbers => FloatFormatter::new_precision(self.fig(), 1).fmt(f),
        }
    }
}

impl AvgFreq {
    pub fn from(metric: &Metric) -> Result<AvgFreq, String> {
        let datapoints = metric
            .data
            .read_points(metric.data.span.start(), metric.data.span.end())
            .expect("Failed to read datapoints for AvgFreq");

        if let Some(point) = datapoints.get(&metric.data.span) {
            Ok(AvgFreq {
                fig: point.fig(),
                span: metric.data.span,
                frequency: metric.frequency,
                display_type: RenderContext::Numbers,
            })
        } else {
            Err(String::from("Failed to create AvgFreq"))
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct Fig {
    pub fig: f64,
    display_type: RenderContext,
}

impl Fig {
    pub fn new(fig: f64, display_type: RenderContext) -> Self {
        Fig { fig, display_type }
    }
}

impl Figure for Fig {
    fn fig(&self) -> f64 {
        self.fig
    }
    fn display_type(&self) -> RenderContext {
        self.display_type
    }
}

impl TryFrom<CommandDisplay> for Fig {
    type Error = Error;

    fn try_from(value: CommandDisplay) -> Result<Self, Self::Error> {
        Ok(Fig::new(value.data.read_point()?.fig(), value.display_type))
    }
}

impl Display for Fig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ShowFigure(self).fmt(f)
    }
}
