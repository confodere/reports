pub mod table;

use crate::pre_process::block::{CommandDisplay, CommandFreq};
use crate::time_span::{TimeFrequency, TimeSpan, TimeSpanIter};
use crate::{DisplayType, Metric, Point};
use anyhow::{anyhow, Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::{self, Display};

pub trait Figure {
    fn fig(&self) -> f64;
    fn display_type(&self) -> DisplayType;

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
            DisplayType::Rounded => write!(f, "{:.1}", self.0.fig()),
            DisplayType::DescribedRounded => {
                let description = if self.0.fig() > 0.0 { "up" } else { "down" };
                write!(f, "{} {:.1}", description, self.0.fig().abs())
            }
            DisplayType::Percentage => write!(f, "{:.1}%", (100.0 * self.0.fig())),
            DisplayType::DescribedPercentage => {
                let description = if self.0.fig() > 0.0 { "up" } else { "down" };
                write!(f, "{} {:.1}%", description, (100.0 * self.0.fig().abs()))
            }
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Change {
    old: f64,
    new: f64,
    span: TimeSpan,
    frequency: TimeFrequency,
    display_type: DisplayType,
}

impl Figure for Change {
    fn fig(&self) -> f64 {
        (self.new - self.old) / self.old
    }

    fn display_type(&self) -> DisplayType {
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
            display_type: match value.display_type {
                Some(v) => v,
                None => DisplayType::Percentage,
            },
        })
    }
}

impl Display for Change {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ShowFigure(self).fmt(f)
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
            display_type: DisplayType::Percentage,
        })
    }
}

#[derive(Serialize, Deserialize)]
pub struct AvgFreq {
    fig: f64,
    span: TimeSpan,
    frequency: TimeFrequency,
    display_type: DisplayType,
}

impl Figure for AvgFreq {
    fn fig(&self) -> f64 {
        self.fig * (self.span.clone() / self.frequency.clone())
    }

    fn display_type(&self) -> DisplayType {
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
            display_type: match value.display_type {
                Some(v) => v,
                None => DisplayType::Rounded,
            },
        })
    }
}

impl Display for AvgFreq {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.display_type() {
            DisplayType::Rounded => write!(f, "{:.1}", self.fig()),
            DisplayType::DescribedRounded => {
                let freq = match self.frequency {
                    TimeFrequency::Yearly => "year",
                    TimeFrequency::Quarterly => "quarter",
                    TimeFrequency::Monthly => "month",
                    TimeFrequency::Weekly => "week",
                    TimeFrequency::Daily => "day",
                };
                write!(f, "{:.1} per {}", self.fig(), freq)
            }
            DisplayType::Percentage => {
                panic!("AvgFreq cannot be represented as a percentage")
            }
            DisplayType::DescribedPercentage => {
                panic!("AvgFreq cannot be represented as a described percentage")
            }
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
                display_type: DisplayType::Rounded,
            })
        } else {
            Err(String::from("Failed to create AvgFreq"))
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct Fig {
    pub fig: f64,
    display_type: DisplayType,
}

impl Fig {
    pub fn new(fig: f64, display_type: Option<DisplayType>) -> Self {
        Fig {
            fig,
            display_type: match display_type {
                Some(display_type) => display_type,
                None => DisplayType::Rounded,
            },
        }
    }
}

impl Figure for Fig {
    fn fig(&self) -> f64 {
        self.fig
    }
    fn display_type(&self) -> DisplayType {
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
