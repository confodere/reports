use anyhow::{anyhow, Error};
use serde::{Deserialize, Serialize};
use std::fmt::{self, Display};
use std::str::FromStr;

#[derive(Serialize, Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
pub enum RenderContext {
    Words,
    Numbers,
}

impl Display for RenderContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

impl FromStr for RenderContext {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Words" => Ok(Self::Words),
            "Numbers" => Ok(Self::Numbers),
            _ => Err(anyhow!("{} is not a valid RenderContext", s)),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum DisplayType {
    Rounded(f64, usize),
    Percentage(f64, usize),
    Described(f64, usize),
    DescribedPercentage(f64, usize),
}

impl Display for DisplayType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DisplayType::Rounded(num, precision) => write!(f, "{num:.precision$}"),
            DisplayType::Percentage(num, precision) => {
                write!(f, "{:.precision$}%", (100.0 * num))
            }
            DisplayType::Described(num, precision) => {
                let description = if *num > 0.0 { "up" } else { "down" };
                write!(f, "{description} {:.precision$}", num.abs())
            }
            DisplayType::DescribedPercentage(num, precision) => {
                let description = if *num > 0.0 { "up" } else { "down" };
                write!(f, "{description} {:.precision$}%", (100.0 * num.abs()))
            }
        }
    }
}

pub fn rounded(num: f64, precision: usize) -> f64 {
    if precision == 0 {
        num.round()
    } else {
        (num * 10_i32.pow(precision as u32) as f64).round() / (10_i32.pow(precision as u32) as f64)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display_type() {
        assert_eq!(
            DisplayType::Rounded(1.234, 1).to_string(),
            "1.2".to_string()
        );
        assert_eq!(DisplayType::Rounded(1.234, 0).to_string(), "1".to_string());
    }

    #[test]
    fn test_rounded() {
        assert_eq!(rounded(1.234, 2), 1.23);
        assert_eq!(rounded(1.234, 1), 1.2);
        assert_eq!(rounded(1.234, 0), 1.0);
    }
}
