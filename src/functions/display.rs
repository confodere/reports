use anyhow::{anyhow, Error};
use num_format::{SystemLocale, ToFormattedString};
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

/// Formatter for floats to be used for rendering support by other function modules
///
pub struct FloatFormatter<'a> {
    percentage: bool,
    description: &'a Option<Box<dyn Fn((f64, String)) -> String>>,
    precision: Option<usize>,
    num: f64,
}

impl<'a> FloatFormatter<'a> {
    pub fn new(num: f64) -> FloatFormatter<'a> {
        FloatFormatter {
            num,
            ..Default::default()
        }
    }

    pub fn new_percentage(num: f64) -> FloatFormatter<'a> {
        FloatFormatter {
            num,
            percentage: true,
            ..Default::default()
        }
    }

    pub fn new_precision(num: f64, precision: usize) -> FloatFormatter<'a> {
        FloatFormatter {
            precision: Some(precision),
            num,
            ..Default::default()
        }
    }

    pub fn new_all(
        percentage: bool,
        description: &'a Option<Box<dyn Fn((f64, String)) -> String>>,
        precision: Option<usize>,
        num: f64,
    ) -> FloatFormatter<'a> {
        FloatFormatter {
            percentage,
            description,
            precision,
            num,
        }
    }
}

impl Default for FloatFormatter<'_> {
    /// `percentage`: false `description`: none `precision`: Some(0) `num`: 0
    fn default() -> Self {
        FloatFormatter {
            percentage: false,
            description: &None,
            precision: Some(0),
            num: 0.0,
        }
    }
}

impl Display for FloatFormatter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let rendered = if self.percentage {
            if let Some(precision) = self.precision {
                if precision == 0 {
                    let locale = SystemLocale::default().unwrap();
                    format!(
                        "{}%",
                        ((100.0 * self.num).round() as i32).to_formatted_string(&locale)
                    )
                } else {
                    format!("{:.*}%", precision, (100.0 * self.num))
                }
            } else {
                format!("{}%", (100.0 * self.num))
            }
        } else {
            if let Some(precision) = self.precision {
                if precision == 0 {
                    let locale = SystemLocale::default().unwrap();
                    (self.num.round() as i32).to_formatted_string(&locale)
                } else {
                    format!("{:.*}", precision, self.num)
                }
            } else {
                format!("{}", self.num)
            }
        };
        if let Some(description) = &self.description {
            write!(f, "{}", description((self.num, rendered)))
        } else {
            write!(f, "{}", &rendered)
        }
    }
}

fn _rounded(num: f64, precision: usize) -> f64 {
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
    fn test_rounded() {
        assert_eq!(_rounded(1.234, 2), 1.23);
        assert_eq!(_rounded(1.234, 1), 1.2);
        assert_eq!(_rounded(1.234, 0), 1.0);
    }

    #[test]
    fn test_float_formatter() {
        let description: Option<Box<dyn Fn((f64, String)) -> String>> =
            Some(Box::new(|(num, num_print)| {
                let description = if num > 0.0 { "up" } else { "down" };
                format!("{description} {num_print}")
            }));

        let formatter = FloatFormatter::new_all(true, &description, Some(1), 1.2345);
        assert_eq!(formatter.to_string(), "up 123.4%".to_string());
    }

    #[test]
    fn test_formatter_comma() {
        let formatter = FloatFormatter::new_percentage(123.45);
        assert_eq!(formatter.to_string(), "12,345%");

        let formatter = FloatFormatter::new(12345.12);
        assert_eq!(formatter.to_string(), "12,345");
    }
}
