use crate::{functions::display::RenderContext, Data, TimeFrequency};
use anyhow::{anyhow, Result};
use std::collections::HashMap;
use thiserror::Error;

use super::expression::Expression;

#[derive(Error, Debug)]
pub enum InfoMissing {
    #[error("TimeFrequency missing")]
    TimeFrequency,
    #[error("RenderContext missing")]
    RenderContext,
}

pub trait Calculate {
    fn calculate<'fo: 'c, 'c>(&self, info: &'fo Info<'c>) -> Result<String>;
}

/// Implementation for bare functions
impl<F: Fn(&Info<'_>) -> Result<String>> Calculate for F {
    fn calculate<'fo: 'c, 'c>(&self, info: &'fo Info<'c>) -> Result<String> {
        self(info)
    }
}

pub struct Info<'c> {
    pub data: Data,
    pub frequency: Option<&'c TimeFrequency>,
    pub render_context: Option<&'c RenderContext>,
}

impl<'c> TryFrom<&'c Expression> for Info<'c> {
    type Error = anyhow::Error;

    fn try_from(expr: &'c Expression) -> Result<Self, Self::Error> {
        let data = Data::try_from(expr)?;

        Ok(Info {
            data,
            frequency: expr.frequency.as_ref(),
            render_context: expr.display_type.as_ref(),
        })
    }
}

pub struct State<'state> {
    calculators: HashMap<&'state str, Box<dyn Calculate>>,
}

impl<'state> State<'state> {
    pub fn new() -> State<'state> {
        State {
            calculators: HashMap::new(),
        }
    }

    pub fn register_calculator(&mut self, name: &'state str, calculator: Box<dyn Calculate>) {
        self.calculators.insert(name, calculator);
    }

    pub fn calculate<'c>(&self, keyword: &str, info: &Info<'c>) -> Result<String> {
        if let Some(val) = self.calculators.get(keyword) {
            val.calculate(info)
        } else {
            Err(anyhow!("Calculation handler: {} missing", keyword))
        }
    }
}

mod getters {
    use crate::functions::Figure;
    use crate::time_span::TimeSpanIter;
    use crate::Data;
    use crate::TimeFrequency;
    use crate::TimeSpan;
    use anyhow::{anyhow, Result};

    pub trait Getter<Output = [f64; 2]> {
        fn get(&self, data: &Data) -> Result<Output>;
    }

    pub struct TrailingGap(pub TimeFrequency);

    impl Getter for TrailingGap {
        fn get(&self, data: &Data) -> Result<[f64; 2]> {
            let points = data.read_points((&data.span - self.0).start(), data.span.end())?;
            let vals: [f64; 2] = TimeSpanIter::new(data.span, self.0, 2)
                .map(|span| {
                    if let Some(point) = points.get(&span) {
                        Ok(point.fig())
                    } else {
                        Err(anyhow!(
                            "Couldn't find datapoint for {} from span: {:?} in {:?}",
                            &data.name,
                            &data.span,
                            &points
                        ))
                    }
                })
                .collect::<Result<Vec<_>>>()?
                .try_into()
                .expect("Getter matched too many values");
            Ok(vals)
        }
    }

    pub struct ComparePrevious;

    impl Getter for ComparePrevious {
        fn get(&self, data: &Data) -> Result<[f64; 2]> {
            let spans: [TimeSpan; 2] = TimeSpanIter::new(data.span, data.span.freq(), 2)
                .collect::<Vec<_>>()
                .try_into()
                .expect("Getter compare previous matched too many values");
            let points = data.read_points(&spans[1].start(), spans[0].end())?;
            let points: [&crate::Point; 2] = spans
                .into_iter()
                .map(|span| {
                    points
                        .get(&span)
                        .ok_or_else(|| anyhow!("Missing data in Compare Previous"))
                })
                .collect::<Result<Vec<_>>>()?
                .try_into()
                .expect("Point error");
            Ok([points[1].fig(), points[0].fig()])
        }
    }

    pub struct Point;
    impl Getter<f64> for Point {
        fn get(&self, data: &Data) -> Result<f64> {
            Ok(data.read_point()?.fig())
        }
    }
}

mod calculators {
    use crate::{TimeFrequency, TimeSpan};

    pub fn change(new: f64, old: f64) -> f64 {
        (new - old) / old
    }

    pub fn avg_freq(fig: f64, span: TimeSpan, freq: TimeFrequency) -> f64 {
        fig * (span / freq)
    }
}

pub mod helpers {
    use super::{
        calculators,
        getters::{Getter, Point, TrailingGap},
        Calculate, Info, InfoMissing,
    };
    use crate::{
        component::expression::CommandDisplay,
        functions::{display::FloatFormatter, Fig},
    };
    use anyhow::Result;

    pub struct ChangeFromFreq {
        pub description: Option<Box<dyn Fn((f64, String)) -> String>>,
    }

    impl Calculate for ChangeFromFreq {
        fn calculate<'fo: 'c, 'c>(&self, info: &'fo Info<'c>) -> Result<String> {
            let [new, old] =
                TrailingGap(*info.frequency.ok_or_else(|| InfoMissing::TimeFrequency)?)
                    .get(&info.data)?;

            Ok(FloatFormatter::new_all(
                true,
                {
                    match *info
                        .render_context
                        .ok_or_else(|| InfoMissing::RenderContext)?
                    {
                        crate::functions::display::RenderContext::Words => &self.description,
                        crate::functions::display::RenderContext::Numbers => &None,
                    }
                },
                Some(1),
                calculators::change(new, old),
            )
            .to_string())
        }
    }

    pub struct AvgFreq;

    impl Calculate for AvgFreq {
        fn calculate<'fo: 'c, 'c>(&self, info: &'fo Info<'c>) -> Result<String> {
            let val = Point.get(&info.data)?;
            Ok(calculators::avg_freq(
                val,
                info.data.span,
                *info.frequency.ok_or_else(|| InfoMissing::TimeFrequency)?,
            )
            .to_string())
        }
    }

    pub struct Name;

    impl Calculate for Name {
        fn calculate<'fo: 'c, 'c>(&self, info: &'fo Info<'c>) -> Result<String> {
            Ok(info.data.long_name.clone())
        }
    }

    pub fn description(info: &Info<'_>) -> Result<String> {
        if let Some(description) = &info.data.description {
            Ok(description.clone())
        } else {
            Ok("-".to_string())
        }
    }

    pub fn span(info: &Info<'_>) -> Result<String> {
        Ok(info.data.span.to_string())
    }

    pub fn prev_freq(info: &Info<'_>) -> Result<String> {
        Ok(
            (&info.data.span - *info.frequency.ok_or_else(|| InfoMissing::TimeFrequency)?)
                .to_string(),
        )
    }

    pub fn fig(info: &Info<'_>) -> Result<String> {
        Ok(Fig::try_from(CommandDisplay {
            data: info.data.clone(),
            display_type: *info
                .render_context
                .ok_or_else(|| InfoMissing::RenderContext)?,
        })?
        .to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::helpers::ChangeFromFreq;
    use super::*;
    use crate::functions::display::RenderContext;
    use crate::Data;
    use chrono::NaiveDate;

    fn data() -> Data {
        Data::read(&"cat_purrs".to_string(), &NaiveDate::from_ymd(2022, 02, 04)).unwrap()
    }
    fn info<'a>() -> Info<'a> {
        Info {
            data: data(),
            frequency: Some(&TimeFrequency::Weekly),
            render_context: Some(&RenderContext::Words),
        }
    }
    fn change_from_freq() -> ChangeFromFreq {
        ChangeFromFreq {
            description: Some(Box::new(|(num, num_print)| {
                let description = if num > 0.0 { "up" } else { "down" };
                let num_print = if let Some(val) = num_print.strip_prefix("-") {
                    val
                } else {
                    num_print.as_str()
                };
                format!("{description} {num_print}")
            })),
        }
    }

    #[test]
    fn test_diff() {
        let diff = change_from_freq().calculate(&info());
        assert_eq!(diff.unwrap(), "up 25.0%");
    }

    #[test]
    fn test_name() {
        assert_eq!(helpers::Name.calculate(&info()).unwrap(), "Cat Purrs");
    }

    #[test]
    fn test_calculate() {
        let mut state = State {
            calculators: HashMap::new(),
        };
        state
            .calculators
            .insert("change", Box::new(change_from_freq()));

        let keyword = "change";

        if let Some(val) = state.calculators.get(keyword) {
            assert_eq!(val.calculate(&info()).unwrap(), "up 25.0%");
        } else {
            panic!("Missing calculator handler");
        }
    }

    #[test]
    fn test_state() {
        let mut state = State::new();
        state.register_calculator("change", Box::new(change_from_freq()));
        assert_eq!(state.calculate("change", &info()).unwrap(), "up 25.0%");

        state.register_calculator("change", Box::new(ChangeFromFreq { description: None }));
        assert_eq!(state.calculate("change", &info()).unwrap(), "25.0%");
    }

    #[test]
    fn test_stateless_calculator() {
        let mut state = State::new();
        state.register_calculator("description", Box::new(helpers::description));
        state.register_calculator("span", Box::new(helpers::span));

        assert_eq!(
            state.calculate("description", &info()).unwrap(),
            "A measure of the number of times my cat purred"
        );

        assert_eq!(
            state.calculate("span", &info()).unwrap(),
            "2022 (31st January to 6th February)"
        )
    }
}
