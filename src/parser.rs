use std::str::FromStr;

use crate::{AvgFreq, Change, Data, DisplayType, Figure, Metric, TimeFrequency};
use anyhow::{anyhow, Result};
use chrono::NaiveDate;
use nom::{
    bytes::complete::{tag, take, take_until},
    character::complete::multispace0,
    combinator::opt,
    error::Error,
    error::{ErrorKind, ParseError},
    multi::separated_list0,
    sequence::{pair, terminated, tuple},
    Err, FindSubstring, IResult, InputLength, Parser,
};
use std::iter;

/// Opening tag for a block element
fn opening(i: &str) -> IResult<&str, (&str, Option<&str>, &str, &str)> {
    tuple((tag("{{#"), opt(multispace0), take_until("}}"), tag("}}")))(i)
}

/// Closing tag for a block element
fn ending(i: &str) -> IResult<&str, &str> {
    terminated(take_until("{{/#}}"), tag("{{/#}}"))(i)
}

/// Block constructor nom parser
fn tag_block(i: &str) -> IResult<&str, (&str, &str)> {
    let (remainder, (_, _, args, _)) = opening(i)?;
    let (remainder, template) = ending(remainder)?;
    Ok((remainder, (args, template)))
}

fn tag_exp(i: &str) -> IResult<&str, (&str, &str)> {
    let (remainder, (_, _, args, junk)) =
        tuple((tag("{{"), opt(multispace0), take_until("}}"), tag("}}")))(i)?;
    Ok((remainder, (args, junk)))
}

/// Finds the earliest occurance of any keywords and calls take until that occurance
/// meant to allow skipping over junk when parsing by taking until the next key element
pub fn either<'a>(i: &'a str, keywords: Vec<&str>) -> IResult<&'a str, &'a str> {
    let mut first: Option<usize> = None;
    for opt in keywords {
        if let Some(num) = i.find_substring(opt) {
            first = Some(match first {
                Some(first) if first < num => first,
                _ => num,
            })
        }
    }

    if let Some(first) = first {
        if first == 0 {
            Ok((i, ""))
        } else {
            take(first)(i)
        }
    } else {
        Ok(("", i))
    }
}

pub fn either_block(i: &str) -> IResult<&str, &str> {
    take_until("{{#")(i)
}

pub fn either_exp(i: &str) -> IResult<&str, &str> {
    either(i, vec![" ", "}}"])
}

pub fn either_long_text(i: &str) -> IResult<&str, &str> {
    either(
        i,
        vec![
            "fig", "prev", "freq", "calc", "name", "Name", "desc", "span",
        ],
    )
}

pub fn find_blocks<'a>(
    i: &'a str,
) -> Result<Option<(&'a str, (&'a str, Vec<((&'a str, &'a str), usize, usize)>))>> {
    match pair(take_until("{{#"), alternate(take_until("{{#"), tag_block))(i) {
        Ok(i) => Ok(Some(i)),
        Err(Err::Error(_)) => Ok(None),
        Err(e) => Err(e.map(|e| Error::new(e.input.to_string(), e.code)).into()),
    }
}

pub fn find_expressions<'a>(
    i: &'a str,
) -> Result<Option<(&'a str, (&'a str, Vec<((&'a str, &'a str), usize, usize)>))>> {
    match pair(take_until("{{"), alternate(take_until("{{"), tag_exp))(i) {
        Ok(i) => Ok(Some(i)),
        Err(Err::Error(_)) => Ok(None),
        Err(e) => Err(e.map(|e| Error::new(e.input.to_string(), e.code)).into()),
    }
}

fn var_list(i: &str) -> IResult<&str, Vec<&str>> {
    separated_list0(tag(" "), either_exp)(i)
}

pub fn key_var_list(i: &str) -> Result<(&str, Vec<&str>)> {
    match var_list(i) {
        Ok((_, mut i)) => {
            if i.len() > 0 {
                Ok((i.remove(0), i))
            } else {
                Err(anyhow!("{{{{#Block}}}} missing an identifier"))
            }
        }
        Err(e) => Err(e.map(|e| Error::new(e.input.to_string(), e.code)).into()),
    }
}

/// Represents a parsed block that can be used to construct a Metric,
/// and tracks where in a string the block originally came from.
#[derive(Debug, PartialEq, Eq)]
pub struct Block<'a> {
    pub block_name: &'a str,
    pub vars: Vec<&'a str>,
    pub template: String,
    pub start: usize,
    pub end: usize,
}

impl Substitute for Block<'_> {
    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }

    fn text(&self) -> Result<String> {
        Ok(self.template.clone())
    }
}

impl Default for Block<'_> {
    fn default() -> Self {
        Self {
            block_name: "Change",
            vars: vec!["Weekly"],
            template: "Weekly change was {{fig}}.".to_string(),
            start: 0,
            end: 54,
        }
    }
}

impl Block<'_> {
    pub fn to_metric(&self, date: &NaiveDate) -> Result<Metric> {
        if let [data_name, frequency] = &self.vars[..] {
            let frequency = frequency.parse::<TimeFrequency>()?;
            let data = Data::read(&data_name.to_string(), date)?;

            Ok(Metric::new(
                data,
                self.template.to_string(),
                frequency,
                self.block_name.to_string(),
            ))
        } else {
            Err(anyhow!(
                "Block attribute error with attributes: {:#?}",
                self.vars
            ))
        }
    }
}

#[derive(Debug, Default)]
pub struct SimpleExpression {
    pub keyword: String,
}

impl SimpleExpression {
    fn to_substitution_content(&self, data: &Data) -> Result<String> {
        data.get_string(self.keyword.clone())
    }
}

#[derive(Debug, Default)]
pub struct FigureExpression {
    pub calculation_type: String,
    pub frequency: TimeFrequency,
    pub context: Option<DisplayType>,
}

impl FigureExpression {
    fn to_substitution_content(&self, data: &Data) -> Result<String> {
        match self.calculation_type.as_str() {
            "Change" => Ok(Change::from_figure_expresssion(&self, data)?.render(
                if let Some(ctx) = self.context {
                    ctx
                } else {
                    DisplayType::DescribedPercentage
                },
            )),
            "AvgFreq" => Ok(AvgFreq::from_figure_expresssion(&self, data)?.render(
                if let Some(ctx) = self.context {
                    ctx
                } else {
                    DisplayType::DescribedRounded
                },
            )),
            _ => {
                return Err(anyhow!(
                    "Invalid calculation_type: {}",
                    self.calculation_type
                ))
            }
        }
    }
}

#[derive(Debug)]
pub enum ExpressionType {
    Simple(SimpleExpression),
    Figure(FigureExpression),
    Prev(TimeFrequency),
}

impl Default for ExpressionType {
    fn default() -> Self {
        ExpressionType::Simple(SimpleExpression {
            ..Default::default()
        })
    }
}

fn match_vars<'a>(
    vars: impl Iterator<Item = &'a str>,
) -> Result<(Option<&'a str>, Option<TimeFrequency>, Option<DisplayType>)> {
    let (mut calculation_type, mut frequency, mut context) = (None, None, None);
    for v in vars {
        match v {
            "Change" | "AvgFreq" => calculation_type = Some(v),
            "Yearly" | "Quarterly" | "Monthly" | "Weekly" | "Daily" => {
                frequency = Some(v.parse::<TimeFrequency>()?)
            }
            "Rounded" | "Percentage" | "DescribedPercentage" => {
                context = Some(v.parse::<DisplayType>()?)
            }
            _ => return Err(anyhow!("Couldn't match variable: {}", v)),
        }
    }
    Ok((calculation_type, frequency, context))
}

impl FromStr for ExpressionType {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (keyword, vars) = key_var_list(s)?;
        let exp = match keyword {
            "fig" | "name" | "Name" | "desc" | "span" => ExpressionType::Simple(SimpleExpression {
                keyword: keyword.to_string(),
            }),
            "prev" => {
                let (_, frequency, _) = match_vars(vars.into_iter())?;
                if let Some(frequency) = frequency {
                    ExpressionType::Prev(frequency)
                } else {
                    return Err(anyhow!("Missing frequency from PrevExpression"));
                }
            }
            keyword => {
                let iter = iter::once(keyword).chain(vars.into_iter());
                let (calculation_type, frequency, context) = match_vars(iter)?;

                if let Some(calculation_type) = calculation_type {
                    if let Some(frequency) = frequency {
                        ExpressionType::Figure(FigureExpression {
                            calculation_type: calculation_type.to_string(),
                            frequency,
                            context,
                        })
                    } else {
                        return Err(anyhow!("Missing frequency from FigureExpression"));
                    }
                } else {
                    return Err(anyhow!("Missing calculation_type from FigureExpression"));
                }
            }
        };
        Ok(exp)
    }
}

#[derive(Debug)]
pub struct Expression<'a> {
    pub vars: ExpressionType,
    pub start: usize,
    pub end: usize,
    pub data: &'a Data,
}

impl Substitute for Expression<'_> {
    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }

    fn text(&self) -> Result<String> {
        match &self.vars {
            ExpressionType::Simple(i) => i.to_substitution_content(self.data),
            ExpressionType::Figure(i) => i.to_substitution_content(self.data),
            ExpressionType::Prev(freq) => Ok((&self.data.span - *freq).to_string()),
        }
    }
}

pub trait Substitute {
    fn start(&self) -> usize;
    fn end(&self) -> usize;
    fn text(&self) -> Result<String>;
}

pub struct BasicSubstitute {
    pub start: usize,
    pub end: usize,
    pub text: String,
}

impl Substitute for BasicSubstitute {
    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }

    fn text(&self) -> Result<String> {
        Ok(self.text.clone())
    }
}

pub struct Substitution<'a> {
    pub template: &'a mut String,
    pub added: i32,
}

impl Substitution<'_> {
    pub fn substitute(&mut self, sub: &impl Substitute) -> Result<()> {
        let prev_len = self.template.len();

        let (start, end) = (
            (sub.start() as i32 + self.added) as usize,
            (sub.end() as i32 + self.added) as usize,
        );

        self.template.replace_range(start..end, &sub.text()?);
        self.added += self.template.len() as i32 - prev_len as i32;

        Ok(())
    }
}

/// Alternate between two parsers to produce a list of elements matching the second parser
/// , but allows a zero width separator
/// as well as returning the start and end positions of the element
///
/// An adjusted form of nom's `separated_list0`
///
/// # Arguments
/// * `sep` Parses the separator
/// * `f` Parses the elements
///
/// ```rust
/// use reports::parser::{alternate, either_block};
/// use nom::{Err, error::ErrorKind, Needed, IResult};
/// use nom::bytes::complete::tag;
///
/// fn parser(s: &str) -> IResult<&str, Vec<(&str, usize, usize)>> {
///     alternate(either_block, tag("{{#Change}}"))(s)
/// }
///
/// assert_eq!(parser("{{#Change}}junk{{/#}}"), Ok(("junk{{/#}}", vec![("{{#Change}}", 0, 11)])));
/// assert_eq!(parser("{{#Change}}junk{{#Change}}more junk{{#Change}}{{/Change}}"), Ok(("{{/Change}}", vec![("{{#Change}}", 0, 11), ("{{#Change}}", 15, 26), ("{{#Change}}", 35, 46)])));
/// ```
pub fn alternate<I, O, O2, E, F, G>(
    mut sep: G,
    mut f: F,
) -> impl FnMut(I) -> IResult<I, Vec<(O, usize, usize)>, E>
where
    I: Clone + InputLength,
    F: Parser<I, O, E>,
    G: Parser<I, O2, E>,
    E: ParseError<I> + core::fmt::Debug,
{
    move |mut i: I| {
        let mut res = Vec::new();
        let mut junk_count: usize = 0;

        loop {
            let len = i.input_len();
            match f.parse(i.clone()) {
                Err(Err::Error(_)) => return Ok((i, res)),
                Err(e) => return Err(e),
                Ok((i1, o)) => {
                    res.push((o, junk_count, junk_count + len - i1.input_len()));
                    match sep.parse(i1.clone()) {
                        Err(Err::Error(_)) => return Ok((i1, res)),
                        Err(e) => return Err(e),
                        Ok((i2, _)) => {
                            if i2.input_len() == len {
                                return Err(nom::Err::Error(E::from_error_kind(
                                    i2,
                                    ErrorKind::SeparatedList,
                                )));
                            }
                            junk_count += len - i2.input_len();
                            //res.push(o);
                            i = i2;
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_nom() {
        assert_eq!(
            opening("{{#Change}}|{{/}}"),
            Ok(("|{{/}}", ("{{#", Some(""), "Change", "}}")))
        );
    }

    #[test]
    fn test_ending() {
        assert_eq!(
            ending(
                "{{span}}
        {{/#}}"
            ),
            Ok((
                "",
                "{{span}}
        "
            ))
        );
    }

    #[test]
    #[should_panic]
    fn test_junk() {
        assert_eq!(
            tag_block("junk {{Change}}{{/Change}}"),
            Ok(("junk {{Change}}{{/Change}}", ("", "")))
        );
    }

    #[test]
    fn test_either() {
        assert_eq!(
            either_block("junk {{#Change}} junk rand {{AvgFreq}}{{/#}}"),
            Ok(("{{#Change}} junk rand {{AvgFreq}}{{/#}}", "junk "))
        );
    }

    #[test]
    fn test_var_list() {
        assert_eq!(either_exp("abc bcd dcf"), Ok((" bcd dcf", "abc")));
        assert_eq!(var_list("abc bcd dcf"), Ok(("", vec!["abc", "bcd", "dcf"])));
    }

    #[test]
    fn test_find_blocks() {
        assert_eq!(
            find_blocks("{{Change Weekly}}Weekly change was {{fig}}.{{/Change}}").unwrap(),
            None
        );

        assert_eq!(
            find_blocks("{{#Change Weekly}}Weekly change was {{fig}}.{{/#}} junk {{#Change Daily}}Daily change of {{fig}}.{{/Change}} more junk {{#AvgFreq}} compared to last month where {{fig}} was observed.{{/#}} final junk").unwrap(),
            Some((" final junk", ("", vec![(("Change Weekly", "Weekly change was {{fig}}."), 0, 50), (("Change Daily", "Daily change of {{fig}}.{{/Change}} more junk {{#AvgFreq}} compared to last month where {{fig}} was observed."), 56, 188)])))
            );

        assert_eq!(
            find_blocks("junk {{# Change}}{{/#}}").unwrap(),
            Some(("", ("junk ", vec![(("Change", ""), 0, 18)])))
        );

        assert_eq!(find_blocks("just just").unwrap(), None);
    }
}
