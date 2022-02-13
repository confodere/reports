use crate::pre_process::block::ExpressionVariable;
use crate::pre_process::tree::{Component, Node};
use crate::{AvgFreq, Change, Data, DisplayType, Metric, TimeFrequency};
use anyhow::{anyhow, Result};
use chrono::NaiveDate;
use nom::combinator::map_res;
use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_until},
    character::complete::{alphanumeric1, multispace0, multispace1},
    combinator::{consumed, opt},
    error::Error,
    error::{ErrorKind, ParseError},
    multi::{many0, separated_list0, separated_list1},
    sequence::{delimited, pair, separated_pair, terminated, tuple},
    AsChar, Err, FindSubstring, IResult, InputLength, InputTakeAtPosition, Parser,
};
use std::collections::HashMap;
use std::iter;
use std::ops::{Deref, DerefMut, Range};
use std::rc::Rc;
use std::str::FromStr;

pub mod substitute;

/// Matches space separated alphanumeric arguments.
///
/// # Example
/// ```
/// use nom::error::{Error, ErrorKind};
/// use nom::Err;
/// use reports::parser::arg_list;
/// assert_eq!(arg_list("one"), Ok(("", vec!["one"])));
/// assert_eq!(arg_list("one two three"), Ok(("", vec!["one", "two", "three"])));
/// assert_eq!(arg_list(" "), Err(Err::Error((Error{input: " ", code: ErrorKind::AlphaNumeric}))));
/// ```
pub fn arg_list(input: &str) -> IResult<&str, Vec<&str>> {
    separated_list1(tag(" "), not_ending)(input)
}

pub fn not_ending<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    input.split_at_position1_complete(
        |item| {
            let c = item.as_char();
            !c.is_ascii_alphabetic() && ((c.to_string() == "}") || (c.is_whitespace()))
        },
        ErrorKind::AlphaNumeric,
    )
}

/// A combinator that wraps an `inner` parser with a leading `start` tag and an ending `end`,
/// returning the output of `inner`.
fn delimited_args<'a, F: 'a, O, E: ParseError<&'a str>>(
    start: &'a str,
    inner: F,
    end: &'a str,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(
        pair(tag(start), multispace0),
        inner,
        pair(multispace0, tag(end)),
    )
}

/// Matches a list of space separated arguments with [arg_list] surrounded by a \[ and \].
///
/// # Example
/// ```
/// use reports::parser::bracket_args;
/// assert_eq!(bracket_args("[one two three]"), Ok(("", vec!["one", "two", "three"])));
/// ```
pub fn bracket_args(input: &str) -> IResult<&str, Vec<&str>> {
    delimited_args("[", arg_list, "]")(input)
}

/// Matches a list of space separated arguments with [arg_list] surrounded by a \{\{ and \}\}.
///
/// # Example
/// ```
/// use reports::parser::curly_args;
/// assert_eq!(curly_args("{{ one two three }}"), Ok(("", vec!["one", "two", "three"])));
/// ```
pub fn curly_args(input: &str) -> IResult<&str, Vec<&str>> {
    delimited_args("{{", arg_list, "}}")(input)
}

/// Matches a list of space separated arguments with [arg_list] surrounded by a \{\{# and \}\}
///
/// # Example
/// ```
/// use reports::parser::curly_block_args;
/// assert_eq!(curly_block_args("{{# one two three }}"), Ok(("", vec!["one", "two", "three"])));
/// ```
pub fn curly_block_args(input: &str) -> IResult<&str, Vec<&str>> {
    delimited_args("{{#", arg_list, "}}")(input)
}

/// Matches many named arguments with the form arg_name=\[arg arg arg\]
///
/// # Example
/// ```
/// use reports::parser::named_args;
/// assert_eq!(named_args("arg=[one two three]"), Ok(("", vec![("arg", vec!["one", "two", "three"])])));
/// ```
pub fn named_args(input: &str) -> IResult<&str, Vec<(&str, Vec<&str>)>> {
    many0(separated_pair(
        alphanumeric1,
        tag("="),
        terminated(bracket_args, multispace0),
    ))(input)
}

fn arg_and_named_args(input: &str) -> IResult<&str, (&str, Vec<(&str, Vec<&str>)>)> {
    pair(terminated(alphanumeric1, multispace1), named_args)(input)
}

/// Matches a named expression with the form {{* arg arg=[one two three] arg=[four five six] }}
///
/// # Example
/// ```
/// use reports::parser::named_expression;
/// assert_eq!(
///     named_expression("{{* arg arg=[one two three] arg=[four five six] }}"),
///     Ok(("", ("arg", vec![("arg", vec!["one", "two", "three"]), ("arg", vec!["four", "five", "six"])])))
/// );
/// ```
pub fn named_expression(input: &str) -> IResult<&str, (&str, Vec<(&str, Vec<&str>)>)> {
    delimited_args("{{*", arg_and_named_args, "}}")(input)
}

/// Matches a block with the form {{# arg arg }}anything{{/#}}
///
/// # Example
/// ```
/// use reports::parser::{block_args, Statement};
/// assert_eq!(block_args("{{# one two three }}{{/#}}"), Ok(("", (vec!["one", "two", "three"], vec![]))));
/// assert_eq!(block_args("{{# one two three }}{{ four five six }}{{/#}}"), Ok(("", (vec!["one", "two", "three"], vec![Statement::Expression(vec!["four", "five", "six"])]))));
/// assert_eq!(block_args("{{# one two three }}{{* table rows=[four five six] cols=[seven eight nine] }}{{/#}}"), Ok(("", (vec!["one", "two", "three"], vec![Statement::NamedExpression("table", vec![("rows", vec!["four", "five", "six"]), ("cols", vec!["seven", "eight", "nine"])])]))));
/// ```
pub fn block_args(input: &str) -> IResult<&str, ((usize, Vec<&str>), Vec<Rc<Node>>)> {
    terminated(
        pair(
            // Passing on the consumed length is so that children of a block can adjust their positioning by the size of the opening clause
            consumed(curly_block_args).map(|(consumed, args)| (consumed.len(), args)),
            terminated(expression_new, take_until("{{/#}}")),
        ),
        tag("{{/#}}"),
    )(input)
}

/*/// Returns a matched collection of statements by:
/// taking until the start of an open/close clause i.e. `{{`,
/// then mapping the entire clause to a [Statement].
pub fn expression(input: &str) -> IResult<&str, Vec<LocatedStatement>> {
    many0(
        pair(
            take_until("{{").map(|skipped: &str| skipped.len()),
            consumed(alt((
                curly_args.map(|exp| Statement::Expression(exp)),
                named_expression.map(|(var, exp)| Statement::ExpressionNamedCollection(var, exp)),
                block_args.map(|((mut opening_len, args), children)| {
                    let mut children = Statements(children);
                    children.adjust(&mut opening_len);
                    Statement::Block(args, children)
                }),
            ))),
        )
        // Skipped refers to ignored text before an opening clause
        // Consumed refers to the text in an expression clause
        // By creating a [LocatedStatement] with these values in a [Range<usize>],
        // they track the relative positioning of a clause
        .map(|(skipped, (consumed, statement))| {
            LocatedStatement(skipped..(skipped + consumed.len()), statement)
        }),
    )(input)
}*/

pub fn expression_new(input: &str) -> IResult<&str, Vec<Rc<Node>>> {
    let (leftover, tree) = many0(pair(
        take_until("{{").map(|filler| Component::Filler(filler)),
        alt((
            expression_tree,
            named_expression_tree,
            block_expression_tree,
        )),
    ))(input)?;
    let mut nodes = Vec::new();
    for (filler, item) in tree {
        nodes.push(Rc::new(Node::new(filler)));
        nodes.push(item);
    }
    Ok((leftover, nodes))
}

pub fn expression_tree(input: &str) -> IResult<&str, Rc<Node>> {
    let date = NaiveDate::from_ymd(2022, 2, 4);
    let (leftover, component) = map_res::<_, _, _, _, anyhow::Error, _, _>(curly_args, |exp| {
        Ok(Component::Vars(
            exp.into_iter()
                .map(|arg| ExpressionVariable::try_new(arg, &date))
                .collect::<Result<Vec<ExpressionVariable>>>()?,
        ))
    })(input)?;
    Ok((leftover, Rc::new(Node::new(component))))
}

pub fn named_expression_tree(input: &str) -> IResult<&str, Rc<Node>> {
    let date = NaiveDate::from_ymd(2022, 2, 4);

    let (leftover, component) =
        map_res::<_, _, _, _, anyhow::Error, _, _>(named_expression, |(var, exp)| {
            Ok(Component::Collection(
                var,
                HashMap::from_iter({
                    exp.into_iter()
                        .map(|(key, val)| {
                            Ok((
                                key,
                                val.into_iter()
                                    .map(|v| ExpressionVariable::try_new(v, &date))
                                    .collect::<Result<Vec<ExpressionVariable>>>()?,
                            ))
                        })
                        .collect::<Result<Vec<(&str, Vec<ExpressionVariable>)>>>()?
                }),
            ))
        })(input)?;
    Ok((leftover, Rc::new(Node::new(component))))
}

pub fn block_expression_tree(input: &str) -> IResult<&str, Rc<Node>> {
    let date = NaiveDate::from_ymd(2022, 2, 4);
    let (leftover, node) =
        map_res::<_, _, _, _, anyhow::Error, _, _>(block_args, |((_, args), nodes)| {
            let parent = Rc::new(Node::new(Component::Vars(
                args.into_iter()
                    .map(|arg| ExpressionVariable::try_new(arg, &date))
                    .collect::<Result<Vec<ExpressionVariable>>>()?,
            )));
            for child in nodes {
                Node::add_child(&parent, &child)
            }
            Ok(parent)
        })(input)?;

    Ok((leftover, node))
}

/// A parsed section of text.
#[derive(Debug, PartialEq, Eq)]
pub enum Statement<'a> {
    Expression(Vec<&'a str>),
    ExpressionNamedCollection(&'a str, Vec<(&'a str, Vec<&'a str>)>),
    Block(Vec<&'a str>, Statements<'a>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct LocatedStatement<'a>(Range<usize>, Statement<'a>);
/// NewType of [Vec<Statement>] that implements [TryFrom] to parse an entire text into a collection of statements.
#[derive(Debug, PartialEq, Eq)]
pub struct Statements<'a>(Vec<LocatedStatement<'a>>);

impl Statements<'_> {
    /// Adjusts a statements location by the length of preceding statements,
    /// so that the overall location is correct in a larger body of text.
    fn adjust(&mut self, count: &mut usize) {
        let skipped = *count;

        self.shift(skipped);

        let mut count = 0;

        for stmt in self.iter_mut() {
            let old_count = count;
            count += stmt.0.end - skipped;
            stmt.0 = (stmt.0.start + old_count)..(stmt.0.end + old_count);
        }
    }

    /// Shifts every child of a block by the blocks [Range] `start`,
    /// so that children can reflect the padding prior to a parent's position.
    ///
    /// Then shifts every top level statement by `shift`.
    fn shift(&mut self, shift: usize) {
        for stmt in self.iter_mut() {
            match &mut stmt.1 {
                Statement::Block(_, sub_stmts) => sub_stmts.shift(stmt.0.start),
                _ => (),
            }
            stmt.0 = (stmt.0.start + shift)..(stmt.0.end + shift);
        }
    }
}

/*
impl<'a> TryFrom<&'a str> for Statements<'a> {
    type Error = anyhow::Error;
    /// Parses a &str into a collection as a [Vec<Statement] NewType.
    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        match expression(value) {
            Ok((_, statements)) => {
                let mut stmts = Statements(statements);
                stmts.adjust(&mut 0);
                Ok(stmts)
            }
            Err(e) => Err(e.map(|e| Error::new(e.input.to_string(), e.code)).into()),
        }
    }
}*/

impl<'a> Deref for Statements<'a> {
    type Target = Vec<LocatedStatement<'a>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> DerefMut for Statements<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

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
            "Change" => Ok(Change::from_figure_expresssion(&self, data)?.to_string()),
            "AvgFreq" => Ok(AvgFreq::from_figure_expresssion(&self, data)?.to_string()),
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

impl From<TimeFrequency> for ExpressionType {
    fn from(v: TimeFrequency) -> Self {
        Self::Prev(v)
    }
}

impl ExpressionType {
    /// Returns `true` if the expression type is [`Prev`].
    ///
    /// [`Prev`]: ExpressionType::Prev
    pub fn is_prev(&self) -> bool {
        matches!(self, Self::Prev(..))
    }

    pub fn try_into_prev(self) -> Result<TimeFrequency, Self> {
        if let Self::Prev(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    pub fn as_prev(&self) -> Option<&TimeFrequency> {
        if let Self::Prev(v) = self {
            Some(v)
        } else {
            None
        }
    }
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
    fn test_expression() {
        let (leftover, nodes) = expression_tree("{{ Weekly cat_purrs change }}").unwrap();
        eprintln!("{leftover} {:?}", nodes);
        assert!(leftover.len() == 0);
    }

    #[test]
    fn test_statements() {
        let (leftover, nodes) = expression_new(
            "junk {{# cat_purrs }} more junk {{ Weekly change }}{{ Quarterly change }} so much junk {{/#}}",
        ).unwrap();
        let top = Rc::new(Node::new(Component::Filler("")));
        for node in nodes {
            Node::add_child(&top, &node)
        }
        Node::add_child(&top, &Rc::new(Node::new(Component::Filler(leftover))));
        let content = top.render().unwrap();
        eprintln!("{content}");
        assert!(leftover.len() == 0);
        /*
        assert_eq!(
            stmts,
            Statements(vec![LocatedStatement(
                5..97,
                Statement::Block(
                    vec!["one", "two", "three"],
                    Statements(vec![
                        LocatedStatement(
                            36..55,
                            Statement::Expression(vec!["four", "five", "six"])
                        ),
                        LocatedStatement(
                            55..77,
                            Statement::Expression(vec!["seven", "eight", "nine"])
                        )
                    ])
                )
            )])
        );

        assert_eq!(
            Statements::try_from("{{# one two three }}{{/#}}").unwrap(),
            Statements(vec![LocatedStatement(
                0..26,
                Statement::Block(vec!["one", "two", "three"], Statements(vec![]))
            )])
        );

        assert_eq!(
            Statements::try_from("{{* table rows=[one two three] cols=[four five six] }}").unwrap(),
            Statements(vec![LocatedStatement(
                0..54,
                Statement::ExpressionNamedCollection(
                    "table",
                    vec![
                        ("rows", vec!["one", "two", "three"]),
                        ("cols", vec!["four", "five", "six"])
                    ]
                )
            )])
        );

        assert_eq!(
            Statements::try_from("{{# one two three }}{{* table rows=[four five six] cols=[seven eight nine] }}{{# ten eleven twelve }}{{ thirteen fourteen fifteen }}{{/#}}{{/#}}").unwrap(),
            Statements(vec![LocatedStatement(0..144, Statement::Block(
                    vec!["one", "two", "three"],
                    Statements(vec![LocatedStatement(20..77, Statement::ExpressionNamedCollection(
                                "table",
                                vec![
                                    ("rows", vec!["four", "five", "six"]),
                                    ("cols", vec!["seven", "eight", "nine"])
                                    ]
                                )),
                        LocatedStatement(77..138, Statement::Block(vec!["ten", "eleven", "twelve"], Statements(vec![
                        LocatedStatement(101..132, Statement::Expression(vec!["thirteen", "fourteen", "fifteen"]))])))
                        ])
                )
            )])
        );
        */
    }

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
