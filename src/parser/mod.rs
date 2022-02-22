use crate::functions::table::Table;
use crate::pre_process::block::{Expression, ExpressionNamedCollection, ExpressionVariable};
use crate::pre_process::tree::Component;
use crate::pre_process::tree::{Node, Text};
use anyhow::{anyhow, Result};
use chrono::NaiveDate;
use nom::bytes::complete::{is_not, take_until1};
use nom::character::complete::digit1;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{alphanumeric1, multispace0},
    error::ParseError,
    multi::{many0, separated_list1},
    sequence::{delimited, pair, separated_pair, terminated, tuple},
    IResult,
};
use std::collections::HashMap;
use std::ops::Deref;

pub mod substitute;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Arg<'a> {
    Arg(&'a str),
    NamedArg(&'a str, &'a str),
    Collection(Vec<&'a str>),
    NamedCollection(&'a str, Vec<&'a str>),
}

impl<'a> From<Vec<Arg<'a>>> for FlatArgs<'a> {
    fn from(args: Vec<Arg<'a>>) -> Self {
        let mut solo = vec![];
        let mut group = vec![];
        let mut longest = 0;

        for arg in args.iter() {
            match arg {
                Arg::Arg(a) => solo.push(*a),
                Arg::NamedArg(_, _) => todo!(),
                Arg::Collection(a) => {
                    if group.len() > longest {
                        longest = group.len()
                    };
                    group.push(a)
                }
                Arg::NamedCollection(_, _) => todo!(),
            }
        }

        let folded = group.into_iter().fold(vec![solo], |acc, x| {
            let mut new_acc = vec![];
            for s in x {
                for a in &acc {
                    let mut v = a.clone();
                    v.push(s);
                    new_acc.push(v);
                }
            }
            new_acc
        });
        FlatArgs(folded)

        /*
        let mut from = vec![];
        for g in group {
            for s in g {
                let mut group = solo.clone();
                group.push(s);
                from.push(group);
            }
        }
        if from.len() == 0 {
            from.push(solo)
        }
        FlatArgs(from)
        */
    }
}

pub struct Args<'a>(pub Vec<Arg<'a>>);

impl<'a> Deref for Args<'a> {
    type Target = Vec<Arg<'a>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub struct FlatArgs<'a>(pub Vec<Vec<&'a str>>);

impl<'a> Deref for FlatArgs<'a> {
    type Target = Vec<Vec<&'a str>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl TryFrom<FlatArgs<'_>> for Vec<Expression> {
    type Error = anyhow::Error;

    fn try_from(value: FlatArgs<'_>) -> Result<Vec<Expression>> {
        value
            .clone()
            .into_iter()
            .map(|args| {
                let mut expr = Expression::new();
                for arg in args {
                    expr += ExpressionVariable::try_from(arg)?;
                }
                Ok(expr)
            })
            .collect::<Result<Vec<Expression>>>()
    }
}

impl TryFrom<Vec<&str>> for ArgExpression {
    type Error = anyhow::Error;

    fn try_from(value: Vec<&str>) -> Result<Self, Self::Error> {
        let vars = value
            .into_iter()
            .map(|v| ExpressionVariable::try_from(v))
            .collect::<Result<Vec<ExpressionVariable>>>()?;
        Ok(ArgExpression(vars))
    }
}

pub struct ArgExpression(pub Vec<ExpressionVariable>);

impl Deref for ArgExpression {
    type Target = Vec<ExpressionVariable>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// A combinator that wraps an `inner` parser with a leading `start` tag and an ending `end`,
/// returning the output of `inner`.
/// Accepts optional spacing after `start` and before `end`.
fn delimited_start_func_end<'a, F: 'a, O, E: ParseError<&'a str>>(
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

/// Matches comma separated keywords.
/// # Example
///
/// ```
/// use reports::parser::comma_separated_keywords;
/// use nom::error::{Error, ErrorKind};
/// use nom::Err;
/// assert_eq!(comma_separated_keywords("one, two, three"), Ok(("", vec!["one", "two", "three"])));
/// assert_eq!(comma_separated_keywords(" "), Err(Err::Error((Error{input: " ", code: ErrorKind::IsNot}))));
/// ```
pub fn comma_separated_keywords(input: &str) -> IResult<&str, Vec<&str>> {
    separated_list1(
        tuple((multispace0, tag(","), multispace0)),
        while_not_control_character,
    )(input)
}

/// Matches a single named keyword with the form name=keyword
fn named_keyword(input: &str) -> IResult<&str, (&str, &str)> {
    separated_pair(
        alphanumeric1,
        tag("="),
        terminated(while_not_control_character, multispace0),
    )(input)
}

/// Matches anything until one of `[]{}, `
fn while_not_control_character(input: &str) -> IResult<&str, &str> {
    is_not("[]{}, ")(input)
}

/// Matches many named arguments with the form arg_name=\[arg, arg, arg\]
pub fn named_square_bracket_delimited_keywords(input: &str) -> IResult<&str, (&str, Vec<&str>)> {
    separated_pair(
        alphanumeric1,
        tag("="),
        terminated(square_bracket_delimited_args, multispace0),
    )(input)
}

/// Matches a list of space separated arguments with [arg_list] surrounded by a \[ and \].
///
/// # Example
/// ```
/// use reports::parser::square_bracket_delimited_args;
/// assert_eq!(square_bracket_delimited_args("[one, two, three]"), Ok(("", vec!["one", "two", "three"])));
/// ```
pub fn square_bracket_delimited_args(input: &str) -> IResult<&str, Vec<&str>> {
    delimited_start_func_end("[", comma_separated_keywords, "]")(input)
}

/// Matches a named single [Arg] with the form `name=arg`
fn named_single_arg(input: &str) -> IResult<&str, Arg> {
    let (res, (name, arg)) = named_keyword(input)?;
    Ok((res, Arg::NamedArg(name, arg)))
}

/// Matches a single [Arg]
fn unnamed_single_arg(input: &str) -> IResult<&str, Arg> {
    let (res, arg) = while_not_control_character(input)?;
    Ok((res, Arg::Arg(arg)))
}

/// Matches a list of named comma separated arguments surrounded by brackets.
/// E.g. `one=[two, three, four]`
fn named_list_arg(input: &str) -> IResult<&str, Arg> {
    let (res, (name, args)) = named_square_bracket_delimited_keywords(input)?;
    Ok((res, Arg::NamedCollection(name, args)))
}

/// Matches a list of comma separated arguments surrounded by brackets.
/// E.g. `\[one, two, three\]`
fn unnamed_list_arg(input: &str) -> IResult<&str, Arg> {
    let (res, args) = square_bracket_delimited_args(input)?;
    Ok((res, Arg::Collection(args)))
}

/// Matches the next [Arg]
fn find_arg(input: &str) -> IResult<&str, Arg> {
    // Returns Arg::Collection, Arg::NamedCollection, Arg::Arg
    alt((
        unnamed_list_arg,
        named_list_arg,
        named_single_arg,
        unnamed_single_arg,
    ))(input)
}

/// Matches an alphanumeric keyword, and then a list of comma separated arguments (after a separating comma).
fn arg_and_named_args(input: &str) -> IResult<&str, (&str, Vec<Arg>)> {
    pair(
        terminated(alphanumeric1, tuple((multispace0, tag(","), multispace0))),
        comma_separated_args,
    )(input)
}

/// Matches a list of comma separated arguments e.g. `one, two, three`.
/// Accepts optional spaces before and after comma.
fn comma_separated_args(input: &str) -> IResult<&str, Vec<Arg>> {
    separated_list1(tuple((multispace0, tag(","), multispace0)), find_arg)(input)
}

/// Matches a list of space separated arguments with [arg_list] surrounded by a \{\{# and \}\}
pub fn curly_hash_delimited_args(input: &str) -> IResult<&str, Vec<Arg>> {
    delimited_start_func_end("{{#", comma_separated_args, "}}")(input)
}

/// Matches a named expression with the form {{* arg, arg=[one, two, three], arg=[four, five, six] }}
///
/// # Example
/// ```
/// use reports::parser::curly_star_delimited_args;
/// use reports::parser::Arg;
/// assert_eq!(
///     curly_star_delimited_args("{{* arg, arg=[one, two, three], arg=[four, five, six] }}"),
///     Ok(("", ("arg", vec![Arg::NamedCollection("arg", vec!["one", "two", "three"]), Arg::NamedCollection("arg", vec!["four", "five", "six"])])))
/// );
/// ```
pub fn curly_star_delimited_args(input: &str) -> IResult<&str, (&str, Vec<Arg>)> {
    delimited_start_func_end("{{*", arg_and_named_args, "}}")(input)
}

/// Matches a list of space separated arguments with [arg_list] surrounded by a \{\{ and \}\}.
///
/// # Example
/// ```
/// use reports::parser::{curly_args, Arg};
/// use reports::pre_process::block::ExpressionVariable;
/// use reports::time_span::TimeFrequency;
/// assert_eq!(curly_args("{{ Daily Monthly }}"), Ok(("", vec![Arg::Arg(ExpressionVariable::TimeFrequency(TimeFrequency::Daily)), Arg::Arg(ExpressionVariable::TimeFrequency(TimeFrequency::Monthly))] )));
/// ```
pub fn curly_delimited_args(input: &str) -> IResult<&str, Vec<Arg>> {
    delimited_start_func_end("{{", comma_separated_args, "}}")(input)
}

/// Matches a block with the form {{# arg, arg }}anything{{/#}}
fn block_clause(input: &str) -> IResult<&str, Clause> {
    let (res, (args, inner)) = terminated(
        pair(curly_hash_delimited_args, take_until("{{/#}}")),
        tag("{{/#}}"),
    )(input)?;
    let (inner_res, mut clauses) = find_clauses(inner)?;
    if inner_res.len() > 0 {
        clauses.push(Clause::Text(inner_res));
    }
    Ok((res, Clause::Block(args, clauses)))
}

/// Matches an expression with the form {{*function, arg, arg}}
fn function_clause(input: &str) -> IResult<&str, Clause> {
    let (res, (name, function)) = curly_star_delimited_args(input)?;

    Ok((res, Clause::Function(name, function)))
}

/// Matches an expression with the form {{arg, arg}}
fn expression_clause(input: &str) -> IResult<&str, Clause> {
    let (res, args) = curly_delimited_args(input)?;

    Ok((res, Clause::Expression(args)))
}

/// Skips to next incident of `{{` in `&str`.
/// Returns `Clause::Text` so that filler is not lost.
fn skip_text(input: &str) -> IResult<&str, Clause> {
    let (res, text) = take_until1("{{")(input)?;
    Ok((res, Clause::Text(text)))
}

/// Matches the next [Clause].
/// Final match skips to next clause opening
fn find_clause(input: &str) -> IResult<&str, Clause> {
    alt((block_clause, function_clause, expression_clause, skip_text))(input)
}

/// Finds all clauses in a `&str` (including text before and between active clauses).
/// Returns clauses and any remaining text after the last clause.
pub fn find_clauses(input: &str) -> IResult<&str, Vec<Clause>> {
    many0(find_clause)(input)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Clause<'a> {
    Expression(Vec<Arg<'a>>),
    Function(&'a str, Vec<Arg<'a>>),
    Block(Vec<Arg<'a>>, Vec<Clause<'a>>),
    Text(&'a str),
}

impl Clause<'_> {
    fn to_child(&self) -> Result<Box<dyn Component>> {
        match self {
            Clause::Expression(e) => {
                let exprs = Vec::<Expression>::try_from(FlatArgs::from(e.clone()))?;
                Ok(Box::new(exprs) as _)
                /*Ok(exprs
                .into_iter()
                .map(|expr| Box::new(expr) as _)
                .collect::<Vec<_>>())*/
            }
            Clause::Function(name, args) => match *name {
                "table" => {
                    let mut named_collections = HashMap::new();
                    let mut expr = Expression::new();
                    for arg in args {
                        match arg {
                            Arg::Arg(a) => {
                                expr += ExpressionVariable::try_from(*a)?;
                            }
                            Arg::NamedArg(_, _) => todo!(),
                            Arg::Collection(_) => todo!(),
                            Arg::NamedCollection(name, collection) => {
                                let collection = collection
                                    .into_iter()
                                    .map(|s| ExpressionVariable::try_from(*s))
                                    .collect::<Result<Vec<ExpressionVariable>>>()?;
                                named_collections.insert(*name, collection);
                            }
                        }
                    }
                    let (rows, cols) =
                        (named_collections.get("rows"), named_collections.get("cols"));
                    let rows = rows.ok_or_else(|| anyhow!("Table requires rows"))?;
                    let cols = cols.ok_or_else(|| anyhow!("Table requires cols"))?;

                    Ok(Box::new(Table::new(rows.clone(), cols.clone(), expr)))
                }
                _ => return Err(anyhow!("Unrecognised function name")),
            },
            Clause::Block(expr, children) => {
                let exprs = Vec::<Expression>::try_from(FlatArgs::from(expr.clone()))?;
                let mut top_node = Node::new(Expression::new());
                for expr in exprs {
                    let mut node = Node::new(expr);
                    for child in children {
                        node.add_child(child.to_child()?);
                    }
                    top_node.add_child(Box::new(node));
                }
                Ok(Box::new(top_node))
            }
            Clause::Text(text) => Ok(Box::new(Text(text.to_string()))),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// A parsed section of text.
pub enum Segment {
    Expression(Expression),
    ExpressionNamedCollection(ExpressionNamedCollection),
    Node(SegmentNode),
    Text(Text),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SegmentNode {
    pub value: Expression,
    pub children: Vec<Segment>,
    pub group: Vec<Vec<ExpressionVariable>>,
}

impl SegmentNode {
    pub fn new(
        value: Expression,
        children: Vec<Segment>,
        group: Vec<Vec<ExpressionVariable>>,
    ) -> Self {
        SegmentNode {
            value,
            children,
            group,
        }
    }
}

pub struct Segments(pub Vec<Segment>);

impl Deref for Segments {
    type Target = Vec<Segment>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl TryFrom<String> for Segments {
    type Error = anyhow::Error;

    fn try_from(_: String) -> Result<Self, Self::Error> {
        unimplemented!()
    }
}

impl From<Vec<Segment>> for Node {
    fn from(stmts: Vec<Segment>) -> Self {
        let mut node = Node::new(Expression::new());
        for stmt in stmts {
            match stmt {
                Segment::Expression(e) => node.add_child(Box::new(e)),
                Segment::ExpressionNamedCollection(e) => node.add_child(Box::new(e)),
                Segment::Node(e) => {
                    if e.group.len() == 0 {
                        let mut sub_node = Node::from(e.children.clone());
                        sub_node.value = e.value.clone();
                        node.add_child(Box::new(sub_node));
                    } else {
                        let mut sub_node = Node::new(e.value.clone());
                        for member in e.group {
                            for sub_member in member {
                                let mut member_node = Node::from(e.children.clone());
                                member_node.value += sub_member;
                                sub_node.add_child(Box::new(member_node))
                            }
                        }
                        node.add_child(Box::new(sub_node));
                    }
                }
                Segment::Text(e) => node.add_child(Box::new(e)),
            }
        }
        node
    }
}

fn dmy_slash(input: &str) -> IResult<&str, (&str, &str, &str, &str, &str)> {
    tuple((digit1, tag("/"), digit1, tag("/"), digit1))(input)
}

fn ymd_dash(input: &str) -> IResult<&str, (&str, &str, &str, &str, &str)> {
    tuple((digit1, tag("-"), digit1, tag("-"), digit1))(input)
}

/// Matches and parses a date in a `&str` to a [NaiveDate] from forms:
/// - %d/%m/%Y
/// - %Y-%m-%d
///
/// # Example
/// ```
/// use reports::parser::parse_date;
/// use chrono::NaiveDate;
/// assert_eq!(parse_date("4/2/2022").unwrap(), NaiveDate::from_ymd(2022, 2, 4));
/// assert_eq!(parse_date("2022-2-4").unwrap(), NaiveDate::from_ymd(2022, 2, 4));
/// ```
pub fn parse_date(input: &str) -> Result<NaiveDate> {
    let date = if let Ok(_) = dmy_slash(input) {
        NaiveDate::parse_from_str(input, "%d/%m/%Y")?
    } else {
        if let Ok(_) = ymd_dash(input) {
            NaiveDate::parse_from_str(input, "%Y-%m-%d")?
        } else {
            return Err(anyhow!("{} is not a Y-m-d or d/m/Y date", input));
        }
    };
    Ok(date)
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_clause_translation() {
        let (_, args) =
            find_clauses("{{#cat_purrs, Words, 2022-02-04, [Quarterly, Weekly]}}Fig: {{fig}} Change: {{change}} {{/#}}")
                .unwrap();
        let mut node = Node::new(Expression::new());
        for arg in args {
            node.add_child(arg.to_child().unwrap())
        }
        let res = node.render(&Expression::new()).unwrap();
        assert_eq!(
            res,
            "Fig: 10 Change: up 233.3% Fig: 10 Change: up 25.0% ".to_string()
        );
    }

    #[test]
    fn test_complex_arg_list() {
        let (res, args) = comma_separated_args("one, two, three").unwrap();
        assert_eq!(res, "");
        assert_eq!(
            args,
            vec![Arg::Arg("one"), Arg::Arg("two"), Arg::Arg("three")]
        );

        let (res, args) = comma_separated_args("one, [two, three]").unwrap();
        assert_eq!(res, "");
        assert_eq!(
            args,
            vec![Arg::Arg("one"), Arg::Collection(vec!["two", "three"])]
        );
        let (res, args) = comma_separated_args("one, two=[three, four]").unwrap();
        assert_eq!(res, "");
        assert_eq!(
            args,
            vec![
                Arg::Arg("one"),
                Arg::NamedCollection("two", vec!["three", "four"])
            ]
        );
    }

    #[test]
    fn test_expression() {
        let (res, args) = expression_clause("{{one, two}}").unwrap();
        assert_eq!(res, "");
        assert_eq!(
            args,
            Clause::Expression(vec![Arg::Arg("one"), Arg::Arg("two")])
        );
    }

    #[test]
    fn test_block() {
        let (res, args) = curly_hash_delimited_args("{{# one }}{{two}}{{/#}}").unwrap();
        assert_eq!(res, "{{two}}{{/#}}");
        assert_eq!(args, vec![Arg::Arg("one")]);

        let (res, args) = block_clause("{{# one }}{{two}}{{/#}}").unwrap();
        assert_eq!(res, "");
        assert_eq!(
            args,
            Clause::Block(
                vec![Arg::Arg("one")],
                vec![Clause::Expression(vec![Arg::Arg("two")])]
            )
        );
    }

    #[test]
    fn test_function() {
        let (res, args) = function_clause("{{*one, two}}").unwrap();
        assert_eq!(res, "");
        assert_eq!(args, Clause::Function("one", vec![Arg::Arg("two")]));
    }

    #[test]
    fn test_expressions() {
        let (res, clauses) = find_clauses("{{# one }}{{two}}{{/#}}{{* three, four}} five").unwrap();
        assert_eq!(res, " five");
        assert_eq!(
            clauses,
            vec![
                Clause::Block(
                    vec![Arg::Arg("one")],
                    vec![Clause::Expression(vec![Arg::Arg("two")])]
                ),
                Clause::Function("three", vec![Arg::Arg("four")])
            ]
        );
    }

    /*
        #[test]
        fn test_space_list() {
            let (res, args) = space_list("Daily Quarterly").unwrap();
            assert_eq!(res, "");
            assert_eq!(
                args,
                vec![
                    Arg::Arg(ExpressionVariable::TimeFrequency(TimeFrequency::Daily)),
                    Arg::Arg(ExpressionVariable::TimeFrequency(TimeFrequency::Quarterly))
                ]
            )
        }

        #[test]
        fn test_complex_arg_list() {
            let (res, args) = complex_arg_list("Monthly, rows=[Daily Quarterly], [Yearly]").unwrap();
            assert_eq!(res, "");
            assert_eq!(
                args,
                vec![
                    Arg::Arg(ExpressionVariable::TimeFrequency(TimeFrequency::Monthly)),
                    Arg::NamedCollection(
                        "rows".to_string(),
                        vec![
                            ExpressionVariable::TimeFrequency(TimeFrequency::Daily),
                            ExpressionVariable::TimeFrequency(TimeFrequency::Quarterly)
                        ]
                    ),
                    Arg::Collection(vec![ExpressionVariable::TimeFrequency(
                        TimeFrequency::Yearly
                    )])
                ]
            )
        }

        #[test]
        fn test_skip_text() {
            assert_eq!(
                skip_text("blah blah blah {{foo"),
                Ok(("{{foo", Segment::Text(Text("blah blah blah ".to_string()))))
            );
            assert_eq!(
                skip_text("blah"),
                Err(Err::Error(Error::new("blah", ErrorKind::TakeUntil)))
            );
        }

        #[test]
        fn test_named_expression() {
            let (_, (command, named)) =
                named_expression("{{* table rows=[Monthly Quarterly] cols=[cat_purrs dog_woofs]}}")
                    .unwrap();
            assert_eq!(command, "table");
            assert_eq!(named.len(), 2);
            assert_eq!(named[0], ("rows", vec!["Monthly", "Quarterly"]));

            let (_, mut s) =
                function_tree("{{* table rows=[Weekly Quarterly] cols=[cat_purrs]}}").unwrap();
            if let Segment::ExpressionNamedCollection(s) = &mut s {
                let mut expr = Expression::new();
                expr.set_command("change".to_string());
                expr.set_date(NaiveDate::from_ymd(2022, 2, 4));
                assert_eq!(
                    s.render(&expr).unwrap(),
                    "
    | _ | cat_purrs |
    | --- | --- |
    | Weekly | 25.0% |
    | Quarterly | 233.3% |
    "
                    .to_string()
                )
            } else {
                panic!("Not correct statement")
            }

            assert_eq!(
                function_tree("blah"),
                Err(Err::Error(Error::new("blah", ErrorKind::Tag)))
            );
        }

        #[test]
        fn test_expression() {
            let (leftover, _) = expression_tree("{{ Weekly cat_purrs change  }}").unwrap();
            assert!(leftover.len() == 0);

            assert_eq!(
                expression_tree("blah"),
                Err(Err::Error(Error::new("blah", ErrorKind::Tag)))
            );
        }

        #[test]
        fn test_block_expression() {
            assert_eq!(
                block_expression_tree("blah"),
                Err(Err::Error(Error::new("blah", ErrorKind::Tag)))
            );

            let (res, seg) =
                block_expression_tree("{{# [cat_purrs dog_woofs] }} {{fig}} {{/#}}").unwrap();
            assert_eq!(res, "");
            match &seg {
                Segment::Node(n) => {
                    assert_eq!(
                        n.group,
                        vec![vec![
                            ExpressionVariable::DataName("cat_purrs".to_string()),
                            ExpressionVariable::DataName("dog_woofs".to_string())
                        ]]
                    );
                }
                _ => panic!("Wrong type of Segment"),
            }

            let mut node = Node::from(vec![seg]);

            let mut exp = Expression::new();
            exp.set_command("fig".to_string());
            exp.set_date(NaiveDate::from_ymd(2022, 2, 4));
            exp.set_frequency(TimeFrequency::Weekly);

            let both = node.render(&exp).unwrap();

            assert_eq!(both, " 10  25 ".to_string());
        }

        #[test]
        fn test_find_segments() {
            assert_eq!(
                find_clauses(""),
                Err(Err::Error(Error::new("", ErrorKind::TakeUntil)))
            );
        }

        fn rendered(s: &str) -> Result<String> {
            let mut node = Node::from(Segments::try_from(s.to_string())?.0);
            node.render(&Expression::new())
        }

        #[test]
        fn test_statements() {
            assert_eq!(
                rendered("{{ Weekly cat_purrs change 2022-02-04}}").unwrap(),
                "25.0%".to_string()
            );

            assert_eq!(
                rendered("{{# cat_purrs, 2022-02-04}}{{ Weekly change}}{{/#}}").unwrap(),
                "25.0%".to_string()
            );
            assert_eq!(
                rendered(
                    "junk {{# cat_purrs, 2022-02-04, Words}} more junk {{ Weekly change }} next junk {{/#}} final junk"
                )
                .unwrap(),
                "junk  more junk up 25.0% next junk  final junk".to_string()
            );
            assert_eq!(
                rendered(
                    "{{# cat_purrs, 2022-02-04 }}{{*table rows=[Weekly Quarterly] cols=[change avg_freq]}}{{/#}}"
                )
                .unwrap(),
                "
    | _ | change | avg_freq |
    | --- | --- | --- |
    | Weekly | 25.0% | 10.0 |
    | Quarterly | 233.3% | 130.0 |
    "
                .to_string()
            );
            */
    /*
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
    //}
}
