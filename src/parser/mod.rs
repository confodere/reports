pub mod substitute;

use anyhow::{anyhow, Result};
use chrono::NaiveDate;
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_until1},
    character::complete::{alphanumeric1, digit1, multispace0},
    error::{Error, ParseError},
    multi::{many0, separated_list1},
    sequence::{delimited, pair, separated_pair, terminated, tuple},
    Finish, IResult,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RawArgGroup<'a> {
    Arg(&'a str),
    LabelledArg(&'a str, &'a str),
    Collection(Vec<&'a str>),
    NamedCollection(&'a str, Vec<&'a str>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RawClause<'a> {
    Function(Vec<RawArgGroup<'a>>),
    NamedFunction(&'a str, Vec<RawArgGroup<'a>>),
    Block(Vec<RawArgGroup<'a>>, Vec<RawClause<'a>>),
    Text(&'a str),
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
    is_not("[]{}, /#")(input)
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
fn named_single_arg(input: &str) -> IResult<&str, RawArgGroup> {
    let (res, (name, arg)) = named_keyword(input)?;
    Ok((res, RawArgGroup::LabelledArg(name, arg)))
}

/// Matches a single [Arg]
fn unnamed_single_arg(input: &str) -> IResult<&str, RawArgGroup> {
    let (res, arg) = while_not_control_character(input)?;
    Ok((res, RawArgGroup::Arg(arg)))
}

/// Matches a list of named comma separated arguments surrounded by brackets.
/// E.g. `one=[two, three, four]`
fn named_list_arg(input: &str) -> IResult<&str, RawArgGroup> {
    let (res, (name, args)) = named_square_bracket_delimited_keywords(input)?;
    Ok((res, RawArgGroup::NamedCollection(name, args)))
}

/// Matches a list of comma separated arguments surrounded by brackets.
/// E.g. `\[one, two, three\]`
fn unnamed_list_arg(input: &str) -> IResult<&str, RawArgGroup> {
    let (res, args) = square_bracket_delimited_args(input)?;
    Ok((res, RawArgGroup::Collection(args)))
}

/// Matches the next [Arg]
fn find_arg(input: &str) -> IResult<&str, RawArgGroup> {
    // Returns Arg::Collection, Arg::NamedCollection, Arg::Arg
    alt((
        unnamed_list_arg,
        named_list_arg,
        named_single_arg,
        unnamed_single_arg,
    ))(input)
}

/// Matches an alphanumeric keyword, and then a list of comma separated arguments (after a separating comma).
fn arg_and_named_args(input: &str) -> IResult<&str, (&str, Vec<RawArgGroup>)> {
    pair(
        terminated(alphanumeric1, tuple((multispace0, tag(","), multispace0))),
        comma_separated_args,
    )(input)
}

/// Matches a list of comma separated arguments e.g. `one, two, three`.
/// Accepts optional spaces before and after comma.
fn comma_separated_args(input: &str) -> IResult<&str, Vec<RawArgGroup>> {
    separated_list1(tuple((multispace0, tag(","), multispace0)), find_arg)(input)
}

/// Matches a list of space separated arguments with [arg_list] surrounded by a \{\{# and \}\}
fn curly_hash_delimited_args(input: &str) -> IResult<&str, Vec<RawArgGroup>> {
    delimited_start_func_end("{{#", comma_separated_args, "}}")(input)
}

/// Matches a named expression with the form {{* arg, arg=[one, two, three], arg=[four, five, six] }}
///
/// # Example
/// ```
/// use reports::parser::curly_star_delimited_args;
/// use reports::parser::RawArgGroup;
/// assert_eq!(
///     curly_star_delimited_args("{{* arg, arg=[one, two, three], arg=[four, five, six] }}"),
///     Ok(("", ("arg", vec![RawArgGroup::NamedCollection("arg", vec!["one", "two", "three"]), RawArgGroup::NamedCollection("arg", vec!["four", "five", "six"])])))
/// );
/// ```
pub fn curly_star_delimited_args(input: &str) -> IResult<&str, (&str, Vec<RawArgGroup>)> {
    delimited_start_func_end("{{*", arg_and_named_args, "}}")(input)
}

/// Matches a list of space separated arguments with [arg_list] surrounded by a \{\{ and \}\}.
///
/// # Example
/// ```
/// use reports::parser::{curly_delimited_args, RawArgGroup};
/// assert_eq!(curly_delimited_args("{{ Daily, Monthly }}"), Ok(("", vec![RawArgGroup::Arg("Daily"), RawArgGroup::Arg("Monthly")] )));
/// ```
pub fn curly_delimited_args(input: &str) -> IResult<&str, Vec<RawArgGroup>> {
    delimited_start_func_end("{{", comma_separated_args, "}}")(input)
}

/// Matches a block with the form {{# arg, arg }}anything{{/#}}
fn block_clause(input: &str) -> IResult<&str, RawClause> {
    let (res, (args, inner)) =
        terminated(pair(curly_hash_delimited_args, find_clauses), tag("{{/#}}"))(input)?;

    Ok((res, RawClause::Block(args, inner)))
}

/// Matches an expression with the form {{*function, arg, arg}}
fn named_expression_clause(input: &str) -> IResult<&str, RawClause> {
    let (res, (name, function)) = curly_star_delimited_args(input)?;

    Ok((res, RawClause::NamedFunction(name, function)))
}

/// Matches an expression with the form {{arg, arg}}
fn expression_clause(input: &str) -> IResult<&str, RawClause> {
    let (res, args) = curly_delimited_args(input)?;

    Ok((res, RawClause::Function(args)))
}

/// Skips to next incident of `{{` in `&str`.
/// Returns `Clause::Text` so that filler is not lost.
fn skip_text(input: &str) -> IResult<&str, RawClause> {
    let (res, text) = alt((take_until1("{{"), is_not("{")))(input)?;
    Ok((res, RawClause::Text(text)))
}

/// Matches the next [Clause].
/// Final match skips to next clause opening
fn find_clause(input: &str) -> IResult<&str, RawClause> {
    alt((
        block_clause,
        named_expression_clause,
        expression_clause,
        skip_text,
    ))(input)
}

/// Finds all clauses in a `&str` (including text before and between active clauses).
/// Returns clauses and any remaining text after the last clause.
pub fn find_clauses(input: &str) -> IResult<&str, Vec<RawClause>> {
    many0(find_clause)(input)
}

/// Calls `find_clauses`, and then converts the error and handles trailing text
pub fn find_all_clauses(input: &str) -> Result<Vec<RawClause>> {
    match find_clauses(input).finish() {
        Ok((res, mut clauses)) => {
            if res.len() > 0 {
                let remainder: Result<(&str, &str), Error<_>> = is_not("{")(res).finish();
                match remainder {
                    Ok((_, text)) => clauses.push(RawClause::Text(text)),
                    Err(e) => return Err(anyhow!("Trailing Parse error: {}", e.input)),
                }
            }
            Ok(clauses)
        }
        Err(e) => Err(anyhow!("Parse error: {}", e.input)),
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
    fn test_complex_arg_list() {
        let (res, args) = comma_separated_args("one, two, three").unwrap();
        assert_eq!(res, "");
        assert_eq!(
            args,
            vec![
                RawArgGroup::Arg("one"),
                RawArgGroup::Arg("two"),
                RawArgGroup::Arg("three")
            ]
        );

        let (res, args) = comma_separated_args("one, [two, three]").unwrap();
        assert_eq!(res, "");
        assert_eq!(
            args,
            vec![
                RawArgGroup::Arg("one"),
                RawArgGroup::Collection(vec!["two", "three"])
            ]
        );
        let (res, args) = comma_separated_args("one, two=[three, four]").unwrap();
        assert_eq!(res, "");
        assert_eq!(
            args,
            vec![
                RawArgGroup::Arg("one"),
                RawArgGroup::NamedCollection("two", vec!["three", "four"])
            ]
        );
    }

    #[test]
    fn test_expression() {
        let (res, args) = expression_clause("{{one, two}}").unwrap();
        assert_eq!(res, "");
        assert_eq!(
            args,
            RawClause::Function(vec![RawArgGroup::Arg("one"), RawArgGroup::Arg("two")])
        );
    }

    #[test]
    fn test_block() {
        let (res, args) = curly_hash_delimited_args("{{# one }}{{two}}{{/#}}").unwrap();
        assert_eq!(res, "{{two}}{{/#}}");
        assert_eq!(args, vec![RawArgGroup::Arg("one")]);

        let (res, args) = block_clause("{{# one }}{{two}}{{/#}}").unwrap();
        assert_eq!(res, "");
        assert_eq!(
            args,
            RawClause::Block(
                vec![RawArgGroup::Arg("one")],
                vec![RawClause::Function(vec![RawArgGroup::Arg("two")])]
            )
        );
    }

    #[test]
    fn test_function() {
        let (res, args) = named_expression_clause("{{*one, two}}").unwrap();
        assert_eq!(res, "");
        assert_eq!(
            args,
            RawClause::NamedFunction("one", vec![RawArgGroup::Arg("two")])
        );
    }

    #[test]
    fn test_expressions() {
        let (res, clauses) = find_clauses("{{# one }}{{two}}{{/#}}{{* three, four}} five").unwrap();
        assert_eq!(res, "");
        assert_eq!(
            clauses,
            vec![
                RawClause::Block(
                    vec![RawArgGroup::Arg("one")],
                    vec![RawClause::Function(vec![RawArgGroup::Arg("two")])]
                ),
                RawClause::NamedFunction("three", vec![RawArgGroup::Arg("four")]),
                RawClause::Text(" five")
            ]
        );
    }

    #[test]
    fn test_table_expression() {
        let clauses = find_all_clauses(
            "{{# cat_purrs, 2022-02-04 }}{{*table, rows=[Quarterly, Weekly], cols=[change]}}{{/#}}",
        )
        .unwrap();

        assert_eq!(
            clauses,
            vec![RawClause::Block(
                vec![
                    RawArgGroup::Arg("cat_purrs"),
                    RawArgGroup::Arg("2022-02-04"),
                ],
                vec![RawClause::NamedFunction(
                    "table",
                    vec![
                        RawArgGroup::NamedCollection("rows", vec!["Quarterly", "Weekly"]),
                        RawArgGroup::NamedCollection("cols", vec!["change"])
                    ]
                )]
            )]
        );
    }

    #[test]
    fn test_nested_blocks() {
        let clauses = find_all_clauses("{{#Words}}{{#Weekly}}{{cat_purrs}}{{/#}}{{/#}}").unwrap();
        assert_eq!(
            clauses,
            vec![RawClause::Block(
                vec![RawArgGroup::Arg("Words")],
                vec![RawClause::Block(
                    vec![RawArgGroup::Arg("Weekly")],
                    vec![RawClause::Function(vec![RawArgGroup::Arg("cat_purrs")])]
                )]
            )]
        );
    }
}
