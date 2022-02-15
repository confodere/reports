use crate::pre_process::block::{Expression, ExpressionNamedCollection, ExpressionVariable};
use crate::pre_process::tree::{Node, Text};
use anyhow::{anyhow, Result};
use chrono::NaiveDate;
use nom::bytes::complete::take_until1;
use nom::character::complete::digit1;
use nom::combinator::map_res;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{alphanumeric1, multispace0, multispace1},
    error::Error,
    error::{ErrorKind, ParseError},
    multi::{many0, separated_list1},
    sequence::{delimited, pair, separated_pair, terminated, tuple},
    AsChar, IResult, InputTakeAtPosition,
};
use std::collections::HashMap;
use std::ops::Deref;

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
            !c.is_ascii_alphabetic()
                && ((c.to_string() == "}") || (c.to_string() == "]") || (c.is_whitespace()))
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
pub fn block_args(input: &str) -> IResult<&str, (Vec<&str>, Vec<Segment>, &str)> {
    terminated(
        tuple((
            // Passing on the consumed length is so that children of a block can adjust their positioning by the size of the opening clause
            curly_block_args,
            expression,
            take_until("{{/#}}"),
        )),
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

fn skip_text(input: &str) -> IResult<&str, Segment> {
    let (rest, text) = take_until1("{{")(input)?;
    Ok((rest, Segment::Text(Text(text.to_string()))))
}

fn find_segments(input: &str) -> IResult<&str, Segment> {
    alt((
        block_expression_tree,
        named_expression_tree,
        expression_tree,
        skip_text,
    ))(input)
}

pub fn expression(input: &str) -> IResult<&str, Vec<Segment>> {
    many0(find_segments)(input)
}

pub fn expression_tree(input: &str) -> IResult<&str, Segment> {
    map_res::<_, _, _, _, anyhow::Error, _, _>(curly_args, |exp| {
        let vars = exp
            .into_iter()
            .map(|var| ExpressionVariable::try_new(var))
            .collect::<Result<Vec<ExpressionVariable>>>()?;
        Ok(Segment::Expression(Expression::from(vars)))
    })(input)
}

pub fn named_expression_tree(input: &str) -> IResult<&str, Segment> {
    map_res::<_, _, _, _, anyhow::Error, _, _>(named_expression, |(var, exp)| {
        let m = exp
            .into_iter()
            .map(|(key, val)| {
                Ok((
                    key.to_string(),
                    val.into_iter()
                        .map(|v| Ok(ExpressionVariable::try_new(v)?))
                        .collect::<Result<Vec<_>>>()?,
                ))
            })
            .collect::<Result<HashMap<_, _>>>()?;
        Ok(Segment::ExpressionNamedCollection(
            ExpressionNamedCollection::new(var.to_string(), m, Expression::new()),
        ))
    })(input)
}

pub fn block_expression_tree(input: &str) -> IResult<&str, Segment> {
    map_res::<_, _, _, _, anyhow::Error, _, _>(block_args, |(args, mut nodes, filler)| {
        nodes.push(Segment::Text(Text(filler.to_string())));
        let node = SegmentNode::new(
            Expression::from(
                args.into_iter()
                    .map(|var| ExpressionVariable::try_new(var))
                    .collect::<Result<Vec<_>>>()?,
            ),
            nodes,
        );
        Ok(Segment::Node(node))
    })(input)
}

#[derive(Debug, PartialEq, Eq)]
/// A parsed section of text.
pub enum Segment {
    Expression(Expression),
    ExpressionNamedCollection(ExpressionNamedCollection),
    Node(SegmentNode),
    Text(Text),
}

#[derive(Debug, PartialEq, Eq)]
pub struct SegmentNode {
    pub value: Expression,
    pub children: Vec<Segment>,
}

impl SegmentNode {
    fn new(value: Expression, children: Vec<Segment>) -> Self {
        SegmentNode { value, children }
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

    fn try_from(value: String) -> Result<Self, Self::Error> {
        match expression(&value) {
            Ok((filler, mut stmts)) => {
                stmts.push(Segment::Text(Text(filler.to_string())));
                Ok(Segments(stmts))
            }
            Err(e) => Err(e.map(|e| Error::new(e.input.to_string(), e.code)).into()),
        }
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
                    let mut sub_node = Node::from(e.children);
                    sub_node.value = e.value;
                    node.add_child(Box::new(sub_node));
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
    use crate::pre_process::tree::Component;
    use nom::Err;

    use super::*;

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
        assert_eq!(
            named_args("rows=[Monthly Quarterly]").unwrap(),
            ("", vec![("rows", vec!["Monthly", "Quarterly"])])
        );

        let (_, (command, named)) =
            named_expression("{{* table rows=[Monthly Quarterly] cols=[cat_purrs dog_woofs]}}")
                .unwrap();
        assert_eq!(command, "table");
        assert_eq!(named.len(), 2);
        assert_eq!(named[0], ("rows", vec!["Monthly", "Quarterly"]));

        let (_, mut s) =
            named_expression_tree("{{* table rows=[Weekly Quarterly] cols=[cat_purrs]}}").unwrap();
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
            named_expression_tree("blah"),
            Err(Err::Error(Error::new("blah", ErrorKind::Tag)))
        );
    }

    #[test]
    fn test_expression() {
        let (leftover, _) =
            expression_tree("{{ Weekly cat_purrs change describedpercentage }}").unwrap();
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
    }

    #[test]
    fn test_find_segments() {
        assert_eq!(
            find_segments(""),
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
            rendered("{{# cat_purrs 2022-02-04}}{{ Weekly change}}{{/#}}").unwrap(),
            "25.0%".to_string()
        );
        assert_eq!(
            rendered(
                "junk {{# cat_purrs 2022-02-04}} more junk {{ Weekly change describedpercentage}} next junk {{/#}} final junk"
            )
            .unwrap(),
            "junk  more junk up 25.0% next junk  final junk".to_string()
        );
        assert_eq!(
            rendered(
                "{{# cat_purrs 2022-02-04 }}{{*table rows=[Weekly Quarterly] cols=[change avg_freq]}}{{/#}}"
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
    }
}
