use crate::pre_process::block::{Expression, ExpressionNamedCollection, ExpressionVariable};
use crate::pre_process::tree::{Filler, Node};
use anyhow::Result;
use chrono::NaiveDate;
use nom::combinator::map_res;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{alphanumeric1, multispace0, multispace1},
    error::Error,
    error::{ErrorKind, ParseError},
    multi::{many0, separated_list1},
    sequence::{delimited, pair, separated_pair, terminated, tuple},
    AsChar, IResult, InputTakeAtPosition, Parser,
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
pub fn block_args(input: &str) -> IResult<&str, (Vec<&str>, Vec<Statement>, &str)> {
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
pub fn expression(input: &str) -> IResult<&str, Vec<Statement>> {
    let (res, r) = many0(
        pair(
            take_until("{{").map(|filler: &str| Statement::Filler(Filler(filler.to_string()))),
            alt((
                expression_tree,
                named_expression_tree,
                block_expression_tree,
            )),
        )
        .map(|(filler, mut expr)| {
            expr.insert(0, filler);
            expr
        }),
    )(input)
    .map(|(leftover, exprs)| {
        (
            leftover,
            exprs.into_iter().flatten().collect::<Vec<Statement>>(),
        )
    })?;

    Ok((res, r))
}

pub fn expression_tree(input: &str) -> IResult<&str, Vec<Statement>> {
    map_res::<_, _, _, _, anyhow::Error, _, _>(curly_args, |exp| {
        let vars = exp
            .into_iter()
            .map(|var| ExpressionVariable::try_new(var, &NaiveDate::from_ymd(2022, 2, 4)))
            .collect::<Result<Vec<ExpressionVariable>>>()?;
        Ok(vec![Statement::Expression(Expression::from(vars))])
    })(input)
}

pub fn named_expression_tree(input: &str) -> IResult<&str, Vec<Statement>> {
    map_res::<_, _, _, _, anyhow::Error, _, _>(named_expression, |(var, exp)| {
        let m = exp
            .into_iter()
            .map(|(key, val)| {
                Ok((
                    key.to_string(),
                    val.into_iter()
                        .map(|v| {
                            Ok(ExpressionVariable::try_new(
                                v,
                                &NaiveDate::from_ymd(2022, 2, 4),
                            )?)
                        })
                        .collect::<Result<Vec<_>>>()?,
                ))
            })
            .collect::<Result<HashMap<_, _>>>()?;
        Ok(vec![Statement::ExpressionNamedCollection(
            ExpressionNamedCollection::new(var.to_string(), m, Expression::new()),
        )])
    })(input)
}

pub fn block_expression_tree(input: &str) -> IResult<&str, Vec<Statement>> {
    map_res::<_, _, _, _, anyhow::Error, _, _>(block_args, |(args, nodes, filler)| {
        let mut node = Node::new(Expression::from(
            args.into_iter()
                .map(|var| ExpressionVariable::try_new(var, &NaiveDate::from_ymd(2022, 2, 4)))
                .collect::<Result<Vec<_>>>()?,
        ));
        for component in nodes {
            match component {
                Statement::Expression(e) => node.add_child(Box::new(e)),
                Statement::ExpressionNamedCollection(e) => node.add_child(Box::new(e)),
                Statement::Node(e) => node.add_child(Box::new(e)),
                Statement::Filler(e) => node.add_child(Box::new(e)),
            };
        }
        Ok(vec![
            Statement::Node(node),
            Statement::Filler(Filler(filler.to_string())),
        ])
    })(input)
}

/// A parsed section of text.
pub enum Statement {
    Expression(Expression),
    ExpressionNamedCollection(ExpressionNamedCollection),
    Node(Node),
    Filler(Filler),
}

pub struct Statements(pub Vec<Statement>);

impl Deref for Statements {
    type Target = Vec<Statement>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl TryFrom<String> for Statements {
    type Error = anyhow::Error;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        match expression(&value) {
            Ok((filler, mut stmts)) => {
                stmts.push(Statement::Filler(Filler(filler.to_string())));
                Ok(Statements(stmts))
            }
            Err(e) => Err(e.map(|e| Error::new(e.input.to_string(), e.code)).into()),
        }
    }
}

impl From<Vec<Statement>> for Node {
    fn from(stmts: Vec<Statement>) -> Self {
        let mut node = Node::new(Expression::new());
        for stmt in stmts {
            match stmt {
                Statement::Expression(e) => node.add_child(Box::new(e)),
                Statement::ExpressionNamedCollection(e) => node.add_child(Box::new(e)),
                Statement::Node(e) => node.add_child(Box::new(e)),
                Statement::Filler(e) => node.add_child(Box::new(e)),
            }
        }
        node
    }
}

#[cfg(test)]
mod tests {
    use crate::pre_process::tree::Component;

    use super::*;
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
        if let Statement::ExpressionNamedCollection(s) = &mut s[0] {
            let mut expr = Expression::new();
            expr.set_command("change".to_string());
            assert_eq!(
                s.render(&expr).unwrap(),
                "
| _ | Cat Purrs |
| --- | --- |
| Weekly | 25.0% |
| Quarterly | 233.3% |
"
                .to_string()
            )
        } else {
            panic!("Not correct statement")
        }
    }

    #[test]
    fn test_expression() {
        let (leftover, _) =
            expression_tree("{{ Weekly cat_purrs change describedpercentage }}").unwrap();
        assert!(leftover.len() == 0);
    }

    fn rendered(s: &str) -> Result<String> {
        let mut node = Node::from(Statements::try_from(s.to_string())?.0);
        node.render(&Expression::new())
    }

    #[test]
    fn test_statements() {
        assert_eq!(
            rendered("{{ Weekly cat_purrs change}}").unwrap(),
            "25.0%".to_string()
        );

        assert_eq!(
            rendered("{{# cat_purrs }}{{ Weekly change}}{{/#}}").unwrap(),
            "25.0%".to_string()
        );
        assert_eq!(
            rendered(
                "junk {{# cat_purrs }} more junk {{ Weekly change describedpercentage}} next junk {{/#}} final junk"
            )
            .unwrap(),
            "junk  more junk up 25.0% next junk  final junk".to_string()
        );
        assert_eq!(
            rendered(
                "{{# cat_purrs }}{{*table rows=[Weekly Quarterly] cols=[change avg_freq]}}{{/#}}"
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
