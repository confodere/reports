use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_until},
    character::complete::multispace0,
    error::Error,
    error::{ErrorKind, ParseError},
    sequence::{preceded, terminated, tuple},
    Err, FindSubstring, IResult, InputLength, Parser,
};

/// Opening tag for a block element
fn opening(i: &str) -> IResult<&str, (&str, &str, &str)> {
    tuple((
        preceded(
            tag("{{"),
            // terminated removes the space after the keyword
            terminated(alt((tag("Change"), tag("AvgFreq"))), multispace0),
        ),
        take_until("}}"),
        tag("}}"),
    ))(i)
}

/// Closing tag for a block element
fn ending<'a>(i: &'a str, clause: &str) -> IResult<&'a str, &'a str> {
    terminated(take_until(clause), tag(clause))(i)
}

/// Block constructor nom parser
fn tag_block(i: &str) -> IResult<&str, (&str, &str, &str)> {
    let (remainder, (clause, args, _)) = opening(i)?;
    let closing_clause = match clause {
        "Change" => "{{/Change}}",
        "AvgFreq" => "{{/AvgFreq}}",
        _ => {
            panic!("Nom construction of invalid return clause")
        }
    };
    let (remainder, block) = ending(remainder, closing_clause)?;
    Ok((remainder, (clause, args, block)))
}

/// Finds the earliest occurance of any keywords and calls take until that occurance
/// meant to allow skipping over junk when parsing by taking until the next key element
pub fn either(i: &str) -> IResult<&str, &str> {
    let mut first: Option<usize> = None;
    for opt in ["Change", "AvgFreq"] {
        if let Some(num) = i.find_substring(&format!("{{{{{}", opt)[..]) {
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
        Err(nom::Err::Error(Error::new("Ran out", ErrorKind::Eof)))
    }
}

pub fn find_blocks(i: &str) -> IResult<&str, Vec<Block>> {
    // Initial either is to remove any preceding junk
    let (i, initial_junk) = either(i)?;
    let (residual, block_tups) = alternate(either, tag_block)(i)?;

    let padding = initial_junk.len();
    let mut blocks: Vec<Block> = Vec::new();
    for ((keyword, vars, template), start, end) in block_tups {
        blocks.push(Block {
            keyword,
            vars,
            template,
            start: start + padding,
            end: end + padding,
        });
    }

    Ok((residual, blocks))
}

#[derive(Debug, PartialEq, Eq)]
pub struct Block<'a> {
    pub keyword: &'a str,
    pub vars: &'a str,
    pub template: &'a str,
    pub start: usize,
    pub end: usize,
}

impl Default for Block<'_> {
    fn default() -> Self {
        Self {
            keyword: "Change",
            vars: "Weekly",
            template: "Weekly change was {{fig}}.",
            start: 0,
            end: 54,
        }
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
/// use reports::parser::{alternate, either};
/// use nom::{Err, error::ErrorKind, Needed, IResult};
/// use nom::bytes::complete::tag;
///
/// fn parser(s: &str) -> IResult<&str, Vec<(&str, usize, usize)>> {
///     alternate(either, tag("{{Change}}"))(s)
/// }
///
/// assert_eq!(parser("{{Change}}junk{{Change}}more junk{{Change}}"), Ok(("", vec![("{{Change}}",0, 10), ("{{Change}}", 14, 24), ("{{Change}}", 33, 43)])));
/// assert_eq!(parser("{{Change}}junk{{Change}}more junk{{Change}}{{Change}}"), Ok(("", vec![("{{Change}}", 0, 10), ("{{Change}}", 14, 24), ("{{Change}}", 33, 43), ("{{Change}}", 43, 53)])));
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
            opening("{{Change}}|{{/Change}}"),
            Ok(("|{{/Change}}", ("Change", "", "}}")))
        );
    }

    #[test]
    #[should_panic]
    fn test_junk() {
        assert_eq!(
            tag_block("junk {{Change}}{{/Change}}"),
            Ok(("junk {{Change}}{{/Change}}", ("", "", "")))
        );
    }

    #[test]
    fn test_either() {
        assert_eq!(
            either("junk {{Change}} junk rand {{AvgFreq}}"),
            Ok(("{{Change}} junk rand {{AvgFreq}}", "junk "))
        );
    }

    #[test]
    fn test_nom2() {
        assert_eq!(
            find_blocks("{{Change Weekly}}Weekly change was {{fig}}.{{/Change}}"),
            Ok((
                "",
                vec![Block {
                    ..Default::default()
                }]
            ))
        );

        assert_eq!(
            find_blocks("{{Change Weekly}}Weekly change was {{fig}}.{{/Change}} junk {{Change Daily}}Daily change of {{fig}}.{{/Change}} more junk {{AvgFreq Monthly}} compared to last month where {{fig}} was observed.{{/AvgFreq}} final junk"),
            Ok((" final junk", vec![
                Block {..Default::default()}, Block { vars: "Daily", template: "Daily change of {{fig}}.", start: 60, end: 111, ..Default::default()}, Block { keyword: "AvgFreq", vars: "Monthly", template: " compared to last month where {{fig}} was observed.", start: 122, end: 204}
            ])));

        assert_eq!(
            find_blocks("junk {{Change}}{{/Change}}"),
            Ok((
                "",
                vec![Block {
                    vars: "",
                    template: "",
                    start: 5,
                    end: 26,
                    ..Default::default()
                }]
            ))
        );
    }
}
