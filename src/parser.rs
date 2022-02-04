use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_until},
    error::Error,
    error::{ErrorKind, ParseError},
    sequence::{preceded, terminated, tuple},
    Err, FindSubstring, IResult, InputLength, Parser,
};

/// Opening tag for a block element
fn opening(i: &str) -> IResult<&str, (&str, &str, &str)> {
    tuple((
        preceded(tag("{{"), alt((tag("Change"), tag("AvgFreq")))),
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

pub fn find_blocks(i: &str) -> IResult<&str, Vec<(&str, &str, &str)>> {
    // Initial either is to remove any preceding junk
    let (i, _) = either(i)?;
    alternate(either, tag_block)(i)
}

/// Alternate between two parsers to produce a list of elements
/// , but allows a zero width separator
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
/// fn parser(s: &str) -> IResult<&str, Vec<&str>> {
///     alternate(either, tag("{{Change}}"))(s)
/// }
///
/// assert_eq!(parser("{{Change}}junk{{Change}}more junk{{Change}}"), Ok(("", vec!["{{Change}}", "{{Change}}", "{{Change}}"])));
/// assert_eq!(parser("{{Change}}junk{{Change}}more junk{{Change}}{{Change}}"), Ok(("", vec!["{{Change}}", "{{Change}}", "{{Change}}", "{{Change}}"])));
/// ```
pub fn alternate<I, O, O2, E, F, G>(mut sep: G, mut f: F) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
where
    I: Clone + InputLength,
    F: Parser<I, O, E>,
    G: Parser<I, O2, E>,
    E: ParseError<I> + core::fmt::Debug,
{
    move |mut i: I| {
        let mut res = Vec::new();

        loop {
            let len = i.input_len();
            match f.parse(i.clone()) {
                Err(Err::Error(_)) => return Ok((i, res)),
                Err(e) => return Err(e),
                Ok((i1, o)) => {
                    res.push(o);
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
            find_blocks("{{AvgFreq}}{{/AvgFreq}}{{Change}}{{/Change}}"),
            Ok(("", vec![("AvgFreq", "", ""), ("Change", "", "")]))
        );

        assert_eq!(
            find_blocks("{{Change Weekly}}Weekly change of {{fig}}.{{/Change}} junk {{Change Daily}}Daily change of {{fig}}.{{/Change}} more junk {{AvgFreq Monthly}} compared to last month where {{fig}} was observed.{{/AvgFreq}} final junk"),
            Ok((" final junk", vec![("Change", " Weekly", "Weekly change of {{fig}}."), ("Change", " Daily", "Daily change of {{fig}}."), ("AvgFreq", " Monthly", " compared to last month where {{fig}} was observed.")]))
        );

        assert_eq!(
            find_blocks("junk {{Change}}{{/Change}}"),
            Ok(("", vec![("Change", "", "")]))
        );
    }
}
