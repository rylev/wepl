use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, multispace0, multispace1};
use nom::combinator::{map, recognize};
use nom::multi::{many0_count, separated_list0};
use nom::sequence::{delimited, pair};
use nom::InputTakeAtPosition;

pub fn builtin(input: &str) -> nom::IResult<&str, (&str, Vec<&str>)> {
    let (rest, ident) = ident(input)?;
    let (rest, args) = separated_list0(multispace1, builtin_argument)(rest)?;

    Ok((rest, (ident, args)))
}

pub fn function_call(input: &str) -> nom::IResult<&str, (&str, Vec<String>)> {
    let (rest, ident) = ident(input)?;
    let args = separated_list0(tag(","), function_argument);
    let (rest, args) = delimited(tag("("), args, tag(")"))(rest)?;

    Ok((rest, (ident, args)))
}

fn function_argument(input: &str) -> nom::IResult<&str, String> {
    let input = input.trim();
    let (rest, result) = alt((
        map(delimited(tag("\""), anything_but_quote, tag("\"")), |r| {
            format!("\"{r}\"")
        }),
        map(anything_but_comma, |s| s.to_owned()),
    ))(input)?;
    Ok((rest, result))
}

fn builtin_argument(input: &str) -> nom::IResult<&str, &str> {
    alt((
        delimited(tag("\""), anything_but_quote, tag("\"")),
        anything_but_space,
    ))(input)
}

fn anything_but_quote(input: &str) -> nom::IResult<&str, &str> {
    input.split_at_position_complete(|c| c == '"')
}

/// Anything that is not whitespace
fn anything_but_space(input: &str) -> nom::IResult<&str, &str> {
    input.split_at_position_complete(char::is_whitespace)
}

/// Anything that is not whitespace
fn anything_but_comma(input: &str) -> nom::IResult<&str, &str> {
    input.split_at_position_complete(|c| c == ',')
}

pub fn ident(input: &str) -> nom::IResult<&str, &str> {
    let ident_parser = recognize(pair(alpha1, many0_count(alt((alpha1, tag("-"))))));
    delimited(multispace0, ident_parser, multispace0)(input)
}
