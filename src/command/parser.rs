use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, digit1, multispace0, multispace1};
use nom::combinator::{cut, map, map_res, recognize};
use nom::multi::{many0_count, separated_list0};
use nom::sequence::{delimited, pair};
use nom::InputTakeAtPosition;

#[derive(Debug, PartialEq)]
pub enum Line<'a> {
    Builtin(&'a str, Vec<&'a str>),
    Expr(Expr<'a>),
    Assignment(&'a str, Expr<'a>),
}

impl<'a> Line<'a> {
    pub fn parse(input: &str) -> nom::IResult<&str, Line> {
        alt((
            map(builtin, |(name, args)| Line::Builtin(name, args)),
            map(assignment, |(ident, expr)| Line::Assignment(ident, expr)),
            map(Expr::parse, Line::Expr),
        ))(input)
    }
}

pub fn builtin(input: &str) -> nom::IResult<&str, (&str, Vec<&str>)> {
    alt((builtin_call, special_char))(input)
}

pub fn builtin_call(input: &str) -> nom::IResult<&str, (&str, Vec<&str>)> {
    let (rest, _) = tag(".")(input)?;
    let (rest, ident) = ident(rest)?;
    if rest.is_empty() {
        return Ok(("", (ident, Vec::new())));
    }
    let (rest, args) = separated_list0(multispace1, builtin_argument)(rest)?;

    Ok((rest, (ident, args)))
}
pub fn special_char(input: &str) -> nom::IResult<&str, (&str, Vec<&str>)> {
    let (rest, _) = tag("?")(input)?;
    let (rest, args) = separated_list0(multispace1, builtin_argument)(rest)?;
    Ok((rest, ("inspect", args)))
}

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    Literal(Literal<'a>),
    Ident(&'a str),
    FunctionCall(&'a str, Vec<Expr<'a>>),
}

impl<'a> Expr<'a> {
    pub fn parse(input: &str) -> nom::IResult<&str, Expr> {
        alt((
            map(Literal::parse, Expr::Literal),
            map(function_call, |(name, args)| Expr::FunctionCall(name, args)),
            map(ident, Expr::Ident),
        ))(input)
    }
}

#[derive(Debug, PartialEq)]
pub enum Literal<'a> {
    String(&'a str),
    Num(usize),
}

impl<'a> Literal<'a> {
    pub fn parse(input: &str) -> nom::IResult<&str, Literal> {
        alt((
            map(string_literal, Literal::String),
            map(map_res(digit1, str::parse), Literal::Num),
        ))(input)
    }
}

fn assignment<'a>(input: &'a str) -> nom::IResult<&str, (&str, Expr<'a>)> {
    let (rest, ident) = ident(input)?;
    let (rest, _) = delimited(multispace0, tag("="), multispace0)(rest)?;
    let (r, value) = cut(Expr::parse)(rest)?;
    Ok((r, (ident, value)))
}

pub fn function_call<'a>(input: &'a str) -> nom::IResult<&str, (&str, Vec<Expr<'a>>)> {
    let (rest, ident) = ident(input)?;
    let args = separated_list0(tag(","), Expr::parse);
    let (rest, args) = delimited(tag("("), args, tag(")"))(rest)?;

    Ok((rest, (ident, args)))
}

fn string_literal(input: &str) -> nom::IResult<&str, &str> {
    delimited(tag("\""), anything_but_quote, tag("\""))(input)
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

pub fn ident(input: &str) -> nom::IResult<&str, &str> {
    let ident_parser = recognize(pair(alpha1, many0_count(alt((alpha1, tag("-"))))));
    delimited(multispace0, ident_parser, multispace0)(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn function_call() {
        let input = r#"my-func(my-other-func("arg"))"#;
        let result = Line::parse(input);
        assert_eq!(
            result,
            Ok((
                "",
                Line::Expr(Expr::FunctionCall(
                    "my-func",
                    vec![Expr::FunctionCall(
                        "my-other-func",
                        vec![Expr::Literal(Literal::String("arg"))]
                    )]
                ))
            ))
        );
    }

    #[test]
    fn builtin() {
        let input = r#".foo bar baz"#;
        let result = Line::parse(input);
        assert_eq!(result, Ok(("", Line::Builtin("foo", vec!["bar", "baz",]))));
    }

    #[test]
    fn builtin_no_args() {
        let input = r#".foo"#;
        let result = Line::parse(input);
        assert_eq!(result, Ok(("", Line::Builtin("foo", vec![]))));
    }

    #[test]
    fn assignment() {
        let input = r#"x = "wow""#;
        let result = Line::parse(input);
        assert_eq!(
            result,
            Ok((
                "",
                Line::Assignment("x", Expr::Literal(Literal::String("wow")))
            ))
        );
    }

    #[test]
    fn nonsense_assignment() {
        let input = r#"x = %&*"#;
        let result = Line::parse(input);
        assert!(matches!(result, Err(nom::Err::Failure(_))));
    }
}
