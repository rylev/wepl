use std::ops::Range;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, digit1, multispace0, multispace1};
use nom::combinator::{cut, map, map_res, recognize};
use nom::multi::{many0_count, separated_list0};
use nom::sequence::{delimited, pair, preceded};
use nom::InputTakeAtPosition;

#[derive(Debug, PartialEq)]
pub enum Line<'a> {
    Builtin(SpannedStr<'a>, Vec<SpannedStr<'a>>),
    Expr(Expr<'a>),
    Assignment(SpannedStr<'a>, Expr<'a>),
}

impl<'a> Line<'a> {
    pub fn parse(input: &'a str) -> nom::IResult<&str, Line> {
        let input = Span::new(input);
        alt((
            map(builtin, |(name, args)| {
                Line::Builtin(name.into(), args.into_iter().map(|s| s.into()).collect())
            }),
            map(assignment, |(ident, expr)| {
                Line::Assignment(ident.into(), expr)
            }),
            map(Expr::parse, Line::Expr),
        ))(input)
        .map_err(|e| e.map(|e| nom::error::Error::new(*e.input.fragment(), e.code)))
        .map(|(rest, result)| (*rest.fragment(), result))
    }
}

/// Used to collect span information during parsing
type Span<'a> = nom_locate::LocatedSpan<&'a str>;

pub fn builtin(input: Span) -> nom::IResult<Span, (Span, Vec<Span>)> {
    alt((builtin_call, special_char))(input)
}

pub fn builtin_call(input: Span) -> nom::IResult<Span, (Span, Vec<Span>)> {
    let (rest, _) = tag(".")(input)?;
    let (rest, ident) = ident(rest)?;
    if rest.is_empty() {
        return Ok((rest, (ident, Vec::new())));
    }
    let (rest, args) = separated_list0(multispace1, builtin_argument)(rest)?;

    Ok((rest, (ident, args)))
}

pub fn special_char(input: Span) -> nom::IResult<Span, (Span, Vec<Span>)> {
    let (rest, _) = tag("?")(input)?;
    let (rest, args) = separated_list0(multispace1, builtin_argument)(rest)?;
    if rest.is_empty() {
        return Ok((rest, (Span::new("help"), Vec::new())));
    }
    Ok((rest, (Span::new("help"), args)))
}

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    Literal(Literal<'a>),
    Ident(SpannedStr<'a>),
    FunctionCall(FunctionIdent<'a>, Vec<Expr<'a>>),
}

impl<'a> Expr<'a> {
    pub fn parse(input: Span) -> nom::IResult<Span, Expr> {
        alt((
            map(function_call, |(name, args)| {
                Expr::FunctionCall(name.into(), args)
            }),
            map(ident, |i| Expr::Ident(i.into())),
            map(Literal::parse, Expr::Literal),
        ))(input)
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ItemIdent<'a> {
    Function(FunctionIdent<'a>),
    Interface(InterfaceIdent<'a>),
}

impl<'a> ItemIdent<'a> {
    pub fn parse(input: Span<'a>) -> nom::IResult<Span<'a>, ItemIdent<'a>> {
        alt((
            map(InterfaceIdent::parse, |i| ItemIdent::Interface(i)),
            map(FunctionIdent::parse, |f| ItemIdent::Function(f)),
        ))(input)
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct FunctionIdent<'a> {
    pub interface: Option<InterfaceIdent<'a>>,
    pub function: SpannedStr<'a>,
}

impl<'a> FunctionIdent<'a> {
    pub fn parse(input: Span<'a>) -> nom::IResult<Span<'a>, FunctionIdent<'a>> {
        fn with_interface(input: Span<'_>) -> nom::IResult<Span<'_>, FunctionIdent<'_>> {
            let (rest, interface) = InterfaceIdent::parse(input)?;
            let (rest, _) = tag("#")(rest)?;
            let (rest, function) = cut(ident)(rest)?;
            Ok((
                rest,
                FunctionIdent {
                    interface: Some(interface),
                    function: function.into(),
                },
            ))
        }
        alt((
            with_interface,
            map(ident, |f| Self {
                interface: None,
                function: f.into(),
            }),
        ))(input)
    }
}

impl std::fmt::Display for FunctionIdent<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(interface) = self.interface {
            write!(f, "{interface}#")?
        }
        write!(f, "{}", self.function)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct InterfaceIdent<'a> {
    package: Option<(SpannedStr<'a>, SpannedStr<'a>)>,
    interface: SpannedStr<'a>,
}

impl<'a> InterfaceIdent<'a> {
    fn parse(input: Span<'a>) -> nom::IResult<Span, Self> {
        fn prefixed<'a>(input: Span<'a>) -> nom::IResult<Span, InterfaceIdent<'a>> {
            let (rest, namespace) = ident(input)?;
            let (rest, _) = tag(":")(rest)?;
            let (rest, package) = cut(ident)(rest)?;
            let (rest, _) = cut(tag("/"))(rest)?;
            let (rest, interface) = cut(ident)(rest)?;
            Ok((
                rest,
                InterfaceIdent {
                    package: Some((namespace.into(), package.into())),
                    interface: interface.into(),
                },
            ))
        }

        alt((
            prefixed,
            map(ident, |i| Self {
                package: None,
                interface: i.into(),
            }),
        ))(input)
    }
}

impl std::fmt::Display for InterfaceIdent<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some((namespace, package)) = self.package {
            write!(f, "{namespace}:{package}/")?;
        }
        write!(f, "{}", self.interface)
    }
}

#[derive(Debug, PartialEq)]
pub enum Literal<'a> {
    Record(Record<'a>),
    List(List<'a>),
    String(SpannedStr<'a>),
    Num(usize),
}

impl<'a> Literal<'a> {
    pub fn parse(input: Span) -> nom::IResult<Span, Literal> {
        alt((
            map(number, Literal::Num),
            map(Record::parse, Literal::Record),
            map(List::parse, Literal::List),
            map(string_literal, |s| Literal::String(s.into())),
        ))(input)
    }
}

#[derive(Debug, PartialEq)]
pub struct List<'a> {
    pub items: Vec<Expr<'a>>,
}

impl<'a> List<'a> {
    fn parse(input: Span<'a>) -> nom::IResult<Span, Self> {
        fn item(input: Span) -> nom::IResult<Span, Expr<'_>> {
            delimited(multispace0, Expr::parse, multispace0)(input)
        }
        let (rest, _) = tag("[")(input)?;
        let (rest, items) = cut(separated_list0(tag(","), item))(rest)?;
        let (rest, _) = cut(tag("]"))(rest)?;
        Ok((rest, Self { items }))
    }
}

fn number(input: Span) -> nom::IResult<Span, usize> {
    fn parse(input: Span) -> Result<usize, std::num::ParseIntError> {
        input.fragment().parse()
    }
    map_res(digit1, parse)(input)
}

#[derive(Debug, PartialEq)]
pub struct Record<'a> {
    pub fields: Vec<(SpannedStr<'a>, Expr<'a>)>,
}

impl<'a> Record<'a> {
    fn parse(input: Span<'a>) -> nom::IResult<Span, Self> {
        fn field(input: Span) -> nom::IResult<Span, (Span, Expr<'_>)> {
            let (rest, name) = preceded(multispace0, ident)(input)?;
            let (rest, _) = tag(":")(rest)?;
            let (rest, expr) = delimited(multispace0, Expr::parse, multispace0)(rest)?;
            Ok((rest, (name, expr)))
        }
        let (rest, _) = tag("{")(input)?;
        let (rest, fields) = cut(separated_list0(tag(","), field))(rest)?;
        let fields = fields.into_iter().map(|(f, e)| (f.into(), e)).collect();
        let (rest, _) = cut(tag("}"))(rest)?;
        Ok((rest, Self { fields }))
    }
}

fn assignment(input: Span) -> nom::IResult<Span, (Span, Expr<'_>)> {
    let (rest, ident) = ident(input)?;
    let (rest, _) = delimited(multispace0, tag("="), multispace0)(rest)?;
    let (r, value) = cut(Expr::parse)(rest)?;
    Ok((r, (ident, value)))
}

pub fn function_call(input: Span) -> nom::IResult<Span, (FunctionIdent<'_>, Vec<Expr<'_>>)> {
    let (rest, ident) = FunctionIdent::parse(input)?;
    let (rest, _) = tag("(")(rest)?;
    let (rest, args) = cut(separated_list0(tag(","), Expr::parse))(rest)?;
    let (rest, _) = cut(tag(")"))(rest)?;

    Ok((rest, (ident, args)))
}

fn string_literal(input: Span) -> nom::IResult<Span, Span> {
    delimited(tag("\""), anything_but_quote, tag("\""))(input)
}

fn builtin_argument(input: Span) -> nom::IResult<Span, Span> {
    alt((
        delimited(tag("\""), anything_but_quote, tag("\"")),
        anything_but_space,
    ))(input)
}

fn anything_but_quote(input: Span) -> nom::IResult<Span, Span> {
    input.split_at_position_complete(|c| c == '"')
}

/// Anything that is not whitespace
fn anything_but_space(input: Span) -> nom::IResult<Span, Span> {
    input.split_at_position_complete(char::is_whitespace)
}

pub fn ident(input: Span) -> nom::IResult<Span, Span> {
    let ident_parser = recognize(pair(alpha1, many0_count(alt((alpha1, tag("-"))))));
    delimited(multispace0, ident_parser, multispace0)(input)
}

/// A view into the input str with span information
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct SpannedStr<'a> {
    str: &'a str,
    offset: usize,
}

impl<'a> SpannedStr<'a> {
    pub fn as_str(&self) -> &'a str {
        self.str
    }

    pub fn range(&self) -> Range<usize> {
        self.offset..(self.offset + self.str.len())
    }
}

impl<'a> PartialEq<&str> for SpannedStr<'a> {
    fn eq(&self, other: &&str) -> bool {
        self.str == *other
    }
}

impl std::fmt::Display for SpannedStr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.str)
    }
}

impl From<SpannedStr<'_>> for String {
    fn from(value: SpannedStr<'_>) -> Self {
        value.str.to_owned()
    }
}

impl<'a> std::ops::Deref for SpannedStr<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.str
    }
}

impl<'a> From<Span<'a>> for SpannedStr<'a> {
    fn from(span: Span<'a>) -> Self {
        Self {
            str: span.fragment(),
            offset: span.location_offset(),
        }
    }
}

impl<'a> From<(&'a str, usize)> for SpannedStr<'a> {
    fn from((str, offset): (&'a str, usize)) -> Self {
        Self { str, offset }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn function_call() {
        let input = r#"my-func(my-other-func("arg"))"#;
        let (rest, result) = Line::parse(input).unwrap();
        assert!(rest.is_empty());
        assert_eq!(
            result,
            Line::Expr(Expr::FunctionCall(
                FunctionIdent {
                    interface: None,
                    function: ("my-func", 0).into()
                },
                vec![Expr::FunctionCall(
                    FunctionIdent {
                        interface: None,
                        function: ("my-other-func", 8).into()
                    },
                    vec![Expr::Literal(Literal::String(("arg", 23).into()))]
                )]
            ))
        );
    }

    #[test]
    fn namespaced_interface_function_call() {
        let input = r#"wasi:cli/terminal-stderr#my-func()"#;
        let (rest, result) = Line::parse(input).unwrap();
        assert!(rest.is_empty());
        assert_eq!(
            result,
            Line::Expr(Expr::FunctionCall(
                FunctionIdent {
                    interface: Some(InterfaceIdent {
                        package: Some((("wasi", 0).into(), ("cli", 5).into())),
                        interface: ("terminal-stderr", 9).into()
                    }),
                    function: ("my-func", 25).into()
                },
                vec![]
            ))
        );
    }

    #[test]
    fn interface_function_call() {
        let input = r#"terminal-stderr#my-func()"#;
        let (rest, result) = Line::parse(input).unwrap();
        assert!(rest.is_empty());
        assert_eq!(
            result,
            Line::Expr(Expr::FunctionCall(
                FunctionIdent {
                    interface: Some(InterfaceIdent {
                        package: None,
                        interface: ("terminal-stderr", 0).into()
                    }),
                    function: ("my-func", 16).into()
                },
                vec![]
            ))
        );
    }

    #[test]
    fn function_call_bad_args() {
        let input = r#"my-func(%^&)"#;
        let result = Line::parse(input);
        assert!(matches!(result, Err(nom::Err::Failure(_))));
    }

    #[test]
    fn function_call_with_record() {
        let input = r#"my-func({n:1,  name:    err("string")  })"#;
        let (rest, result) = Line::parse(input).unwrap();
        assert!(rest.is_empty());

        assert_eq!(
            result,
            Line::Expr(Expr::FunctionCall(
                FunctionIdent {
                    interface: None,
                    function: ("my-func", 0).into()
                },
                vec![Expr::Literal(Literal::Record(Record {
                    fields: vec![
                        (("n", 9).into(), Expr::Literal(Literal::Num(1))),
                        (
                            ("name", 15).into(),
                            Expr::FunctionCall(
                                FunctionIdent {
                                    interface: None,
                                    function: ("err", 24).into()
                                },
                                vec![Expr::Literal(Literal::String(("string", 29).into()))]
                            )
                        )
                    ]
                }))]
            ))
        );
    }

    #[test]
    fn builtin() {
        let input = r#".foo bar baz"#;
        let (rest, result) = Line::parse(input).unwrap();
        assert!(rest.is_empty());
        assert_eq!(
            result,
            Line::Builtin(
                ("foo", 1).into(),
                vec![("bar", 5).into(), ("baz", 9).into(),]
            )
        );
    }

    #[test]
    fn builtin_no_args() {
        let input = r#".foo"#;
        let result = Line::parse(input);
        assert_eq!(
            result,
            Ok(("".into(), Line::Builtin(("foo", 1).into(), vec![])))
        );
    }

    #[test]
    fn assignment() {
        let input = r#"x = "wow""#;
        let (rest, result) = Line::parse(input).unwrap();
        assert!(rest.is_empty());

        assert_eq!(
            result,
            Line::Assignment(
                ("x", 0).into(),
                Expr::Literal(Literal::String(("wow", 5).into()))
            )
        );
    }

    #[test]
    fn ident_line() {
        let input = r#"hello-world"#;
        let (rest, result) = Line::parse(input).unwrap();
        assert!(rest.is_empty());
        assert_eq!(result, Line::Expr(Expr::Ident(("hello-world", 0).into())));
    }

    #[test]
    fn nonsense_assignment() {
        let input = r#"x = %&*"#;
        let result = Line::parse(input);
        assert!(matches!(result, Err(nom::Err::Failure(_))));
    }
}
