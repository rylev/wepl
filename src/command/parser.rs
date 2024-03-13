use std::collections::VecDeque;

use crate::command::tokenizer::TokenKind;

use super::tokenizer::Token;

#[derive(Debug, PartialEq)]
pub enum Line<'a> {
    Expr(Expr<'a>),
    BuiltIn(BuiltIn<'a>),
    Assignment(&'a str, Expr<'a>),
}

impl<'a> Line<'a> {
    pub fn parse(mut tokens: VecDeque<Token<'a>>) -> Result<Line<'a>, ParserError<'a>> {
        let result = match BuiltIn::try_parse(&mut tokens)? {
            Some(builtin) => Ok(Self::BuiltIn(builtin)),
            None => match Self::try_parse_assignment(&mut tokens)? {
                Some((ident, expr)) => Ok(Self::Assignment(ident, expr)),
                None => match Expr::try_parse(&mut tokens)? {
                    Some(e) => Ok(Self::Expr(e)),
                    None => {
                        return match tokens.front() {
                            Some(t) => Err(ParserError::UnexpectedToken(*t)),
                            None => Err(ParserError::UnexpectedEndOfInput),
                        }
                    }
                },
            },
        };
        if !tokens.is_empty() {
            return Err(ParserError::RemainingInput);
        }
        result
    }

    fn try_parse_assignment(
        tokens: &mut VecDeque<Token<'a>>,
    ) -> Result<Option<(&'a str, Expr<'a>)>, ParserError<'a>> {
        let Some(token) = tokens.front() else {
            return Ok(None);
        };
        let TokenKind::Ident(ident) = token.token() else {
            return Ok(None);
        };
        let token = *token;
        let _ = tokens.pop_front();
        if matches!(tokens.front().map(|t| t.token()), Some(TokenKind::Equal)) {
            let _ = tokens.pop_front();
            match Expr::try_parse(tokens)? {
                Some(e) => Ok(Some((ident, e))),
                None => Err(ParserError::ExpectedExpr),
            }
        } else {
            tokens.push_front(token);
            Ok(None)
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct BuiltIn<'a> {
    pub name: &'a str,
    pub rest: Vec<Token<'a>>,
}

impl<'a> BuiltIn<'a> {
    fn try_parse(tokens: &mut VecDeque<Token<'a>>) -> Result<Option<BuiltIn<'a>>, ParserError<'a>> {
        let Some(TokenKind::Builtin(ident)) = tokens.front().map(|t| t.token()) else {
            return Ok(None);
        };
        tokens.pop_front();
        Ok(Some(BuiltIn {
            name: ident,
            rest: tokens.drain(..).collect(),
        }))
    }
}

#[derive(Debug, PartialEq)]
pub enum ParserError<'a> {
    UnexpectedToken(Token<'a>),
    UnexpectedEndOfInput,
    RemainingInput,
    ExpectedExpr,
}

impl std::error::Error for ParserError<'_> {}

impl std::fmt::Display for ParserError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::UnexpectedToken(_) => f.write_str("unexpected token"),
            ParserError::UnexpectedEndOfInput => f.write_str("unexpected end of input"),
            ParserError::RemainingInput => f.write_str("remaining input"),
            ParserError::ExpectedExpr => f.write_str("unexpected expression"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    FunctionCall(FunctionCall<'a>),
    Ident(&'a str),
    Literal(Literal<'a>),
}

impl<'a> Expr<'a> {
    fn try_parse(input: &mut VecDeque<Token<'a>>) -> Result<Option<Expr<'a>>, ParserError<'a>> {
        let Some(first) = input.front() else {
            return Ok(None);
        };
        match first.token() {
            TokenKind::String(s) => {
                input.pop_front();
                Ok(Some(Expr::Literal(Literal::String(s))))
            }
            TokenKind::Number(n) => {
                input.pop_front();
                Ok(Some(Expr::Literal(Literal::Number(n))))
            }
            TokenKind::OpenBracket => {
                input.pop_front();
                enum State {
                    ExpectExpr,
                    ExpectComma,
                }
                let mut state = State::ExpectExpr;
                let mut items = vec![];
                while let Some(token) = input.front() {
                    match token.token() {
                        TokenKind::ClosedBracket => {
                            input.pop_front();
                            return Ok(Some(Expr::Literal(Literal::List(List { items }))));
                        }
                        TokenKind::Comma if matches!(state, State::ExpectComma) => {
                            input.pop_front();
                            state = State::ExpectExpr;
                        }
                        _ => {
                            let expr = Expr::try_parse(input)?;
                            if let Some(expr) = expr {
                                items.push(expr);
                                state = State::ExpectComma;
                            } else {
                                return Err(ParserError::UnexpectedEndOfInput);
                            }
                        }
                    }
                }
                return Err(ParserError::UnexpectedEndOfInput);
            }
            TokenKind::OpenBrace => {
                input.pop_front();
                enum State<'a> {
                    ExpectIdent,
                    ExpectColon(&'a str),
                    ExpectExpr(&'a str),
                    ExpectComma,
                }
                let mut state = State::ExpectIdent;
                let mut fields = vec![];
                while let Some(token) = input.front() {
                    match (token.token(), state) {
                        (TokenKind::ClosedBrace, State::ExpectComma | State::ExpectIdent) => {
                            input.pop_front();
                            return Ok(Some(Expr::Literal(Literal::Record(Record { fields }))));
                        }
                        (TokenKind::Comma, State::ExpectComma) => {
                            input.pop_front();
                            state = State::ExpectIdent;
                        }
                        (TokenKind::Colon, State::ExpectColon(ident)) => {
                            input.pop_front();
                            state = State::ExpectExpr(ident);
                        }
                        (_, State::ExpectIdent) => {
                            let ident = Literal::parse_ident(input)?;
                            state = State::ExpectColon(ident);
                        }
                        (_, State::ExpectExpr(ident)) => {
                            let expr = Expr::try_parse(input)?;
                            if let Some(expr) = expr {
                                fields.push((ident, expr));
                                state = State::ExpectComma;
                            } else {
                                return Err(ParserError::UnexpectedEndOfInput);
                            }
                        }
                        _ => return Err(ParserError::UnexpectedToken(*token)),
                    }
                }
                return Err(ParserError::UnexpectedEndOfInput);
            }
            TokenKind::Ident(_) => {
                let func = FunctionCall::try_parse(input)?;
                match func {
                    Some(f) => Ok(Some(Expr::FunctionCall(f))),
                    None => Ok(Some(Expr::Ident(Literal::parse_ident(input)?))),
                }
            }

            _ => return Ok(None),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionCall<'a> {
    pub ident: ItemIdent<'a>,
    pub args: Vec<Expr<'a>>,
}

impl<'a> FunctionCall<'a> {
    fn try_parse(input: &mut VecDeque<Token<'a>>) -> Result<Option<Self>, ParserError<'a>> {
        let original = input.clone();
        let Some(function_ident) = ItemIdent::try_parse(input)? else {
            return Ok(None);
        };
        let next = input.front();
        if next.map(|t| t.token()) != Some(TokenKind::OpenParen) {
            // If we failed to find an open paren then we need to completely bail
            // on function parsing which means restoring the input state back to
            // its original form.
            *input = original;
            return Ok(None);
        }
        expect_token(input, |t| t == TokenKind::OpenParen)?;
        let mut args = Vec::new();
        loop {
            let Some(expr) = Expr::try_parse(input)? else {
                break;
            };
            args.push(expr);
            if input.front().map(|t| t.token()) != Some(TokenKind::Comma) {
                break;
            }
        }
        expect_token(input, |t| t == TokenKind::ClosedParen)?;
        Ok(Some(FunctionCall {
            ident: function_ident,
            args,
        }))
    }
}

fn expect_token<'a>(
    input: &mut VecDeque<Token<'a>>,
    pred: impl FnOnce(TokenKind<'a>) -> bool,
) -> Result<(), ParserError<'a>> {
    let Some(token) = input.pop_front() else {
        return Err(ParserError::UnexpectedEndOfInput);
    };
    if !pred(token.token()) {
        return Err(ParserError::UnexpectedToken(token));
    }
    Ok(())
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Ident<'a> {
    Item(ItemIdent<'a>),
    Interface(InterfaceIdent<'a>),
}

impl<'a> Ident<'a> {
    pub(crate) fn try_parse(
        input: &mut VecDeque<Token<'a>>,
    ) -> Result<Option<Ident<'a>>, ParserError<'a>> {
        match ItemIdent::try_parse(input)? {
            Some(i) => Ok(Some(Self::Item(i))),
            None => Ok(InterfaceIdent::try_parse(input)?.map(Self::Interface)),
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct ItemIdent<'a> {
    pub interface: Option<InterfaceIdent<'a>>,
    pub item: &'a str,
}

impl<'a> ItemIdent<'a> {
    fn try_parse(input: &mut VecDeque<Token<'a>>) -> Result<Option<Self>, ParserError<'a>> {
        let interface = InterfaceIdent::try_parse(input)?;
        match interface {
            Some(i) if i.package.is_none() => {
                if input.front().map(|t| t.token()) == Some(TokenKind::Hash) {
                    input.pop_front();
                    let ident = Literal::parse_ident(input)?;
                    Ok(Some(ItemIdent {
                        interface: Some(i),
                        item: ident,
                    }))
                } else {
                    // We parsed the function ident as the interface ident
                    // Map the interface ident to the function ident
                    Ok(Some(ItemIdent {
                        interface: None,
                        item: i.interface,
                    }))
                }
            }
            Some(i) => {
                // if we parse an interface id with a full package, we must
                // be expecting a `#` next with the function ident
                match input.pop_front() {
                    Some(t) if t.token() == TokenKind::Hash => {
                        let ident = Literal::parse_ident(input)?;
                        Ok(Some(ItemIdent {
                            interface: Some(i),
                            item: ident,
                        }))
                    }
                    Some(t) => Err(ParserError::UnexpectedToken(t)),
                    None => Err(ParserError::UnexpectedEndOfInput),
                }
            }

            None => Ok(None),
        }
    }
}

impl std::fmt::Display for ItemIdent<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(interface) = self.interface {
            write!(f, "{interface}#")?
        }
        write!(f, "{}", self.item)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct InterfaceIdent<'a> {
    package: Option<(&'a str, &'a str)>,
    interface: &'a str,
}

impl<'a> InterfaceIdent<'a> {
    fn try_parse<'b>(input: &'b mut VecDeque<Token<'a>>) -> Result<Option<Self>, ParserError<'a>> {
        #[derive(Debug)]
        enum State<'a> {
            ExpectFirst,
            ExpectColon(&'a str),
            ExpectSecond(&'a str),
            ExpectSlash(&'a str, &'a str),
            ExpectThird(&'a str, &'a str),
        }
        let mut state = State::ExpectFirst;
        loop {
            let token = input.front();
            match (token.map(|t| t.token()), state) {
                (Some(TokenKind::Ident(i)), State::ExpectFirst) => {
                    input.pop_front();
                    state = State::ExpectColon(i);
                }
                (Some(TokenKind::Colon), State::ExpectColon(first)) => {
                    input.pop_front();
                    state = State::ExpectSecond(first);
                }
                (Some(TokenKind::Ident(second)), State::ExpectSecond(first)) => {
                    input.pop_front();
                    state = State::ExpectSlash(first, second);
                }
                (Some(TokenKind::Slash), State::ExpectSlash(first, second)) => {
                    input.pop_front();
                    state = State::ExpectThird(first, second);
                }
                (Some(TokenKind::Ident(third)), State::ExpectThird(first, second)) => {
                    input.pop_front();
                    return Ok(Some(InterfaceIdent {
                        package: Some((first, second)),
                        interface: third,
                    }));
                }
                (_, State::ExpectColon(first)) => {
                    return Ok(Some(InterfaceIdent {
                        package: None,
                        interface: first,
                    }));
                }
                (_, State::ExpectFirst) => return Ok(None),
                (Some(_), _) => return Err(ParserError::UnexpectedToken(*token.unwrap())),
                _ => return Err(ParserError::UnexpectedEndOfInput),
            }
        }
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
    String(&'a str),
    Number(usize),
    List(List<'a>),
    Record(Record<'a>),
}

impl<'a> Literal<'a> {
    fn parse_ident(input: &mut VecDeque<Token<'a>>) -> Result<&'a str, ParserError<'a>> {
        let Some(token) = input.front() else {
            return Err(ParserError::UnexpectedEndOfInput);
        };
        match token.token() {
            TokenKind::Ident(i) => {
                input.pop_front();
                Ok(i)
            }
            _ => Err(ParserError::UnexpectedToken(*token)),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct List<'a> {
    pub items: Vec<Expr<'a>>,
}

impl<'a> From<Vec<Expr<'a>>> for List<'a> {
    fn from(items: Vec<Expr<'a>>) -> Self {
        Self { items }
    }
}

#[derive(Debug, PartialEq)]
pub struct Record<'a> {
    pub fields: Vec<(&'a str, Expr<'a>)>,
}

impl<'a> From<Vec<(&'a str, Expr<'a>)>> for Record<'a> {
    fn from(fields: Vec<(&'a str, Expr<'a>)>) -> Self {
        Self { fields }
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use crate::command::tokenizer::{SpannedStr, TokenKind};

    use super::*;

    fn dummy_spanned_str() -> SpannedStr<'static> {
        SpannedStr { str: "", offset: 0 }
    }

    fn token(kind: TokenKind<'static>) -> Token<'static> {
        Token {
            input: dummy_spanned_str(),
            token: kind,
        }
    }

    fn tokens(tokens: impl IntoIterator<Item = TokenKind<'static>>) -> VecDeque<Token<'static>> {
        tokens.into_iter().map(token).collect()
    }

    fn parse(
        ts: impl IntoIterator<Item = TokenKind<'static>>,
    ) -> Result<Line<'static>, ParserError<'static>> {
        Line::parse(tokens(ts))
    }

    #[test]
    fn parse_string_literals() {
        let line = parse([TokenKind::String("hello-world")]).unwrap();
        assert_eq!(
            line,
            Line::Expr(Expr::Literal(Literal::String("hello-world")))
        );
    }

    #[test]
    fn parse_list_literals() {
        let list_of_string = Line::Expr(Expr::Literal(Literal::List(
            vec![Expr::Literal(Literal::String("hello-world"))].into(),
        )));
        let line = parse([
            TokenKind::OpenBracket,
            TokenKind::String("hello-world"),
            TokenKind::ClosedBracket,
        ])
        .unwrap();
        assert_eq!(line, list_of_string);

        let line = parse([
            TokenKind::OpenBracket,
            TokenKind::String("hello-world"),
            TokenKind::Comma,
            TokenKind::ClosedBracket,
        ])
        .unwrap();
        assert_eq!(line, list_of_string);

        let err = parse([
            TokenKind::OpenBracket,
            TokenKind::String("hello-world"),
            TokenKind::Comma,
        ])
        .unwrap_err();
        assert_eq!(err, ParserError::UnexpectedEndOfInput);
    }

    #[test]
    fn parse_record_literals() {
        let record = Line::Expr(Expr::Literal(Literal::Record(
            vec![("foo", Expr::Literal(Literal::String("bar")))].into(),
        )));
        let line = parse([
            TokenKind::OpenBrace,
            TokenKind::Ident("foo"),
            TokenKind::Colon,
            TokenKind::String("bar"),
            TokenKind::ClosedBrace,
        ])
        .unwrap();
        assert_eq!(line, record);

        let line = parse([
            TokenKind::OpenBrace,
            TokenKind::Ident("foo"),
            TokenKind::Colon,
            TokenKind::String("bar"),
            TokenKind::Comma,
            TokenKind::ClosedBrace,
        ])
        .unwrap();
        assert_eq!(line, record);

        let err = parse([
            TokenKind::OpenBrace,
            TokenKind::Ident("foo"),
            TokenKind::String("bar"),
            TokenKind::ClosedBrace,
        ])
        .unwrap_err();
        assert_eq!(
            err,
            ParserError::UnexpectedToken(token(TokenKind::String("bar")))
        );
    }

    #[test]
    fn parse_function_calls() {
        let function = Line::Expr(Expr::FunctionCall(FunctionCall {
            ident: ItemIdent {
                interface: Some(InterfaceIdent {
                    package: Some(("foo", "bar")),
                    interface: "baz",
                }),
                item: "qux",
            },
            args: vec![],
        }));
        let line = parse([
            TokenKind::Ident("foo"),
            TokenKind::Colon,
            TokenKind::Ident("bar"),
            TokenKind::Slash,
            TokenKind::Ident("baz"),
            TokenKind::Hash,
            TokenKind::Ident("qux"),
            TokenKind::OpenParen,
            TokenKind::ClosedParen,
        ])
        .unwrap();
        assert_eq!(line, function);

        let function = Line::Expr(Expr::FunctionCall(FunctionCall {
            ident: ItemIdent {
                interface: None,
                item: "qux",
            },
            args: vec![],
        }));
        let line = parse([
            TokenKind::Ident("qux"),
            TokenKind::OpenParen,
            TokenKind::ClosedParen,
        ])
        .unwrap();
        assert_eq!(line, function);

        let function = Line::Expr(Expr::FunctionCall(FunctionCall {
            ident: ItemIdent {
                interface: None,
                item: "foo",
            },
            args: vec![Expr::FunctionCall(FunctionCall {
                ident: ItemIdent {
                    interface: None,
                    item: "bar",
                },
                args: vec![],
            })],
        }));
        let line = parse([
            TokenKind::Ident("foo"),
            TokenKind::OpenParen,
            TokenKind::Ident("bar"),
            TokenKind::OpenParen,
            TokenKind::ClosedParen,
            TokenKind::ClosedParen,
        ])
        .unwrap();
        assert_eq!(line, function);

        let err = parse([
            TokenKind::Ident("foo"),
            TokenKind::Colon,
            TokenKind::Ident("bar"),
            TokenKind::OpenParen,
            TokenKind::ClosedParen,
        ])
        .unwrap_err();
        assert_eq!(
            err,
            ParserError::UnexpectedToken(token(TokenKind::OpenParen))
        );

        let err = parse([
            TokenKind::Ident("foo"),
            TokenKind::Colon,
            TokenKind::Ident("bar"),
            TokenKind::Hash,
            TokenKind::Ident("baz"),
            TokenKind::OpenParen,
            TokenKind::ClosedParen,
        ])
        .unwrap_err();
        assert_eq!(err, ParserError::UnexpectedToken(token(TokenKind::Hash)));
    }

    #[test]
    fn parse_ident_expr() {
        let line = parse([TokenKind::Ident("foo")]).unwrap();
        assert_eq!(line, Line::Expr(Expr::Ident("foo")));
    }

    #[test]
    fn parse_builtin() {
        let line = parse([TokenKind::Builtin("foo"), TokenKind::Ident("foo")]).unwrap();
        assert_eq!(
            line,
            Line::BuiltIn(BuiltIn {
                name: "foo",
                rest: vec![token(TokenKind::Ident("foo"))]
            })
        );
    }

    #[test]
    fn parse_assignment() {
        let line = parse([
            TokenKind::Ident("foo"),
            TokenKind::Equal,
            TokenKind::String("bar"),
        ])
        .unwrap();
        assert_eq!(
            line,
            Line::Assignment("foo", Expr::Literal(Literal::String("bar")))
        );
    }
}
