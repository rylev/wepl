use std::{collections::VecDeque, fmt::Write, ops::Deref};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Token<'a> {
    pub input: SpannedStr<'a>,
    pub token: TokenKind<'a>,
}

impl<'a> Token<'a> {
    pub fn token(&self) -> TokenKind<'a> {
        self.token
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind<'a> {
    String(&'a str),
    Ident(&'a str),
    Builtin(&'a str),
    Flag(&'a str),
    Number(usize),
    Equal,
    OpenParen,
    ClosedParen,
    Slash,
    Hash,
    Colon,
    OpenBracket,
    ClosedBracket,
    OpenBrace,
    ClosedBrace,
    Comma,
    Period,
}

impl<'a> Token<'a> {
    pub fn tokenize(input: &'a str) -> Result<VecDeque<Token<'a>>, TokenizeError> {
        let mut tokens = VecDeque::new();
        let mut rest = SpannedStr {
            str: input,
            offset: 0,
        };
        while !rest.is_empty() {
            let (new_rest, token) = Token::next(rest)?;
            if let Some(token) = token {
                tokens.push_back(token);
            }
            rest = new_rest;
        }
        Ok(tokens)
    }

    fn next(rest: SpannedStr<'a>) -> Result<(SpannedStr<'a>, Option<Token<'a>>), TokenizeError> {
        let mut chars = rest.chars().peekable();
        let original_offset = rest.offset;
        let Some(first) = chars.next() else {
            panic!("TODO")
        };
        let (offset, token_kind) = match first {
            '"' => {
                let len: usize = chars.take_while(|c| *c != '"').map(|c| c.len_utf8()).sum();
                let offset = 2 * '"'.len_utf8() + len;
                let str = &rest.str[1..(offset - 1)];
                (offset, Some(TokenKind::String(str)))
            }
            c if c.is_ascii_alphabetic() => {
                let len: usize = chars
                    .take_while(|c| c.is_ascii_alphabetic() || *c == '-')
                    .map(|c| c.len_utf8())
                    .sum();
                let offset = c.len_utf8() + len;
                let str = &rest.str[..offset];
                if str.ends_with('-') {
                    return Err(TokenizeError::UnexpectedChar(
                        '-',
                        original_offset + offset - 1,
                    ));
                }
                (offset, Some(TokenKind::Ident(str)))
            }
            c if c.is_ascii_digit() => {
                let len: usize = chars
                    .take_while(|c| c.is_ascii_digit())
                    .map(|c| c.len_utf8())
                    .sum();
                let offset = c.len_utf8() + len;
                let num = rest.str[..offset].parse().expect("TODO");
                (offset, Some(TokenKind::Number(num)))
            }
            c if c.is_whitespace() => (c.len_utf8(), None),
            '=' => ('='.len_utf8(), Some(TokenKind::Equal)),
            '(' => ('('.len_utf8(), Some(TokenKind::OpenParen)),
            ')' => (')'.len_utf8(), Some(TokenKind::ClosedParen)),
            '/' => ('/'.len_utf8(), Some(TokenKind::Slash)),
            '#' => ('/'.len_utf8(), Some(TokenKind::Hash)),
            ':' => ('/'.len_utf8(), Some(TokenKind::Colon)),
            '[' => ('['.len_utf8(), Some(TokenKind::OpenBracket)),
            ']' => (']'.len_utf8(), Some(TokenKind::ClosedBracket)),
            ',' => (','.len_utf8(), Some(TokenKind::Comma)),
            '.' => {
                if matches!(chars.peek(), Some(c) if c.is_alphabetic()) {
                    let len: usize = chars
                        .take_while(|c| c.is_ascii_alphabetic() || *c == '_')
                        .map(|c| c.len_utf8())
                        .sum();
                    let offset = '.'.len_utf8() + len;
                    let ident = &rest.str[1..offset];
                    (offset, Some(TokenKind::Builtin(ident)))
                } else {
                    ('.'.len_utf8(), Some(TokenKind::Period))
                }
            }
            '{' => ('.'.len_utf8(), Some(TokenKind::OpenBrace)),
            '}' => ('.'.len_utf8(), Some(TokenKind::ClosedBrace)),
            '-' if chars.peek() == Some(&'-') => {
                let len: usize = chars
                    .skip(1)
                    .take_while(|c| c.is_ascii_alphabetic() || *c == '_' || *c == '-')
                    .map(|c| c.len_utf8())
                    .sum();
                let offset = '-'.len_utf8() * 2 + len;
                let ident = &rest.str[2..offset];
                (offset, Some(TokenKind::Flag(ident)))
            }
            _ => return Err(TokenizeError::UnexpectedChar(first, original_offset)),
        };
        Ok((
            rest.offset(offset),
            token_kind.map(|token| Token {
                input: SpannedStr {
                    str: &rest.str[..offset],
                    offset: original_offset,
                },
                token,
            }),
        ))
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenizeError {
    UnexpectedChar(char, usize),
}

impl std::error::Error for TokenizeError {}

impl std::fmt::Display for TokenizeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenizeError::UnexpectedChar(char, _) => {
                f.write_str("unexpected character: ")?;
                f.write_char(*char)
            }
        }
    }
}

/// A view into the input str with span information
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct SpannedStr<'a> {
    pub str: &'a str,
    pub offset: usize,
}

impl<'a> SpannedStr<'a> {
    fn offset(self, offset: usize) -> SpannedStr<'a> {
        Self {
            str: &self.str[offset..],
            offset: self.offset + offset,
        }
    }
}

impl Deref for SpannedStr<'_> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.str
    }
}

#[cfg(test)]
mod tests {
    pub use super::*;
    #[test]
    fn tokenize_literals() {
        let input = r#"  "hello-world" "#;
        let tokens = Token::tokenize(input).unwrap();
        assert_eq!(tokens.len(), 1);
        assert_eq!(
            tokens[0],
            Token {
                input: SpannedStr {
                    str: r#""hello-world""#,
                    offset: 2
                },
                token: TokenKind::String("hello-world")
            }
        );

        let input = "  hello ";
        let tokens = Token::tokenize(input).unwrap();
        assert_eq!(tokens.len(), 1);
        assert_eq!(
            tokens[0],
            Token {
                input: SpannedStr {
                    str: "hello",
                    offset: 2
                },
                token: TokenKind::Ident("hello")
            }
        );
    }

    #[test]
    fn tokenize_ident() {
        let input = "  hello- ";
        let err = Token::tokenize(input).unwrap_err();
        assert_eq!(err, TokenizeError::UnexpectedChar('-', 7))
    }

    #[test]
    fn tokenize_assignment() {
        let input = r#"  hello  = "world"  "#;
        let tokens = Token::tokenize(input)
            .unwrap()
            .into_iter()
            .map(|t| t.token)
            .collect::<Vec<_>>();
        assert_eq!(
            tokens,
            vec![
                TokenKind::Ident("hello"),
                TokenKind::Equal,
                TokenKind::String("world"),
            ]
        )
    }

    #[test]
    fn tokenize_builtin() {
        let input = ".foo hello";
        let tokens = Token::tokenize(input)
            .unwrap()
            .into_iter()
            .map(|t| t.token)
            .collect::<Vec<_>>();
        assert_eq!(
            tokens,
            vec![TokenKind::Builtin("foo"), TokenKind::Ident("hello"),]
        )
    }
}
