pub mod parser;

use crate::token::{Token, TokenKind};
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ParseError {
    UnexpectedToken(TokenKind, Token),
    NotExpression(Token),
    NotOperator(Token),
    UnclosedOpenParen(Token),
    RedundantExpression(Token),
    NoSemicolon(Token),
    Eof,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken(expected, actual) => write!(
                f,
                "{}: Unexpected token '{}', expected {}",
                actual.loc, actual.value, expected
            ),
            ParseError::NotExpression(t) => write!(f, "{}: Not expression '{}'", t.loc, t.value),
            ParseError::NotOperator(t) => write!(f, "{}: Not operator '{}'", t.loc, t.value),
            ParseError::UnclosedOpenParen(t) => {
                write!(f, "{}: Unclosed open parenthesis '{}'", t.loc, t.value)
            }
            ParseError::RedundantExpression(t) => {
                write!(f, "{}: Redundant expression '{}'", t.loc, t.value)
            }
            ParseError::NoSemicolon(t) => write!(f, "{}: Missing semicolon '{:}'", t.loc, t.value),
            ParseError::Eof => write!(f, "End of file"),
        }
    }
}
