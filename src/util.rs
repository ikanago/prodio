use crate::{Token, TokenKind};
use std::fmt;

/// Struct to have location of code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Loc(pub usize, pub usize);

impl Loc {
    /// Function to merge two Loc.
    pub fn merge(&self, other: &Loc) -> Loc {
        use std::cmp::{max, min};
        Loc(min(self.0, other.0), max(self.1, other.1))
    }
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.0, self.1)
    }
}

/// Struct to hold value and location.
/// `value` will be Token or AST node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Annotation<T> {
    pub value: T,
    pub loc: Loc,
}

impl<T> Annotation<T> {
    /// Construct new `Annotation`
    pub fn new(value: T, loc: Loc) -> Self {
        Self { value, loc }
    }
}

// ---ERROR DEFINITION---
/// Data type that represents lexical error.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LexErrorKind {
    InvalidChar(char),
    Eof,
}

pub type LexError = Annotation<LexErrorKind>;

impl LexError {
    /// Unacceptable character.
    pub fn invalid_char(c: char, loc: Loc) -> Self {
        LexError::new(LexErrorKind::InvalidChar(c), loc)
    }

    pub fn eof(loc: Loc) -> Self {
        LexError::new(LexErrorKind::Eof, loc)
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let loc = &self.loc;
        match self.value {
            LexErrorKind::InvalidChar(c) => write!(f, "{}: Invalid character '{}'", loc, c),
            LexErrorKind::Eof => write!(f, "End of file"),
        }
    }
}

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
            ParseError::UnexpectedToken(expected, actual) => {
                write!(f, "{}: Unexpected token '{}', expected {}", actual.loc, actual.value, expected)
            }
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
