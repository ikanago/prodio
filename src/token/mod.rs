pub mod lexer;

use crate::Annotation;
use crate::Loc;
use std::fmt;

/// Data type that represents Token.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Number(usize),
    Identifier(String),
    U64,
    Plus,
    Minus,
    Asterisk,
    Slash,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Colon,
    Comma,
    Let,
    Func,
    If,
    Assignment,
    Semicolon,
    Return,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenKind::*;
        match self {
            Number(n) => n.fmt(f),
            Identifier(ident) => write!(f, "{}", ident),
            U64 => write!(f, "u64"),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Asterisk => write!(f, "*"),
            Slash => write!(f, "/"),
            LParen => write!(f, "("),
            RParen => write!(f, ")"),
            LBrace => write!(f, "{{"),
            RBrace => write!(f, "}}"),
            Func => write!(f, "func"),
            Colon => write!(f, ":"),
            Comma => write!(f, ","),
            Let => write!(f, "let"),
            If => write!(f, "if"),
            Assignment => write!(f, "="),
            Semicolon => write!(f, ";"),
            Return => write!(f, "return"),
        }
    }
}

pub type Token = Annotation<TokenKind>;

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
