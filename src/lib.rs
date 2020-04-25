#[macro_use]
pub mod macros;
pub mod code_gen;
pub mod dump_info;
pub mod gen_ir;
pub mod lexer;
pub mod parser;
pub mod reg_alloc;
pub mod util;

use std::fmt;
use std::fs::File;
use std::io::Read;

use crate::util::Annotation;

const REGISTER_COUNT: usize = 7;
const ARG_REGISTER_COUNT: usize = 6;

pub fn read_file_content<P: AsRef<std::path::Path>>(
    source_file_path: P,
) -> Result<String, std::io::Error> {
    let mut source_file = File::open(source_file_path)?;
    let mut source_code = String::new();
    source_file.read_to_string(&mut source_code)?;
    Ok(source_code)
}

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
