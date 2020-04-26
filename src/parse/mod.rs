pub mod parser;

use crate::token::{Token, TokenKind};
use crate::{Annotation, Loc};
use std::fmt;

/// Data type of AST node.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstKind {
    Num(usize),
    Variable(String),
    Decl {
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },
    UniOp {
        op: UniOpKind,
        node: Box<Ast>,
    },
    BinOp {
        op: BinOpKind,
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },
    Func {
        name: String,
        params: Vec<Ast>,
        body: Box<Ast>,
    },
    FuncCall {
        name: String,
        args: Vec<Ast>,
    },
    If {
        cond: Box<Ast>,
        then: Box<Ast>,
        els: Option<Box<Ast>>,
    },
    CompStmt {
        stmts: Vec<Ast>,
    },
    Assignment {
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },
    Return {
        expr: Box<Ast>,
    },
}

pub type Ast = Annotation<AstKind>;

impl Ast {
    pub fn num(n: usize, loc: Loc) -> Self {
        Self::new(AstKind::Num(n), loc)
    }

    pub fn variable(var: String, loc: Loc) -> Self {
        Self::new(AstKind::Variable(var), loc)
    }

    pub fn decl(lhs: Ast, rhs: Ast, loc: Loc) -> Self {
        Self::new(
            AstKind::Decl {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            loc,
        )
    }

    pub fn uniop(op: UniOpKind, e: Ast, loc: Loc) -> Self {
        Self::new(
            AstKind::UniOp {
                op,
                node: Box::new(e),
            },
            loc,
        )
    }

    pub fn binop(op: BinOpKind, lhs: Ast, rhs: Ast, loc: Loc) -> Self {
        Self::new(
            AstKind::BinOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            loc,
        )
    }

    pub fn func(name: String, params: Vec<Ast>, body: Ast, loc: Loc) -> Self {
        Self::new(
            AstKind::Func {
                name,
                params,
                body: Box::new(body),
            },
            loc,
        )
    }

    pub fn func_call(name: String, args: Vec<Ast>, loc: Loc) -> Self {
        Self::new(AstKind::FuncCall { name, args }, loc)
    }

    pub fn if_stmt(cond: Ast, then: Ast, els: Option<Ast>, loc: Loc) -> Self {
        let els = match els {
            Some(els) => Some(Box::new(els)),
            None => None,
        };
        Self::new(
            AstKind::If {
                cond: Box::new(cond),
                then: Box::new(then),
                els,
            },
            loc,
        )
    }

    pub fn comp_stmt(stmts: Vec<Ast>, loc: Loc) -> Self {
        Self::new(AstKind::CompStmt { stmts }, loc)
    }

    pub fn assignment(lhs: Ast, rhs: Ast, loc: Loc) -> Self {
        Self::new(
            AstKind::Assignment {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            loc,
        )
    }

    pub fn return_stmt(expr: Ast, loc: Loc) -> Self {
        Self::new(
            AstKind::Return {
                expr: Box::new(expr),
            },
            loc,
        )
    }
}

/// Data type of unary operator.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UniOpKind {
    Minus,
}

/// Data type of binary operator.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
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
