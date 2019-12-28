use std::iter::Peekable;

use crate::lexer::{Token, TokenKind};
use crate::util::{Annotation, Loc};

/// Data type of AST node.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstKind {
    Num(u64),
    UniOp {
        op: UniOp,
        node: Box<Ast>,
    },
    BinOp {
        op: BinOp,
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },
}

pub type Ast = Annotation<AstKind>;

impl Ast {
    pub fn num(n: u64, loc: Loc) -> Self {
        Self::new(AstKind::Num(n), loc)
    }

    pub fn uniop(op: UniOp, e: Ast, loc: Loc) -> Self {
        Self::new(
            AstKind::UniOp {
                op,
                node: Box::new(e),
            },
            loc,
        )
    }

    pub fn binop(op: BinOp, lhs: Ast, rhs: Ast, loc: Loc) -> Self {
        Self::new(
            AstKind::BinOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            loc,
        )
    }
}

/// Data type of unary operator.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UniOpKind {
    Plus,
    Minus,
}

pub type UniOp = Annotation<UniOpKind>;

impl UniOp {
    fn plus(loc: Loc) -> Self {
        Self::new(UniOpKind::Plus, loc)
    }

    fn minus(loc: Loc) -> Self {
        Self::new(UniOpKind::Minus, loc)
    }
}

/// Data type of binary operator.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
}

pub type BinOp = Annotation<BinOpKind>;

impl BinOp {
    fn add(loc: Loc) -> Self {
        Self::new(BinOpKind::Add, loc)
    }

    fn sub(loc: Loc) -> Self {
        Self::new(BinOpKind::Sub, loc)
    }

    fn mul(loc: Loc) -> Self {
        Self::new(BinOpKind::Mul, loc)
    }

    fn div(loc: Loc) -> Self {
        Self::new(BinOpKind::Div, loc)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ParseError {
    UnexpectedToken(Token),
    NotExpression(Token),
    NotOperator(Token),
    UnclosedOpenParen(Token),
    RedundantExpression(Token),
    Eof,
}

/// Parse tokens and build AST.
/// BNF:
///     EXPR ::= ADD
pub fn parse(tokens: Vec<Token>) -> Result<Ast, ParseError> {
    let mut tokens = tokens.into_iter().peekable();
    let result = parse_add(&mut tokens)?;
    match tokens.next() {
        Some(token) => Err(ParseError::RedundantExpression(token)),
        None => Ok(result),
    }
}

/// Abstract function for binary operator.
fn parse_left_binop<Tokens>(
    tokens: &mut Peekable<Tokens>,
    sub_expr_parser: fn(&mut Peekable<Tokens>) -> Result<Ast, ParseError>,
    op_parser: fn(&mut Peekable<Tokens>) -> Result<BinOp, ParseError>,
) -> Result<Ast, ParseError>
    where
        Tokens: Iterator<Item=Token>,
{
    let mut lhs = sub_expr_parser(tokens)?;
    loop {
        match tokens.peek() {
            Some(_) => {
                let op = match op_parser(tokens) {
                    Ok(op) => op,
                    Err(_) => break,
                };
                let rhs = sub_expr_parser(tokens)?;
                let loc = lhs.loc.merge(&rhs.loc);
                lhs = Ast::binop(op, lhs, rhs, loc)
            }
            _ => break,
        }
    }
    Ok(lhs)
}

/// BNF:
///     ADD ::= MUL ("+" MUL | "-" MUL)*
fn parse_add<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
    where
        Tokens: Iterator<Item=Token>,
{
    // Helper function to parse `+` or `-`.
    fn parse_add_op<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<BinOp, ParseError>
        where
            Tokens: Iterator<Item=Token>,
    {
        let op = tokens
            .peek()
            .ok_or(ParseError::Eof)
            .and_then(|token| match token.value {
                TokenKind::Plus => Ok(BinOp::add(token.loc.clone())),
                TokenKind::Minus => Ok(BinOp::sub(token.loc.clone())),
                _ => Err(ParseError::NotOperator(token.clone())),
            })?;
        tokens.next();
        Ok(op)
    }

    parse_left_binop(tokens, parse_mul, parse_add_op)
}

/// BNF:
///     MUL ::= UNARY ("*" UNARY | "/"UNARY)*
fn parse_mul<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
    where
        Tokens: Iterator<Item=Token>,
{
    // Helper function to parse `*` or `/`.
    fn parse_mul_op<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<BinOp, ParseError>
        where
            Tokens: Iterator<Item=Token>,
    {
        let op = tokens
            .peek()
            .ok_or(ParseError::Eof)
            .and_then(|token| match token.value {
                TokenKind::Asterisk => Ok(BinOp::mul(token.loc.clone())),
                TokenKind::Slash => Ok(BinOp::div(token.loc.clone())),
                _ => Err(ParseError::NotOperator(token.clone())),
            })?;
        tokens.next();
        Ok(op)
    }

    parse_left_binop(tokens, parse_unary, parse_mul_op)
}

/// BNF:
///     UNARY ::= NUMBER | "(" ADD ")"
fn parse_unary<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
    where
        Tokens: Iterator<Item=Token>,
{
    match tokens.peek().map(|token| &token.value) {
        Some(TokenKind::Plus) | Some(TokenKind::Minus) => {
            let op = match tokens.next() {
                Some(Token {
                         value: TokenKind::Plus,
                         loc,
                     }) => UniOp::plus(loc),
                Some(Token {
                         value: TokenKind::Minus,
                         loc,
                     }) => UniOp::minus(loc),
                _ => unreachable!(),
            };
            let node = parse_term(tokens)?;
            let loc = op.loc.merge(&node.loc);
            Ok(Ast::uniop(op, node, loc))
        }
        _ => parse_term(tokens),
    }
}

/// BNF:
///     NUMBER ::= DIGIT*
///     DIGIT  ::= "0" | "1" | | "2" | | "3" | | "4" | | "5" | | "6" | | "7" | | "8" | | "9" |
fn parse_term<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
    where
        Tokens: Iterator<Item=Token>,
{
    tokens
        .next()
        .ok_or(ParseError::Eof)
        .and_then(|token| match token.value {
            TokenKind::Number(n) => Ok(Ast::new(AstKind::Num(n), token.loc)),
            TokenKind::LParen => {
                let node = parse_add(tokens)?;
                match tokens.next() {
                    Some(Token {
                             value: TokenKind::RParen,
                             ..
                         }) => Ok(node),
                    Some(t) => Err(ParseError::RedundantExpression(t)),
                    _ => Err(ParseError::UnclosedOpenParen(token)),
                }
            }
            _ => Err(ParseError::NotExpression(token)),
        })
}

#[test]
fn test_parser() {
    // (5 + 2) * 31 - -10
    let ast = parse(vec![
        Token::lparen(Loc(0, 1)),
        Token::number(5, Loc(1, 2)),
        Token::plus(Loc(3, 4)),
        Token::number(2, Loc(5, 6)),
        Token::rparen(Loc(6, 7)),
        Token::asterisk(Loc(8, 9)),
        Token::number(31, Loc(10, 12)),
        Token::minus(Loc(13, 14)),
        Token::minus(Loc(15, 16)),
        Token::number(10, Loc(16, 18)),
    ]);
    assert_eq!(
        ast,
        Ok(Ast::binop(
            BinOp::sub(Loc(13, 14)),
            Ast::binop(
                BinOp::mul(Loc(8, 9)),
                Ast::binop(
                    BinOp::add(Loc(3, 4)),
                    Ast::num(5, Loc(1, 2)),
                    Ast::num(2, Loc(5, 6)),
                    Loc(1, 6),
                ),
                Ast::num(31, Loc(10, 12)),
                Loc(1, 12),
            ),
            Ast::uniop(
                UniOp::minus(Loc(15, 16)),
                Ast::num(10, Loc(16, 18)),
                Loc(15, 18),
            ),
            Loc(1, 18),
        ))
    )
}
