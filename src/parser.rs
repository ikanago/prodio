use std::iter::Peekable;

use crate::lexer::{Token, TokenKind};
use crate::util::{Annotation, Loc};

#[macro_export]
macro_rules! tokens {
    ($token_kind: ident, $start: expr, $end: expr) => {
        Token::new(TokenKind::$token_kind, Loc($start, $end))
    };
    ($token_kind: ident ($var: expr), $start: expr, $end: expr) => {
        Token::new(TokenKind::$token_kind($var), Loc($start, $end))
    };
}

/// Data type of AST node.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstKind {
    Num(u64),
    UniOp {
        op: UniOpKind,
        node: Box<Ast>,
    },
    BinOp {
        op: BinOpKind,
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },
    Assignment {
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },
    Variable(String),
}

pub type Ast = Annotation<AstKind>;

impl Ast {
    pub fn num(n: u64, loc: Loc) -> Self {
        Self::new(AstKind::Num(n), loc)
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

    pub fn assignment(lhs: Ast, rhs: Ast, loc: Loc) -> Self {
        Self::new(
            AstKind::Assignment {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            loc,
        )
    }

    pub fn variable(var: String, loc: Loc) -> Self {
        Self::new(AstKind::Variable(var), loc)
    }
}

/// Data type of unary operator.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UniOpKind {
    Plus,
    Minus,
}

//pub type UniOp = Annotation<UniOpKind>;
/*
impl UniOp {
    fn plus(loc: Loc) -> Self {
        Self::new(UniOpKind::Plus, loc)
    }

    fn minus(loc: Loc) -> Self {
        Self::new(UniOpKind::Minus, loc)
    }
}*/

/// Data type of binary operator.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
}

//pub type BinOp = Annotation<BinOpKind>;
/*
impl BinOpKind {
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
}*/

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ParseError {
    UnexpectedToken(Token),
    NotExpression(Token),
    NotOperator(Token),
    UnclosedOpenParen(Token),
    RedundantExpression(Token),
    NoSemicolon,
    Eof,
}

/// Parse tokens and build AST.
/// BNF:
///     EXPR ::= ADD
pub fn parse(tokens: Vec<Token>) -> Result<Vec<Ast>, ParseError> {
    let mut tokens = tokens.into_iter().peekable();
    let mut asts = Vec::new();
    loop {
        let ast = parse_assign(&mut tokens)?;
        expect_semicolon(&tokens.next().unwrap())?;
        asts.push(ast);
        match tokens.peek() {
            Some(token) => continue,
            None => break,
        }
    }
    Ok(asts)
}

/// BNF:
///     ASSIGN ::= ADD ("=" ASSIGN)?
fn parse_assign<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
    where
        Tokens: Iterator<Item=Token>,
{
    let lhs = parse_add(tokens)?;
    match tokens.peek().map(|token| &token.value) {
        Some(TokenKind::Assignment) => {
            tokens.next();
            let rhs = parse_assign(tokens)?;
            let loc = lhs.loc.merge(&rhs.loc);

            Ok(Ast::assignment(lhs, rhs, loc))
        }
        _ => Ok(lhs),
    }
}

fn expect_semicolon(token: &Token) -> Result<(), ParseError> {
    match &token.value {
        TokenKind::Semicolon => Ok(()),
        _ => Err(ParseError::NoSemicolon),
    }
}

/// Abstract function for binary operator.
fn parse_left_binop<Tokens>(
    tokens: &mut Peekable<Tokens>,
    sub_expr_parser: fn(&mut Peekable<Tokens>) -> Result<Ast, ParseError>,
    op_parser: fn(&mut Peekable<Tokens>) -> Result<BinOpKind, ParseError>,
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
    fn parse_add_op<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<BinOpKind, ParseError>
        where
            Tokens: Iterator<Item=Token>,
    {
        let op = tokens
            .peek()
            .ok_or(ParseError::Eof)
            .and_then(|token| match token.value {
                TokenKind::Plus => Ok(BinOpKind::Add),
                TokenKind::Minus => Ok(BinOpKind::Sub),
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
    fn parse_mul_op<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<BinOpKind, ParseError>
        where
            Tokens: Iterator<Item=Token>,
    {
        let op = tokens
            .peek()
            .ok_or(ParseError::Eof)
            .and_then(|token| match token.value {
                TokenKind::Asterisk => Ok(BinOpKind::Mul),
                TokenKind::Slash => Ok(BinOpKind::Div),
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
                     }) => UniOpKind::Plus,
                Some(Token {
                         value: TokenKind::Minus,
                         loc,
                     }) => UniOpKind::Minus,
                _ => unreachable!(),
            };
            let node = parse_term(tokens)?;
            //            let loc = op.loc.merge(&node.loc);
            let loc = node.loc.clone();
            Ok(Ast::uniop(op, node, loc))
        }
        _ => parse_term(tokens),
    }
}

/// BNF:
///     NUMBER ::= DIGIT* | VARIABLE
///     DIGIT  ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" |
fn parse_term<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
    where
        Tokens: Iterator<Item=Token>,
{
    tokens
        .next()
        .ok_or(ParseError::Eof)
        .and_then(|token| match token.value {
            TokenKind::Number(n) => Ok(Ast::new(AstKind::Num(n), token.loc)),
            TokenKind::Identifier(var) => Ok(Ast::new(AstKind::Variable(var), token.loc)),
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

#[macro_use]
#[cfg(test)]
mod tests {
    use crate::lexer::{Lexer, Token, TokenKind};
    use crate::parser::{Ast, AstKind, BinOpKind, UniOpKind};
    use crate::parser::parse;
    use crate::util::Loc;

    #[test]
    fn test_calculate() {
        // (5 + 2) * 31 - -10;
        let ast = parse(vec![
            tokens!(LParen, 0, 1),
            tokens!(Number(5), 1, 2),
            tokens!(Plus, 3, 4),
            tokens!(Number(2), 5, 6),
            tokens!(RParen, 6, 7),
            tokens!(Asterisk, 8, 9),
            tokens!(Number(31), 10, 12),
            tokens!(Minus, 13, 14),
            tokens!(Minus, 15, 16),
            tokens!(Number(10), 16, 18),
            tokens!(Semicolon, 18, 19),
        ]);
        assert_eq!(
            ast,
            Ok(vec![Ast::binop(
                BinOpKind::Sub,
                Ast::binop(
                    BinOpKind::Mul,
                    Ast::binop(
                        BinOpKind::Add,
                        Ast::num(5, Loc(1, 2)),
                        Ast::num(2, Loc(5, 6)),
                        Loc(1, 6),
                    ),
                    Ast::num(31, Loc(10, 12)),
                    Loc(1, 12),
                ),
                Ast::uniop(UniOpKind::Minus, Ast::num(10, Loc(16, 18)), Loc(16, 18)),
                Loc(1, 18),
            )])
        )
    }

    #[test]
    fn test_assignment() {
        let ast = parse(vec![
            tokens!(Identifier("abc".to_string()), 0, 3),
            tokens!(Assignment, 4, 5),
            tokens!(Number(3), 6, 7),
            tokens!(Semicolon, 7, 8),
            tokens!(Identifier("def".to_string()), 9, 12),
            tokens!(Assignment, 13, 14),
            tokens!(Number(5), 15, 16),
            tokens!(Semicolon, 16, 17),
            tokens!(Identifier("abc".to_string()), 18, 21),
            tokens!(Plus, 22, 23),
            tokens!(Identifier("def".to_string()), 24, 27),
            tokens!(Semicolon, 27, 28),
        ]);
        assert_eq!(
            ast,
            Ok(vec![
                Ast::assignment(
                    Ast::variable("abc".to_string(), Loc(0, 3)),
                    Ast::num(3, Loc(6, 7)),
                    Loc(0, 7),
                ),
                Ast::assignment(
                    Ast::variable("def".to_string(), Loc(9, 12)),
                    Ast::num(5, Loc(15, 16)),
                    Loc(9, 16),
                ),
                Ast::binop(
                    BinOpKind::Add,
                    Ast::variable("abc".to_string(), Loc(18, 21)),
                    Ast::variable("def".to_string(), Loc(24, 27)),
                    Loc(18, 27),
                ),
            ])
        )
    }
}
