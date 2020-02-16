use std::collections::HashMap;
use std::iter::Peekable;

use crate::lexer::{Token, TokenKind};
use crate::util::{Annotation, Loc};

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
}

/// Data type of unary operator.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UniOpKind {
    Plus,
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
    UnexpectedToken(Token),
    NotExpression(Token),
    NotOperator(Token),
    UnclosedOpenParen(Token),
    RedundantExpression(Token),
    NoSemicolon,
    Eof,
}

fn expect_token<Tokens>(
    tokens: &mut Peekable<Tokens>,
    token_kind: TokenKind,
) -> Result<(), ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let token = tokens.next().unwrap();
    if token.value == token_kind {
        Ok(())
    } else {
        Err(ParseError::UnexpectedToken(token))
    }
}

#[derive(Debug, Clone)]
pub struct Parser {
    pub env: HashMap<String, usize>,
    pub stack_offset: usize,
}

impl Parser {
    pub fn new() -> Self {
        Parser {
            env: HashMap::new(),
            stack_offset: 0,
        }
    }

    /// Parse tokens and build AST.
    pub fn parse(&mut self, tokens: Vec<Token>) -> Result<Vec<Ast>, ParseError> {
        let mut tokens = tokens.into_iter().peekable();
        let mut asts = Vec::new();
        loop {
            let ast = self.parse_stmt(&mut tokens)?;
            expect_token(&mut tokens, TokenKind::Semicolon)?;
            asts.push(ast);
            match tokens.peek() {
                Some(_) => continue,
                None => break,
            }
        }
        Ok(asts)
    }

    /// BNF:
    ///     STMT ::= ASSIGN | DECL_VAR | "return" ASSIGN
    fn parse_stmt<Tokens>(&mut self, tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
    where
        Tokens: Iterator<Item = Token>,
    {
        match tokens.peek().map(|token| &token.value) {
            Some(TokenKind::Int) => self.parse_decl_var(tokens),
            Some(TokenKind::Return) => {
                tokens.next();
                let expr = self.parse_assign(tokens)?;
                let loc = &expr.loc;
                Ok(Ast::new(
                    AstKind::Return {
                        expr: Box::new(expr.clone()),
                    },
                    loc.clone(),
                ))
            }
            _ => self.parse_assign(tokens),
        }
    }

    /// BNF:
    ///     ASSIGN ::= ADD ("=" ASSIGN)?
    fn parse_assign<Tokens>(&mut self, tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
    where
        Tokens: Iterator<Item = Token>,
    {
        let lhs = self.parse_add(tokens)?;
        match tokens.peek().map(|token| &token.value) {
            Some(TokenKind::Assignment) => {
                tokens.next();
                let rhs = self.parse_assign(tokens)?;
                let loc = lhs.loc.merge(&rhs.loc);

                Ok(Ast::assignment(lhs, rhs, loc))
            }
            _ => Ok(lhs),
        }
    }

    /// BNF:
    ///     DECL_VAR ::= TYPE VARIABLE "=" ADD
    fn parse_decl_var<Tokens>(&mut self, tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
    where
        Tokens: Iterator<Item = Token>,
    {
        tokens.next();
        tokens
            .next()
            .ok_or(ParseError::Eof)
            .and_then(|token| match token.value {
                TokenKind::Identifier(var) => {
                    self.stack_offset += 8;
                    self.env.insert(var.clone(), self.stack_offset);

                    let lhs = Ast::new(AstKind::Variable(var.clone()), token.loc);
                    expect_token(tokens, TokenKind::Assignment)?;
                    let rhs = self.parse_add(tokens)?;
                    let loc = lhs.loc.merge(&rhs.loc);
                    Ok(Ast::new(
                        AstKind::Decl {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        loc,
                    ))
                }
                _ => Err(ParseError::UnexpectedToken(token.clone())),
            })
    }

    /// BNF:
    ///     ADD ::= MUL ("+" MUL | "-" MUL)*
    fn parse_add<Tokens>(&mut self, tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
    where
        Tokens: Iterator<Item = Token>,
    {
        let mut lhs = self.parse_mul(tokens)?;
        loop {
            if let Some(TokenKind::Plus) = tokens.peek().map(|token| &token.value) {
                tokens.next();
                let rhs = self.parse_mul(tokens)?;
                let loc = lhs.loc.merge(&rhs.loc);
                lhs = Ast::binop(BinOpKind::Add, lhs, rhs, loc);
            } else if let Some(TokenKind::Minus) = tokens.peek().map(|token| &token.value) {
                tokens.next();
                let rhs = self.parse_mul(tokens)?;
                let loc = lhs.loc.merge(&rhs.loc);
                lhs = Ast::binop(BinOpKind::Sub, lhs, rhs, loc);
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    /// BNF:
    ///     MUL ::= UNARY ("*" UNARY | "/"UNARY)*
    ///
    fn parse_mul<Tokens>(&mut self, tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
    where
        Tokens: Iterator<Item = Token>,
    {
        let mut lhs = self.parse_unary(tokens)?;
        loop {
            if let Some(TokenKind::Asterisk) = tokens.peek().map(|token| &token.value) {
                tokens.next();
                let rhs = self.parse_unary(tokens)?;
                let loc = lhs.loc.merge(&rhs.loc);
                lhs = Ast::binop(BinOpKind::Mul, lhs, rhs, loc);
            } else if let Some(TokenKind::Slash) = tokens.peek().map(|token| &token.value) {
                tokens.next();
                let rhs = self.parse_unary(tokens)?;
                let loc = lhs.loc.merge(&rhs.loc);
                lhs = Ast::binop(BinOpKind::Div, lhs, rhs, loc);
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    /// BNF:
    ///     UNARY ::= NUMBER | "(" ADD ")"
    fn parse_unary<Tokens>(&mut self, tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
    where
        Tokens: Iterator<Item = Token>,
    {
        match tokens.peek().map(|token| &token.value) {
            Some(TokenKind::Plus) | Some(TokenKind::Minus) => {
                let op = match tokens.next() {
                    Some(Token {
                        value: TokenKind::Plus,
                        loc: _loc,
                    }) => UniOpKind::Plus,
                    Some(Token {
                        value: TokenKind::Minus,
                        loc: _loc,
                    }) => UniOpKind::Minus,
                    _ => unreachable!(),
                };
                let node = self.parse_term(tokens)?;
                //            let loc = op.loc.merge(&node.loc);
                let loc = node.loc.clone();
                Ok(Ast::uniop(op, node, loc))
            }
            _ => self.parse_term(tokens),
        }
    }

    /// BNF:
    ///     TERM ::= DIGIT* | VARIABLE | "(" ASSIGN ")"
    ///     DIGIT  ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" |
    fn parse_term<Tokens>(&mut self, tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
    where
        Tokens: Iterator<Item = Token>,
    {
        tokens
            .next()
            .ok_or(ParseError::Eof)
            .and_then(|token| match token.value {
                TokenKind::Number(n) => Ok(Ast::new(AstKind::Num(n), token.loc)),
                TokenKind::Identifier(var) => Ok(Ast::new(AstKind::Variable(var), token.loc)),
                TokenKind::LParen => {
                    let node = self.parse_add(tokens)?;
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
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::parser::{Ast, BinOpKind, UniOpKind};
    use crate::util::Loc;

    #[test]
    fn test_calculate() {
        let code = "(5 + 2) * 31 - -10;";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new();
        let ast = parser.parse(tokens.to_vec());
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
        let code = "abc = 3; def = 5; abc + def;";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new();
        let ast = parser.parse(tokens.to_vec());
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
