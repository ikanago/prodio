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

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    // Reference to vector of tokens given from `Lexer`.
    tokens: &'a Vec<Token>,
    // Current position of a token stream.
    pos: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    /// Take a look at a following token.
    fn peek(&self) -> Option<&Token> {
        if self.tokens.len() == self.pos {
            return None;
        }
        Some(&self.tokens[self.pos])
    }

    /// Return current token and move `pos` forward.
    fn next(&mut self) -> Option<Token> {
        if self.tokens.len() == self.pos {
            return None;
        }
        let token = self.tokens[self.pos].clone();
        self.pos += 1;
        Some(token)
    }

    /// Check if a current token has expected type and proceed to next one.
    fn expect_token(&mut self, token_kind: TokenKind) -> Result<(), ParseError> {
        self.next().ok_or(ParseError::Eof).and_then(|token| {
            if token.value == token_kind {
                Ok(())
            } else {
                Err(ParseError::UnexpectedToken(token))
            }
        })
    }

    /// Parse tokens and build AST.
    pub fn parse(&mut self) -> Result<Vec<Ast>, ParseError> {
        let mut asts = Vec::new();
        loop {
            let ast = self.parse_stmt()?;
            asts.push(ast);
            match self.peek() {
                Some(_) => continue,
                None => break,
            }
        }
        Ok(asts)
    }

    /// BNF:
    ///     STMT ::= ASSIGN | DECL_VAR | "if" COMP_STMT | COMP_STMT | "return" ASSIGN
    fn parse_stmt(&mut self) -> Result<Ast, ParseError> {
        match self.peek().map(|token| &token.value) {
            Some(TokenKind::Let) => self.parse_decl_var(),
            Some(TokenKind::If) => {
                self.next();
                let cond = self.parse_assign()?;
                let then = self.parse_stmt()?;
                let loc = cond.loc.merge(&then.loc);
                Ok(Ast::new(
                    AstKind::If {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        els: None,
                    },
                    loc.clone(),
                ))
            }
            Some(TokenKind::LBrace) => {
                self.next();
                let mut stmts = Vec::new();
                let mut loc = Loc(std::usize::MAX, 0);
                while self.peek().map(|token| &token.value) != Some(&TokenKind::RBrace) {
                    let stmt = self.parse_stmt()?;
                    loc = loc.merge(&stmt.loc);
                    stmts.push(stmt);
                }
                self.next();
                Ok(Ast::new(AstKind::CompStmt { stmts }, loc))
            }
            Some(TokenKind::Return) => {
                self.next();
                let expr = self.parse_assign()?;
                let loc = &expr.loc;
                self.expect_token(TokenKind::Semicolon)?;
                Ok(Ast::new(
                    AstKind::Return {
                        expr: Box::new(expr.clone()),
                    },
                    loc.clone(),
                ))
            }
            _ => {
                let ast = self.parse_assign()?;
                self.expect_token(TokenKind::Semicolon)?;
                Ok(ast)
            }
        }
    }

    /// BNF:
    ///     ASSIGN ::= ADD ("=" ASSIGN)?
    fn parse_assign(&mut self) -> Result<Ast, ParseError> {
        let lhs = self.parse_add()?;
        match self.peek().map(|token| &token.value) {
            Some(TokenKind::Assignment) => {
                self.next();
                let rhs = self.parse_assign()?;
                let loc = lhs.loc.merge(&rhs.loc);
                Ok(Ast::assignment(lhs, rhs, loc))
            }
            _ => Ok(lhs),
        }
    }

    /// BNF:
    ///     DECL_VAR ::= TYPE VARIABLE "=" ADD
    fn parse_decl_var(&mut self) -> Result<Ast, ParseError> {
        self.next();
        self.next()
            .ok_or(ParseError::Eof)
            .and_then(|token| match token.value {
                TokenKind::Identifier(var) => {
                    let lhs = Ast::new(AstKind::Variable(var.clone()), token.loc);
                    self.expect_token(TokenKind::Colon)?;
                    self.expect_token(TokenKind::U64)?;
                    self.expect_token(TokenKind::Assignment)?;
                    let rhs = self.parse_add()?;
                    let loc = lhs.loc.merge(&rhs.loc);
                    self.expect_token(TokenKind::Semicolon)?;
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
    fn parse_add(&mut self) -> Result<Ast, ParseError> {
        let mut lhs = self.parse_mul()?;
        loop {
            if let Some(TokenKind::Plus) = self.peek().map(|token| &token.value) {
                self.next();
                let rhs = self.parse_mul()?;
                let loc = lhs.loc.merge(&rhs.loc);
                lhs = Ast::binop(BinOpKind::Add, lhs, rhs, loc);
            } else if let Some(TokenKind::Minus) = self.peek().map(|token| &token.value) {
                self.next();
                let rhs = self.parse_mul()?;
                let loc = lhs.loc.merge(&rhs.loc);
                lhs = Ast::binop(BinOpKind::Sub, lhs, rhs, loc);
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    /// BNF:
    ///     MUL ::= UNARY ("*" UNARY | "/" UNARY)*
    ///
    fn parse_mul(&mut self) -> Result<Ast, ParseError> {
        let mut lhs = self.parse_unary()?;
        loop {
            if let Some(TokenKind::Asterisk) = self.peek().map(|token| &token.value) {
                self.next();
                let rhs = self.parse_unary()?;
                let loc = lhs.loc.merge(&rhs.loc);
                lhs = Ast::binop(BinOpKind::Mul, lhs, rhs, loc);
            } else if let Some(TokenKind::Slash) = self.peek().map(|token| &token.value) {
                self.next();
                let rhs = self.parse_unary()?;
                let loc = lhs.loc.merge(&rhs.loc);
                lhs = Ast::binop(BinOpKind::Div, lhs, rhs, loc);
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    /// BNF:
    ///     UNARY ::= ("+" | "-") PRIMARY | PRIMARY
    fn parse_unary(&mut self) -> Result<Ast, ParseError> {
        match self.peek().map(|token| &token.value) {
            Some(TokenKind::Plus) | Some(TokenKind::Minus) => {
                let op = match self.next() {
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
                let node = self.parse_primary()?;
                let loc = node.loc.clone();
                Ok(Ast::uniop(op, node, loc))
            }
            _ => self.parse_primary(),
        }
    }

    /// BNF:
    ///     PRIMARY ::= DIGIT* | VARIABLE | "(" ASSIGN ")"
    ///     DIGIT  ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" |
    fn parse_primary(&mut self) -> Result<Ast, ParseError> {
        self.next()
            .ok_or(ParseError::Eof)
            .and_then(|token| match token.value {
                TokenKind::Number(n) => Ok(Ast::new(AstKind::Num(n), token.loc)),
                TokenKind::Identifier(var) => Ok(Ast::new(AstKind::Variable(var), token.loc)),
                TokenKind::LParen => {
                    let node = self.parse_add()?;
                    match self.next() {
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
    fn test_calc() -> std::io::Result<()> {
        let source_code = crate::read_file_content("examples/calc.pr")?;
        let mut lexer = Lexer::new(&source_code);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new(&tokens);
        let ast = parser.parse();
        assert_eq!(
            ast,
            Ok(vec![
                Ast::decl(
                    Ast::variable("a".to_string(), Loc(4, 5)),
                    Ast::num(3, Loc(13, 14)),
                    Loc(4, 14)
                ),
                Ast::decl(
                    Ast::variable("b".to_string(), Loc(20, 21)),
                    Ast::num(2, Loc(29, 30)),
                    Loc(20, 30)
                ),
                Ast::decl(
                    Ast::variable("c".to_string(), Loc(36, 37)),
                    Ast::binop(
                        BinOpKind::Mul,
                        Ast::variable("a".to_string(), Loc(45, 46)),
                        Ast::variable("b".to_string(), Loc(49, 50)),
                        Loc(45, 50)
                    ),
                    Loc(36, 50)
                ),
                Ast::return_stmt(Ast::variable("c".to_string(), Loc(59, 60)), Loc(59, 60)),
            ])
        );
        Ok(())
    }

    #[test]
    fn test_stmt() -> std::io::Result<()> {
        let source_code = crate::read_file_content("examples/stmt.pr")?;
        let mut lexer = Lexer::new(&source_code);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new(&tokens);
        let ast = parser.parse();
        assert_eq!(
            ast,
            Ok(vec![
                Ast::decl(
                    Ast::variable("a".to_string(), Loc(4, 5)),
                    Ast::num(1, Loc(13, 14)),
                    Loc(4, 14)
                ),
                Ast::if_stmt(
                    Ast::variable("a".to_string(), Loc(19, 20)),
                    Ast::comp_stmt(
                        vec![
                            Ast::assignment(
                                Ast::variable("a".to_string(), Loc(27, 28)),
                                Ast::num(2, Loc(31, 32)),
                                Loc(27, 32)
                            ),
                            Ast::return_stmt(
                                Ast::variable("a".to_string(), Loc(45, 46)),
                                Loc(45, 46)
                            )
                        ],
                        Loc(27, 46)
                    ),
                    None,
                    Loc(19, 46)
                ),
                Ast::return_stmt(Ast::variable("a".to_string(), Loc(57, 58)), Loc(57, 58)),
            ])
        );
        Ok(())
    }
}
