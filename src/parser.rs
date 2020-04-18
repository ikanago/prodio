use crate::util::{Annotation, Loc, ParseError};
use crate::{Token, TokenKind};

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
        body: Box<Ast>,
    },
    FuncCall {
        name: String,
        // params: Vec<
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

    pub fn func(name: String, body: Ast, loc: Loc) -> Self {
        Self::new(
            AstKind::Func {
                name,
                body: Box::new(body),
            },
            loc,
        )
    }

    pub fn func_call(name: String, loc: Loc) -> Self {
        Self::new(AstKind::FuncCall { name }, loc)
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
                Err(ParseError::UnexpectedToken(token_kind, token))
            }
        })
    }

    /// Parse tokens and build AST.
    pub fn parse(&mut self) -> Result<Vec<Ast>, ParseError> {
        let mut asts = Vec::new();
        loop {
            let ast = self.parse_func_def()?;
            asts.push(ast);
            match self.peek() {
                Some(_) => continue,
                None => break,
            }
        }
        Ok(asts)
    }

    /// BNF:
    ///     FUNC_DEF ::= "func" IDENTIFIER "(" ")" COMP_STMT
    pub fn parse_func_def(&mut self) -> Result<Ast, ParseError> {
        self.expect_token(TokenKind::Func)?;
        let func_name = self
            .next()
            .ok_or(ParseError::Eof)
            .and_then(|token| match token.value {
                TokenKind::Identifier(name) => Ok(name),
                _ => Err(ParseError::UnexpectedToken(
                    TokenKind::Identifier("func".to_string()),
                    token,
                )),
            })?;

        self.expect_token(TokenKind::LParen)?;
        self.expect_token(TokenKind::RParen)?;
        let body = self.parse_comp_stmt()?;
        let loc = body.loc.clone();
        Ok(Ast::func(func_name, body, loc))
    }

    /// BNF:
    ///     STMT ::= DECL_VAR | IF_STMT | COMP_STMT | RETURN_STMT | ASSIGN ";"
    fn parse_stmt(&mut self) -> Result<Ast, ParseError> {
        match self.peek().map(|token| &token.value) {
            Some(TokenKind::Let) => self.parse_decl_var(),
            Some(TokenKind::If) => self.parse_if(),
            Some(TokenKind::LBrace) => self.parse_comp_stmt(),
            Some(TokenKind::Return) => self.parse_return(),
            _ => {
                let ast = self.parse_assign()?;
                self.expect_token(TokenKind::Semicolon)?;
                Ok(ast)
            }
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
                _ => Err(ParseError::UnexpectedToken(
                    TokenKind::Identifier("variable".to_string()),
                    token.clone(),
                )),
            })
    }

    ///BNF:
    ///    IF_STMT ::= "if" ASSIGN COMP_STMT
    fn parse_if(&mut self) -> Result<Ast, ParseError> {
        self.next();
        let cond = self.parse_assign()?;
        let then = self.parse_comp_stmt()?;
        let loc = cond.loc.merge(&then.loc);
        Ok(Ast::if_stmt(cond, then, None, loc))
    }

    /// BNF:
    ///     COMP_STMT ::= "{" STMT* "}"
    fn parse_comp_stmt(&mut self) -> Result<Ast, ParseError> {
        self.expect_token(TokenKind::LBrace)?;
        let mut vec_stmt = Vec::new();
        let mut loc = Loc(std::usize::MAX, 0);
        while self.peek().map(|token| &token.value) != Some(&TokenKind::RBrace) {
            let stmt = self.parse_stmt()?;
            loc = loc.merge(&stmt.loc);
            vec_stmt.push(stmt);
        }
        self.next();
        Ok(Ast::comp_stmt(vec_stmt, loc))
    }

    /// BNF:
    ///     "return" ASSIGN ";"
    fn parse_return(&mut self) -> Result<Ast, ParseError> {
        self.next();
        let expr = self.parse_assign()?;
        let loc = expr.loc.clone();
        self.expect_token(TokenKind::Semicolon)?;
        Ok(Ast::return_stmt(expr, loc))
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
            Some(TokenKind::Minus) => {
                let op = match self.next() {
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
    ///     PRIMARY ::= DIGIT* | IDENTIFIER | IDENTIFIER "(" params? ")" | "(" ASSIGN ")"
    ///     DIGIT  ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" |
    fn parse_primary(&mut self) -> Result<Ast, ParseError> {
        self.next()
            .ok_or(ParseError::Eof)
            .and_then(|token| match token.value {
                TokenKind::Number(n) => Ok(Ast::new(AstKind::Num(n), token.loc)),
                TokenKind::Identifier(var) => {
                    // Function call.
                    if let Some(TokenKind::LParen) = self.peek().map(|t| &t.value) {
                        self.next();
                        let node = Ast::func_call(var, token.loc);
                        self.expect_token(TokenKind::RParen)?;
                        Ok(node)
                    }
                    // Access to local variable.
                    else {
                        Ok(Ast::new(AstKind::Variable(var), token.loc))
                    }
                }
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
