use crate::parse::{Ast, AstKind, BinOpKind, ParseError, UniOpKind};
use crate::token::{Token, TokenKind};
use crate::Loc;

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

    /// Take a look at a next token and return its kind.
    fn peek(&self) -> Option<&TokenKind> {
        if self.tokens.len() == self.pos {
            return None;
        }
        Some(&self.tokens[self.pos].value)
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
            if self.peek().is_none() {
                break;
            }
        }
        Ok(asts)
    }

    /// BNF:
    ///     FUNC_DEF ::= "func" IDENTIFIER "(" PARAMS ")" COMP_STMT
    fn parse_func_def(&mut self) -> Result<Ast, ParseError> {
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

        let params = self.parse_params()?;
        let body = self.parse_comp_stmt()?;
        let loc = body.loc.clone();
        Ok(Ast::func(func_name, params, body, loc))
    }

    /// BNF:
    ///     PARAMS ::= PARAM*
    fn parse_params(&mut self) -> Result<Vec<Ast>, ParseError> {
        self.expect_token(TokenKind::LParen)?;
        let mut vec_param: Vec<Ast> = Vec::new();
        // Todo: more simple way to extract `TokenKind`
        if self.peek() == Some(&TokenKind::RParen) {
            self.next();
            return Ok(vec_param);
        }

        vec_param.push(self.parse_param()?);
        while self.peek() == Some(&TokenKind::Comma) {
            self.next();
            vec_param.push(self.parse_param()?);
        }
        self.expect_token(TokenKind::RParen)?;
        Ok(vec_param)
    }

    fn parse_param(&mut self) -> Result<Ast, ParseError> {
        self.next()
            .ok_or(ParseError::Eof)
            .and_then(|token| match token.value {
                TokenKind::Identifier(var) => {
                    self.expect_token(TokenKind::Colon)?;
                    self.expect_token(TokenKind::U64)?;
                    Ok(Ast::new(AstKind::Variable(var), token.loc))
                }
                _ => Err(ParseError::UnexpectedToken(
                    TokenKind::Identifier("variable".to_string()),
                    token.clone(),
                )),
            })
    }

    /// BNF:
    ///     STMT ::= DECL_VAR | IF_STMT | COMP_STMT | RETURN_STMT | ASSIGN ";"
    fn parse_stmt(&mut self) -> Result<Ast, ParseError> {
        match self.peek() {
            Some(&TokenKind::Let) => self.parse_decl_var(),
            Some(&TokenKind::If) => self.parse_if(),
            Some(&TokenKind::LBrace) => self.parse_comp_stmt(),
            Some(&TokenKind::Return) => self.parse_return(),
            _ => {
                let ast = self.parse_assign()?;
                self.expect_token(TokenKind::Semicolon)?;
                Ok(ast)
            }
        }
    }

    /// BNF:
    ///     DECL_VAR ::= "let" VARIABLE ":" TYPE "=" ADD
    ///     TYPE     ::= "u64"
    fn parse_decl_var(&mut self) -> Result<Ast, ParseError> {
        self.next();
        self.next()
            .ok_or(ParseError::Eof)
            .and_then(|token| match token.value {
                TokenKind::Identifier(var) => {
                    let lhs = Ast::new(AstKind::Variable(var), token.loc);
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
        while self.peek() != Some(&TokenKind::RBrace) {
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
        match self.peek() {
            Some(&TokenKind::Assignment) => {
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
            if self.peek() == Some(&TokenKind::Plus) {
                self.next();
                let rhs = self.parse_mul()?;
                let loc = lhs.loc.merge(&rhs.loc);
                lhs = Ast::binop(BinOpKind::Add, lhs, rhs, loc);
            } else if self.peek() == Some(&TokenKind::Minus) {
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
            if self.peek() == Some(&TokenKind::Asterisk) {
                self.next();
                let rhs = self.parse_unary()?;
                let loc = lhs.loc.merge(&rhs.loc);
                lhs = Ast::binop(BinOpKind::Mul, lhs, rhs, loc);
            } else if self.peek() == Some(&TokenKind::Slash) {
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
        match self.peek() {
            Some(&TokenKind::Minus) => {
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
    ///     PRIMARY ::= DIGIT* | IDENTIFIER | IDENTIFIER "(" ASSIGN? ")" | "(" ASSIGN ")"
    ///     DIGIT  ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" |
    fn parse_primary(&mut self) -> Result<Ast, ParseError> {
        self.next()
            .ok_or(ParseError::Eof)
            .and_then(|token| match token.value {
                TokenKind::Number(n) => Ok(Ast::new(AstKind::Num(n), token.loc)),
                TokenKind::Identifier(var) => {
                    // Function call.
                    if self.peek() == Some(&TokenKind::LParen) {
                        self.next();
                        let mut args = Vec::new();
                        if self.peek() == Some(&TokenKind::RParen) {
                            return Ok(Ast::func_call(var, args, token.loc));
                        }

                        args.push(self.parse_assign()?);
                        while self.peek() == Some(&TokenKind::Comma) {
                            self.next();
                            args.push(self.parse_assign()?);
                        }
                        self.expect_token(TokenKind::RParen)?;
                        Ok(Ast::func_call(var, args, token.loc))
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
