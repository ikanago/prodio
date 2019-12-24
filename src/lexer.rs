use crate::util::{Annotation, Loc};

/// Data type that represents Token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Number(u64),
    Plus,
    Minus,
    Asterisk,
    Slash,
    LParen,
    RParen,
}

pub type Token = Annotation<TokenKind>;

impl Token {
    pub fn number(n: u64, loc: Loc) -> Self {
        Self::new(TokenKind::Number(n), loc)
    }

    pub fn plus(loc: Loc) -> Self {
        Self::new(TokenKind::Plus, loc)
    }

    pub fn minus(loc: Loc) -> Self {
        Self::new(TokenKind::Minus, loc)
    }

    pub fn asterisk(loc: Loc) -> Self {
        Self::new(TokenKind::Asterisk, loc)
    }

    pub fn slash(loc: Loc) -> Self {
        Self::new(TokenKind::Slash, loc)
    }

    pub fn lparen(loc: Loc) -> Self {
        Self::new(TokenKind::LParen, loc)
    }

    pub fn rparen(loc: Loc) -> Self {
        Self::new(TokenKind::RParen, loc)
    }
}

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

/// Struct to hold a input code, reading position, processed tokens.
pub struct Lexer<'a> {
    /// Input code.
    input: &'a [u8],
    /// Position where an instance of `Lexer` is reading.
    pos: usize,
    /// `Vec` of processed tokens.
    pub tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    /// Generate new `Lexer`.
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input: input.as_bytes(),
            pos: 0,
            tokens: Vec::new(),
        }
    }

    /// Read all characters in a input code and push token into `tokens`.
    pub fn lex(&mut self) -> Result<&Vec<Token>, LexError> {
        while self.pos < self.input.len() {
            match self.input[self.pos] {
                b'0'..=b'9' => self.lex_number(),
                b'+' => self.lex_plus(),
                b'-' => self.lex_minus(),
                b'*' => self.lex_asterisk(),
                b'/' => self.lex_slash(),
                b'(' => self.lex_lparen(),
                b')' => self.lex_rparen(),
                b' ' | b'\n' | b'\t' => self.skip_spaces(),
                b => {
                    return Err(LexError::invalid_char(
                        b as char,
                        Loc(self.pos, self.pos + 1),
                    ));
                }
            }
        }

        Ok(&self.tokens)
    }

    fn lex_plus(&mut self) {
        self.tokens.push(Token::plus(Loc(self.pos, self.pos + 1)));
        self.pos += 1;
    }

    fn lex_minus(&mut self) {
        self.tokens.push(Token::minus(Loc(self.pos, self.pos + 1)));
        self.pos += 1;
    }

    fn lex_asterisk(&mut self) {
        self.tokens
            .push(Token::asterisk(Loc(self.pos, self.pos + 1)));
        self.pos += 1;
    }

    fn lex_slash(&mut self) {
        self.tokens.push(Token::slash(Loc(self.pos, self.pos + 1)));
        self.pos += 1;
    }

    fn lex_lparen(&mut self) {
        self.tokens.push(Token::lparen(Loc(self.pos, self.pos + 1)));
        self.pos += 1;
    }

    fn lex_rparen(&mut self) {
        self.tokens.push(Token::rparen(Loc(self.pos, self.pos + 1)));
        self.pos += 1;
    }

    fn lex_number(&mut self) {
        use std::str::from_utf8;

        let start = self.pos;
        let end = self.recognize_multiple_char(|b| b"0123456789".contains(&b));
        let num = from_utf8(&self.input[start..end]).unwrap().parse().unwrap();

        self.tokens.push(Token::number(num, Loc(start, end)));
        self.pos = end;
    }

    /// Read a code while `f` returns `true` and return position of the end of fragment; each character in the fragment satisfies `f`.
    fn recognize_multiple_char(&mut self, mut f: impl FnMut(u8) -> bool) -> usize {
        let mut pos = self.pos;
        while pos < self.input.len() && f(self.input[pos]) {
            pos += 1;
        }
        pos
    }

    fn skip_spaces(&mut self) {
        let pos = self.recognize_multiple_char(|b| b" \n\t".contains(&b));
        self.pos = pos;
    }
}

#[test]
fn test_lexer() {
    let mut lexer = Lexer::new("+/*(-)");
    let tokens = lexer.lex();
    assert_eq!(
        tokens,
        Ok(&vec![
            Token::plus(Loc(0, 1)),
            Token::slash(Loc(1, 2)),
            Token::asterisk(Loc(2, 3)),
            Token::lparen(Loc(3, 4)),
            Token::minus(Loc(4, 5)),
            Token::rparen(Loc(5, 6)),
        ]),
    );

    let mut lexer = Lexer::new("1 + 2 * 3 - -10");
    let tokens = lexer.lex();
    assert_eq!(
        tokens,
        Ok(&vec![
            Token::number(1, Loc(0, 1)),
            Token::plus(Loc(2, 3)),
            Token::number(2, Loc(4, 5)),
            Token::asterisk(Loc(6, 7)),
            Token::number(3, Loc(8, 9)),
            Token::minus(Loc(10, 11)),
            Token::minus(Loc(12, 13)),
            Token::number(10, Loc(13, 15)),
        ]),
    );
}

#[test]
fn test_lexer_error() {
    let mut lexer = Lexer::new("1 $ 2 * 3 - -10");
    let tokens = lexer.lex();
    assert_eq!(tokens, Err(LexError::invalid_char('$', Loc(2, 3))), );
}
