use std::str::from_utf8;

use crate::util::{Annotation, Loc};

/// Data type that represents Token.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Plus,
    Minus,
    Asterisk,
    Slash,
    LParen,
    RParen,
    Identifier(String),
    Semicolon,
    Assignment,
    Number(u64),
}

pub type Token = Annotation<TokenKind>;

impl Token {
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

    pub fn identifier(ident: String, loc: Loc) -> Self {
        Self::new(TokenKind::Identifier(ident), loc)
    }

    pub fn number(n: u64, loc: Loc) -> Self {
        Self::new(TokenKind::Number(n), loc)
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
                b'+' => self.lex_plus(),
                b'-' => self.lex_minus(),
                b'*' => self.lex_asterisk(),
                b'/' => self.lex_slash(),
                b'(' => self.lex_lparen(),
                b')' => self.lex_rparen(),
                b'0'..=b'9' => self.lex_number(),
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.lex_identifier(),
                b';' => self.lex_semicolon(),
                b'=' => self.lex_assignment(),
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
        self.tokens.push(token!(Plus, self.pos, self.pos + 1));
        self.pos += 1;
    }

    fn lex_minus(&mut self) {
        self.tokens.push(token!(Minus, self.pos, self.pos + 1));
        self.pos += 1;
    }

    fn lex_asterisk(&mut self) {
        self.tokens.push(token!(Asterisk, self.pos, self.pos + 1));
        self.pos += 1;
    }

    fn lex_slash(&mut self) {
        self.tokens.push(token!(Slash, self.pos, self.pos + 1));
        self.pos += 1;
    }

    fn lex_lparen(&mut self) {
        self.tokens.push(token!(LParen, self.pos, self.pos + 1));
        self.pos += 1;
    }

    fn lex_rparen(&mut self) {
        self.tokens.push(token!(RParen, self.pos, self.pos + 1));
        self.pos += 1;
    }

    fn lex_number(&mut self) {
        let start = self.pos;
        let end = self.recognize_multiple_char(|b| b"0123456789".contains(&b));
        let num = from_utf8(&self.input[start..end]).unwrap().parse().unwrap();

        self.tokens.push(token!(Number(num), start, end));
        self.pos = end;
    }

    fn lex_identifier(&mut self) {
        let start = self.pos;
        let end = self.recognize_multiple_char(|b| b.is_ascii_alphanumeric() || b == b'_');
        let ident = from_utf8(&self.input[start..end]).unwrap();
        self.tokens
            .push(token!(Identifier(ident.to_string()), start, end));
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

    fn lex_semicolon(&mut self) {
        self.tokens.push(token!(Semicolon, self.pos, self.pos + 1));
        self.pos += 1;
    }

    fn lex_assignment(&mut self) {
        self.tokens.push(token!(Assignment, self.pos, self.pos + 1));
        self.pos += 1;
    }

    fn skip_spaces(&mut self) {
        let pos = self.recognize_multiple_char(|b| b" \n\t".contains(&b));
        self.pos = pos;
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{Lexer, Token, TokenKind};
    use crate::util::Loc;

    #[test]
    fn test_lexer() {
        let mut lexer = Lexer::new("+/*(-)");
        let tokens = lexer.lex();
        assert_eq!(
            tokens,
            Ok(&vec![
                token!(Plus, 0, 1),
                token!(Slash, 1, 2),
                token!(Asterisk, 2, 3),
                token!(LParen, 3, 4),
                token!(Minus, 4, 5),
                token!(RParen, 5, 6),
            ]),
        );

        let mut lexer = Lexer::new("(5 + 2) * 31 - -10");
        let tokens = lexer.lex();
        assert_eq!(
            tokens,
            Ok(&vec![
                token!(LParen, 0, 1),
                token!(Number(5), 1, 2),
                token!(Plus, 3, 4),
                token!(Number(2), 5, 6),
                token!(RParen, 6, 7),
                token!(Asterisk, 8, 9),
                token!(Number(31), 10, 12),
                token!(Minus, 13, 14),
                token!(Minus, 15, 16),
                token!(Number(10), 16, 18),
            ]),
        );

        let mut lexer = Lexer::new("abc = 3; def = 5; abc + def;");
        let tokens = lexer.lex();
        assert_eq!(
            tokens,
            Ok(&vec![
                token!(Identifier("abc".to_string()), 0, 3),
                token!(Assignment, 4, 5),
                token!(Number(3), 6, 7),
                token!(Semicolon, 7, 8),
                token!(Identifier("def".to_string()), 9, 12),
                token!(Assignment, 13, 14),
                token!(Number(5), 15, 16),
                token!(Semicolon, 16, 17),
                token!(Identifier("abc".to_string()), 18, 21),
                token!(Plus, 22, 23),
                token!(Identifier("def".to_string()), 24, 27),
                token!(Semicolon, 27, 28),
            ]),
        );
    }

    #[test]
    fn test_lexer_error() {
        use crate::lexer::LexError;
        let mut lexer = Lexer::new("1 $ 2 * 3 - -10");
        let tokens = lexer.lex();
        assert_eq!(tokens, Err(LexError::invalid_char('$', Loc(2, 3))),);
    }
}
