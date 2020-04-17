use std::collections::HashMap;
use std::str::from_utf8;

use crate::util::{LexError, Loc};
use crate::{Token, TokenKind};

fn new_token(token_kind: TokenKind, start: usize, end: usize) -> Token {
    Token::new(token_kind, Loc(start, end))
}

fn reserve_keywords() -> HashMap<String, TokenKind> {
    let mut keywords = HashMap::new();
    keywords.insert("let".to_string(), TokenKind::Let);
    keywords.insert("u64".to_string(), TokenKind::U64);
    keywords.insert("func".to_string(), TokenKind::Func);
    keywords.insert("if".to_string(), TokenKind::If);
    keywords.insert("return".to_string(), TokenKind::Return);
    keywords
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
        let keywords = reserve_keywords();
        while self.pos < self.input.len() {
            match self.input[self.pos] {
                b'+' => self.lex_plus(),
                b'-' => self.lex_minus(),
                b'*' => self.lex_asterisk(),
                b'/' => self.lex_slash(),
                b'(' => self.lex_lparen(),
                b')' => self.lex_rparen(),
                b'{' => self.lex_lbrace(),
                b'}' => self.lex_rbrace(),
                b':' => self.lex_colon(),
                b'0'..=b'9' => self.lex_number(),
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.lex_identifier(&keywords),
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

    fn lex_lbrace(&mut self) {
        self.tokens.push(token!(LBrace, self.pos, self.pos + 1));
        self.pos += 1;
    }

    fn lex_rbrace(&mut self) {
        self.tokens.push(token!(RBrace, self.pos, self.pos + 1));
        self.pos += 1;
    }

    fn lex_colon(&mut self) {
        self.tokens.push(token!(Colon, self.pos, self.pos + 1));
        self.pos += 1;
    }

    fn lex_number(&mut self) {
        let start = self.pos;
        let end = self.recognize_multiple_char(|b| b"0123456789".contains(&b));
        let num = from_utf8(&self.input[start..end]).unwrap().parse().unwrap();

        self.tokens.push(token!(Number(num), start, end));
        self.pos = end;
    }

    fn lex_identifier(&mut self, keywords: &HashMap<String, TokenKind>) {
        let start = self.pos;
        let end = self.recognize_multiple_char(|b| b.is_ascii_alphanumeric() || b == b'_');
        let identifier = from_utf8(&self.input[start..end]).unwrap();
        let identifier = identifier.to_string();
        match keywords.get(&identifier) {
            Some(token_kind) => self.tokens.push(new_token(token_kind.clone(), start, end)),
            None => self.tokens.push(token!(Identifier(identifier), start, end)),
        }
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
    use crate::lexer::Lexer;
    use crate::util::Loc;

    #[test]
    fn test_lexer_error() {
        use crate::lexer::LexError;
        let mut lexer = Lexer::new("1 $ 2 * 3 - -10");
        let tokens = lexer.lex();
        assert_eq!(tokens, Err(LexError::invalid_char('$', Loc(2, 3))),);
    }
}
