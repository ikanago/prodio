#[macro_export]
macro_rules! token {
    ($token_kind: ident, $start: expr, $end: expr) => {
        Token::new(TokenKind::$token_kind, Loc($start, $end))
    };
    ($token_kind: ident ($var: expr), $start: expr, $end: expr) => {
        Token::new(TokenKind::$token_kind($var), Loc($start, $end))
    };
}

#[macro_export]
macro_rules! ident_val {
    ($ident:expr) => {
        match &$ident {
            Variable(val) => val.clone(),
            _ => String::new(),
        }
    };
}
