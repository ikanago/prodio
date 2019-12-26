use crate::code_gen::code_gen;
use crate::lexer::Lexer;
use crate::parser::parse;

mod code_gen;
mod lexer;
mod parser;
mod util;

fn main() {
    let code = "(1 + 5 * 4) / 3";
    let mut lexer = Lexer::new(code);
    let tokens = lexer.lex().unwrap();
    let ast = parse(tokens.to_vec()).unwrap();
    code_gen(&ast);
}
