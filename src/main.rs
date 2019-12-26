use prodio::code_gen::code_gen;
use prodio::lexer::Lexer;
use prodio::parser::parse;

fn main() {
    let code = "(1 + 5 * 4) / 3";
    let mut lexer = Lexer::new(code);
    let tokens = lexer.lex().unwrap();
    let ast = parse(tokens.to_vec()).unwrap();
    code_gen(&ast);
}
