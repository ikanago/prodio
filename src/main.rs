use prodio::code_gen::Generator;
use prodio::lexer::Lexer;
use prodio::parser::Parser;

fn main() {
    let code = "(1 + 5 * 4) / 3";
    let mut lexer = Lexer::new(code);
    let tokens = lexer.lex().unwrap();
    let parser = Parser::new();
    let ast = parser.parse(tokens.to_vec()).unwrap();
    let mut generator = Generator::new();
    generator.code_gen(&ast);
    for code in generator.code {
        println!("{}", code);
    }
}
