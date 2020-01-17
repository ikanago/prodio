#[macro_use]
extern crate clap;

use clap::{App, Arg};
use prodio::code_gen::Generator;
use prodio::lexer::Lexer;
use prodio::parser::Parser;

fn main() {
    let matches = App::new(crate_name!())
        .version(crate_version!())
        .author(crate_authors!())
        .about(crate_description!())
        .arg(
            Arg::with_name("code")
                .long("--code")
                .value_name("CODE")
                .help("Raw source code in one line.")
                .takes_value(true),
        )
        .get_matches();

    if let Some(ref raw_code) = matches.value_of("code") {
        let mut lexer = Lexer::new(&raw_code);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new();
        let ast = parser.parse(tokens.to_vec()).unwrap();
        let mut generator = Generator::new(parser);
        generator.code_gen(&ast);
        for code in generator.code {
            println!("{}", code);
        }
    }
}
