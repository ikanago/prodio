#[macro_use]
extern crate clap;

use clap::{App, Arg};
use prodio::code_gen::Generator;
use prodio::dump_ir;
use prodio::gen_ir;
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
        .arg(
            Arg::with_name("dump-ir")
                .long("--dump-ir")
                .value_name("DUMP-IR")
                .help("Dump inner representation.")
                .takes_value(false),
        )
        .get_matches();

    if let Some(ref raw_code) = matches.value_of("code") {
        let mut lexer = Lexer::new(&raw_code);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new();
        let ast = parser.parse(tokens.to_vec()).unwrap();
        let stack_offset = parser.stack_offset;
        let mut ir_generator = gen_ir::IRGenerator::new(parser);
        ir_generator.gen_ir(&ast);

        if matches.is_present("dump-ir") {
            dump_ir::dump(&ir_generator.ir_vec);
        } else {
            let mut generator = Generator::new();
            generator.code_gen(&ir_generator.ir_vec, stack_offset);
            for code in generator.code {
                println!("{}", code);
            }
        }
    }
}
