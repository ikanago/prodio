#[macro_use]
extern crate clap;

use prodio::code_gen::Generator;
use prodio::dump_info;
use prodio::gen_ir;
use prodio::lexer::Lexer;
use prodio::parser::Parser;

fn main() {
    let matches = clap_app!(prodio =>
        (version: crate_version!())
        (author: crate_authors!())
        (about: crate_description!())
        (@arg CODE: +required "Raw source code in one line.")
        (@arg dump_token: --("dump-token") "Dump tokens into stderr.")
        (@arg dump_ast: --("dump-ast") "Dump AST into stderr.")
        (@arg dump_ir: --("dump-ir") "Dump inner representation into stderr.")
    )
    .get_matches();

    if let Some(ref raw_code) = matches.value_of("CODE") {
        let mut lexer = Lexer::new(&raw_code);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new();
        let asts = parser.parse(tokens.to_vec()).unwrap();
        let stack_offset = parser.stack_offset;
        let mut ir_generator = gen_ir::IRGenerator::new(parser);
        ir_generator.gen_ir(&asts);

        if matches.is_present("dump_token") {
            dump_info::dump_tokens(&tokens);
        }
        if matches.is_present("dump_ast") {
            dump_info::dump_asts(&asts);
        }
        if matches.is_present("dump_ir") {
            dump_info::dump_ir(&ir_generator.ir_vec);
        }

        let mut generator = Generator::new();
        generator.code_gen(&ir_generator.ir_vec, stack_offset);
        for code in generator.code {
            println!("{}", code);
        }
    }
}
