#[macro_use]
extern crate clap;

use prodio::code_gen::Generator;
use prodio::dump_info;
use prodio::gen_ir;
use prodio::lexer::Lexer;
use prodio::parser::Parser;
use std::fs::File;
use std::io::Write;

fn main() -> std::io::Result<()> {
    let matches = clap_app!(prodio =>
        (version: crate_version!())
        (author: crate_authors!())
        (about: crate_description!())
        (@arg CODE: +required "Input source file.")
        (@arg OUTPUT: -o +takes_value "Specify output file.")
        (@arg dump_token: --("dump-token") "Dump tokens into stderr.")
        (@arg dump_ast: --("dump-ast") "Dump AST into stderr.")
        (@arg dump_ir_v: --("dump-ir-v") "Dump inner representation (using virtual register) into stderr.")
        (@arg dump_ir_r: --("dump-ir-r") "Dump inner representation (using real register) into stderr.")
    )
    .get_matches();

    if let Some(ref source_file_path) = matches.value_of("CODE") {
        let source_code = prodio::read_file_content(source_file_path)?;

        // Lex
        let mut lexer = Lexer::new(&source_code);
        let tokens = lexer.lex().unwrap();
        if matches.is_present("dump_token") {
            dump_info::dump_tokens(&tokens);
        }

        // Parse
        let mut parser = Parser::new(&tokens);
        let asts = parser.parse().unwrap();
        if matches.is_present("dump_ast") {
            dump_info::dump_asts(&asts);
        }

        // IR Generation
        let mut ir_generator = gen_ir::IRGenerator::new();
        ir_generator.gen_ir(&asts);
        if matches.is_present("dump_ir_v") {
            dump_info::dump_ir(&ir_generator);
        }

        // Register allocation
        ir_generator.reg_alloc();
        if matches.is_present("dump_ir_r") {
            dump_info::dump_ir(&ir_generator);
        }
        // let stack_offset = ir_generator.sum_stack_offset();

        // Code Generation
        let mut generator = Generator::new();
        generator.code_gen(&ir_generator);

        let output_file_path = match matches.value_of("OUTPUT") {
            Some(path) => path,
            None => "main.s",
        };
        let mut output_file = File::create(output_file_path)?;
        for code in generator.code {
            writeln!(output_file, "{}", code)?;
        }
    }
    Ok(())
}
