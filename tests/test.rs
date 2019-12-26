extern crate prodio;

use std::fs::File;
use std::io::{self, Write};
use std::process::Command;
use std::str;

use prodio::code_gen::code_gen;
use prodio::lexer::Lexer;
use prodio::parser::parse;

#[test]
fn test_binary_operator() {
    let code = "(1 + 5 * 4) / 3";
    let mut lexer = Lexer::new(code);
    let tokens = lexer.lex().unwrap();
    let ast = parse(tokens.to_vec()).unwrap();
    code_gen(&ast);

    let file = File::create("tmp.s").unwrap();
    let mut writer = io::BufWriter::new(file);
    write!(writer, "{}", "OUTPUT ASSEMBLY HERE").unwrap();

    Command::new("gcc -o tmp tmp.s").spawn().unwrap();
    let status = Command::new("./tmp").status().unwrap();
    assert_eq!(status.code(), Some(7));
}
