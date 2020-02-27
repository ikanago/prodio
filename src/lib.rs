#[macro_use]
pub mod macros;
pub mod code_gen;
pub mod dump_info;
pub mod gen_ir;
pub mod lexer;
pub mod parser;
pub mod reg_alloc;
pub mod util;

use std::fs::File;
use std::io::Read;

const REGISTER_COUNT: usize = 7;

pub fn read_file_content<P: AsRef<std::path::Path>>(
    source_file_path: P,
) -> Result<String, std::io::Error> {
    let mut source_file = File::open(source_file_path)?;
    let mut source_code = String::new();
    source_file.read_to_string(&mut source_code)?;
    Ok(source_code)
}
