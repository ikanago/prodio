#[macro_use]
pub mod macros;
pub mod code_gen;
pub mod dump_info;
pub mod gen_ir;
pub mod lexer;
pub mod parser;
pub mod reg_alloc;
pub mod util;

const REGISTER_COUNT: usize = 7;
