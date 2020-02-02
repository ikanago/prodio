use crate::gen_ir::IR;
use crate::lexer::Token;

pub fn dump_tokens(tokens: &Vec<Token>) {
    for token in tokens {
        eprintln!("{:?}", token.value);
    }
}

pub fn dump_ir(ir_vec: &Vec<IR>) {
    for ir in ir_vec {
        eprintln!("({:?}, {:?}, {:?})", ir.op, ir.lhs, ir.rhs);
    }
}
