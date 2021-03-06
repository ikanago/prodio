use crate::ir::gen_ir::IRGenerator;
use crate::parse::Ast;
use crate::token::Token;

pub fn dump_tokens(tokens: &[Token]) {
    eprintln!("------DUMP TOKEN------");
    for token in tokens {
        eprintln!("{:?}, {:?}, {:?}", token.value, token.loc.0, token.loc.1);
    }
    eprintln!();
}

pub fn dump_asts(asts: &[Ast]) {
    eprintln!("------DUMP AST------");
    for ast in asts {
        eprintln!("{:#?},", ast);
    }
    eprintln!();
}

pub fn dump_ir(ir_generator: &IRGenerator) {
    eprintln!("------DUMP IR------");
    for func in &ir_generator.funcs {
        eprintln!("{}", func.name);
        for ir in &func.ir_vec {
            eprintln!("({:?}, {:?}, {:?})", ir.op, ir.lhs, ir.rhs);
        }
    }
    eprintln!();
}
