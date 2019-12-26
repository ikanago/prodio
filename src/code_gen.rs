use crate::parser::{Ast, BinOpKind, UniOpKind};
use crate::parser::AstKind::{BinOp, Num, UniOp};

/// Entry point of code generation.
pub fn code_gen(ast: &Ast) {
    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");
    println!("  push rbp");
    println!("  mov rbp, rsp");
    gen(ast);
    println!("  pop rax");
    println!("  mov rsp, rbp");
    println!("  pop rbp");
    println!("  ret");
}

/// Generate assembly code for an AST.
fn gen(ast: &Ast) {
    match &ast.value {
        Num(n) => gen_num(n),
        BinOp { op, lhs, rhs } => gen_binary_operator(op.clone().value, lhs, rhs),
        UniOp { op, node } => gen_unary_operator(op.clone().value, node),
    }
}

/// Generate code for positive number.
fn gen_num(n: &u64) {
    println!("  push {}", n);
}

/// Generate code for binary operator.
fn gen_binary_operator(op: BinOpKind, lhs: &Ast, rhs: &Ast) {
    gen(lhs);
    gen(rhs);
    println!("  pop rdi");
    println!("  pop rax");

    match op {
        BinOpKind::Add => println!("  add rax, rdi"),
        BinOpKind::Sub => println!("  sub rax, rdi"),
        BinOpKind::Mul => println!("  imul rax, rdi"),
        BinOpKind::Div => {
            println!("  cqo");
            println!("  idiv rdi");
        }
    }

    println!("  push rax");
}

/// Generate code for unary operator.
fn gen_unary_operator(op: UniOpKind, node: &Ast) {
    match op {
        UniOpKind::Plus => {
            gen(node);
            println!("  pop rax");
        }
        UniOpKind::Minus => {
            println!("  mov rax 0");
            gen(node);
            println!("  pop rdi");
            println!("  sub rax, rdi");
        }
    }
    println!("  push rax");
}
