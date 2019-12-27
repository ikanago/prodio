use crate::parser::{Ast, BinOpKind, UniOpKind};
use crate::parser::AstKind::{BinOp, Num, UniOp};

/// Struct for retain generated code.
#[derive(Debug, Clone)]
pub struct Generator {
    pub code: Vec<String>,
}

impl Generator {
    pub fn new() -> Self {
        Generator { code: Vec::new() }
    }

    /// Entry point of code generation.
    pub fn code_gen(&mut self, ast: &Ast) {
        self.code.push(
            concat!(
            ".intel_syntax noprefix\n",
            ".global main\n",
            "main:\n",
            "  push rbp\n",
            "  mov rbp, rsp",
            )
                .to_string(),
        );
        self.gen(ast);
        self.code
            .push(concat!("  pop rax\n", "  mov rsp, rbp\n", "  pop rbp\n", "  ret", ).to_string());
    }

    /// Generate assembly code for an AST.
    fn gen(&mut self, ast: &Ast) {
        match &ast.value {
            Num(n) => self.gen_num(n),
            BinOp { op, lhs, rhs } => self.gen_binary_operator(op.clone().value, lhs, rhs),
            UniOp { op, node } => self.gen_unary_operator(op.clone().value, node),
        }
    }

    /// Generate code for positive number.
    fn gen_num(&mut self, n: &u64) {
        self.code.push(format!("  push {}", n));
    }

    /// Generate code for binary operator.
    fn gen_binary_operator(&mut self, op: BinOpKind, lhs: &Ast, rhs: &Ast) {
        self.gen(lhs);
        self.gen(rhs);
        self.code.push("  pop rdi".to_string());
        self.code.push("  pop rax".to_string());

        match op {
            BinOpKind::Add => self.code.push("  add rax, rdi".to_string()),
            BinOpKind::Sub => self.code.push("  sub rax, rdi".to_string()),
            BinOpKind::Mul => self.code.push("  imul rax, rdi".to_string()),
            BinOpKind::Div => {
                self.code.push("  cqo".to_string());
                self.code.push("  idiv rdi".to_string());
            }
        }

        self.code.push("  push rax".to_string());
    }

    /// Generate code for unary operator.
    fn gen_unary_operator(&mut self, op: UniOpKind, node: &Ast) {
        match op {
            UniOpKind::Plus => {
                self.gen(node);
                self.code.push("  pop rax".to_string());
            }
            UniOpKind::Minus => {
                self.code.push("  mov rax 0".to_string());
                self.gen(node);
                self.code.push("  pop rdi".to_string());
                self.code.push("  sub rax, rdi".to_string());
            }
        }
        self.code.push("  push rax".to_string());
    }
}
