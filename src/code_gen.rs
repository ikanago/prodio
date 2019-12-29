use crate::parser::{Ast, BinOpKind, Parser, UniOpKind};
use crate::parser::AstKind::*;

macro_rules! ident_val {
    ($ident:expr) => {
        match &$ident {
            Variable(val) => val.clone(),
            _ => String::new(),
        }
    };
}

/// Struct for retain generated code.
#[derive(Debug, Clone)]
pub struct Generator {
    pub code: Vec<String>,
    pub parser: Parser,
}

impl Generator {
    pub fn new(parser: Parser) -> Self {
        Generator {
            code: Vec::new(),
            parser,
        }
    }

    /// Entry point of code generation.
    pub fn code_gen(&mut self, asts: &Vec<Ast>) {
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
        self.code.push(format!("  sub rsp, {}", self.parser.offset));
        for ast in asts {
            self.gen(ast);
        }
        self.code
            .push(concat!("  pop rax\n", "  mov rsp, rbp\n", "  pop rbp\n", "  ret",).to_string());
    }

    /// Generate assembly code for an AST.
    fn gen(&mut self, ast: &Ast) {
        match &ast.value {
            Num(n) => self.gen_num(n),
            BinOp { op, lhs, rhs } => self.gen_binary_operator(op.clone(), lhs, rhs),
            UniOp { op, node } => self.gen_unary_operator(op.clone(), node),
            Assignment { lhs, rhs } => self.gen_assignment(lhs, rhs),
            Variable(val) => self.gen_variable(val),
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

    /// Generate code for assignment into variable.
    fn gen_assignment(&mut self, lhs: &Ast, rhs: &Ast) {
        let val_name = ident_val!(&lhs.value);
        self.gen_lval(&val_name);
        self.gen(rhs);
        self.gen_store_value();
    }

    /// Generate code of loading variable address.
    fn gen_lval(&mut self, val: &String) {
        let offset = self.parser.env.get(val).unwrap();
        self.code.push(format!("  lea rax, [rbp-{}]", offset));
        self.code.push("  push rax".to_string());
    }

    /// Generate code for storing value into variable.
    fn gen_store_value(&mut self) {
        self.code
            .push(concat!("  pop rdi\n", "  pop rax").to_string());
        self.code.push("  mov [rax], rdi".to_string());
    }

    /// Generate code of variable.
    fn gen_variable(&mut self, val: &String) {
        self.gen_lval(val);
        self.code.push("  pop rax".to_string());
        self.code.push("  mov rax, [rax]".to_string());
    }
}
