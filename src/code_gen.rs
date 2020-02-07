use crate::gen_ir::{IROp, IR};

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
    pub fn code_gen(&mut self, ir_vec: &Vec<IR>, stack_offset: u64) {
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
        self.code.push(format!("  sub rsp, {}", stack_offset));
        for ir in ir_vec {
            self.gen(ir);
        }
        self.code
            .push(concat!("  pop rax\n", "  mov rsp, rbp\n", "  pop rbp\n", "  ret",).to_string());
    }

    /// Generate assembly code for an IR.
    fn gen(&mut self, ir: &IR) {
        match &ir.op {
            IROp::Imm => self
                .code
                .push(format!("  push {}", ir.lhs.unwrap()).to_string()),
            IROp::Add | IROp::Sub | IROp::Mul | IROp::Div => self.gen_binary_operator(ir),
            IROp::BpOffset => self.gen_bprel(ir),
            IROp::Load => self.gen_load(),
            IROp::Store => self.gen_store(),
            _ => unimplemented!(),
        }
    }

    /// Generate code for binary operator.
    fn gen_binary_operator(&mut self, ir: &IR) {
        self.code.push("  pop rdi".to_string());
        self.code.push("  pop rax".to_string());

        match ir.op {
            IROp::Add => self.code.push("  add rax, rdi".to_string()),
            IROp::Sub => self.code.push("  sub rax, rdi".to_string()),
            IROp::Mul => self.code.push("  imul rax, rdi".to_string()),
            IROp::Div => {
                self.code.push("  mov rdx, 0".to_string());
                self.code.push("  div rdi".to_string());
            }
            _ => unreachable!(),
        }

        self.code.push("  push rax".to_string());
    }

    /// Generate code for unary operator.
    // fn gen_unary_operator(&mut self, op: UniOpKind, node: &Ast) {
    //     match op {
    //         UniOpKind::Plus => {
    //             self.gen(node);
    //             self.code.push("  pop rax".to_string());
    //         }
    //         UniOpKind::Minus => {
    //             self.code.push("  mov rax 0".to_string());
    //             self.gen(node);
    //             self.code.push("  pop rdi".to_string());
    //             self.code.push("  sub rax, rdi".to_string());
    //         }
    //     }
    //     self.code.push("  push rax".to_string());
    // }

    fn gen_bprel(&mut self, ir: &IR) {
        let offset = ir.lhs.expect("Offset from $rbp is not specified.");
        self.code.push(format!("  lea rax, [rbp-{}]", offset));
        self.code.push("  push rax".to_string());
    }

    fn gen_load(&mut self) {
        self.code
            .push("  pop rax\n  mov rax, [rax]\n  push rax".to_string());
    }

    /// Generate code for storing value into variable.
    fn gen_store(&mut self) {
        self.code
            .push(concat!("  pop rdi\n", "  pop rax").to_string());
        self.code.push("  mov [rax], rdi".to_string());
    }
}
