use crate::gen_ir::{IROp, IR};

const REGISTER_COUNT: usize = 7;
const REGISTERS: [&str; REGISTER_COUNT] = ["rbx", "r10", "r11", "r12", "r13", "r14", "r15"];

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
    pub fn code_gen(&mut self, ir_vec: &Vec<IR>, stack_offset: usize) {
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
        let mut result_register: usize = 0;
        for ir in ir_vec {
            result_register = self.gen(ir);
        }
        self.code
            .push(format!("  mov rax, {}", REGISTERS[result_register]));
        self.code
            .push(("  mov rsp, rbp\n  pop rbp\n  ret").to_string());
    }

    /// Generate assembly code for an IR.
    fn gen(&mut self, ir: &IR) -> usize {
        match &ir.op {
            IROp::Imm => self.gen_immidiate(ir),
            IROp::Add | IROp::Sub | IROp::Mul | IROp::Div => self.gen_binary_operator(ir),
            IROp::Plus | IROp::Minus => self.gen_unary_operator(ir),
            IROp::BpOffset => self.gen_bprel(ir),
            IROp::Load => self.gen_load(ir),
            IROp::Store => self.gen_store(ir),
            IROp::Return => unimplemented!(),
        }
    }

    /// Generate code for storing immidiate to a register.
    fn gen_immidiate(&mut self, ir: &IR) -> usize {
        let reg_count = ir.lhs.unwrap();
        self.code.push(format!(
            "  mov {}, {}",
            REGISTERS[reg_count],
            ir.rhs.unwrap()
        ));
        reg_count
    }

    /// Generate code for binary operator.
    fn gen_binary_operator(&mut self, ir: &IR) -> usize {
        let lhs_reg_count = ir.lhs.unwrap();
        let rhs_reg_count = ir.rhs.unwrap();
        match ir.op {
            IROp::Add => self.code.push(format!(
                "  add {}, {}",
                REGISTERS[lhs_reg_count], REGISTERS[rhs_reg_count]
            )),
            IROp::Sub => self.code.push(format!(
                "  sub {}, {}",
                REGISTERS[lhs_reg_count], REGISTERS[rhs_reg_count]
            )),
            IROp::Mul => self.code.push(format!(
                "  imul {}, {}",
                REGISTERS[lhs_reg_count], REGISTERS[rhs_reg_count]
            )),
            IROp::Div => {
                self.code
                    .push(format!("  mov rax, {}", REGISTERS[lhs_reg_count]));
                self.code.push("  cqo".to_string());
                self.code
                    .push(format!("  idiv {}", REGISTERS[rhs_reg_count]));
                // Because operand of `idiv` is rhs in division.
                return rhs_reg_count;
            }
            _ => unreachable!(),
        }
        lhs_reg_count
    }

    /// Generate code for unary operator.
    fn gen_unary_operator(&mut self, ir: &IR) -> usize {
        let reg_count = ir.lhs.unwrap();
        match ir.op {
            IROp::Plus => {
                self.code.push("  pop rax".to_string());
            }
            IROp::Minus => {
                self.code.push(format!("  neg {}", REGISTERS[reg_count]));
            }
            _ => unreachable!(),
        }
        reg_count
    }

    fn gen_bprel(&mut self, ir: &IR) -> usize {
        let offset = ir.lhs.expect("Offset from $rbp is not specified.");
        self.code.push(format!("  lea rax, [rbp-{}]", offset));
        self.code.push("  push rax".to_string());
        0
    }

    fn gen_load(&mut self, ir: &IR) -> usize {
        let reg_count = ir.lhs.unwrap();
        self.code.push(format!(
            "  mov {}, [rbp-{}]",
            REGISTERS[reg_count],
            ir.rhs.unwrap()
        ));
        reg_count
    }

    /// Generate code for storing value into variable.
    fn gen_store(&mut self, ir: &IR) -> usize {
        let reg_count = ir.rhs.unwrap();
        self.code.push(format!(
            "  mov [rbp-{}], {}",
            ir.lhs.unwrap(),
            REGISTERS[reg_count]
        ));
        reg_count
    }
}
