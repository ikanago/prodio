use crate::gen_ir::{IRGenerator, IROp, IR};
use crate::REGISTER_COUNT;

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
    pub fn code_gen(&mut self, ir_generator: &IRGenerator) {
        for func in &ir_generator.funcs {
            let stack_offset = func.sum_stack_offset();
            self.code.push(
                ".intel_syntax noprefix\n.global main\nmain:\n  push rbp\n  mov rbp, rsp"
                    .to_string(),
            );
            self.code.push(format!("  sub rsp, {}", stack_offset));
            for ir in &func.ir_vec {
                self.gen(ir);
            }
            self.code
                .push((".Lreturn:\n  mov rsp, rbp\n  pop rbp\n  ret").to_string());
        }
    }

    /// Generate assembly code for an IR.
    fn gen(&mut self, ir: &IR) {
        match &ir.op {
            IROp::Imm => self.gen_immidiate(ir),
            IROp::Add | IROp::Sub | IROp::Mul | IROp::Div => self.gen_binary_operator(ir),
            IROp::Plus | IROp::Minus => self.gen_unary_operator(ir),
            IROp::BpOffset => self.gen_bprel(ir),
            IROp::Load => self.gen_load(ir),
            IROp::Store => self.gen_store(ir),
            IROp::Cond => self.gen_cond(ir),
            IROp::Label => self.gen_label(ir),
            IROp::Return => self.gen_return(ir),
            IROp::Kill => (),
        }
    }

    /// Generate code for storing immidiate to a register.
    fn gen_immidiate(&mut self, ir: &IR) {
        let reg_count = ir.lhs.unwrap();
        self.code.push(format!(
            "  mov {}, {}",
            REGISTERS[reg_count],
            ir.rhs.unwrap()
        ));
    }

    /// Generate code for binary operator.
    fn gen_binary_operator(&mut self, ir: &IR) {
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
            }
            _ => unreachable!(),
        }
    }

    /// Generate code for unary operator.
    fn gen_unary_operator(&mut self, ir: &IR) {
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
    }

    /// Generate code to store an address into the register.
    fn gen_bprel(&mut self, ir: &IR) {
        let offset = ir.rhs.expect("Offset from $rbp is not specified.");
        let reg_count = ir.lhs.unwrap();
        self.code
            .push(format!("  lea {}, [rbp-{}]", REGISTERS[reg_count], offset));
    }

    /// Make sure the destination register has an address.
    fn gen_load(&mut self, ir: &IR) {
        let src_reg_count = ir.lhs.unwrap();
        let dst_reg_count = ir.rhs.unwrap();
        self.code.push(format!(
            "  mov {}, [{}]",
            REGISTERS[src_reg_count], REGISTERS[dst_reg_count]
        ));
    }

    /// Make sure the source register has an address.
    fn gen_store(&mut self, ir: &IR) {
        let dst_reg_count = ir.lhs.unwrap();
        let src_reg_count = ir.rhs.unwrap();
        self.code.push(format!(
            "  mov [{}], {}",
            REGISTERS[dst_reg_count], REGISTERS[src_reg_count]
        ));
    }

    fn gen_cond(&mut self, ir: &IR) {
        let reg_flag = ir.lhs.unwrap();
        let label_number = ir.rhs.unwrap();
        self.code.push(format!("  cmp {}, 0", REGISTERS[reg_flag]));
        self.code.push(format!("  je .Lelse{}", label_number));
    }

    fn gen_label(&mut self, ir: &IR) {
        let label_number = ir.lhs.unwrap();
        self.code.push(format!(".Lelse{}:", label_number));
    }

    fn gen_return(&mut self, ir: &IR) {
        self.code
            .push(format!("  mov rax, {}", REGISTERS[ir.lhs.unwrap()]));
        self.code.push("  jmp .Lreturn".to_string());
    }
}
