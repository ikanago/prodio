use crate::gen_ir::{IRGenerator, IROp, IR};
use crate::ARG_REGISTER_COUNT;
use crate::REGISTER_COUNT;

const REGISTERS: [&str; REGISTER_COUNT] = ["rbx", "r10", "r11", "r12", "r13", "r14", "r15"];
const ARG_REGISTERS: [&str; ARG_REGISTER_COUNT] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

/// Struct for retain generated code.
#[derive(Debug, Default, Clone)]
pub struct Generator {
    pub code: Vec<String>,
}

impl Generator {
    pub fn new() -> Self {
        Default::default()
    }

    /// Entry point of code generation.
    pub fn code_gen(&mut self, ir_generator: &IRGenerator) {
        self.code.push(".intel_syntax noprefix\n".to_string());
        for func in &ir_generator.funcs {
            self.code.push(format!(
                ".global {}\n{}:\n  push rbp\n  mov rbp, rsp",
                func.name, func.name
            ));
            self.code.push(format!("  sub rsp, {}", func.stack_size));
            for ir in &func.ir_vec {
                self.gen(ir);
            }
            self.code.push(format!(
                ".Lreturn_{}:\n  mov rsp, rbp\n  pop rbp\n  ret\n",
                func.name
            ));
        }
    }

    /// Generate assembly code for an IR.
    fn gen(&mut self, ir: &IR) {
        match &ir.op {
            IROp::Imm => self.gen_immidiate(ir),
            IROp::Add | IROp::Sub | IROp::Mul | IROp::Div => self.gen_binary_operator(ir),
            IROp::Plus | IROp::Minus => self.gen_unary_operator(ir),
            IROp::BpOffset => self.gen_bprel(ir),
            IROp::FuncCall(name) => self.gen_func_call(ir, name.to_string()),
            IROp::Load => self.gen_load(ir),
            IROp::LoadParam => self.gen_load_param(ir),
            IROp::Store => self.gen_store(ir),
            IROp::StoreArg => self.gen_store_arg(ir),
            IROp::Cond => self.gen_cond(ir),
            IROp::Label(label_name) => self.gen_label(ir, label_name.to_string()),
            IROp::Jmp(label_name) => self.gen_jmp(label_name.to_string()),
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

    /// Generate code to call a function.
    fn gen_func_call(&mut self, ir: &IR, name: String) {
        self.save_registers(ir.lhs);
        self.code.push(format!("  call {}", name));
        let ret_reg = ir.lhs.unwrap();
        self.code.push(format!("  mov {}, rax", REGISTERS[ret_reg]));
        self.restore_registers(ir.lhs);
    }

    /// Save used registers into a stack.
    /// After register allocation, the register number in the function call IR
    /// is the number of registers that need to saved.
    fn save_registers(&mut self, reg_num: Option<usize>) {
        if let Some(reg_num) = reg_num {
            for register in REGISTERS.iter().take(reg_num) {
                self.code.push(format!("  push {}", register));
            }
        }
    }

    /// Restore registers saved before function call from a stack.
    fn restore_registers(&mut self, reg_num: Option<usize>) {
        if let Some(reg_num) = reg_num {
            for i in (0..reg_num).rev() {
                self.code.push(format!("  pop {}", REGISTERS[i]));
            }
        }
    }

    /// Make sure the destination register has an address.
    fn gen_load(&mut self, ir: &IR) {
        self.code.push(format!(
            "  mov {}, [{}]",
            REGISTERS[ir.rhs.unwrap()],
            REGISTERS[ir.lhs.unwrap()]
        ));
    }

    /// Source register: Register to pass an argument(lhs)
    /// Destination register: Register which contains an address of a result of evaled an argument(rhs)
    fn gen_load_param(&mut self, ir: &IR) {
        self.code.push(format!(
            "  mov [{}], {}",
            REGISTERS[ir.rhs.unwrap()],
            ARG_REGISTERS[ir.lhs.unwrap()]
        ));
    }

    /// Make sure the source register has an address.
    fn gen_store(&mut self, ir: &IR) {
        self.code.push(format!(
            "  mov [{}], {}",
            REGISTERS[ir.lhs.unwrap()],
            REGISTERS[ir.rhs.unwrap()]
        ));
    }

    /// Source register: Register which contains a result of evaled an argument(rhs)
    /// Destination register: Register to pass an argument(lhs)
    fn gen_store_arg(&mut self, ir: &IR) {
        self.code.push(format!(
            "  mov {}, {}",
            ARG_REGISTERS[ir.lhs.unwrap()],
            REGISTERS[ir.rhs.unwrap()]
        ));
    }

    fn gen_cond(&mut self, ir: &IR) {
        let reg_flag = ir.lhs.unwrap();
        let label_number = ir.rhs.unwrap();
        self.code.push(format!("  cmp {}, 0", REGISTERS[reg_flag]));
        self.code.push(format!("  je .Lelse{}", label_number));
    }

    fn gen_label(&mut self, ir: &IR, name: String) {
        if let Some(label_number) = ir.lhs {
            self.code.push(format!(".L{}{}:", name, label_number));
        } else {
            self.code.push(format!(".L{}:", name));
        }
    }

    fn gen_jmp(&mut self, label_name: String) {
        self.code.push(format!("  jmp .L{}", label_name));
    }

    fn gen_return(&mut self, ir: &IR) {
        self.code
            .push(format!("  mov rax, {}", REGISTERS[ir.lhs.unwrap()]));
    }
}
