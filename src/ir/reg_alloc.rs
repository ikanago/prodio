use std::collections::HashMap;

use crate::ir::gen_ir::{Function, IROp};
use crate::REGISTER_COUNT;

impl Function {
    pub fn reg_alloc(&mut self) {
        // Remember whether each real register is used.
        let mut is_reg_used = [false; REGISTER_COUNT];
        // Map a virtual register onto a real register.
        // key: virtual register, value: real register
        let mut reg_map: HashMap<usize, usize> = HashMap::new();

        for ir in &mut self.ir_vec {
            match ir.op {
                IROp::Imm
                | IROp::Minus
                | IROp::BpOffset
                | IROp::Cond
                | IROp::Return
                | IROp::FuncCall(_) => {
                    ir.lhs = Function::alloc(ir.lhs, &mut is_reg_used, &mut reg_map)
                }
                IROp::LoadParam | IROp::StoreArg => {
                    ir.rhs = Function::alloc(ir.rhs, &mut is_reg_used, &mut reg_map);
                }
                IROp::Add | IROp::Sub | IROp::Mul | IROp::Div | IROp::Store | IROp::Load => {
                    ir.lhs = Function::alloc(ir.lhs, &mut is_reg_used, &mut reg_map);
                    ir.rhs = Function::alloc(ir.rhs, &mut is_reg_used, &mut reg_map);
                }
                IROp::Kill => {
                    ir.lhs = Function::alloc(ir.lhs, &mut is_reg_used, &mut reg_map);
                    is_reg_used[ir.lhs.unwrap()] = false;
                }
                _ => (),
            }
        }
    }

    /// Allocate virtual register to real register.
    fn alloc(
        ir_reg: Option<usize>,
        is_reg_used: &mut [bool],
        reg_map: &mut HashMap<usize, usize>,
    ) -> Option<usize> {
        let ir_reg = ir_reg.expect(&format!("{:?}", ir_reg));
        if let Some(real_reg) = reg_map.get(&ir_reg) {
            return Some(*real_reg);
        }

        for i in 0..REGISTER_COUNT {
            if is_reg_used[i] {
                continue;
            }
            is_reg_used[i] = true;
            reg_map.insert(ir_reg, i);
            return Some(i);
        }

        for (r, v) in reg_map.iter() {
            println!("{}: {}", r, v);
        }
        panic!("No availabale register: {}", ir_reg);
    }
}

#[cfg(test)]
mod tests {
    use crate::ir::gen_ir::*;
    use crate::parse::parser::Parser;
    use crate::token::lexer::Lexer;
    #[test]
    fn test_calc() -> std::io::Result<()> {
        let source_code = crate::read_file_content("examples/calc.pr")?;
        let mut lexer = Lexer::new(&source_code);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new(&tokens);
        let ast = parser.parse().unwrap();
        let mut ir_generator = IRGenerator::new();
        ir_generator.gen_ir(&ast);
        ir_generator.reg_alloc();

        assert_eq!(
            ir_generator.funcs[0].ir_vec,
            vec![
                IR::new(IROp::BpOffset, Some(0), Some(8)),
                IR::new(IROp::LoadParam, Some(0), Some(0)),
                IR::new(IROp::Kill, Some(0), None),
                IR::new(IROp::BpOffset, Some(0), Some(16)),
                IR::new(IROp::LoadParam, Some(1), Some(0)),
                IR::new(IROp::Kill, Some(0), None),
                IR::new(IROp::BpOffset, Some(0), Some(24)),
                IR::new(IROp::Imm, Some(1), Some(4)),
                IR::new(IROp::Store, Some(0), Some(1)),
                IR::new(IROp::Kill, Some(0), None),
                IR::new(IROp::Kill, Some(1), None),
                IR::new(IROp::BpOffset, Some(0), Some(32)),
                IR::new(IROp::Imm, Some(1), Some(2)),
                IR::new(IROp::Store, Some(0), Some(1)),
                IR::new(IROp::Kill, Some(0), None),
                IR::new(IROp::Kill, Some(1), None),
                IR::new(IROp::BpOffset, Some(0), Some(8)),
                IR::new(IROp::Load, Some(0), Some(0)),
                IR::new(IROp::BpOffset, Some(1), Some(24)),
                IR::new(IROp::Load, Some(1), Some(1)),
                IR::new(IROp::Mul, Some(0), Some(1)),
                IR::new(IROp::Kill, Some(1), None),
                IR::new(IROp::BpOffset, Some(1), Some(16)),
                IR::new(IROp::Load, Some(1), Some(1)),
                IR::new(IROp::BpOffset, Some(2), Some(32)),
                IR::new(IROp::Load, Some(2), Some(2)),
                IR::new(IROp::Mul, Some(1), Some(2)),
                IR::new(IROp::Kill, Some(2), None),
                IR::new(IROp::Add, Some(0), Some(1)),
                IR::new(IROp::Kill, Some(1), None),
                IR::new(IROp::Return, Some(0), None),
                IR::new(IROp::Kill, Some(0), None),
                IR::new(IROp::Jmp("return_f".to_string()), None, None),
            ]
        );
        assert_eq!(
            ir_generator.funcs[1].ir_vec,
            vec![
                IR::new(IROp::BpOffset, Some(0), Some(8)),
                IR::new(IROp::Imm, Some(1), Some(3)),
                IR::new(IROp::Store, Some(0), Some(1)),
                IR::new(IROp::Kill, Some(0), None),
                IR::new(IROp::Kill, Some(1), None),
                IR::new(IROp::BpOffset, Some(0), Some(16)),
                IR::new(IROp::Imm, Some(1), Some(2)),
                IR::new(IROp::Store, Some(0), Some(1)),
                IR::new(IROp::Kill, Some(0), None),
                IR::new(IROp::Kill, Some(1), None),
                IR::new(IROp::BpOffset, Some(0), Some(24)),
                IR::new(IROp::BpOffset, Some(1), Some(8)),
                IR::new(IROp::Load, Some(1), Some(1)),
                IR::new(IROp::StoreArg, Some(0), Some(1)),
                IR::new(IROp::BpOffset, Some(2), Some(16)),
                IR::new(IROp::Load, Some(2), Some(2)),
                IR::new(IROp::StoreArg, Some(1), Some(2)),
                IR::new(IROp::FuncCall("f".to_string()), Some(3), None),
                IR::new(IROp::Kill, Some(1), None),
                IR::new(IROp::Kill, Some(2), None),
                IR::new(IROp::Store, Some(0), Some(3)),
                IR::new(IROp::Kill, Some(0), None),
                IR::new(IROp::Kill, Some(3), None),
                IR::new(IROp::BpOffset, Some(0), Some(24)),
                IR::new(IROp::Load, Some(0), Some(0)),
                IR::new(IROp::Return, Some(0), None),
                IR::new(IROp::Kill, Some(0), None),
                IR::new(IROp::Jmp("return_main".to_string()), None, None),
            ]
        );
        Ok(())
    }

    #[test]
    fn test_stmt() -> std::io::Result<()> {
        let source_code = crate::read_file_content("examples/stmt.pr")?;
        let mut lexer = Lexer::new(&source_code);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new(&tokens);
        let ast = parser.parse().unwrap();
        let mut ir_generator = IRGenerator::new();
        ir_generator.gen_ir(&ast);
        ir_generator.reg_alloc();

        assert_eq!(
            ir_generator.funcs[0].ir_vec,
            vec![
                IR::new(IROp::BpOffset, Some(0), Some(8)),
                IR::new(IROp::Imm, Some(1), Some(1)),
                IR::new(IROp::Store, Some(0), Some(1)),
                IR::new(IROp::Kill, Some(0), None),
                IR::new(IROp::Kill, Some(1), None),
                IR::new(IROp::BpOffset, Some(0), Some(8)),
                IR::new(IROp::Load, Some(0), Some(0)),
                IR::new(IROp::Cond, Some(0), Some(1)),
                IR::new(IROp::Kill, Some(0), None),
                IR::new(IROp::BpOffset, Some(0), Some(8)),
                IR::new(IROp::Imm, Some(1), Some(2)),
                IR::new(IROp::Store, Some(0), Some(1)),
                IR::new(IROp::BpOffset, Some(2), Some(8)),
                IR::new(IROp::Load, Some(2), Some(2)),
                IR::new(IROp::Return, Some(2), None),
                IR::new(IROp::Kill, Some(2), None),
                IR::new(IROp::Jmp("return_main".to_string()), None, None),
                IR::new(IROp::Label("else".to_string()), Some(1), None),
                IR::new(IROp::BpOffset, Some(2), Some(8)),
                IR::new(IROp::Load, Some(2), Some(2)),
                IR::new(IROp::Return, Some(2), None),
                IR::new(IROp::Kill, Some(2), None),
                IR::new(IROp::Jmp("return_main".to_string()), None, None),
            ]
        );
        Ok(())
    }
}
