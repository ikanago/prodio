use std::collections::HashMap;

use crate::gen_ir::{IRGenerator, IROp};
use crate::REGISTER_COUNT;

impl IRGenerator {
    pub fn reg_alloc(&mut self) {
        // Remember whether each real register is used.
        let mut is_reg_used: [bool; REGISTER_COUNT] = [false; REGISTER_COUNT];
        // Map a virtual register onto a real register.
        // key: virtual register, value: real register
        let mut reg_map: HashMap<usize, usize> = HashMap::new();

        for ir in &mut self.ir_vec {
            match ir.op {
                IROp::Imm | IROp::BpOffset | IROp::Return | IROp::Kill => {
                    ir.lhs = IRGenerator::alloc(ir.lhs, &mut is_reg_used, &mut reg_map)
                }
                IROp::Add | IROp::Sub | IROp::Mul | IROp::Div | IROp::Store | IROp::Load => {
                    ir.lhs = IRGenerator::alloc(ir.lhs, &mut is_reg_used, &mut reg_map);
                    ir.rhs = IRGenerator::alloc(ir.rhs, &mut is_reg_used, &mut reg_map);
                }
                _ => (),
            }

            if ir.op == IROp::Kill {
                let ir_reg = ir.lhs;
                let real_reg = IRGenerator::alloc(ir_reg, &mut is_reg_used, &mut reg_map);
                is_reg_used[real_reg.unwrap()] = false;
            }
        }
    }

    /// Allocate virtual register to real register.
    fn alloc(
        ir_reg: Option<usize>,
        is_reg_used: &mut [bool],
        reg_map: &mut HashMap<usize, usize>,
    ) -> Option<usize> {
        let ir_reg = ir_reg.unwrap();
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
