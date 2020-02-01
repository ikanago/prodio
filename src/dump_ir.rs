use crate::gen_ir::IR;

pub fn dump(ir_vec: Vec<IR>) {
    for ir in ir_vec {
        println!("({:?}, {:?}, {:?})", ir.op, ir.lhs, ir.rhs);
    }
}
