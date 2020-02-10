// use crate::code_gen;
use std::collections::HashMap;

use crate::parser::AstKind::*;
use crate::parser::{Ast, BinOpKind, Parser, UniOpKind};

/// Kinds of IR operand.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IROp {
    Imm,
    Add,
    Sub,
    Mul,
    Div,
    Plus,
    Minus,
    BpOffset, // Load variable offset from $rbp.
    Load,
    Store,
}

/// Inner representation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IR {
    pub op: IROp,
    pub lhs: Option<u64>,
    pub rhs: Option<u64>,
}

impl IR {
    pub fn new(op: IROp, lhs: Option<u64>, rhs: Option<u64>) -> Self {
        IR { op, lhs, rhs }
    }
}

/// Entry point to generate IR.
#[derive(Debug, Clone)]
pub struct IRGenerator {
    pub ir_vec: Vec<IR>,
    // HashMap of variable offset from $rbp.
    pub variable_map: HashMap<String, u64>,
    // HashMap of which register each variable(represented by offset) is stored.
    pub variable_reg_map: HashMap<u64, u64>,
    // Used register count in a AST.
    reg_count: u64,
}

impl IRGenerator {
    pub fn new(parser: Parser) -> Self {
        IRGenerator {
            ir_vec: Vec::new(),
            variable_map: parser.env,
            variable_reg_map: HashMap::new(),
            reg_count: 0,
        }
    }

    pub fn gen_ir(&mut self, asts: &Vec<Ast>) {
        for ast in asts {
            self.gen_expr(ast);
            self.reg_count = 0;
        }
    }

    /// Generate IR for an AST.
    fn gen_expr(&mut self, ast: &Ast) -> Option<u64> {
        match &ast.value {
            Num(n) => self.gen_ir_immidiate(*n),
            BinOp { op, lhs, rhs } => self.gen_ir_binary_operator(op.clone(), lhs, rhs),
            UniOp { op, node } => self.gen_ir_unary_operator(op.clone(), node),
            Assignment { lhs, rhs } => self.gen_ir_assignment(lhs, rhs),
            Variable(val) => self.gen_ir_variable(val),
        }
    }

    fn gen_ir_immidiate(&mut self, n: u64) -> Option<u64> {
        self.reg_count += 1;
        let ir = IR::new(IROp::Imm, Some(self.reg_count), Some(n));
        self.ir_vec.push(ir);
        Some(self.reg_count)
    }

    fn gen_ir_binary_operator(&mut self, op: BinOpKind, lhs: &Ast, rhs: &Ast) -> Option<u64> {
        let reg_lhs = self.gen_expr(lhs);
        let reg_rhs = self.gen_expr(rhs);

        let ir = match op {
            BinOpKind::Add => IR::new(IROp::Add, reg_lhs, reg_rhs),
            BinOpKind::Sub => IR::new(IROp::Sub, reg_lhs, reg_rhs),
            BinOpKind::Mul => IR::new(IROp::Mul, reg_lhs, reg_rhs),
            BinOpKind::Div => IR::new(IROp::Div, reg_lhs, reg_rhs),
        };
        self.ir_vec.push(ir);
        reg_lhs
    }

    fn gen_ir_unary_operator(&mut self, op: UniOpKind, node: &Ast) -> Option<u64> {
        let node = self.gen_expr(node);

        let ir = match op {
            UniOpKind::Plus => panic!("Unary plus is not implemented!"),
            UniOpKind::Minus => IR::new(IROp::Minus, node, None),
        };
        self.ir_vec.push(ir);
        node
    }

    fn gen_ir_assignment(&mut self, lhs: &Ast, rhs: &Ast) -> Option<u64> {
        let val_name = ident_val!(&lhs.value);
        let reg_lhs = self.gen_ir_lval(&val_name);
        let reg_rhs = self.gen_expr(rhs);
        let ir = IR::new(IROp::Store, reg_lhs, reg_rhs);
        self.ir_vec.push(ir);

        let offset = self.variable_map.get(&val_name).unwrap();
        self.variable_reg_map.insert(*offset, reg_lhs.unwrap());
        reg_lhs
    }

    fn gen_ir_lval(&mut self, val: &String) -> Option<u64> {
        let offset = self.variable_map.get(val).unwrap();
        // let ir = IR::new(IROp::BpOffset, Some(*offset), None);
        // self.ir_vec.push(ir);
        Some(*offset)
    }

    fn gen_ir_variable(&mut self, val: &String) -> Option<u64> {
        self.reg_count += 1;
        let var_offset = self.gen_ir_lval(val);
        let ir = IR::new(IROp::Load, Some(self.reg_count), var_offset);
        self.ir_vec.push(ir);
        Some(self.reg_count)
    }
}

#[cfg(test)]
mod tests {
    use crate::gen_ir::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    #[test]
    fn test_assignment() {
        let code = "a = 3; b = 4; c = 2; d = a + b * -c; d;";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new();
        let ast = parser.parse(tokens.to_vec()).unwrap();
        let mut ir_generator = IRGenerator::new(parser);
        ir_generator.gen_ir(&ast);

        let ir_vec = vec![
            IR::new(IROp::Imm, Some(1), Some(3)),
            IR::new(IROp::Store, Some(8), Some(1)),
            IR::new(IROp::Imm, Some(1), Some(4)),
            IR::new(IROp::Store, Some(16), Some(1)),
            IR::new(IROp::Imm, Some(1), Some(2)),
            IR::new(IROp::Store, Some(24), Some(1)),
            IR::new(IROp::Load, Some(1), Some(8)),
            IR::new(IROp::Load, Some(2), Some(16)),
            IR::new(IROp::Load, Some(3), Some(24)),
            IR::new(IROp::Minus, Some(3), None),
            IR::new(IROp::Mul, Some(2), Some(3)),
            IR::new(IROp::Add, Some(1), Some(2)),
            IR::new(IROp::Store, Some(32), Some(1)),
            IR::new(IROp::Load, Some(1), Some(32)),
        ];
        assert_eq!(ir_generator.ir_vec, ir_vec)
    }
}
