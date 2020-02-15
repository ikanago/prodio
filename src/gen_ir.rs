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
    Return,
    Kill,
}

/// Inner representation.
/// Each `lhs` and `rhs` specifies a indice of virtual register.
/// Later, they are allocated to a real register in `reg_alloc.rs`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IR {
    pub op: IROp,
    pub lhs: Option<usize>,
    pub rhs: Option<usize>,
}

impl IR {
    pub fn new(op: IROp, lhs: Option<usize>, rhs: Option<usize>) -> Self {
        IR { op, lhs, rhs }
    }
}

/// Entry point to generate IR.
#[derive(Debug, Clone)]
pub struct IRGenerator {
    // Vec to store IRs.
    pub ir_vec: Vec<IR>,
    // HashMap of variable offset from $rbp.
    pub variable_map: HashMap<String, usize>,
    // Used register count in a AST.
    reg_count: usize,
}

impl IRGenerator {
    pub fn new(parser: Parser) -> Self {
        IRGenerator {
            ir_vec: Vec::new(),
            variable_map: parser.env,
            reg_count: 0,
        }
    }

    pub fn gen_ir(&mut self, asts: &Vec<Ast>) {
        for ast in asts {
            self.gen_expr(ast);
        }
    }

    /// Generate IR for an AST.
    fn gen_expr(&mut self, ast: &Ast) -> Option<usize> {
        match &ast.value {
            Num(n) => self.gen_ir_immidiate(*n),
            Variable(var) => self.gen_ir_variable(var),
            Decl { lhs, rhs } => self.gen_ir_decl_var(lhs, rhs),
            BinOp { op, lhs, rhs } => self.gen_ir_binary_operator(op.clone(), lhs, rhs),
            UniOp { op, node } => self.gen_ir_unary_operator(op.clone(), node),
            Assignment { lhs, rhs } => self.gen_ir_assignment(lhs, rhs),
            Return { expr } => self.gen_ir_return(expr),
        }
    }

    fn gen_ir_immidiate(&mut self, n: usize) -> Option<usize> {
        self.reg_count += 1;
        let ir = IR::new(IROp::Imm, Some(self.reg_count), Some(n));
        self.ir_vec.push(ir);
        Some(self.reg_count)
    }

    fn gen_ir_lval(&mut self, val: &String) -> Option<usize> {
        let var_offset = self.variable_map.get(val).unwrap();
        self.reg_count += 1;
        let reg_dst = self.reg_count;
        let ir = IR::new(IROp::BpOffset, Some(reg_dst), Some(*var_offset));
        self.ir_vec.push(ir);
        Some(reg_dst)
    }

    fn gen_ir_variable(&mut self, var: &String) -> Option<usize> {
        let reg = self.gen_ir_lval(var);
        let ir = IR::new(IROp::Load, reg, reg);
        self.ir_vec.push(ir);
        reg
    }

    fn gen_ir_decl_var(&mut self, lhs: &Ast, rhs: &Ast) -> Option<usize> {
        let val_name = ident_val!(&lhs.value);
        let reg_lhs = self.gen_ir_lval(&val_name);
        let reg_rhs = self.gen_expr(rhs);
        let ir = IR::new(IROp::Store, reg_lhs, reg_rhs);
        self.ir_vec.push(ir);

        self.kill(reg_lhs);
        self.kill(reg_rhs);
        None
    }

    fn gen_ir_binary_operator(&mut self, op: BinOpKind, lhs: &Ast, rhs: &Ast) -> Option<usize> {
        let reg_lhs = self.gen_expr(lhs);
        let reg_rhs = self.gen_expr(rhs);

        let ir = match op {
            BinOpKind::Add => IR::new(IROp::Add, reg_lhs, reg_rhs),
            BinOpKind::Sub => IR::new(IROp::Sub, reg_lhs, reg_rhs),
            BinOpKind::Mul => IR::new(IROp::Mul, reg_lhs, reg_rhs),
            BinOpKind::Div => IR::new(IROp::Div, reg_lhs, reg_rhs),
        };
        self.ir_vec.push(ir);
        self.kill(reg_rhs);
        reg_lhs
    }

    fn gen_ir_unary_operator(&mut self, op: UniOpKind, node: &Ast) -> Option<usize> {
        let node = self.gen_expr(node);

        let ir = match op {
            UniOpKind::Plus => panic!("Unary plus is not implemented!"),
            UniOpKind::Minus => IR::new(IROp::Minus, node, None),
        };
        self.ir_vec.push(ir);
        node
    }

    fn gen_ir_assignment(&mut self, lhs: &Ast, rhs: &Ast) -> Option<usize> {
        let val_name = ident_val!(&lhs.value);
        let reg_lhs = self.gen_ir_lval(&val_name);
        let reg_rhs = self.gen_expr(rhs);
        let ir = IR::new(IROp::Store, reg_lhs, reg_rhs);
        self.ir_vec.push(ir);

        reg_lhs
    }

    fn gen_ir_return(&mut self, expr: &Ast) -> Option<usize> {
        let reg_expr = self.gen_expr(expr);
        let ir = IR::new(IROp::Return, reg_expr, None);
        self.ir_vec.push(ir);
        reg_expr
    }

    fn kill(&mut self, reg: Option<usize>) {
        let ir = IR::new(IROp::Kill, reg, None);
        self.ir_vec.push(ir);
    }
}

#[cfg(test)]
mod tests {
    use crate::gen_ir::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    #[test]
    fn test_assignment() {
        let code = "a = 3; b = 2; c = a * b; return c;";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new();
        let ast = parser.parse(tokens.to_vec()).unwrap();
        let mut ir_generator = IRGenerator::new(parser);
        ir_generator.gen_ir(&ast);

        let ir_vec = vec![
            IR::new(IROp::Imm, Some(1), Some(3)),
            IR::new(IROp::Store, Some(8), Some(1)),
            IR::new(IROp::Imm, Some(2), Some(2)),
            IR::new(IROp::Store, Some(16), Some(2)),
            IR::new(IROp::Load, Some(3), Some(8)),
            IR::new(IROp::Load, Some(4), Some(16)),
            IR::new(IROp::Mul, Some(3), Some(4)),
            IR::new(IROp::Store, Some(24), Some(3)),
            IR::new(IROp::Load, Some(5), Some(24)),
            IR::new(IROp::Return, Some(5), None),
        ];
        assert_eq!(ir_generator.ir_vec, ir_vec)
    }
}
