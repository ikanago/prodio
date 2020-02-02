// use crate::code_gen;
use std::collections::HashMap;

use crate::parser::AstKind::*;
use crate::parser::{Ast, BinOpKind, Parser, UniOpKind};



macro_rules! ident_val {
    ($ident:expr) => {
        match &$ident {
            Variable(val) => val.clone(),
            _ => String::new(),
        }
    };
}

/// Kinds of IR operand.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IROp {
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
    pub variable_map: HashMap<String, u64>, // Lookup table of variable offset from $rbp.
}

impl IRGenerator {
    pub fn new(parser: Parser) -> Self {
        IRGenerator {
            ir_vec: Vec::new(),
            variable_map: parser.env,
        }
    }

    pub fn gen_ir(&mut self, asts: &Vec<Ast>) {
        for ast in asts {
            self.gen_expr(ast);
        }
    }

    /// Generate IR for an AST.
    fn gen_expr(&mut self, ast: &Ast) -> Option<u64> {
        match &ast.value {
            Num(n) => Some(*n),
            BinOp { op, lhs, rhs } => self.gen_ir_binary_operator(op.clone(), lhs, rhs),
            UniOp { op, node } => self.gen_ir_unary_operator(op.clone(), node),
            Assignment { lhs, rhs } => self.gen_ir_assignment(lhs, rhs),
            Variable(val) => self.gen_ir_variable(val),
        }
    }

    fn gen_ir_binary_operator(&mut self, op: BinOpKind, lhs: &Ast, rhs: &Ast) -> Option<u64> {
        let lhs = self.gen_expr(lhs);
        let rhs = self.gen_expr(rhs);

        let ir = match op {
            BinOpKind::Add => IR::new(IROp::Add, lhs, rhs),
            BinOpKind::Sub => IR::new(IROp::Sub, lhs, rhs),
            BinOpKind::Mul => IR::new(IROp::Mul, lhs, rhs),
            BinOpKind::Div => IR::new(IROp::Div, lhs, rhs),
        };
        self.ir_vec.push(ir);
        None
    }

    fn gen_ir_unary_operator(&mut self, op: UniOpKind, node: &Ast) -> Option<u64> {
        let node = self.gen_expr(node);

        let ir = match op {
            UniOpKind::Plus => panic!("Unary plus is not implemented!"),
            UniOpKind::Minus => IR::new(IROp::Minus, node, None),
        };
        self.ir_vec.push(ir);
        None
    }

    fn gen_ir_assignment(&mut self, lhs: &Ast, rhs: &Ast) -> Option<u64> {
        let val_name = ident_val!(&lhs.value);
        let lhs = self.gen_ir_lval(&val_name);
        let rhs = self.gen_expr(rhs);
        let ir = IR::new(IROp::Store, lhs, rhs);
        self.ir_vec.push(ir);
        None
    }

    fn gen_ir_lval(&mut self, val: &String) -> Option<u64> {
        let offset = self.variable_map.get(val).unwrap();
        let ir = IR::new(IROp::BpOffset, Some(*offset), None);
        self.ir_vec.push(ir);
        Some(*offset)
    }

    fn gen_ir_variable(&mut self, val: &String) -> Option<u64> {
        let var_offset = self.gen_ir_lval(val);
        let ir = IR::new(IROp::Load, None, None);
        self.ir_vec.push(ir);
        var_offset
    }
}

#[cfg(test)]
mod tests {
    use crate::gen_ir::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    #[test]
    fn test_assignment() {
        let code = "abc = 3; def = 5; abc + def;";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new();
        let ast = parser.parse(tokens.to_vec()).unwrap();
        let mut ir_generator = IRGenerator::new(parser);
        ir_generator.gen_ir(&ast);

        let ir_vec = vec![
            IR::new(IROp::BpOffset, Some(8), None),
            IR::new(IROp::Store, Some(8), Some(3)),
            IR::new(IROp::BpOffset, Some(16), None),
            IR::new(IROp::Store, Some(16), Some(5)),
            IR::new(IROp::BpOffset, Some(8), None),
            IR::new(IROp::Load, None, None),
            IR::new(IROp::BpOffset, Some(16), None),
            IR::new(IROp::Load, None, None),
            IR::new(IROp::Add, Some(8), Some(16)),
        ];
        assert_eq!(ir_generator.ir_vec, ir_vec)
    }
}
