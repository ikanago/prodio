// use crate::code_gen;
use std::collections::{HashMap, VecDeque};

use crate::parser::AstKind::*;
use crate::parser::{Ast, BinOpKind, UniOpKind};

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
    FuncCall(String),
    Load,
    Store,
    Cond,
    Label(String),
    Jmp(String),
    Return,
    Kill,
}

/// Inner representation.
/// Each `lhs` and `rhs` specifies a indice of virtual register or integer literal.
/// What kind of element each `lhs` and `rhs` represents is defined by kind of `IROp`.
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

/// Struct to contain pairs of variables and offset values from rbp.
#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    // Mapping variable name to offset.
    pub local_var_map: HashMap<String, usize>,
    // Current maximum offset from rbp.
    pub current_var_offset: usize,
}

impl Env {
    fn new() -> Self {
        Env {
            local_var_map: HashMap::new(),
            current_var_offset: 0,
        }
    }

    /// Add a new pair of a variable and a offset value.
    fn add(&mut self, var_name: String, offset: usize) {
        self.current_var_offset += offset;
        self.local_var_map.insert(var_name, self.current_var_offset);
    }
}

/// Entry point to generate IR.
#[derive(Debug, Clone)]
pub struct IRGenerator {
    pub funcs: Vec<Function>,
}

impl IRGenerator {
    pub fn new() -> Self {
        IRGenerator { funcs: Vec::new() }
    }

    /// Iterates over a vector of AST whose root is a function definition
    /// and generate IR for each of them.
    pub fn gen_ir(&mut self, asts: &Vec<Ast>) {
        for ast in asts {
            let mut func = Function::new();
            func.gen_ir(ast);
            self.funcs.push(func);
        }
    }

    /// Do register allocation for each `Function`.
    pub fn reg_alloc(&mut self) {
        for func in &mut self.funcs {
            func.reg_alloc();
        }
    }
}

/// Holds a result of IR generation from an AST.
#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    // Vec to store generated IRs.
    pub ir_vec: Vec<IR>,
    // Vector of `Env`.
    pub env: VecDeque<Env>,
    // Function name.
    pub name: String,
    // Used register count in the AST.
    reg_count: usize,
    // Current label number for controll statement.
    pub label_number: usize,
    // Total stack size.
    pub stack_size: usize,
}

impl Function {
    pub fn new() -> Self {
        Function {
            ir_vec: Vec::new(),
            env: VecDeque::new(),
            name: String::new(),
            reg_count: 0,
            label_number: 0,
            stack_size: 0,
        }
    }

    /// Returns sum of stack size of the function.
    pub fn sum_stack_offset(&self) -> usize {
        let mut sum = 0;
        for env in self.env.iter() {
            sum += env.current_var_offset;
        }
        sum
    }

    pub fn gen_ir(&mut self, ast: &Ast) {
        self.gen_expr(ast);
    }

    /// Generate IR for an AST.
    fn gen_expr(&mut self, ast: &Ast) -> Option<usize> {
        match &ast.value {
            Num(n) => self.gen_ir_immidiate(*n),
            Variable(var) => self.gen_ir_variable(var),
            Decl { lhs, rhs } => self.gen_ir_decl_var(lhs, rhs),
            BinOp { op, lhs, rhs } => self.gen_ir_binary_operator(op.clone(), lhs, rhs),
            UniOp { op, node } => self.gen_ir_unary_operator(op.clone(), node),
            Func { name, body } => self.gen_ir_func(name, body),
            FuncCall { name } => self.gen_ir_func_call(name.to_string()),
            If { cond, then, els } => self.gen_ir_if(cond, then, els),
            CompStmt { stmts } => self.gen_ir_comp_stmt(stmts),
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

    fn gen_ir_lval(&mut self, var_name: &String) -> Option<usize> {
        let mut env_iter = self.env.iter_mut();
        // Because `Env` of inner scope is placed in the front of vector,
        // accessibility of local variables is controlled by iterating over vector from begining.
        let var_offset = loop {
            let env = env_iter.next().expect("Variable not found");
            if let Some(offset) = env.local_var_map.get(var_name) {
                break Some(*offset);
            }
        };
        self.reg_count += 1;
        let reg_dst = Some(self.reg_count);
        let ir = IR::new(IROp::BpOffset, reg_dst, var_offset);
        self.ir_vec.push(ir);
        reg_dst
    }

    fn gen_ir_variable(&mut self, var_name: &String) -> Option<usize> {
        let reg = self.gen_ir_lval(var_name);
        let ir = IR::new(IROp::Load, reg, reg);
        self.ir_vec.push(ir);
        reg
    }

    fn gen_ir_decl_var(&mut self, lhs: &Ast, rhs: &Ast) -> Option<usize> {
        let var_name = ident_val!(&lhs.value);
        let env = self.env.front_mut().unwrap();
        env.add(var_name, 8);

        self.reg_count += 1;
        let reg_lhs = Some(self.reg_count);
        let ir = IR::new(IROp::BpOffset, reg_lhs, Some(env.current_var_offset));
        self.ir_vec.push(ir);
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
            UniOpKind::Minus => IR::new(IROp::Minus, node, None),
        };
        self.ir_vec.push(ir);
        node
    }

    fn gen_ir_func(&mut self, name: &String, body: &Vec<Ast>) -> Option<usize> {
        self.name = name.to_string();
        self.gen_ir_comp_stmt(body);
        None
    }

    fn gen_ir_func_call(&mut self, name: String) -> Option<usize> {
        self.reg_count += 1;
        let reg = Some(self.reg_count);
        let ir = IR::new(IROp::FuncCall(name), reg, None);
        self.ir_vec.push(ir);
        reg
    }

    fn gen_ir_if(&mut self, cond: &Ast, then: &Ast, _els: &Option<Box<Ast>>) -> Option<usize> {
        self.label_number += 1;
        let reg_flag = self.gen_expr(cond);
        let ir_condition = IR::new(IROp::Cond, reg_flag, Some(self.label_number));
        self.ir_vec.push(ir_condition);
        self.kill(reg_flag);

        self.gen_expr(then);
        let ir_label = IR::new(IROp::Label("else".to_string()), Some(self.label_number), None);
        self.ir_vec.push(ir_label);
        None
    }

    fn gen_ir_comp_stmt(&mut self, stmts: &Vec<Ast>) -> Option<usize> {
        self.env.push_front(Env::new());
        for stmt in stmts {
            self.gen_expr(stmt);
        }
        self.stack_size += self.sum_stack_offset();
        self.env.pop_front();
        None
    }

    fn gen_ir_assignment(&mut self, lhs: &Ast, rhs: &Ast) -> Option<usize> {
        let val_name = ident_val!(&lhs.value);
        let reg_lhs = self.gen_ir_lval(&val_name);
        let reg_rhs = self.gen_expr(rhs);
        let ir = IR::new(IROp::Store, reg_lhs, reg_rhs);
        self.ir_vec.push(ir);

        reg_lhs
    }

    fn gen_ir_label(&mut self, name: String) -> Option<usize> {
        let ir = IR::new(IROp::Label(name), None, None);
        self.ir_vec.push(ir);
        None
    }

    fn gen_ir_jmp(&mut self, label_name: String) -> Option<usize> {
        let ir = IR::new(IROp::Jmp(label_name), None, None);
        self.ir_vec.push(ir);
        None
    }

    fn gen_ir_return(&mut self, expr: &Ast) -> Option<usize> {
        let reg_expr = self.gen_expr(expr);
        let ir = IR::new(IROp::Return, reg_expr, None);
        self.ir_vec.push(ir);
        self.kill(reg_expr);
        self.gen_ir_jmp(format!("return_{}", self.name));
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
    fn test_calc() -> std::io::Result<()> {
        let source_code = crate::read_file_content("examples/calc.pr")?;
        let mut lexer = Lexer::new(&source_code);
        let tokens = lexer.lex().unwrap();
        let mut parser = Parser::new(&tokens);
        let ast = parser.parse().unwrap();
        let mut ir_generator = IRGenerator::new();
        ir_generator.gen_ir(&ast);

        assert_eq!(
            ir_generator.funcs[0].ir_vec,
            vec![
                IR::new(IROp::BpOffset, Some(1), Some(8)),
                IR::new(IROp::Imm, Some(2), Some(3)),
                IR::new(IROp::Store, Some(1), Some(2)),
                IR::new(IROp::Kill, Some(1), None),
                IR::new(IROp::Kill, Some(2), None),
                IR::new(IROp::BpOffset, Some(3), Some(16)),
                IR::new(IROp::Imm, Some(4), Some(2)),
                IR::new(IROp::Store, Some(3), Some(4)),
                IR::new(IROp::Kill, Some(3), None),
                IR::new(IROp::Kill, Some(4), None),
                IR::new(IROp::BpOffset, Some(5), Some(24)),
                IR::new(IROp::BpOffset, Some(6), Some(8)),
                IR::new(IROp::Load, Some(6), Some(6)),
                IR::new(IROp::BpOffset, Some(7), Some(16)),
                IR::new(IROp::Load, Some(7), Some(7)),
                IR::new(IROp::Mul, Some(6), Some(7)),
                IR::new(IROp::Kill, Some(7), None),
                IR::new(IROp::Store, Some(5), Some(6)),
                IR::new(IROp::Kill, Some(5), None),
                IR::new(IROp::Kill, Some(6), None),
                IR::new(IROp::BpOffset, Some(8), Some(24)),
                IR::new(IROp::Load, Some(8), Some(8)),
                IR::new(IROp::Return, Some(8), None),
                IR::new(IROp::Kill, Some(8), None),
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

        assert_eq!(
            ir_generator.funcs[0].ir_vec,
            vec![
                IR::new(IROp::BpOffset, Some(1), Some(8)),
                IR::new(IROp::Imm, Some(2), Some(1)),
                IR::new(IROp::Store, Some(1), Some(2)),
                IR::new(IROp::Kill, Some(1), None),
                IR::new(IROp::Kill, Some(2), None),
                IR::new(IROp::BpOffset, Some(3), Some(8)),
                IR::new(IROp::Load, Some(3), Some(3)),
                IR::new(IROp::Cond, Some(3), Some(1)),
                IR::new(IROp::Kill, Some(3), None),
                IR::new(IROp::BpOffset, Some(4), Some(8)),
                IR::new(IROp::Imm, Some(5), Some(2)),
                IR::new(IROp::Store, Some(4), Some(5)),
                IR::new(IROp::BpOffset, Some(6), Some(8)),
                IR::new(IROp::Load, Some(6), Some(6)),
                IR::new(IROp::Return, Some(6), None),
                IR::new(IROp::Kill, Some(6), None),
                IR::new(IROp::Label, Some(1), None),
                IR::new(IROp::BpOffset, Some(7), Some(8)),
                IR::new(IROp::Load, Some(7), Some(7)),
                IR::new(IROp::Return, Some(7), None),
                IR::new(IROp::Kill, Some(7), None),
            ]
        );
        Ok(())
    }
}
