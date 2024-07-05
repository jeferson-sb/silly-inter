use std::collections::HashMap;

use crate::{ast::*, parser::Parser, token::TokenType};

/// Tree-walker
///
/// Going through every child node from left to right (postorder-traversel)
pub trait NodeVisitor {
    fn visit(&mut self, node: &AST) -> i64;
}

pub struct Interpreter {
    pub parser: Parser,
    pub global_scope: HashMap<String, i64>,
}

impl Interpreter {
    pub fn new(parser: Parser) -> Self {
        Interpreter {
            parser,
            global_scope: HashMap::new(),
        }
    }

    pub fn interpret(&mut self) -> i64 {
        let tree = self.parser.parse();
        self.visit(&tree)
    }

    fn visit_binop(&mut self, node: &BinOp) -> i64 {
        // TODO: Make it work with floats
        let left_val = self.visit(&node.left);
        let right_val = self.visit(&node.right);

        match node.op.token_type {
            TokenType::PLUS => left_val + right_val,
            TokenType::MINUS => left_val - right_val,
            TokenType::MUL => left_val * right_val,
            TokenType::INTEGER_DIV => left_val / right_val,
            TokenType::FLOAT_DIV => left_val / right_val,
            _ => panic!("Invalid operator"),
        }
    }

    fn visit_unaryop(&mut self, node: &UnaryOp) -> i64 {
        let expr_val = self.visit(&node.expr);

        match node.op.token_type {
            TokenType::PLUS => expr_val,
            TokenType::MINUS => -expr_val,
            _ => panic!("Invalid operator"),
        }
    }

    fn visit_num(&self, node: &Number) -> i64 {
        node.value
    }

    fn visit_noop(&self, _node: &NoOp) -> i64 {
        // TODO: change to empty block and remove i64
        0
    }

    fn visit_compound(&mut self, node: &Compound) -> i64 {
        for child in &node.children {
            self.visit(&child);
        }
        0
    }

    // Stores the variable to global scope hash table
    fn visit_assign(&mut self, node: &Assign) -> i64 {
        if let AST::Var(var) = &node.left {
            let var_name = var.token.value.as_ref().unwrap();
            let value = self.visit(&node.right);
            self.global_scope.insert(var_name.clone(), value);
        } else {
            panic!("AssignmentError: Left side of assignment is not a variable");
        }
        0
    }

    fn visit_var(&self, node: &Var) -> i64 {
        let var_name = node.token.value.as_ref().unwrap();
        match self.global_scope.get(var_name) {
            Some(&val) => val,
            None => panic!("NameError: Variable not defined {}", var_name),
        }
    }

    fn visit_program(&mut self, node: &Program) -> i64 {
        self.visit(&node.block)
    }

    fn visit_block(&mut self, node: &Block) -> i64 {
        for declaration in &node.declarations {
            self.visit(declaration);
        }

        self.visit(&node.compound_statement)
    }

    fn visit_vardecl(&self, node: &VarDecl) -> i64 {
        0
    }

    fn visit_type(&self, node: &Type) -> i64 {
        0
    }
}

impl NodeVisitor for Interpreter {
    fn visit(&mut self, node: &AST) -> i64 {
        match node {
            AST::Program(prog) => self.visit_program(prog),
            AST::Block(block) => self.visit_block(block),
            AST::Var(var) => self.visit_var(var),
            AST::UnaryOp(unary_op) => self.visit_unaryop(unary_op),
            AST::Number(num) => self.visit_num(num),
            AST::BinOp(bin_op) => self.visit_binop(bin_op),
            AST::NoOp(noop) => self.visit_noop(noop),
            AST::Assign(assign) => self.visit_assign(assign),
            AST::VarDecl(var_decl) => self.visit_vardecl(var_decl),
            AST::ProcedureDecl(proc_decl) => 0,
            AST::Type(type_spec) => self.visit_type(type_spec),
            AST::Compound(compound) => self.visit_compound(compound),
        }
    }
}
