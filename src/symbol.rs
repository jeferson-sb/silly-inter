use std::fmt::Debug;
use std::{collections::HashMap, rc::Rc};

use crate::ast::*;
use crate::interpr::NodeVisitor;

pub trait SymbolTrait: Debug {
    fn name(&self) -> &str;
}

#[derive(Debug)]
pub struct Symbol {
    pub name: String,
    pub symbol_type: Option<Rc<dyn SymbolTrait>>,
}

impl Symbol {
    pub fn new(name: String, symbol_type: Option<Rc<dyn SymbolTrait>>) -> Self {
        Symbol { name, symbol_type }
    }
}

impl SymbolTrait for Symbol {
    fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Debug)]
pub struct BuiltinTypeSymbol {
    pub name: String,
}

impl BuiltinTypeSymbol {
    pub fn new(name: String) -> Self {
        BuiltinTypeSymbol { name }
    }
}

impl SymbolTrait for BuiltinTypeSymbol {
    fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Debug)]
pub struct VarSymbol {
    pub name: String,
    pub var_type: Rc<dyn SymbolTrait>,
}

impl VarSymbol {
    pub fn new(name: String, var_type: Rc<dyn SymbolTrait>) -> Self {
        VarSymbol { name, var_type }
    }
}

impl SymbolTrait for VarSymbol {
    fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    pub symbols: HashMap<String, Rc<dyn SymbolTrait>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        let mut st = SymbolTable {
            symbols: HashMap::new(),
        };
        st.init_builtins();
        st
    }

    fn init_builtins(&mut self) {
        let int_type = BuiltinTypeSymbol::new(String::from("INTEGER"));
        self.define(Rc::new(int_type));
        let real_type = BuiltinTypeSymbol::new(String::from("REAL"));
        self.define(Rc::new(real_type));
        // TMP: prevent colon variable declaration token from not being able to lookup
        let colon_symbol = Symbol::new(String::from(";"), None);
        self.define(Rc::new(colon_symbol));
    }

    pub fn define(&mut self, symbol: Rc<dyn SymbolTrait>) {
        self.symbols.insert(symbol.name().to_string(), symbol);
    }

    pub fn lookup(&self, name: &str) -> Option<Rc<dyn SymbolTrait>> {
        self.symbols.get(name).cloned()
    }
}

pub struct SymbolTableBuilder {
    pub symtab: SymbolTable,
}

impl SymbolTableBuilder {
    pub fn new() -> Self {
        SymbolTableBuilder {
            symtab: SymbolTable::new(),
        }
    }

    pub fn visit_block(&mut self, node: &Block) -> i64 {
        for declaration in &node.declarations {
            self.visit(declaration);
        }
        self.visit(&node.compound_statement)
    }

    pub fn visit_program(&mut self, node: &Program) -> i64 {
        self.visit(&node.block)
    }

    pub fn visit_compound(&mut self, node: &Compound) -> i64 {
        for child in &node.children {
            self.visit(child);
        }
        0
    }

    pub fn visit_binop(&mut self, node: &BinOp) -> i64 {
        self.visit(&node.left);
        self.visit(&node.right)
    }

    pub fn visit_unaryop(&mut self, node: &UnaryOp) -> i64 {
        self.visit(&node.expr)
    }

    pub fn visit_vardecl(&mut self, node: &VarDecl) -> i64 {
        let type_name = if let AST::Type(ref value) = *node.type_node {
            value.clone()
        } else {
            panic!("Expected Type node");
        };

        let type_symbol = self
            .symtab
            .lookup(&type_name.value)
            .expect("Type not found");

        let var_name = if let AST::Var(ref value) = *node.var_node {
            value.clone()
        } else {
            panic!("Expected Var node");
        };

        let var_symbol = VarSymbol::new(var_name.value, type_symbol);
        self.symtab.define(Rc::new(var_symbol));
        0
    }

    pub fn visit_var(&self, node: &Var) -> i64 {
        let var_name = &node.value;
        let var_symbol = self.symtab.lookup(&var_name);
        0
    }

    pub fn visit_assign(&mut self, node: &Assign) -> i64 {
        if let AST::Var(var) = &node.left {
            let var_name = var.token.value.as_ref().unwrap();
            let var_symbol = self.symtab.lookup(&var_name);
            self.visit(&node.right);
            0
        } else {
            panic!("AssignmentError: Left side of assignment is not a variable");
        }
    }

    pub fn visit_num(&self, node: &Number) -> i64 {
        node.value
    }

    pub fn visit_noop(&self, node: &NoOp) -> i64 {
        0
    }

    pub fn visit_type(&self, node: &Type) -> i64 {
        0
    }
}

// Tree-walking
impl NodeVisitor for SymbolTableBuilder {
    fn visit(&mut self, node: &AST) -> i64 {
        match node {
            AST::Program(prog) => self.visit_program(prog),
            AST::Block(block) => self.visit_block(block),
            AST::Var(var) => self.visit_var(var),
            AST::UnaryOp(unary_op) => self.visit_unaryop(unary_op),
            AST::Number(num) => self.visit_num(num),
            AST::BinOp(bin_op) => self.visit_binop(bin_op),
            AST::NoOp(noop) => self.visit_noop(noop),
            AST::Assign(assign) => {
                self.visit_assign(assign);
                0
            }
            AST::VarDecl(var_decl) => self.visit_vardecl(var_decl),
            AST::Type(type_spec) => self.visit_type(type_spec),
            AST::Compound(compound) => self.visit_compound(compound),
        }
    }
}
