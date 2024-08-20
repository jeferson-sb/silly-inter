use std::collections::HashMap;
use std::fmt::Debug;
use std::rc::Rc;

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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct ProcedureSymbol {
    pub name: String,
    pub params: Vec<VarSymbol>,
}

impl ProcedureSymbol {
    pub fn new(name: String, params: Vec<VarSymbol>) -> Self {
        ProcedureSymbol { name, params }
    }
}

impl SymbolTrait for ProcedureSymbol {
    fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Debug)]
pub struct ScopedSymbolTable {
    pub symbols: HashMap<String, Rc<dyn SymbolTrait>>,
    pub scope_name: String,
    pub scope_level: i32,
    pub enclosing_scope: Box<Option<ScopedSymbolTable>>,
}

impl ScopedSymbolTable {
    pub fn new(
        scope_name: String,
        scope_level: i32,
        enclosing_scope: Option<ScopedSymbolTable>,
    ) -> Self {
        println!("\x1b[35mNEW SCOPE {}\x1b[0m", scope_name);
        let mut st = ScopedSymbolTable {
            symbols: HashMap::new(),
            scope_name,
            scope_level,
            enclosing_scope: Box::new(enclosing_scope),
        };
        st.init_builtins();
        st
    }

    fn init_builtins(&mut self) {
        let int_type = BuiltinTypeSymbol::new(String::from("INTEGER"));
        self.define(Rc::new(int_type));
        let real_type = BuiltinTypeSymbol::new(String::from("REAL"));
        self.define(Rc::new(real_type));
    }

    pub fn define(&mut self, symbol: Rc<dyn SymbolTrait>) {
        println!("\x1b[35mINSERT {}\x1b[0m", symbol.name());
        self.symbols.insert(symbol.name().to_string(), symbol);
    }

    pub fn lookup(&self, name: &str, current_scope_only: bool) -> Option<Rc<dyn SymbolTrait>> {
        println!("\x1b[35mLOOKUP {}\x1b[0m", name);
        // Try to find the symbol in the current scope
        if let Some(symbol) = self.symbols.get(name) {
            return Some(symbol.clone());
        }

        if current_scope_only {
            return None;
        }

        // Recursively look up in the enclosing scope
        if let Some(ref enclosing_scope) = *self.enclosing_scope {
            return enclosing_scope.lookup(name, current_scope_only);
        }

        None
    }
}

#[derive(Debug)]
pub struct SemanticAnalyzer {
    // tree pointer
    pub current_scope: Option<ScopedSymbolTable>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer {
            current_scope: None,
        }
    }

    pub fn visit_block(&mut self, node: &Block) -> i64 {
        for declaration in &node.declarations {
            self.visit(declaration);
        }
        self.visit(&node.compound_statement)
    }

    pub fn visit_program(&mut self, node: &Program) -> i64 {
        println!("\x1b[35m(GLOBAL SCOPE)\x1b[0m");
        let global_scope =
            ScopedSymbolTable::new(String::from("global"), 1, self.current_scope.take());
        self.current_scope = Some(global_scope);
        self.visit(&node.block);

        // Restore scope after visiting all blocks in the program
        // self.current_scope = *self.current_scope.take().unwrap().enclosing_scope;
        println!("\x1b[35m(END GLOBAL SCOPE)\x1b[0m");
        0
    }

    // TODO: Work with other types than i64
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
        let type_name = if let AST::Type(ref value) = node.type_node {
            value.clone()
        } else {
            panic!("Expected Type node");
        };

        let type_symbol = self
            .current_scope
            .as_ref()
            .unwrap()
            .lookup(&type_name.value, true)
            .expect("Type not found");

        let var_name = if let AST::Var(ref value) = node.var_node {
            value.clone()
        } else {
            panic!("Expected Var node");
        };

        let var_symbol = VarSymbol::new(var_name.value, type_symbol);
        self.current_scope
            .as_mut()
            .unwrap()
            .define(Rc::new(var_symbol));
        0
    }

    pub fn visit_var(&self, node: &Var) -> i64 {
        let var_name = &node.value;
        let var_symbol = self
            .current_scope
            .as_ref()
            .unwrap()
            .lookup(&var_name, false)
            .expect("Symbol not found");
        0
    }

    pub fn visit_assign(&mut self, node: &Assign) -> i64 {
        if let AST::Var(var) = &node.left {
            self.visit(&node.right);
        } else {
            panic!("AssignmentError: Left side of assignment is not a variable");
        }
        0
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

    pub fn visit_proceduredecl(&mut self, node: &ProcedureDecl) -> i64 {
        let proc_name = &node.proc_name;
        let mut proc_symbol = ProcedureSymbol::new(proc_name.to_string(), vec![]);
        self.current_scope
            .as_mut()
            .unwrap()
            .define(Rc::new(proc_symbol.clone()));

        println!("\x1b[35m(ENTER SCOPE {})\x1b[0m", proc_name);

        // Scope for params and local variables
        let procedure_scope = ScopedSymbolTable::new(
            proc_name.to_string(),
            self.current_scope.as_ref().unwrap().scope_level + 1,
            self.current_scope.take(),
        );

        // new scope
        self.current_scope = Some(procedure_scope);

        for param in &node.params {
            let type_name = if let AST::Type(ref value) = param.type_node {
                value.clone()
            } else {
                panic!("Expected Type node");
            };
            let var_name = param.var_node.clone();

            let param_type = self
                .current_scope
                .as_ref()
                .unwrap()
                .lookup(&type_name.value, false);
            let var_symbol = VarSymbol::new(var_name.value, param_type.unwrap());
            self.current_scope
                .as_mut()
                .unwrap()
                .define(Rc::new(var_symbol.clone()));

            proc_symbol.params.push(var_symbol);
        }

        self.visit(&node.block_node);

        // restore scope
        // self.current_scope = *self.current_scope.take().unwrap().enclosing_scope;
        println!("\x1b[35m(LEAVE SCOPE {})\x1b[0m", proc_name);
        0
    }
}

// Tree-walking
impl NodeVisitor for SemanticAnalyzer {
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
            AST::ProcedureDecl(proc_decl) => self.visit_proceduredecl(proc_decl),
            AST::Type(type_spec) => self.visit_type(type_spec),
            AST::Compound(compound) => self.visit_compound(compound),
            AST::Param(_) => todo!(),
        }
    }
}
