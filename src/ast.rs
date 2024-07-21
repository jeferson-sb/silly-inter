use crate::token::Token;

// Binary operation
#[derive(Debug, Clone)]
pub struct BinOp {
    pub left: AST,
    pub op: Token,
    pub right: AST,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub declarations: Vec<AST>,
    pub compound_statement: AST,
}

#[derive(Debug, Clone)]
pub struct Compound {
    pub children: Vec<AST>,
}

#[derive(Debug, Clone)]
pub struct NoOp;

#[derive(Debug, Clone)]
pub struct Number {
    pub token: Token,
    pub value: i64,
}

// Unary operation
#[derive(Debug, Clone)]
pub struct UnaryOp {
    pub op: Token,
    pub expr: AST,
}

#[derive(Debug, Clone)]
pub struct Var {
    pub token: Token,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub var_node: AST,
    pub type_node: AST,
}

#[derive(Debug, Clone)]
pub struct Type {
    pub token: Token,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub name: String,
    pub block: AST,
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub left: AST,
    pub op: Token,
    pub right: AST,
}

#[derive(Debug, Clone)]
pub struct ProcedureDecl {
    pub proc_name: String,
    pub block_node: AST,
    pub params: Vec<Param>,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub var_node: AST,
    pub type_node: AST,
}

#[derive(Debug, Clone)]
pub enum AST {
    BinOp(Box<BinOp>),
    UnaryOp(Box<UnaryOp>),
    Number(Number),
    Compound(Box<Compound>),
    Assign(Box<Assign>),
    Var(Box<Var>),
    Program(Box<Program>),
    Block(Box<Block>),
    VarDecl(Box<VarDecl>),
    ProcedureDecl(Box<ProcedureDecl>),
    Type(Box<Type>),
    Param(Box<Param>),
    NoOp(Box<NoOp>),
}
