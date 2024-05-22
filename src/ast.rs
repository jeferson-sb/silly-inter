use crate::token::Token;

// Binary operation
#[derive(Debug, Clone)]
pub struct BinOp {
    pub left: Box<AST>,
    pub op: Token,
    pub right: Box<AST>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub declarations: Vec<AST>,
    pub compound_statement: Box<AST>,
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
    pub expr: Box<AST>,
}

#[derive(Debug, Clone)]
pub struct Var {
    pub token: Token,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub var_node: Box<AST>,
    pub type_node: Box<AST>,
}

#[derive(Debug, Clone)]
pub struct Type {
    pub token: Token,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub name: String,
    pub block: Box<AST>,
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub left: AST,
    pub op: Token,
    pub right: AST,
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
    Type(Box<Type>),
    NoOp(Box<NoOp>),
}
