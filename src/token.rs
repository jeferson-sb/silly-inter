#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    INTEGER,
    PLUS,
    MINUS,
    MUL,
    DIV,
    LPAREN,
    RPAREN,
    BEGIN,
    END,
    PROGRAM,
    VAR,
    INTEGER_DIV,
    INTEGER_CONST,
    REAL_CONST,
    FLOAT_DIV,
    REAL,
    COLON,
    COMMA,
    DOT,
    ID,
    ASSIGN,
    SEMI,
    PROCEDURE,
    EOF,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub value: Option<String>,
}

impl Token {
    pub fn new(token_type: TokenType, value: Option<String>) -> Self {
        Token { token_type, value }
    }
}
