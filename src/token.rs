#[derive(Debug, PartialEq, Clone)]
#[allow(non_camel_case_types)]
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

impl TokenType {
    pub fn from_char(c: char) -> Option<Self> {
        match c {
            ';' => Some(TokenType::SEMI),
            '.' => Some(TokenType::DOT),
            ':' => Some(TokenType::COLON),
            ',' => Some(TokenType::COMMA),
            '+' => Some(TokenType::PLUS),
            '-' => Some(TokenType::MINUS),
            '*' => Some(TokenType::MUL),
            '/' => Some(TokenType::FLOAT_DIV),
            '(' => Some(TokenType::LPAREN),
            ')' => Some(TokenType::RPAREN),
            _ => None,
        }
    }
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

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Token({:?}, {:#?})", self.token_type, self.value)
    }
}
