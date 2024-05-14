use std::io::{self, Write};

// Tokens
static LPAREN: char = '(';
static RPAREN: char = ')';
static PLUS: char = '+';
static MINUS: char = '-';
static MUL: char = '*';
static DIV: char = '/';

#[derive(Debug, PartialEq, Clone)]
enum TokenType {
    INTEGER,
    PLUS,
    MINUS,
    MUL,
    DIV,
    LPAREN,
    RPAREN,
    EOF,
}

#[derive(Debug, Clone)]
struct Token {
    token_type: TokenType,
    value: Option<String>,
}

impl Token {
    fn new(token_type: TokenType, value: Option<String>) -> Self {
        Token { token_type, value }
    }
}

// Lexer
struct Lexer {
    text: String,
    pos: usize,
    current_char: Option<char>,
}

impl Lexer {
    fn new(text: String) -> Self {
        Lexer {
            text: text.clone(),
            pos: 0,
            current_char: text.chars().nth(0),
        }
    }

    // Go to the next token
    fn advance(&mut self) {
        self.pos += 1;

        if self.pos > self.text.len() - 1 {
            self.current_char = None
        } else {
            self.current_char = self.text.chars().nth(self.pos);
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(current_char) = self.current_char {
            if !current_char.is_whitespace() {
                break;
            }
            self.advance()
        }
    }

    // Return a integer consumed from the input.
    fn integer(&mut self) -> String {
        let mut result = String::new();
        while let Some(current_char) = self.current_char {
            if !current_char.is_digit(10) {
                break;
            }
            result.push(current_char);
            self.advance();
        }

        result
    }

    /*
       Lexical analyzer (or tokenizer)

       This method is responsible for breaking a text
       apart into tokens.
    */
    fn get_next_token(&mut self) -> Token {
        while let Some(current_char) = self.current_char {
            if current_char.is_whitespace() {
                self.skip_whitespace();
                continue;
            }

            if current_char.is_digit(10) {
                return Token::new(TokenType::INTEGER, Some(self.integer()));
            }

            if current_char == PLUS {
                self.advance();
                return Token::new(TokenType::PLUS, Some(current_char.to_string()));
            }

            if current_char == MINUS {
                self.advance();
                return Token::new(TokenType::MINUS, Some(current_char.to_string()));
            }

            if current_char == MUL {
                self.advance();
                return Token::new(TokenType::MUL, Some(current_char.to_string()));
            }

            if current_char == DIV {
                self.advance();
                return Token::new(TokenType::DIV, Some(current_char.to_string()));
            }

            if current_char == LPAREN {
                self.advance();
                return Token::new(TokenType::LPAREN, Some(current_char.to_string()));
            }

            if current_char == RPAREN {
                self.advance();
                return Token::new(TokenType::RPAREN, Some(current_char.to_string()));
            }
        }

        Token::new(TokenType::EOF, None)
    }
}

// AST
#[derive(Debug)]
enum AST {
    BinOp(Box<BinOp>),
    Number(Number),
}

// Binary operation
#[derive(Debug)]
struct BinOp {
    left: Box<AST>,
    op: Token,
    right: Box<AST>,
}

#[derive(Debug)]
struct Number {
    token: Token,
    value: i64,
}

/*
   Parser
   The parser will hold a lexer and its current token
*/
struct Parser {
    lexer: Lexer,
    current_token: Token,
}

impl Parser {
    fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.get_next_token();
        Parser {
            lexer,
            current_token,
        }
    }

    fn syntax_error(&self) {
        panic!("Invalid syntax");
    }

    // Compare current token type with a given token and proceed to the next
    fn eat(&mut self, token_type: TokenType) {
        if self.current_token.token_type == token_type {
            self.current_token = self.lexer.get_next_token();
        } else {
            self.syntax_error();
        }
    }

    // Grammar rules
    // Parse a factor (integer)
    fn factor(&mut self) -> AST {
        let token = self.current_token.clone();
        match token.token_type {
            TokenType::INTEGER => {
                self.eat(TokenType::INTEGER);
                AST::Number(Number {
                    token: token.clone(),
                    value: token.value.unwrap().parse::<i64>().unwrap(),
                })
            }
            TokenType::LPAREN => {
                self.eat(TokenType::LPAREN);
                let node = self.expr();
                self.eat(TokenType::RPAREN);
                node
            }
            _ => panic!("Invalid syntax"),
        }
    }

    /*
        term : factor((MUL|DIV) factor)*
    */
    fn term(&mut self) -> AST {
        let mut node = self.factor();

        while self.current_token.token_type == TokenType::MUL
            || self.current_token.token_type == TokenType::DIV
        {
            let token = self.current_token.clone();
            if token.token_type == TokenType::MUL {
                self.eat(TokenType::MUL);
            } else if token.token_type == TokenType::DIV {
                self.eat(TokenType::DIV);
            }
            node = AST::BinOp(Box::new(BinOp {
                left: Box::new(node),
                op: token,
                right: Box::new(self.factor()),
            }));
        }

        node
    }

    /*
        Arithmetic expression parser
        expr -> INTEGER PLUS INTEGER
        expr -> INTEGER MINUS INTEGER
    */
    fn expr(&mut self) -> AST {
        let mut node = self.term();

        while self.current_token.token_type == TokenType::PLUS
            || self.current_token.token_type == TokenType::MINUS
        {
            let token = self.current_token.clone();
            if token.token_type == TokenType::PLUS {
                self.eat(TokenType::PLUS);
            } else if token.token_type == TokenType::MINUS {
                self.eat(TokenType::MINUS);
            }
            node = AST::BinOp(Box::new(BinOp {
                left: Box::new(node),
                op: token,
                right: Box::new(self.term()),
            }));
        }

        node
    }

    fn parse(&mut self) -> AST {
        self.expr()
    }
}

// NodeVisitor
trait NodeVisitor {
    fn visit(&mut self, node: &AST) -> i64;
}

// Interpreter
struct Interpreter {
    parser: Parser,
}

impl Interpreter {
    fn new(parser: Parser) -> Self {
        Interpreter { parser }
    }

    fn interpret(&mut self) -> i64 {
        let tree = self.parser.parse();
        self.visit(&tree)
    }

    fn visit_binop(&mut self, node: &BinOp) -> i64 {
        let left_val = self.visit(&node.left);
        let right_val = self.visit(&node.right);

        match node.op.token_type {
            TokenType::PLUS => left_val + right_val,
            TokenType::MINUS => left_val - right_val,
            TokenType::MUL => left_val * right_val,
            TokenType::DIV => left_val / right_val,
            _ => panic!("Invalid operator"),
        }
    }

    fn visit_num(&self, node: &Number) -> i64 {
        node.value
    }
}

impl NodeVisitor for Interpreter {
    fn visit(&mut self, node: &AST) -> i64 {
        match node {
            AST::BinOp(bin_op) => self.visit_binop(bin_op),
            AST::Number(num) => self.visit_num(num),
        }
    }
}

fn main() {
    loop {
        let mut text = String::new();

        // Read line
        print!("repl> ");
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut text).unwrap();

        let input: String = text.trim().to_lowercase();

        // Skip if empty
        if input.is_empty() {
            continue;
        }

        // Exit
        if input == "exit" {
            break;
        }

        // Interpret the line
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let mut interpreter = Interpreter::new(parser);
        let result = interpreter.interpret();

        // Output result
        println!("{:?}", result);
    }
}

mod tests {
    use super::*;

    #[test]
    fn single_integer() {
        let lexer = Lexer::new(String::from("3"));
        let parser = Parser::new(lexer);
        let mut interpreter = Interpreter::new(parser);
        let result = interpreter.interpret();
        assert_eq!(result, 3);
    }

    #[test]
    fn same_operators() {
        let lexer = Lexer::new(String::from("5 + 10 + 25"));
        let parser = Parser::new(lexer);
        let mut interpreter = Interpreter::new(parser);
        let result = interpreter.interpret();
        assert_eq!(result, 40);
    }

    #[test]
    fn right_precedence() {
        let lexer = Lexer::new(String::from("2 + 7 * 4"));
        let parser = Parser::new(lexer);
        let mut interpreter = Interpreter::new(parser);
        let result = interpreter.interpret();
        assert_eq!(result, 30);
    }

    #[test]
    fn multiple_operators() {
        let lexer = Lexer::new(String::from("14 + 2 * 3 - 6 / 2"));
        let parser = Parser::new(lexer);
        let mut interpreter = Interpreter::new(parser);
        let result = interpreter.interpret();
        assert_eq!(result, 17);
    }

    #[test]
    fn handle_parens() {
        let lexer = Lexer::new(String::from("7 + 3 * (10 / (12 / (3 + 1) - 1))"));
        let parser = Parser::new(lexer);
        let mut interpreter = Interpreter::new(parser);
        let result = interpreter.interpret();
        assert_eq!(result, 22);
    }
}
