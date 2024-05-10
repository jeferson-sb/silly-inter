use std::io::{self, Write};

static LPAREN: char = '(';
static RPAREN: char = ')';

static PLUS: char = '+';
static MINUS: char = '-';
static MUL: char = '*';
static DIV: char = '/';

#[derive(Debug, PartialEq, Clone)]
enum Token {
    Integer(i32),
    Plus,
    Minus,
    Mul,
    Div,
    LPAREN,
    RPAREN,
    EOF,
}

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
    fn integer(&mut self) -> i32 {
        let mut result = String::new();
        while let Some(current_char) = self.current_char {
            if !current_char.is_digit(10) {
                break;
            }
            result.push(current_char);
            self.advance();
        }

        match result.parse::<i32>() {
            Ok(value) => value,
            Err(_) => panic!("Error parsing integer"),
        }
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
                return Token::Integer(self.integer());
            }

            if current_char == PLUS {
                self.advance();
                return Token::Plus;
            }

            if current_char == MINUS {
                self.advance();
                return Token::Minus;
            }

            if current_char == MUL {
                self.advance();
                return Token::Mul;
            }

            if current_char == DIV {
                self.advance();
                return Token::Div;
            }

            if current_char == LPAREN {
                self.advance();
                return Token::LPAREN;
            }

            if current_char == RPAREN {
                self.advance();
                return Token::RPAREN;
            }
        }

        Token::EOF
    }
}

/*
   Parser / Interpreter
   Compare current token type with a given token and proceed to the next
*/
struct Interpreter {
    lexer: Lexer,
    current_token: Token,
}

impl Interpreter {
    fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.get_next_token();
        Interpreter {
            lexer,
            current_token,
        }
    }

    fn eat(&mut self, token_type: Token) {
        if self.current_token == token_type {
            self.current_token = self.lexer.get_next_token();
        } else {
            panic!("invalid syntax");
        }
    }

    // Parse a factor (integer)
    fn factor(&mut self) -> i32 {
        match self.current_token {
            Token::Integer(value) => {
                self.eat(Token::Integer(value));
                value
            }
            Token::LPAREN => {
                self.eat(Token::LPAREN);
                let result = self.expr();
                self.eat(Token::RPAREN);
                result
            }
            _ => panic!("Invalid syntax"),
        }
    }

    /*
        term : factor((MUL|DIV) factor)*
    */
    fn term(&mut self) -> i32 {
        let mut result = self.factor();

        while matches!(self.current_token, Token::Mul | Token::Div) {
            match self.current_token {
                Token::Mul => {
                    self.eat(Token::Mul);
                    result *= self.factor();
                }
                Token::Div => {
                    self.eat(Token::Div);
                    result /= self.factor();
                }
                _ => panic!("Invalid syntax"),
            }
        }
        result
    }

    /*
        Arithmetic expression parser
        expr -> INTEGER PLUS INTEGER
        expr -> INTEGER MINUS INTEGER
    */
    fn expr(&mut self) -> i32 {
        let mut result = self.term();

        while matches!(self.current_token, Token::Minus | Token::Plus) {
            match self.current_token {
                Token::Plus => {
                    self.eat(Token::Plus);
                    result += self.term();
                }
                Token::Minus => {
                    self.eat(Token::Minus);
                    result -= self.term();
                }
                _ => panic!("Invalid syntax"),
            }
        }
        result
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
        let mut interpreter = Interpreter::new(lexer);
        let result = interpreter.expr();

        // Output result
        println!("{:?}", result);
    }
}

mod tests {
    use super::*;

    #[test]
    fn single_integer() {
        let lexer = Lexer::new(String::from("3"));
        let mut interpreter = Interpreter::new(lexer);
        let result = interpreter.expr();
        assert_eq!(result, 3);
    }

    #[test]
    fn same_operators() {
        let lexer = Lexer::new(String::from("5 + 10 + 25"));
        let mut interpreter = Interpreter::new(lexer);
        let result = interpreter.expr();
        assert_eq!(result, 40);
    }

    #[test]
    fn right_precedence() {
        let lexer = Lexer::new(String::from("2 + 7 * 4"));
        let mut interpreter = Interpreter::new(lexer);
        let result = interpreter.expr();
        assert_eq!(result, 30);
    }

    #[test]
    fn multiple_operators() {
        let lexer = Lexer::new(String::from("14 + 2 * 3 - 6 / 2"));
        let mut interpreter = Interpreter::new(lexer);
        let result = interpreter.expr();
        assert_eq!(result, 17);
    }

    #[test]
    fn handle_parens() {
        let lexer = Lexer::new(String::from("7 + 3 * (10 / (12 / (3 + 1) - 1))"));
        let mut interpreter = Interpreter::new(lexer);
        let result = interpreter.expr();
        assert_eq!(result, 22);
    }
}
