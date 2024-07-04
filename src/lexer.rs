use std::collections::HashMap;

use crate::token::{Token, TokenType};

pub struct Lexer {
    text: String,
    pos: usize,
    current_char: Option<char>,
    reserved: HashMap<String, Token>,
}

impl Lexer {
    pub fn new(text: String) -> Self {
        Lexer {
            text: text.clone(),
            pos: 0,
            current_char: text.chars().nth(0),
            reserved: HashMap::from([
                (
                    "BEGIN".to_string(),
                    Token::new(TokenType::BEGIN, Some("BEGIN".to_string())),
                ),
                (
                    "END".to_string(),
                    Token::new(TokenType::END, Some("END".to_string())),
                ),
                (
                    "PROGRAM".to_string(),
                    Token::new(TokenType::PROGRAM, Some("PROGRAM".to_string())),
                ),
                (
                    "VAR".to_string(),
                    Token::new(TokenType::VAR, Some("VAR".to_string())),
                ),
                (
                    "INTEGER".to_string(),
                    Token::new(TokenType::INTEGER, Some("INTEGER".to_string())),
                ),
                (
                    "DIV".to_string(),
                    Token::new(TokenType::INTEGER_DIV, Some("DIV".to_string())),
                ),
                (
                    "REAL".to_string(),
                    Token::new(TokenType::REAL, Some("REAL".to_string())),
                ),
                (
                    "PROCEDURE".to_string(),
                    Token::new(TokenType::PROCEDURE, Some("PROCEDURE".to_string())),
                ),
            ]),
        }
    }

    // Go to the next token
    pub fn advance(&mut self) {
        self.pos += 1;

        if self.pos > self.text.len() - 1 {
            self.current_char = None
        } else {
            self.current_char = self.text.chars().nth(self.pos);
        }
    }

    pub fn skip_whitespace(&mut self) {
        while let Some(current_char) = self.current_char {
            if !current_char.is_whitespace() {
                break;
            }
            self.advance()
        }
    }

    pub fn skip_comment(&mut self) {
        while self.current_char != Some('}') {
            self.advance()
        }
        self.advance()
    }

    // Return a integer consumed from the input.
    pub fn number(&mut self) -> Token {
        let mut result = String::new();

        while let Some(char) = self.current_char {
            if !char.is_digit(10) {
                break;
            }
            result.push(char);
            self.advance();
        }

        if self.current_char == Some('.') {
            result.push('.');
            self.advance();

            while let Some(char) = self.current_char {
                if !char.is_digit(10) {
                    break;
                }
                result.push(char);
                self.advance();
            }

            Token::new(TokenType::REAL_CONST, Some(result))
        } else {
            Token::new(TokenType::INTEGER_CONST, Some(result))
        }
    }

    pub fn id(&mut self) -> Token {
        let mut result = String::new();
        while let Some(char) = self.current_char {
            if !char.is_alphanumeric() {
                break;
            }
            result.push(char);
            self.advance();
        }

        if let Some(token) = self.reserved.get(&result.to_uppercase()) {
            token.clone()
        } else {
            Token::new(TokenType::ID, Some(result))
        }
    }

    /// Use to differentiate between tokens that starts with same char :=, ==, =>
    pub fn peek(&mut self) -> Option<char> {
        let peek_pos = self.pos + 1;
        if peek_pos > self.text.len() - 1 {
            None
        } else {
            self.text.chars().nth(peek_pos)
        }
    }

    /*
       Lexical analyzer (or tokenizer)

       This method is responsible for breaking a text
       apart into tokens.
    */
    pub fn get_next_token(&mut self) -> Token {
        while let Some(current_char) = self.current_char {
            if current_char.is_whitespace() {
                self.skip_whitespace();
                continue;
            }

            if current_char.is_digit(10) {
                return self.number();
            }

            if current_char.is_alphanumeric() {
                return self.id();
            }

            if current_char == '{' {
                self.advance();
                self.skip_comment();
                continue;
            }

            if current_char == ':' && self.peek().unwrap() == '=' {
                self.advance();
                self.advance();
                return Token::new(TokenType::ASSIGN, Some(String::from(":=")));
            }

            if let Some(token_type) = TokenType::from_char(current_char) {
                self.advance();
                return Token::new(token_type, Some(current_char.to_string()));
            }
        }

        Token::new(TokenType::EOF, None)
    }
}
