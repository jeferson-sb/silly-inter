use std::{borrow::Borrow, collections::HashMap};

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
    EOF,
}

#[derive(Debug, Clone, PartialEq)]
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
    reserved: HashMap<String, Token>,
}

impl Lexer {
    fn new(text: String) -> Self {
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
            ]),
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

    fn skip_comment(&mut self) {
        while self.current_char != Some('}') {
            self.advance()
        }
        self.advance()
    }

    // Return a integer consumed from the input.
    fn number(&mut self) -> Token {
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

    fn id(&mut self) -> Token {
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

    fn peek(&mut self) -> Option<char> {
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
    fn get_next_token(&mut self) -> Token {
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

            if current_char == LPAREN {
                self.advance();
                return Token::new(TokenType::LPAREN, Some(current_char.to_string()));
            }

            if current_char == RPAREN {
                self.advance();
                return Token::new(TokenType::RPAREN, Some(current_char.to_string()));
            }

            if current_char == ':' && self.peek().unwrap() == '=' {
                self.advance();
                self.advance();
                return Token::new(TokenType::ASSIGN, Some(current_char.to_string()));
            }

            if current_char == ';' {
                self.advance();
                return Token::new(TokenType::SEMI, Some(current_char.to_string()));
            }

            if current_char == '.' {
                self.advance();
                return Token::new(TokenType::DOT, Some(current_char.to_string()));
            }

            if current_char == ':' {
                self.advance();
                return Token::new(TokenType::COLON, Some(current_char.to_string()));
            }

            if current_char == ',' {
                self.advance();
                return Token::new(TokenType::COMMA, Some(current_char.to_string()));
            }

            if current_char == '/' {
                self.advance();
                return Token::new(TokenType::FLOAT_DIV, Some(current_char.to_string()));
            }
        }

        Token::new(TokenType::EOF, None)
    }
}

// AST
#[derive(Debug, Clone)]
enum AST {
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

// Binary operation
#[derive(Debug, Clone)]
struct BinOp {
    left: Box<AST>,
    op: Token,
    right: Box<AST>,
}

#[derive(Debug, Clone)]
struct Number {
    token: Token,
    value: i64,
}

// Unary
#[derive(Debug, Clone)]
struct UnaryOp {
    op: Token,
    expr: Box<AST>,
}

// AST nodes
#[derive(Debug, Clone)]
struct Compound {
    children: Vec<AST>,
}

#[derive(Debug, Clone)]
struct Assign {
    left: AST,
    op: Token,
    right: AST,
}

#[derive(Debug, Clone)]
struct Var {
    token: Token,
    value: String,
}

#[derive(Debug, Clone)]
struct Program {
    name: String,
    block: Box<AST>,
}

#[derive(Debug, Clone)]
struct Block {
    declarations: Vec<AST>,
    compound_statement: Box<AST>,
}

#[derive(Debug, Clone)]
struct VarDecl {
    var_node: Box<AST>,
    type_node: Box<AST>,
}

#[derive(Debug, Clone)]
struct Type {
    token: Token,
    value: String,
}

#[derive(Debug, Clone)]
struct NoOp;

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
    // Parse a factor like unary op, numbers, parenthesis, etc
    fn factor(&mut self) -> AST {
        let token = self.current_token.clone();
        match token.token_type {
            TokenType::PLUS => {
                self.eat(TokenType::PLUS);
                AST::UnaryOp(Box::new(UnaryOp {
                    op: token,
                    expr: Box::new(self.factor()),
                }))
            }
            TokenType::MINUS => {
                self.eat(TokenType::MINUS);
                AST::UnaryOp(Box::new(UnaryOp {
                    op: token,
                    expr: Box::new(self.factor()),
                }))
            }
            TokenType::INTEGER_CONST => {
                self.eat(TokenType::INTEGER_CONST);
                AST::Number(Number {
                    token: token.clone(),
                    value: token.value.unwrap().parse::<i64>().unwrap(),
                })
            }
            TokenType::REAL_CONST => {
                self.eat(TokenType::REAL_CONST);
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
            _ => self.variable(),
        }
    }

    /*
        term : factor((MUL|DIV) factor)*
    */
    fn term(&mut self) -> AST {
        let mut node = self.factor();

        while matches!(
            self.current_token.token_type,
            TokenType::MUL | TokenType::INTEGER_DIV | TokenType::FLOAT_DIV
        ) {
            let token = self.current_token.clone();
            match token.token_type {
                TokenType::MUL => self.eat(TokenType::MUL),
                TokenType::INTEGER_DIV => self.eat(TokenType::INTEGER_DIV),
                TokenType::FLOAT_DIV => self.eat(TokenType::FLOAT_DIV),
                _ => panic!("Unexpected token"),
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

    fn program(&mut self) -> AST {
        self.eat(TokenType::PROGRAM);
        let var_node = self.variable();
        let prog_name = match &var_node {
            AST::Var(var) => var.value.clone(),
            _ => panic!("Expected variable"),
        };
        self.eat(TokenType::SEMI);
        let block_node = self.block();
        let program_node = Program {
            name: prog_name,
            block: Box::new(block_node),
        };
        self.eat(TokenType::DOT);
        AST::Program(Box::new(program_node))
    }

    fn compound_statement(&mut self) -> AST {
        self.eat(TokenType::BEGIN);
        let nodes = self.statement_list();
        self.eat(TokenType::END);

        let mut root = Compound {
            children: Vec::new(),
        };
        for node in nodes {
            root.children.push(node);
        }

        AST::Compound(Box::new(root))
    }

    fn statement_list(&mut self) -> Vec<AST> {
        let node = self.statement();
        let mut results = vec![node];

        while self.current_token.token_type == TokenType::SEMI {
            self.eat(TokenType::SEMI);
            results.push(self.statement());
        }

        if self.current_token.token_type == TokenType::ID {
            self.syntax_error();
        }

        results
    }

    fn statement(&mut self) -> AST {
        match self.current_token.token_type {
            TokenType::BEGIN => self.compound_statement(),
            TokenType::ID => self.assignment_statement(),
            _ => self.empty(),
        }
    }

    fn assignment_statement(&mut self) -> AST {
        let left = self.variable();
        let token = self.current_token.clone();
        self.eat(TokenType::ASSIGN);
        let right = self.expr();
        AST::Assign(Box::new(Assign {
            left,
            op: token,
            right,
        }))
    }

    fn variable(&mut self) -> AST {
        let node = Var {
            value: self.current_token.clone().value.unwrap(),
            token: self.current_token.clone(),
        };
        self.eat(TokenType::ID);
        AST::Var(Box::new(node))
    }

    fn block(&mut self) -> AST {
        let declr_nodes = self.declarations();
        let compound_statement_node = self.compound_statement();
        let node = Block {
            declarations: declr_nodes,
            compound_statement: Box::new(compound_statement_node),
        };

        AST::Block(Box::new(node))
    }

    fn declarations(&mut self) -> Vec<AST> {
        let mut declarations = Vec::new();

        if self.current_token.token_type == TokenType::VAR {
            self.eat(TokenType::VAR);

            while self.current_token.token_type == TokenType::ID {
                let decl = self.variable_declaration();
                declarations.extend(decl);
                self.eat(TokenType::SEMI);
            }
        }

        declarations
    }

    fn variable_declaration(&mut self) -> Vec<AST> {
        let mut nodes = vec![AST::Var(Box::new(Var {
            token: self.current_token.clone(),
            value: self.current_token.clone().value.unwrap(),
        }))];
        self.eat(TokenType::ID);

        while self.current_token.token_type == TokenType::COMMA {
            self.eat(TokenType::COMMA);
            nodes.push(AST::Var(Box::new(Var {
                value: self.current_token.clone().value.unwrap(),
                token: self.current_token.clone(),
            })));
            self.eat(TokenType::ID);
        }

        self.eat(TokenType::COLON);

        let type_node = self.type_spec();
        nodes
            .into_iter()
            .map(|var_node| {
                AST::VarDecl(Box::new(VarDecl {
                    var_node: Box::new(var_node),
                    type_node: Box::new(type_node.clone()),
                }))
            })
            .collect()
    }

    fn type_spec(&mut self) -> AST {
        match self.current_token.token_type {
            TokenType::INTEGER => {
                self.eat(TokenType::INTEGER);
            }
            TokenType::REAL => {
                self.eat(TokenType::REAL);
            }
            _ => {
                self.syntax_error();
            }
        }
        AST::Type(Box::new(Type {
            token: self.current_token.clone(),
            value: self.current_token.clone().value.unwrap(),
        }))
    }

    fn empty(&self) -> AST {
        AST::NoOp(Box::new(NoOp {}))
    }

    fn parse(&mut self) -> AST {
        let node = self.program();
        if self.current_token.token_type != TokenType::EOF {
            self.syntax_error()
        }

        node
    }
}

// NodeVisitor
trait NodeVisitor {
    fn visit(&mut self, node: &AST) -> i64;
}

// Interpreter
struct Interpreter {
    parser: Parser,
    global_scope: HashMap<String, i64>,
}

impl Interpreter {
    fn new(parser: Parser) -> Self {
        Interpreter {
            parser,
            global_scope: HashMap::new(),
        }
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
            TokenType::INTEGER_DIV => left_val / right_val, // TODO: Fix integer division
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
        0
    }

    fn visit_compound(&mut self, node: &Compound) -> i64 {
        for child in &node.children {
            self.visit(&child);
        }
        0
    }

    // Stores the variable to global scope hash table
    fn visit_assign(&mut self, node: &Assign) {
        if let AST::Var(var) = &node.left {
            let var_name = var.token.value.as_ref().unwrap();
            let value = self.visit(&node.right);
            self.global_scope.insert(var_name.clone(), value);
        } else {
            panic!("AssignmentError: Left side of assignment is not a variable");
        }
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

fn main() {
    // Interpret the line
    let lexer = Lexer::new(String::from(
        "
    PROGRAM scopes;
    BEGIN
        x := 10 / 2.0;
    END.
    ",
    ));
    let parser = Parser::new(lexer);
    let mut interpreter = Interpreter::new(parser);
    let result = interpreter.interpret();

    // Output result
    println!("{:?}", interpreter.global_scope);
}

mod tests {
    use super::*;

    // Helpers
    fn interpret_expr(input: String) -> i64 {
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let mut interpreter = Interpreter::new(parser);
        interpreter.interpret()
    }

    fn interpret(input: String) -> i64 {
        let line = "PROGRAM hello; BEGIN n := ".to_owned() + &input + "; END.";
        let lexer = Lexer::new(line);
        let parser = Parser::new(lexer);
        let mut interpreter = Interpreter::new(parser);
        interpreter.interpret();
        *interpreter.global_scope.get("n").unwrap()
    }

    #[test]
    fn single_integer() {
        let result = interpret(String::from("3"));
        assert_eq!(result, 3);
    }

    #[test]
    fn same_operators() {
        let result = interpret(String::from("5 + 10 + 25"));
        assert_eq!(result, 40);
    }

    #[test]
    fn right_precedence() {
        let result = interpret(String::from("2 + 7 * 4"));
        assert_eq!(result, 30);
    }

    #[test]
    fn multiple_operators() {
        let result = interpret(String::from("14 + 2 * 3 - 6 DIV 2"));
        assert_eq!(result, 17);
    }

    #[test]
    fn handle_parens() {
        let result = interpret(String::from("7 + 3 * (10 DIV (12 DIV (3 + 1) - 1))"));
        assert_eq!(result, 22);
    }

    #[test]
    fn unary_op_minus() {
        let result = interpret(String::from("-3"));
        assert_eq!(result, -3);
    }

    #[test]
    fn unary_op_minus_repetitive() {
        let result = interpret(String::from("5---2"));
        assert_eq!(result, 3);
    }

    #[test]
    fn unary_op_with_parens() {
        let result = interpret(String::from("5---+-(3 + 4)"));
        assert_eq!(result, 12);
    }

    #[test]
    fn parse_begin_statement() {
        let mut lexer = Lexer::new(String::from("BEGIN a := 2; END."));
        let begintok = Token::new(TokenType::BEGIN, Some(String::from("BEGIN")));
        assert_eq!(lexer.get_next_token(), begintok);
    }

    #[test]
    fn parse_variable() {
        let mut lexer = Lexer::new(String::from("BEGIN x := 5; END."));
        let vartok = Token::new(TokenType::ID, Some(String::from("x")));
        lexer.get_next_token();
        assert_eq!(lexer.get_next_token(), vartok);
    }

    #[test]
    fn parse_assignment() {
        let mut lexer = Lexer::new(String::from("BEGIN z := 7; END."));
        let assigntok = Token::new(TokenType::ASSIGN, Some(String::from(":")));
        lexer.get_next_token();
        lexer.get_next_token();
        assert_eq!(lexer.get_next_token(), assigntok);
    }

    #[test]
    fn parse_variables_in_global_scope() {
        let lexer = Lexer::new(String::from(
            "
        PROGRAM scopes;
        BEGIN
            BEGIN
                x := 2;
                a := x;
                b := 10 * a + 10 * x DIV 4;
            END;
            x := 11;
        END.
        ",
        ));
        let parser = Parser::new(lexer);
        let mut interpreter = Interpreter::new(parser);
        interpreter.interpret();

        assert_eq!(*interpreter.global_scope.get("x").unwrap(), 11);
        assert_eq!(*interpreter.global_scope.get("a").unwrap(), 2);
        assert_eq!(*interpreter.global_scope.get("b").unwrap(), 25);
    }
}
