use crate::{
    ast::*,
    lexer::Lexer,
    token::{Token, TokenType},
};

/// Parser
///
/// The parser will hold a lexer and its current token
pub struct Parser {
    lexer: Lexer,
    current_token: Token,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.get_next_token();
        Parser {
            lexer,
            current_token,
        }
    }

    pub fn syntax_error(&self) {
        panic!("Invalid syntax");
    }

    /// Compare current token type with a given token and proceed to the next
    pub fn eat(&mut self, token_type: TokenType) {
        if self.current_token.token_type == token_type {
            self.current_token = self.lexer.get_next_token();
        } else {
            self.syntax_error();
        }
    }

    /// Grammar rules
    ///
    /// Parse a factor like unary op, numbers, parenthesis, etc
    pub fn factor(&mut self) -> AST {
        let token = self.current_token.clone();
        match token.token_type {
            TokenType::PLUS => {
                self.eat(TokenType::PLUS);
                AST::UnaryOp(Box::new(UnaryOp {
                    op: token,
                    expr: self.factor(),
                }))
            }
            TokenType::MINUS => {
                self.eat(TokenType::MINUS);
                AST::UnaryOp(Box::new(UnaryOp {
                    op: token,
                    expr: self.factor(),
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

    /// term : *factor((MUL|DIV) factor)*
    pub fn term(&mut self) -> AST {
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
                left: node,
                op: token,
                right: self.factor(),
            }));
        }

        node
    }

    /*
        Arithmetic expression parser
        expr -> INTEGER PLUS INTEGER
        expr -> INTEGER MINUS INTEGER
    */
    pub fn expr(&mut self) -> AST {
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
                left: node,
                op: token,
                right: self.term(),
            }));
        }

        node
    }

    pub fn program(&mut self) -> AST {
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
            block: block_node,
        };
        self.eat(TokenType::DOT);
        AST::Program(Box::new(program_node))
    }

    pub fn compound_statement(&mut self) -> AST {
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

    pub fn statement_list(&mut self) -> Vec<AST> {
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

    pub fn statement(&mut self) -> AST {
        match self.current_token.token_type {
            TokenType::BEGIN => self.compound_statement(),
            TokenType::ID => self.assignment_statement(),
            _ => self.empty(),
        }
    }

    pub fn assignment_statement(&mut self) -> AST {
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

    pub fn variable(&mut self) -> AST {
        let node = Var {
            value: self.current_token.clone().value.unwrap(),
            token: self.current_token.clone(),
        };
        self.eat(TokenType::ID);
        AST::Var(Box::new(node))
    }

    pub fn block(&mut self) -> AST {
        let declr_nodes = self.declarations();
        let compound_statement_node = self.compound_statement();
        let node = Block {
            declarations: declr_nodes,
            compound_statement: compound_statement_node,
        };

        AST::Block(Box::new(node))
    }

    pub fn declarations(&mut self) -> Vec<AST> {
        let mut declarations = Vec::new();

        if self.current_token.token_type == TokenType::VAR {
            self.eat(TokenType::VAR);

            while self.current_token.token_type == TokenType::ID {
                let decl = self.variable_declaration();
                declarations.extend(decl);
                self.eat(TokenType::SEMI);
            }
        }

        while self.current_token.token_type == TokenType::PROCEDURE {
            self.eat(TokenType::PROCEDURE);
            let proc_name = self.current_token.clone();
            self.eat(TokenType::ID);
            self.eat(TokenType::SEMI);
            let block_node = self.block();
            let proc_decl = ProcedureDecl {
                proc_name: proc_name.value.unwrap(),
                block_node,
                params: vec![],
            };
            declarations.push(AST::ProcedureDecl(Box::new(proc_decl)));
            self.eat(TokenType::SEMI);
        }

        declarations
    }

    /// variable_declaration : ID (COMMA ID)* COLON type_spec
    pub fn variable_declaration(&mut self) -> Vec<AST> {
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
                    var_node: var_node,
                    type_node: type_node.clone(),
                }))
            })
            .collect()
    }

    pub fn type_spec(&mut self) -> AST {
        let token = self.current_token.clone();

        if token.token_type == TokenType::INTEGER {
            self.eat(TokenType::INTEGER);
        } else {
            self.eat(TokenType::REAL);
        }

        AST::Type(Box::new(Type {
            token: token.clone(),
            value: token.clone().value.unwrap(),
        }))
    }

    pub fn empty(&self) -> AST {
        AST::NoOp(Box::new(NoOp {}))
    }

    pub fn parse(&mut self) -> AST {
        let node = self.program();
        if self.current_token.token_type != TokenType::EOF {
            self.syntax_error()
        }

        node
    }
}
