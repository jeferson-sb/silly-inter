use std::rc::Rc;

use interpr::Interpreter;
use lexer::Lexer;
use parser::Parser;

use crate::{
    interpr::NodeVisitor,
    symbol::{BuiltinTypeSymbol, Symbol, SymbolTable, SymbolTableBuilder, VarSymbol},
};

mod ast;
mod interpr;
mod lexer;
mod parser;
mod symbol;
mod token;

fn interpreter_example() {
    let line = String::from(
        "
    PROGRAM hello; 
    VAR
        z : INTEGER;
    BEGIN 

    END.
    ",
    );
    let lexer = Lexer::new(line);
    let parser = Parser::new(lexer);
    let mut interpreter = Interpreter::new(parser);
    interpreter.interpret();

    println!("{:?}", interpreter.global_scope);
}

fn symbol_example() {
    // Interpret the line
    let input = String::from(
        "
    PROGRAM symbols;
    VAR
        x : INTEGER;
        y : REAL;
    BEGIN

    END.
    ",
    );
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let tree = parser.parse();
    let mut symtab_builder = SymbolTableBuilder::new();

    symtab_builder.visit(&tree);
    println!("{:?}", symtab_builder.symtab);
}

fn main() {
    symbol_example();
}

mod tests {
    use crate::token::{Token, TokenType};

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

    #[test]
    fn define_a_symbol() {
        let mut symtab = SymbolTable::new();
        let int_type = Symbol::new(String::from("INTEGER"), None);
        symtab.define(Rc::new(int_type));

        assert_eq!(symtab.symbols.is_empty(), false);
        assert_eq!(symtab.symbols.get("INTEGER").unwrap().name(), "INTEGER");
    }

    #[test]
    fn define_a_variable_symbol() {
        let mut symtab = SymbolTable::new();
        let real_type = Rc::new(BuiltinTypeSymbol::new(String::from("REAL")));
        symtab.define(real_type.clone());
        let var_sym = VarSymbol::new(String::from("y"), real_type);
        symtab.define(Rc::new(var_sym));

        assert_eq!(symtab.symbols.contains_key("y"), true);
        assert_eq!(symtab.symbols.contains_key("REAL"), true);
    }
}
