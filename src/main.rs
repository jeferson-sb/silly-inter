use std::rc::Rc;

use interpr::Interpreter;
use lexer::Lexer;
use parser::Parser;

use crate::{
    interpr::NodeVisitor,
    symbol::{BuiltinTypeSymbol, ScopedSymbolTable, SemanticAnalyzer, Symbol, VarSymbol},
};

mod ast;
mod interpr;
mod lexer;
mod parser;
mod symbol;
mod token;

fn main() {}

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
        let line: String = "PROGRAM hello; BEGIN n := ".to_owned() + &input + "; END.";
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
    fn lexer_begin_statement() {
        let mut lexer = Lexer::new(String::from("BEGIN a := 2; END."));
        let begintok = Token::new(TokenType::BEGIN, Some(String::from("BEGIN")));
        assert_eq!(lexer.get_next_token(), begintok);
    }

    #[test]
    fn lexer_variable() {
        let mut lexer = Lexer::new(String::from("BEGIN x := 5; END."));
        let vartok = Token::new(TokenType::ID, Some(String::from("x")));
        lexer.get_next_token();
        assert_eq!(lexer.get_next_token(), vartok);
    }

    #[test]
    fn lexer_assignment() {
        let mut lexer = Lexer::new(String::from("a := 42;"));
        let assigntok = Token::new(TokenType::ASSIGN, Some(String::from(":=")));
        lexer.get_next_token();
        assert_eq!(lexer.get_next_token(), assigntok);
    }

    #[test]
    fn lexer_program_token() {
        let mut lexer = Lexer::new(String::from("PROGRAM;"));
        let programtok = Token::new(TokenType::PROGRAM, Some(String::from("PROGRAM")));
        assert_eq!(lexer.get_next_token(), programtok);
    }

    #[test]
    fn lexer_id_token() {
        let mut lexer = Lexer::new(String::from("PROGRAM hello;"));
        let idtok = Token::new(TokenType::ID, Some(String::from("hello")));
        lexer.get_next_token();
        assert_eq!(lexer.get_next_token(), idtok);
    }

    #[test]
    fn lexer_semi_token() {
        let mut lexer = Lexer::new(String::from("n := 1;"));
        let semitok = Token::new(TokenType::SEMI, Some(String::from(";")));
        lexer.get_next_token();
        lexer.get_next_token();
        lexer.get_next_token();
        assert_eq!(lexer.get_next_token(), semitok);
    }

    #[test]
    #[ignore]
    fn empty_ast() {}

    #[test]
    #[ignore]
    fn parse_compound_statement() {}

    // #[test]
    // fn parse_types() {
    //     let lexer = Lexer::new(String::from(
    //         "
    //     PROGRAM Part11;
    //     VAR
    //         x : INTEGER;
    //         y : REAL;

    //     BEGIN

    //     END.
    //     ",
    //     ));
    //     let mut parser = Parser::new(lexer);
    //     let tree = parser.parse();
    //     let mut symtab_builder = SemanticAnalyzer::new();
    //     symtab_builder.visit(&tree);
    //     assert_eq!(symtab_builder.scope.symbols.len(), 4);
    // }
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
        let mut scope = ScopedSymbolTable::new(String::from("global"), 1);
        let int_type = Symbol::new(String::from("INTEGER"), None);
        scope.define(Rc::new(int_type));

        assert_eq!(scope.symbols.is_empty(), false);
        assert_eq!(scope.symbols.get("INTEGER").unwrap().name(), "INTEGER");
    }

    #[test]
    fn define_a_variable_symbol() {
        let mut scope = ScopedSymbolTable::new(String::from("global"), 1);
        let real_type = Rc::new(BuiltinTypeSymbol::new(String::from("REAL")));
        scope.define(real_type.clone());
        let var_sym = VarSymbol::new(String::from("y"), real_type);
        scope.define(Rc::new(var_sym));

        assert_eq!(scope.symbols.contains_key("y"), true);
        assert_eq!(scope.symbols.contains_key("REAL"), true);
    }

    #[test]
    fn scope_symbol_table_builtins() {
        let scope = ScopedSymbolTable::new(String::from("global"), 1);
        assert_eq!(scope.symbols.len(), 2);
        assert_eq!(scope.symbols.contains_key("INTEGER"), true);
        assert_eq!(scope.symbols.contains_key("REAL"), true);
    }

    #[test]
    #[should_panic]
    fn undefined_variable_referenced() {
        let lexer = Lexer::new(String::from(
            "
        PROGRAM var_refs;
        VAR
            a : INTEGER;

        BEGIN
            a := 2 + b
        END.
        ",
        ));
        let mut parser = Parser::new(lexer);
        let tree = parser.parse();
        let mut symtab_builder = SemanticAnalyzer::new();
        symtab_builder.visit(&tree);
    }

    #[test]
    fn nested_scopes() {
        let lexer = Lexer::new(String::from(
            "
        program Main;
            var x, y: real;

            procedure Alpha;
                var y : integer;
            begin

            end;

        begin

        end.
        ",
        ));
        let mut parser = Parser::new(lexer);
        let tree = parser.parse();
        let mut semantic_analyzer = SemanticAnalyzer::new();

        semantic_analyzer.visit(&tree);

        println!("\n\nFINAL SCOPE - {:?}", semantic_analyzer.current_scope);

        let global = semantic_analyzer.scope;
        let current = semantic_analyzer.current_scope.unwrap();

        assert_eq!(global.scope_name, "global");
        assert_eq!(current.scope_name, "Alpha");
        assert_eq!(current.scope_level, 2);

        // assert_eq!(semantic_analyzer.scope.symbols.contains_key("y"), true);
        // assert_eq!(semantic_analyzer.scope.symbols.contains_key("x"), true);
    }
}
