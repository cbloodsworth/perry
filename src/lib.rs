mod interpreter;
mod lexer;
mod parser;

use interpreter::{Compile, Interpreter};
use itertools::Itertools;
use lexer::Lexer;
use parser::Parser;


#[cfg(not(target_arch = "wasm32"))]
pub fn repl() {
    use interpreter::native::RunCommand;
    Interpreter::run_repl();
}

pub fn compile(program: &str) -> Result<String, CompilerError> {
    Ok(Interpreter::from_source(program)?.to_string())
}

// Wasm bindings: exposed to the browser via wasm-bindgen.
#[cfg(all(target_arch = "wasm32", feature = "wasm"))]
mod wasm_api {
    use wasm_bindgen::prelude::*;

    #[wasm_bindgen]
    pub fn perry_eval_expr(input: &str) -> Result<String, JsError> {
        let wrapped = format!("({input})");
        super::compile(&wrapped).map_err(|e| JsError::new(&e.to_string()))
    }

    #[wasm_bindgen]
    pub fn perry_run_file(input: &str) -> Result<String, JsError> {
        super::compile(input).map_err(|e| JsError::new(&e.to_string()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TokenLocation(usize, usize);

#[derive(Debug)]
pub enum CompilerError {
    LexerError(lexer::LexerError),
    ParserError(parser::ParserError),
    InterpreterError(interpreter::InterpreterError),

    GenericError(anyhow::Error),
}

impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let err_msg = match self {
            CompilerError::LexerError(lexer_error) => format!("LEXER ERROR: {lexer_error}"),
            CompilerError::ParserError(parser_error) => format!("PARSER ERROR: {parser_error}"),
            CompilerError::InterpreterError(interpreter_error) => {
                format!("INTERPRETER ERROR: {interpreter_error}")
            }
            CompilerError::GenericError(generic_error) => format!("{generic_error}"),
        };

        write!(f, "{err_msg}")
    }
}

impl From<lexer::LexerError> for CompilerError {
    fn from(err: lexer::LexerError) -> Self {
        Self::LexerError(err)
    }
}
impl From<parser::ParserError> for CompilerError {
    fn from(err: parser::ParserError) -> Self {
        Self::ParserError(err)
    }
}
impl From<interpreter::InterpreterError> for CompilerError {
    fn from(err: interpreter::InterpreterError) -> Self {
        Self::InterpreterError(err)
    }
}
impl From<anyhow::Error> for CompilerError {
    fn from(err: anyhow::Error) -> Self {
        Self::GenericError(err)
    }
}

pub fn print_lex_results(input: &str) -> Result<String, CompilerError> {
    let result = Lexer::lex(input)?
        .into_iter()
        .map(|token| token.to_string())
        .join("\n");

    Ok(result)
}

pub fn print_parse_results(input: &str) -> Result<String, CompilerError> {
    let tokens = Lexer::lex(input)?;
    let ast = Parser::parse(tokens)?;
    //crate::parser::ast_printer::pretty_print(&ast);
    //println!("{}", ast);

    Ok(ast.to_string())
}
