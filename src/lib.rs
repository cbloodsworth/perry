mod lexer;
mod parser;
mod interpreter;

use std::io::{stdin, stdout, Read, Write};

use lexer::Lexer;
use parser::Parser;
use interpreter::Interpreter;

#[derive(Debug)]
pub enum CompilerError {
    LexerError(lexer::LexerError),
    ParserError(parser::ParserError),
    InterpreterError(interpreter::InterpreterError),
}

impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let err_msg = match self {
            CompilerError::LexerError(lexer_error) => format!("LEXER ERROR: {lexer_error}"),
            CompilerError::ParserError(parser_error) => format!("PARSER ERROR: {parser_error}"),
            CompilerError::InterpreterError(interpreter_error) => format!("INTERPRETER ERROR: {interpreter_error}")
        };

        write!(f, "{err_msg}")
    }
}

impl From<lexer::LexerError> for CompilerError { fn from(err: lexer::LexerError) -> Self { Self::LexerError(err) }}
impl From<parser::ParserError> for CompilerError { fn from(err: parser::ParserError) -> Self { Self::ParserError(err) }}
impl From<interpreter::InterpreterError> for CompilerError { fn from(err: interpreter::InterpreterError) -> Self { Self::InterpreterError(err) }}

pub fn print_lex_results(input: &str) -> Result<(), CompilerError> {
    Ok(Lexer::lex(input)?.iter().for_each(|token| println!("{token}")))
}

pub fn print_parse_results(input: &str) -> Result<(), CompilerError> {
    let tokens = Lexer::lex(input)?;
    let ast = Parser::parse(tokens)?;
    crate::parser::ast_printer::pretty_print(&ast);
    Ok(println!("{}", ast))
}

pub fn parse_repl() {
    fn enable_raw_mode() {
        use libc::{tcgetattr, tcsetattr, termios, ECHO, ICANON, TCSANOW};
        let mut term = unsafe { std::mem::zeroed::<termios>() };
        unsafe {
            // Get the terminal attributes
            tcgetattr(0, &mut term);
            // Disable canonical mode and echo
            term.c_lflag &= !(ICANON | ECHO);
            // Set the terminal attributes
            tcsetattr(0, TCSANOW, &term);
        }
    }

    fn disable_raw_mode() {
        use libc::{tcgetattr, tcsetattr, termios, ECHO, ICANON, TCSANOW};
        let mut term = unsafe { std::mem::zeroed::<termios>() };
        unsafe {
            // Get the terminal attributes
            tcgetattr(0, &mut term);
            // Disable canonical mode and echo
            term.c_lflag &= ICANON | ECHO;
            // Set the terminal attributes
            tcsetattr(0, TCSANOW, &term);
        }
    }


    enable_raw_mode();
    print!("perry> ");
    loop {
        let _ = stdout().flush();
        let mut input = String::new();

        for byte in stdin().bytes() {
            let byte = byte.unwrap();
            match byte {
                b'\n' => {
                    println!();
                    print_parse_results(&input).unwrap_or_else(|err| println!("{err}"));
                    input.clear();
                    print!("perry> ");
                    stdout().flush().unwrap();
                }
                // backspace
                127 => {
                    if !input.is_empty() {
                        input.pop();
                        print!("\x1b[1D \x1b[1D");
                        stdout().flush().unwrap();
                    }
                }
                // Ctrl+L to clear screen
                12 => {
                    print!("\x1b[2J\x1b[Hperry> ");
                    stdout().flush().unwrap();
                }
                b => {
                    input.push(b as char);
                    stdout().write_all(&[b]).unwrap();
                    stdout().flush().unwrap();
                }
            }
        }
    }
}