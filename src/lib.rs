mod interpreter;
mod lexer;
mod parser;

use std::io::{stdin, stdout, Read, Write};

use interpreter::{Compile, Interpreter};
use lexer::Lexer;
use parser::Parser;

pub fn compile(program: &str) -> Result<String, String> {
    Interpreter::from_source(program)
        .map(|val| val.to_string())
        .map_err(|err| format!("{err}"))
}

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
            CompilerError::InterpreterError(interpreter_error) => {
                format!("INTERPRETER ERROR: {interpreter_error}")
            }
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

pub fn print_lex_results(input: &str) -> Result<(), CompilerError> {
    Lexer::lex(input)?
        .iter()
        .for_each(|token| println!("{token}"));

    Ok(())
}

pub fn print_parse_results(input: &str) -> Result<(), CompilerError> {
    let tokens = Lexer::lex(input)?;
    let ast = Parser::parse(tokens)?;
    crate::parser::ast_printer::pretty_print(&ast);
    println!("{}", ast);

    Ok(())
}

pub fn run_repl() {
    const PROMPT: &str = "perry> ";
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
            // Enable canonical mode and echo
            term.c_lflag |= ICANON | ECHO;
            // Set the terminal attributes
            tcsetattr(0, TCSANOW, &term);
        }
    }

    enable_raw_mode();
    let repl_loop = || -> i32 {
        print!("{PROMPT}");
        let mut last_input = String::new();
        let mut curr_input = String::new();
        let mut cursor_pos = 0;
        loop {
            let _ = stdout().flush();
            let mut input = String::new();

            for byte in stdin().bytes() {
                let byte = byte.unwrap();
                match byte {
                    b'\n' => {
                        println!();
                        if input == "exit" {
                            return 0;
                        }
                        last_input = input.clone();
                        match compile(&format!("({input})")) {
                            Ok(msg) => println!("{msg}"),
                            Err(msg) => println!("error: {msg}")
                        };
                        input.clear();
                        cursor_pos = 0;
                        print!("perry> ");
                        stdout().flush().unwrap();
                    }
                    b'\x1b' => {
                        // Possible escape sequence, read two more bytes
                        let mut seq = [0; 2];
                        if stdin().read_exact(&mut seq).is_ok() {
                            match seq {
                                // up arrow
                                [b'[', b'A'] => {
                                    if input != last_input {
                                        curr_input = input.clone();
                                        input = last_input.clone();
                                    }
                                    cursor_pos = input.len();
                                    print!("\r\x1b[2Kperry> {}", input);
                                    stdout().flush().unwrap();
                                }
                                // down arrow
                                [b'[', b'B'] => {
                                    input = curr_input.clone();
                                    cursor_pos = input.len();
                                    print!("\r\x1b[2Kperry> {}", input);
                                    stdout().flush().unwrap();
                                }
                                // left arrow
                                [b'[', b'D'] => {
                                    if cursor_pos > 0 {
                                        cursor_pos -= 1;
                                        print!("\x1b[1D");
                                        stdout().flush().unwrap();
                                    }
                                }
                                // right arrow
                                [b'[', b'C'] => {
                                    if cursor_pos < input.len() {
                                        cursor_pos += 1;
                                        print!("\x1b[1C");
                                        stdout().flush().unwrap();
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    // backspace
                    127 => {
                        if cursor_pos > 0 {
                            input.remove(cursor_pos - 1);
                            cursor_pos -= 1;
                            print!("\r\x1b[2Kperry> {}", input);
                            print!("\x1b[{}G", cursor_pos + PROMPT.len()+1); // Move cursor back to correct position
                            stdout().flush().unwrap();
                        }
                    }
                    // Ctrl+L to clear screen
                    12 => {
                        print!("\x1b[2J\x1b[Hperry> ");
                        stdout().flush().unwrap();
                    }
                    b => {
                        input.insert(cursor_pos, byte as char);
                        cursor_pos += 1;
                        print!("\r\x1b[2Kperry> {}", input);
                        print!("\x1b[{}G", cursor_pos + PROMPT.len()+1); // Move cursor back to correct position
                        stdout().flush().unwrap();
                    }
                }
            }
        }
    };

    loop {
        let exit = std::panic::catch_unwind(repl_loop);
        if let Ok(0) = exit { break; }
    }
    disable_raw_mode()
}
