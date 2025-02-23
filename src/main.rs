use std::{fs::File, io::{stdin, stdout, Read, Write}, path::Path};

mod lexer;
mod parser;

use lexer::Lexer;
use parser::Parser;

fn print_lex_results(input: &str) {
    match Lexer::lex(input) {
        Ok(x) => {
            x.iter().for_each(|x| {
                let lexeme = format!("{}:{}: [{}]", x.line_number, x.col_number, x.lexeme);
                print!("{0: <10}: ", lexeme);
                println!("{:?}", x.kind)
            });
        }
        Err(err) => {
            println!("LEXING ERROR: {}", err);
        }
    }
}

fn print_parse_results(input: &str) {
    match Parser::parse(input) {
        Ok(x) => {
            println!("{:?}", x);
        }
        Err(err) => {
            println!("PARSING ERROR: {}", err);
        }
    }
}

fn parse_repl() {
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
                    print_lex_results(&input);
                    print_parse_results(&input);
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

fn main() {
    let args = std::env::args().collect::<Vec<_>>();

    if args.contains(&"--repl".to_string()) { parse_repl(); }

    let default = String::from("test.pr");
    let filename = args.get(1).unwrap_or(&default);

    match File::open(Path::new(filename)) {
        Ok(mut file) => {
            let input = &mut String::new();
            file.read_to_string(input).expect("Could not read file {}");

            print_lex_results(&input.clone());
            print_parse_results(input);
        }
        Err(why) => {
            print!("Could not open {}, {}", filename, why)
        }
    };
}
