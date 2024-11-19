use std::{fs::File, io::Read, path::Path};

mod lexer;
mod parser;

fn print_lex_results(input: &str) {
    match lexer::Lexer::lex(input) {
        Ok(x) => {
            x.iter().for_each(|x| {
                let lexeme = format!("[{}]", x.lexeme);
                print!("{0: <10}: ", lexeme);
                println!("{:?}", x.kind)
            });
        }
        Err(err) => {
            println!("ERROR: {}", err);
        }
    }
}

fn print_parse_results(input: &str) {
    match parser::Parser::parse(input) {
        Ok(x) => {
            println!("{:?}", x);
        }
        Err(err) => {
            println!("ERROR: {}", err);
        }
    }
}

fn main() {
    let args = std::env::args().collect::<Vec<_>>();

    let default = String::from("test.c");
    let filename = args.get(1).unwrap_or(&default);

    match File::open(Path::new(filename)) {
        Ok(mut file) => {
            let input = &mut String::new();
            file.read_to_string(input).expect("Could not read file {}");

            print_lex_results(&input.clone());
            print_parse_results(&input);
        }
        Err(why) => {
            print!("Could not open {}, {}", filename, why)
        }
    };
}
