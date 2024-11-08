use std::io::stdin;

mod lexer;
mod parser;

fn print_lex_results(input: &str) {
    let output = lexer::tokenize(input);
    match output {
        Ok(x) => {
            x.iter().for_each(|x| {
                let lexeme = format!("[{}]", x.lexeme);
                print!("{0: <10}: ", lexeme);
                println!("{:?}", x.kind)
            });
        }
        Err(err) => {
            println!("ERROR: {}", err)
        }
    }
}

fn main() {
    loop {
        let mut input = String::new();
        stdin().read_line(&mut input).unwrap();
        print_lex_results(input.trim());
    }
}
