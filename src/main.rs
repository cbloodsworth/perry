use std::io::stdin;

mod lexer;
mod parser;

fn print_lex_results(input: String) {
    lexer::tokenize(input)
        .unwrap()
        .iter()
        .for_each(|x| {
            let lexeme = format!("[{}]", x.lexeme);
            print!("{0: <10}: ", lexeme);
            println!("{:?}", x.kind)
        });
}

fn main() {
    loop {
        let mut input = String::new();
        stdin().read_line(&mut input).unwrap();
        print_lex_results(input.trim().to_string());
    }
}
