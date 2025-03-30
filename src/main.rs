// We should suppress these warnings for now
#![allow(unused_variables)]
#![allow(dead_code)]

use std::{fs::File, io::Read, path::Path};

fn main() -> Result<(), perry::CompilerError> {
    let args = std::env::args().collect::<Vec<_>>();

    if args.contains(&"--repl".to_string()) {
        perry::run_repl();
        return Ok(());
    }

    let default = String::from("test.pr");
    let filename = args.get(1).unwrap_or(&default);

    let compile = || -> Result<(), perry::CompilerError> {
        match File::open(Path::new(filename)) {
            Ok(mut file) => {
                let input = &mut String::new();
                file.read_to_string(input).expect("Could not read file {}");

                println!("=============");
                print!("INPUT PROGRAM:\n{input}");
                println!("=============");

                perry::print_lex_results(input)?;
                perry::print_parse_results(input)?;

                perry::compile(input)?;
            }
            Err(why) => {
                print!("Could not open {}, {}", filename, why)
            }
        };

        Ok(())
    };

    if let Err(err) = compile() {
        println!("{err}");
    }

    Ok(())
}
