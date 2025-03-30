use std::io::{stdin, stdout, Read, Write};

use crate::{print_lex_results, print_parse_results};

pub fn run_repl() {
    const EMOJI_HAPPY: &str = "(^▽^) ♪> ";
    const EMOJI_NEUTRAL: &str = "('ω') ?> ";
    const EMOJI_ANGRY: &str = "(‵□′) #> ";
    const EMOJI_WAITING: &str = "('ω')... ";
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
        let mut prompt = EMOJI_NEUTRAL;
        let mut last_input = String::new();
        let mut input = String::new();
        let mut next_input = String::new();
        let mut cursor_pos = 0;
        let mut debug = false;
        loop {
            let _ = stdout().flush();
            let mut program = String::new();

            print!("{prompt}");
            stdout().flush().unwrap();

            for byte in stdin().bytes() {
                let byte = byte.unwrap();
                match byte {
                    b'\n' => {
                        println!();
                        last_input = input.clone();
                        match input.as_str() {
                            "" => {
                                prompt = EMOJI_NEUTRAL;
                            }
                            "exit" => {
                                return 0;
                            }
                            "debug(on)" => {
                                if !debug {
                                    println!("debug mode on");
                                    prompt = EMOJI_HAPPY;
                                    debug = true;
                                } else {
                                    prompt = EMOJI_NEUTRAL;
                                }
                            }
                            "debug(off)" => {
                                if debug {
                                    println!("debug mode off");
                                    prompt = EMOJI_HAPPY;
                                    debug = false;
                                } else {
                                    prompt = EMOJI_NEUTRAL;
                                }
                            }
                            line if line.ends_with('\\') => {
                                input.pop();
                                program += &format!(" {input}");
                                input.clear();
                                cursor_pos = 0;
                                prompt = EMOJI_WAITING;
                                print!("{prompt}");
                                stdout().flush().unwrap();

                                continue;
                            }
                            _ => {
                                program += &format!(" {input}");
                                match crate::compile(&format!("({program})")) {
                                    Ok(msg) => {
                                        if debug {
                                            println!(
                                                "{}",
                                                print_lex_results(&format!("({program})")).unwrap()
                                            );
                                            println!(
                                                "{}",
                                                print_parse_results(&format!("({program})"))
                                                    .unwrap()
                                            );
                                        }
                                        println!("{msg}");

                                        prompt = EMOJI_HAPPY;
                                    }
                                    Err(msg) => {
                                        if debug {
                                            let _ = print_lex_results(&format!("({program})"))
                                                .inspect(|msg| println!("{msg}"))
                                                .and_then(|_| {
                                                    print_parse_results(&format!("({program})"))
                                                })
                                                .inspect(|msg| println!("{msg}"))
                                                .inspect_err(|err| eprintln!("{err}"));
                                        }
                                        eprintln!("\x1b[1;31m❌ {msg}\x1b[0;0m");

                                        prompt = EMOJI_ANGRY;
                                    }
                                };
                                program.clear();
                            }
                        }

                        input.clear();
                        cursor_pos = 0;
                        print!("{prompt}");
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
                                        next_input = input.clone();
                                        input = last_input.clone();
                                    }
                                    cursor_pos = input.len();
                                    print!("\r\x1b[2K{prompt}{}", input);
                                    stdout().flush().unwrap();
                                }
                                // down arrow
                                [b'[', b'B'] => {
                                    input = next_input.clone();
                                    cursor_pos = input.len();
                                    print!("\r\x1b[2K{prompt}{}", input);
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
                            print!("\r\x1b[2K{prompt}{}", input);
                            print!("\x1b[{}G", cursor_pos + prompt.chars().count() + 1); // Move cursor back to correct position
                            stdout().flush().unwrap();
                        }
                    }
                    // Ctrl+L to clear screen
                    12 => {
                        print!("\x1b[2J\x1b[H{prompt}");
                        stdout().flush().unwrap();
                    }
                    _ => {
                        input.insert(cursor_pos, byte as char);
                        cursor_pos += 1;
                        print!("\r\x1b[2K{prompt}{}", input);
                        print!("\x1b[{}G", cursor_pos + prompt.chars().count() + 1); // Move cursor back to correct position
                        stdout().flush().unwrap();
                    }
                }
            }
        }
    };

    loop {
        let exit = std::panic::catch_unwind(repl_loop);
        if let Ok(0) = exit {
            break;
        }
    }
    disable_raw_mode()
}
