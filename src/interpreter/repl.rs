use std::io::{stdin, stdout, Read, Write};

use libc::{signal, SIGINT, SIGTERM, tcgetattr, tcsetattr, termios, ECHO, ICANON, TCSANOW};

pub trait RunCommand {
    const PROMPT_SUCCESS: &str = "(^▽^) ♪> ";
    const PROMPT_NEUTRAL: &str = "('ω') ?> ";
    const PROMPT_FAILURE: &str = "(‵□′) #> ";
    const PROMPT_WAITING: &str = "('ω')... ";
    const PROMPT_RESTART: &str = "(x_x)~~~ ";
    /// Given a command (program) as a `&str`, evaluate it and return its output as a String.
    /// 
    /// # Errors
    /// If an error occurred during evaluation, returns that error as a String wrapped in Err.
    fn run_cmd(cmd: &str) -> Result<String, String>;

    /// Given a command (program) as a `&str`, evaluate it and return its output as a String.
    /// 
    /// Also prints debug information, if there is any.
    /// 
    /// # Errors
    /// If an error occurred during evaluation, returns that error as a String wrapped in Err.
    fn run_cmd_debug(cmd: &str) -> Result<String, String>;

    /// Starts a shell that can act as a repl for the implementer.
    /// 
    fn run_repl() {

        enable_raw_mode();
        let repl_loop = || -> i32 {
            let mut prompt = Self::PROMPT_NEUTRAL;
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
                                "exit" => {
                                    return 0;
                                }
                                "debug(on)" => {
                                    if !debug {
                                        println!("debug mode on");
                                        prompt = Self::PROMPT_SUCCESS;
                                        debug = true;
                                    } else {
                                        prompt = Self::PROMPT_NEUTRAL;
                                    }
                                }
                                "debug(off)" => {
                                    if debug {
                                        println!("debug mode off");
                                        prompt = Self::PROMPT_SUCCESS;
                                        debug = false;
                                    } else {
                                        prompt = Self::PROMPT_NEUTRAL;
                                    }
                                }
                                // continue on new line
                                line if line.ends_with('\\') => {
                                    input.pop();        // get rid of the backslash 
                                    program += &input;
                                    input.clear();
                                    cursor_pos = 0;
                                    prompt = Self::PROMPT_WAITING;
                                    print!("{prompt}");
                                    stdout().flush().unwrap();

                                    continue;
                                }
                                _ => {
                                    program += &input;
                                    let response = if debug {
                                        Self::run_cmd_debug(&program)
                                    }
                                    else {
                                        Self::run_cmd(&program)
                                    };

                                    match response {
                                        Ok(msg) => {
                                            println!("{msg}");
                                            prompt = Self::PROMPT_SUCCESS;
                                        }
                                        Err(err) => {
                                            eprintln!("\x1b[1;31m❌ {err}\x1b[0;0m");
                                            prompt = Self::PROMPT_FAILURE;
                                        }
                                    }
                                    program.clear();
                                }
                            }

                            input.clear();
                            cursor_pos = 0;
                            print!("{prompt}");
                            stdout().flush().unwrap();
                        }
                        // Arrow key inputs start with this byte
                        b'\x1b' => {
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

        unsafe {
            signal(SIGINT,  handle_signal as usize);
            signal(SIGTERM, handle_signal as usize);
        }

        loop {
            let exit = std::panic::catch_unwind(repl_loop);
            if let Ok(0) = exit {
                break;
            } else {
                eprintln!("\x1b[1;31m{}(panicked)\x1b[0;0m", Self::PROMPT_RESTART);
            }
        }

        disable_raw_mode();
    }
}

extern "C" fn handle_signal(_signal: libc::c_int) {
    disable_raw_mode();
    println!();
    std::process::exit(0);
}

fn enable_raw_mode() {
    let mut term = unsafe { std::mem::zeroed::<termios>() };
    unsafe {
        tcgetattr(0, &mut term);
        term.c_lflag &= !(ICANON | ECHO);
        tcsetattr(0, TCSANOW, &term);
    }
}

fn disable_raw_mode() {
    let mut term = unsafe { std::mem::zeroed::<termios>() };
    unsafe {
        tcgetattr(0, &mut term);
        term.c_lflag |= ICANON | ECHO;
        tcsetattr(0, TCSANOW, &term);
    }
}