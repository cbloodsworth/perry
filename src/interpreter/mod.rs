mod interpreter;
pub use interpreter::*;

#[cfg(not(target_arch = "wasm32"))]
pub mod native {
    pub mod repl;
    pub use repl::RunCommand;
}

#[cfg(test)]
mod tests;
