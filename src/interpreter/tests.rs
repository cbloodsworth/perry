#[cfg(test)]
mod interpreter_tests {
    use crate::interpreter::{Compile, Interpreter};

    #[test]
    fn div_by_zero() {
        let error_message = Interpreter::from_source("(1 / 0)")
            .expect_err("Expected a divide-by-zero error.")
            .to_string();

        assert!(error_message.contains("div"));
        assert!(error_message.contains("zero"));
    }
}