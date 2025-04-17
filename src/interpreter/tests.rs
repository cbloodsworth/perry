#[cfg(test)]
mod interpreter_tests {
    use crate::interpreter::{Compile, Interpreter};
    use crate::interpreter::Value;

    #[test]
    fn div_by_zero() {
        let error_message = Interpreter::from_source("(1 / 0)")
            .expect_err("Expected a divide-by-zero error.")
            .to_string();

        assert!(error_message.contains("div"));
        assert!(error_message.contains("zero"));
    }

    #[test]
    fn empty_tuple_literal() {
        let value = Interpreter::from_source("()")
            .unwrap_or_else(|err| panic!("expected `()` to be evaluated correctly, got: \n- {err}"));

        match value {
            Value::Tuple(t) => assert!(t.is_empty()),
            other => panic!("expected Value::Tuple, got: {other}")
        }
    }

    #[test]
    fn simple_tuple_literal() {
        let value = Interpreter::from_source("(41, 42)")
            .unwrap_or_else(|err| panic!("expected `(41, 42)` to be evaluated correctly, got: \n- {err}"));

        match value {
            Value::Tuple(tuple) => {
                let first_element = &tuple[0];
                let second_element = &tuple[1];
                assert!(matches!(first_element, Value::Integer(41)));
                assert!(matches!(second_element, Value::Integer(42)));
            }
            other => panic!("expected Value::Tuple, got: {other}")
        }
    }

    #[test]
    fn empty_array_literal() {
        let value = Interpreter::from_source("[]")
            .unwrap_or_else(|err| panic!("expected `[]` to be evaluated correctly, got: \n- {err}"));

        match value {
            Value::Array(a) => assert!(a.is_empty()),
            other => panic!("expected Value::Array, got: {other}")
        }
    }

    #[test]
    fn simple_array_literal() {
        let value = Interpreter::from_source("[41, 42]")
            .unwrap_or_else(|err| panic!("expected `[41, 42]` to be evaluated correctly, got: \n- {err}"));

        match value {
            Value::Array(array) => {
                let first_element = &array[0];
                let second_element = &array[1];
                assert!(matches!(first_element, Value::Integer(41)));
                assert!(matches!(second_element, Value::Integer(42)));
            }
            other => panic!("expected Value::Array, got: {other}")
        }
    }

    #[test]
    fn heterogeneous_array_error() {
        let error_message = Interpreter::from_source("[42, \"hello\"]")
            .expect_err("Expected a type error, got none.")
            .to_string();

        assert!(error_message.contains("type"));
    }
}