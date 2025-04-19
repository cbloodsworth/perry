macro_rules! eval_unwrap {
    ($arg:tt) => {
        crate::interpreter::Interpreter::from_source(&format!("({})", $arg))
            .unwrap_or_else(|err| panic!("expected `{}` to be evaluated correctly, got: \n- {err}", $arg))
    };
}

#[cfg(test)]
mod collection_tests {
    use crate::interpreter::{Compile, Interpreter};
    use crate::interpreter::Value;

    #[test]
    fn empty_tuple_literal() {
        let value = eval_unwrap!("()");

        match value {
            Value::Tuple(t) => assert!(t.is_empty()),
            other => panic!("expected Value::Tuple, got: {other}")
        }
    }

    #[test]
    fn simple_tuple_literal() {
        let value = eval_unwrap!("(41, 42)");

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
        let value = eval_unwrap!("[]");

        match value {
            Value::Array(a) => assert!(a.is_empty()),
            other => panic!("expected Value::Array, got: {other}")
        }
    }

    #[test]
    fn simple_array_literal() {
        let value = eval_unwrap!("[41, 42]");

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

#[cfg(test)]
mod arithmetic_tests {
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

#[cfg(test)]
mod logic_tests {
    use crate::interpreter::{Compile, Interpreter, Value};

    macro_rules! assert_bool_val {
        ($value:tt, $truth:tt) => {
            match $value {
                Value::Boolean(boolean) => { assert_eq!(boolean, $truth); }
                other => panic!("expected Value::Boolean, got: {other}")
            }
        }
    }

    #[test]
    fn test_scalar_equality() {
        let value = eval_unwrap!("1 == 1");
        assert_bool_val!(value, true);
    }

    #[test]
    fn test_scalar_equality_false() {
        let value = eval_unwrap!("1 == 2");
        assert_bool_val!(value, false);
    }

    #[test]
    fn test_scalar_inequality() {
        let value = eval_unwrap!("1 != 2");
        assert_bool_val!(value, true);
    }

    #[test]
    fn test_scalar_inequality_false() {
        let value = eval_unwrap!("1 != 1");
        assert_bool_val!(value, false);
    }

    #[test]
    fn test_string_cmp() {
        let value = eval_unwrap!("\"hello\" == \"hello\"");
        assert_bool_val!(value, true);
    }

    #[test]
    fn test_string_cmp_false() {
        let value = eval_unwrap!("\"hello\" == \"world\"");
        assert_bool_val!(value, false);
    }

    #[test]
    fn test_string_cmp_ineq() {
        let value = eval_unwrap!("\"hello\" != \"world\"");
        assert_bool_val!(value, true);
    }

    #[test]
    fn test_array_cmp() {
        let value = eval_unwrap!("[1, 2] == [1, 2]");
        assert_bool_val!(value, true);
    }

    #[test]
    fn test_array_cmp_ineq() {
        let value = eval_unwrap!("[1, 2] != [3, 2]");
        assert_bool_val!(value, true);
    }

    #[test]
    fn test_array_cmp_difflengths() {
        let value = eval_unwrap!("[1, 2, 3] != [1, 2]");
        assert_bool_val!(value, true);
    }

    #[test]
    fn test_array_cmp_empty() {
        let value = eval_unwrap!("[] != [1, 2]");
        assert_bool_val!(value, true);
    }

    #[test]
    fn test_array_cmp_bothempty() {
        let value = eval_unwrap!("[] == []");
        assert_bool_val!(value, true);
    }

    #[test]
    fn test_tuple_cmp() {
        let value = eval_unwrap!("(42, \"hello\") == (42, \"hello\")");
        assert_bool_val!(value, true);
    }

    #[test]
    fn test_tuple_ineq() {
        let value = eval_unwrap!("(42, 43) != (42, \"43\")");
        assert_bool_val!(value, true);
    }
}