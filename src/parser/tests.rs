#[cfg(test)]
mod parser_tests {
    use crate::parser::*;

    macro_rules! validate_program {
        ($program:expr) => {{
            let tokens =
                crate::lexer::Lexer::lex($program).expect("Program should have lexed properly.");

            parser::Parser::parse(tokens).unwrap_or_else(|err| {
                panic!("Expected program to parse successfully, but err was issued: {err}")
            });
        }};
    }

    #[test]
    fn parse_basic_parens() {
        validate_program!("(1 + 1)")
    }

    #[test]
    fn parse_multiline_parens() {
        validate_program!(
            r#"
(1 + 1)
(2 + 2)
        "#
        )
    }

    #[test]
    fn parse_nested_parens() {
        validate_program!("((1 + 1) + (2 + 2))")
    }

    #[test]
    fn error_on_unbalanced_parentheses_1() {
        let tokens =
            crate::lexer::Lexer::lex("(1 + 1").expect("Program should have lexed properly.");

        let err = parser::Parser::parse(tokens)
            .expect_err("Expected parsing error, but none was issued.");

        let lparen_token = err
            .token
            .as_ref()
            .expect("Expected token attached to error message.");

        assert!(
            err.message.contains("unmatched"),
            "Error message should report an unmatched parenthesis. Got: \"{}\"",
            err.message
        );

        let crate::TokenLocation(line_number, col_number) = lparen_token.loc;
        assert!(
            line_number == 1 && col_number == 1,
            "Error message should report an unmatched parenthesis on line 1, column 1. \
            \nInstead, got line {}, column {}.",
            line_number,
            col_number,
        );
    }

    #[test]
    fn error_on_unbalanced_parentheses_2() {
        let tokens = crate::lexer::Lexer::lex(
            r#"
(1 + 1)
(1
        "#,
        )
        .expect("Program should have lexed properly.");

        let err = parser::Parser::parse(tokens)
            .expect_err("Expected parsing error, but none was issued.");

        let lparen_token = err
            .token
            .as_ref()
            .expect("Expected token attached to error message.");

        assert!(
            err.message.contains("unmatched"),
            "Error message should report an unmatched parenthesis. Got: \"{}\"",
            err.message
        );

        let crate::TokenLocation(line_number, col_number) = lparen_token.loc;
        assert!(
            line_number == 2 && col_number == 1,
            "Error message should report an unmatched parenthesis on line 2, column 1. \
            \nInstead, got line {}, column {}.",
            line_number,
            col_number
        );
    }

    #[test]
    fn error_on_unbalanced_parentheses_3() {
        let tokens = crate::lexer::Lexer::lex(
            r#"
(1 + 1)
fn (x
        "#,
        )
        .expect("Program should have lexed properly.");

        let err = parser::Parser::parse(tokens)
            .expect_err("Expected parsing error, but none was issued.");

        let lparen_token = err
            .token
            .as_ref()
            .expect("Expected token attached to error message.");

        assert!(
            err.message.contains("unmatched"),
            "Error message should report an unmatched parenthesis. Got: \"{}\"",
            err.message
        );

        let crate::TokenLocation(line_number, col_number) = lparen_token.loc;
        assert!(
            line_number == 2 && col_number == 4,
            "Error message should report an unmatched parenthesis on line 2, column 4. \
            \nInstead, got line {}, column {}.",
            line_number,
            col_number
        );
    }
}
