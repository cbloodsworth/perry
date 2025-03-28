///=================== Lexer test helpers. ===================
macro_rules! validate_tokens {
    ($program:expr, $($expected_lexeme:expr),+) => {
        let mut tokens = lex($program)
            .expect(&format!("Internal error: Could not lex the given program: {}.", $program))
            .into_iter();
        $(
            let token = tokens.next().expect(&format!(
                    "Expected token with lexeme \"{}\", got end of file.", $expected_lexeme));

            // Check that the lexeme is what we expect
            assert_eq!(token.lexeme, $expected_lexeme,
                "Token's actual lexeme differed from expected lexeme");
        )+
    };
}

#[cfg(test)]
mod lexer_tests {
    use crate::lexer::*;

    fn assert_first_token_kind(text: &str, expected_tokenkind: TokenKind) {
        let vec = Lexer::lex(text).unwrap();
        let first_token = vec.get(0).unwrap();

        assert_eq!(first_token.lexeme, text);
        assert_eq!(first_token.kind, expected_tokenkind);
    }

    #[test]
    fn one_char_arithmetic() {
        assert_first_token_kind("+", TokenKind::Plus);
        assert_first_token_kind("-", TokenKind::Minus);
        assert_first_token_kind("*", TokenKind::Star);
        assert_first_token_kind("/", TokenKind::Slash);
        assert_first_token_kind("%", TokenKind::Percent);
    }

    #[test]
    fn one_char_grouping() {
        assert_first_token_kind("(", TokenKind::LParen);
        assert_first_token_kind(")", TokenKind::RParen);
        assert_first_token_kind("{", TokenKind::LCurly);
        assert_first_token_kind("}", TokenKind::RCurly);
        assert_first_token_kind("[", TokenKind::LSquare);
        assert_first_token_kind("]", TokenKind::RSquare);
    }

    #[test]
    fn one_char_logical() {
        assert_first_token_kind(">", TokenKind::Greater);
        assert_first_token_kind("<", TokenKind::Less);
        assert_first_token_kind("&", TokenKind::Ampersand);
        assert_first_token_kind("|", TokenKind::Bar);
        assert_first_token_kind("?", TokenKind::Question);
        assert_first_token_kind("!", TokenKind::Bang);
    }

    #[test]
    fn one_char_other() {
        assert_first_token_kind("=", TokenKind::Equal);
        assert_first_token_kind(".", TokenKind::Dot);
        assert_first_token_kind(",", TokenKind::Comma);
        assert_first_token_kind(":", TokenKind::Colon);
        assert_first_token_kind(";", TokenKind::Semicolon);
        assert_first_token_kind("~", TokenKind::Tilde);
        assert_first_token_kind("^", TokenKind::Caret);
    }

    #[test]
    fn two_char_assignment() {
        assert_first_token_kind("+=", TokenKind::PlusEqual);
        assert_first_token_kind("-=", TokenKind::MinusEqual);
        assert_first_token_kind("*=", TokenKind::StarEqual);
        assert_first_token_kind("/=", TokenKind::SlashEqual);
        assert_first_token_kind("%=", TokenKind::PercentEqual);
        assert_first_token_kind("|=", TokenKind::BarEqual);
        assert_first_token_kind("&=", TokenKind::AmpersandEqual);
        assert_first_token_kind("++", TokenKind::PlusPlus);
        assert_first_token_kind("--", TokenKind::MinusMinus);
    }

    #[test]
    fn two_char_logical() {
        assert_first_token_kind(">=", TokenKind::GreaterEqual);
        assert_first_token_kind("<=", TokenKind::LessEqual);
        assert_first_token_kind("&&", TokenKind::LogicalAnd);
        assert_first_token_kind("||", TokenKind::LogicalOr);
        assert_first_token_kind("!=", TokenKind::BangEqual);
        assert_first_token_kind("==", TokenKind::EqualEqual);
    }

    #[test]
    fn two_char_comments() {
        assert_first_token_kind("//", TokenKind::LineComment);
        assert_first_token_kind("/*", TokenKind::BlockCommentStart);
        assert_first_token_kind("*/", TokenKind::BlockCommentEnd);
    }

    #[test]
    fn two_char_other() {
        assert_first_token_kind("->", TokenKind::Arrow);
        assert_first_token_kind("::", TokenKind::ColonColon);
    }

    #[test]
    fn keywords_types() {
        assert_first_token_kind("int", TokenKind::TypeInt);
        assert_first_token_kind("long", TokenKind::TypeLong);
        assert_first_token_kind("float", TokenKind::TypeFloat);
        assert_first_token_kind("double", TokenKind::TypeDouble);
        assert_first_token_kind("char", TokenKind::TypeChar);
        assert_first_token_kind("void", TokenKind::TypeVoid);
    }

    #[test]
    fn keywords_controlflow() {
        assert_first_token_kind("if", TokenKind::If);
        assert_first_token_kind("else", TokenKind::Else);
        assert_first_token_kind("for", TokenKind::For);
        assert_first_token_kind("while", TokenKind::While);
        assert_first_token_kind("return", TokenKind::Return);
    }

    #[test]
    fn literals_string() {
        assert_first_token_kind("\"Hello\"", TokenKind::StringLiteral);
        assert_first_token_kind("\"\"", TokenKind::StringLiteral);
        assert_first_token_kind("\"\\\"\"", TokenKind::StringLiteral);
    }

    #[test]
    fn literals_char() {
        assert_first_token_kind("'o'", TokenKind::CharLiteral);
    }

    #[test]
    fn literals_ident() {
        assert_first_token_kind("some_identifier", TokenKind::Identifier);
        assert_first_token_kind("hello123", TokenKind::Identifier);
        assert_first_token_kind("_hello", TokenKind::Identifier);
        assert_first_token_kind("_", TokenKind::Identifier);
        assert_first_token_kind("_123", TokenKind::Identifier);
    }

    #[test]
    fn line_numbers_1() {
        let vec = Lexer::lex(
            r#"int a = 1;
float b = 3.14;"#,
        )
        .unwrap();
        let int_token = vec
            .iter()
            .find(|token| token.kind == TokenKind::TypeInt)
            .unwrap();
        let float_token = vec
            .iter()
            .find(|token| token.kind == TokenKind::TypeFloat)
            .unwrap();
        let pi_token = vec.iter().find(|token| token.lexeme == "3.14").unwrap();

        assert_eq!(int_token.line_number, 1);
        assert_eq!(float_token.line_number, 2);
        assert_eq!(pi_token.line_number, 2);
    }
}
