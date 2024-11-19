#[cfg(test)]
mod lexer_tests {
    use crate::lexer::*;

    fn assert_first_token(text: &str, expected_token: Token) {
        let vec = Lexer::lex(text).unwrap();
        let first_token = vec.get(0).unwrap();
        assert_eq!(*first_token, expected_token);
    }


    #[test]
    fn one_char_arithmetic() {
        assert_first_token("+", Token{lexeme: "+".to_string(), kind: TokenKind::Plus});
        assert_first_token("-", Token{lexeme: "-".to_string(), kind: TokenKind::Minus});
        assert_first_token("*", Token{lexeme: "*".to_string(), kind: TokenKind::Star});
        assert_first_token("/", Token{lexeme: "/".to_string(), kind: TokenKind::Slash});
        assert_first_token("%", Token{lexeme: "%".to_string(), kind: TokenKind::Percent});
    }

    #[test]
    fn one_char_grouping() {
        assert_first_token("(", Token{lexeme: "(".to_string(), kind: TokenKind::LParen});
        assert_first_token(")", Token{lexeme: ")".to_string(), kind: TokenKind::RParen});
        assert_first_token("{", Token{lexeme: "{".to_string(), kind: TokenKind::LCurly});
        assert_first_token("}", Token{lexeme: "}".to_string(), kind: TokenKind::RCurly});
        assert_first_token("[", Token{lexeme: "[".to_string(), kind: TokenKind::LSquare});
        assert_first_token("]", Token{lexeme: "]".to_string(), kind: TokenKind::RSquare});
    }

    #[test]
    fn one_char_logical() {
        assert_first_token(">", Token{lexeme: ">".to_string(), kind: TokenKind::Greater});
        assert_first_token("<", Token{lexeme: "<".to_string(), kind: TokenKind::Less});
        assert_first_token("&", Token{lexeme: "&".to_string(), kind: TokenKind::Ampersand});
        assert_first_token("|", Token{lexeme: "|".to_string(), kind: TokenKind::Bar});
        assert_first_token("?", Token{lexeme: "?".to_string(), kind: TokenKind::Question});
        assert_first_token("!", Token{lexeme: "!".to_string(), kind: TokenKind::Bang});
    }

    #[test]
    fn one_char_other() {
        assert_first_token("=", Token{lexeme: "=".to_string(), kind: TokenKind::Equal});
        assert_first_token(".", Token{lexeme: ".".to_string(), kind: TokenKind::Dot});
        assert_first_token(",", Token{lexeme: ",".to_string(), kind: TokenKind::Comma});
        assert_first_token(":", Token{lexeme: ":".to_string(), kind: TokenKind::Colon});
        assert_first_token(";", Token{lexeme: ";".to_string(), kind: TokenKind::Semicolon});
        assert_first_token("~", Token{lexeme: "~".to_string(), kind: TokenKind::Tilde});
        assert_first_token("^", Token{lexeme: "^".to_string(), kind: TokenKind::Caret});
    }

    #[test]
    fn two_char_assignment() {
        assert_first_token("+=", Token{lexeme: "+=".to_string(), kind: TokenKind::PlusEqual});
        assert_first_token("-=", Token{lexeme: "-=".to_string(), kind: TokenKind::MinusEqual});
        assert_first_token("*=", Token{lexeme: "*=".to_string(), kind: TokenKind::StarEqual});
        assert_first_token("/=", Token{lexeme: "/=".to_string(), kind: TokenKind::SlashEqual});
        assert_first_token("%=", Token{lexeme: "%=".to_string(), kind: TokenKind::PercentEqual});
        assert_first_token("|=", Token{lexeme: "|=".to_string(), kind: TokenKind::BarEqual});
        assert_first_token("&=", Token{lexeme: "&=".to_string(), kind: TokenKind::AmpersandEqual});
        assert_first_token("++", Token{lexeme: "++".to_string(), kind: TokenKind::PlusPlus});
        assert_first_token("--", Token{lexeme: "--".to_string(), kind: TokenKind::MinusMinus});
    }

    #[test]
    fn two_char_logical() {
        assert_first_token(">=", Token{lexeme: ">=".to_string(), kind: TokenKind::GreaterEqual});
        assert_first_token("<=", Token{lexeme: "<=".to_string(), kind: TokenKind::LessEqual});
        assert_first_token("&&", Token{lexeme: "&&".to_string(), kind: TokenKind::LogicalAnd});
        assert_first_token("||", Token{lexeme: "||".to_string(), kind: TokenKind::LogicalOr});
        assert_first_token("!=", Token{lexeme: "!=".to_string(), kind: TokenKind::BangEqual});
        assert_first_token("==", Token{lexeme: "==".to_string(), kind: TokenKind::EqualEqual});
    }

    #[test]
    fn two_char_comments() {
        assert_first_token("//", Token{lexeme: "//".to_string(), kind: TokenKind::LineComment});
        assert_first_token("/*", Token{lexeme: "/*".to_string(), kind: TokenKind::BlockCommentStart});
        assert_first_token("*/", Token{lexeme: "*/".to_string(), kind: TokenKind::BlockCommentEnd});
    }

    #[test]
    fn two_char_other() {
        assert_first_token("->", Token{lexeme: "->".to_string(), kind: TokenKind::Arrow});
        assert_first_token("::", Token{lexeme: "::".to_string(), kind: TokenKind::ColonColon});
    }

    #[test]
    fn keywords_types() {
        assert_first_token("int", Token{lexeme: "int".to_string(), kind: TokenKind::TypeInt});
        assert_first_token("long", Token{lexeme: "long".to_string(), kind: TokenKind::TypeLong});
        assert_first_token("float", Token{lexeme: "float".to_string(), kind: TokenKind::TypeFloat});
        assert_first_token("double", Token{lexeme: "double".to_string(), kind: TokenKind::TypeDouble});
        assert_first_token("char", Token{lexeme: "char".to_string(), kind: TokenKind::TypeChar});
        assert_first_token("void", Token{lexeme: "void".to_string(), kind: TokenKind::TypeVoid});
    }

    #[test]
    fn keywords_controlflow() {
        assert_first_token("if", Token{lexeme: "if".to_string(), kind: TokenKind::If});
        assert_first_token("else", Token{lexeme: "else".to_string(), kind: TokenKind::Else});
        assert_first_token("for", Token{lexeme: "for".to_string(), kind: TokenKind::For});
        assert_first_token("while", Token{lexeme: "while".to_string(), kind: TokenKind::While});
        assert_first_token("return", Token{lexeme: "return".to_string(), kind: TokenKind::Return});
    }

    #[test]
    fn literals_string() {
        assert_first_token("\"Hello\"", Token{lexeme: "\"Hello\"".to_string(), kind: TokenKind::StringLiteral});
        assert_first_token("\"\"", Token{lexeme: "\"\"".to_string(), kind: TokenKind::StringLiteral});
        assert_first_token("\"\\\"\"", Token{lexeme: "\"\\\"\"".to_string(), kind: TokenKind::StringLiteral});
    }

    #[test]
    fn literals_char() {
        assert_first_token("'o'", Token{lexeme: "'o'".to_string(), kind: TokenKind::CharLiteral});
    }

    #[test]
    fn literals_ident() {
        assert_first_token("some_identifier", Token{lexeme: "some_identifier".to_string(), kind: TokenKind::Identifier});
        assert_first_token("hello123", Token{lexeme: "hello123".to_string(), kind: TokenKind::Identifier});
        assert_first_token("_hello", Token{lexeme: "_hello".to_string(), kind: TokenKind::Identifier});
        assert_first_token("_", Token{lexeme: "_".to_string(), kind: TokenKind::Identifier});
        assert_first_token("_123", Token{lexeme: "_123".to_string(), kind: TokenKind::Identifier});
    }
}