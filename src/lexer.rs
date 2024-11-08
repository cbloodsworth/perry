use std::{error::Error, iter::Peekable, str::Chars};
use itertools::Itertools;

fn match_one(iter: &mut Peekable<Chars>, kind: TokenKind) -> Token {
    let lexeme = iter.peek()
        .expect("The iterator should point to a valid char when this method is called.")
        .to_string();

    iter.next();
    Token{kind, lexeme}
}

/// Matches either one or two characters, and returns the token.
/// Changes state of iterator.
///
/// There are cases where we want to lex two-character sequences, but
///     need to look ahead one character to determine what we are looking at.
///
/// Example: !xxx <--- Unparsed characters
/// .        ^--- If we are here, we need to look ahead to see if we are at an
/// .             Inequality '!=', or just LogicalNot '!'.
/// .
///
fn match_two_or_one(iter: &mut Peekable<Chars>,
                  matches: &[(char, TokenKind)],
                  otherwise: TokenKind) -> Token {
    let lexeme = iter.peek()
        .expect("The iterator should point to a valid char when this method is called.")
        .clone()
        .to_string();

    // Consume the first character, move to the second
    iter.next();
    for (second, kind) in matches {
        if iter.peek() == Some(&second) {
            iter.next();
            return Token{kind: kind.clone(), lexeme: format!("{}{}",lexeme,second)}
        }
    }

    Token{kind: otherwise, lexeme}
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, &'static str> {
    let mut iter = input.chars().peekable();
    let mut tokens = Vec::new();
    while let Some(c) = iter.peek() {
        tokens.push(match c {
            //---- Single-character tokens
            ';' => match_one(&mut iter, TokenKind::Semicolon),
            '.' => match_one(&mut iter, TokenKind::Dot),
            ',' => match_one(&mut iter, TokenKind::Comma),
            '?' => match_one(&mut iter, TokenKind::Question),
            '~' => match_one(&mut iter, TokenKind::Tilde),
            '^' => match_one(&mut iter, TokenKind::Caret),
            '(' => match_one(&mut iter, TokenKind::LParen),
            ')' => match_one(&mut iter, TokenKind::RParen),
            '{' => match_one(&mut iter, TokenKind::LCurly),
            '}' => match_one(&mut iter, TokenKind::RCurly),
            '[' => match_one(&mut iter, TokenKind::LSquare),
            ']' => match_one(&mut iter, TokenKind::RSquare),

            //---- Double-character tokens
            '=' => match_two_or_one(&mut iter,
                &[('=', TokenKind::EqualEqual)],
                        TokenKind::Equal),
            '+' => match_two_or_one(&mut iter,
                &[('+', TokenKind::PlusPlus),
                  ('=', TokenKind::PlusEqual)],
                        TokenKind::Plus),
            '-' => match_two_or_one(&mut iter,
                &[('-', TokenKind::MinusMinus),
                  ('>', TokenKind::Arrow),
                  ('=', TokenKind::MinusEqual)],
                        TokenKind::Minus),
            '*' => match_two_or_one(&mut iter,
                &[('=', TokenKind::StarEqual),
                  ('/', TokenKind::BlockCommentEnd)],
                        TokenKind::Star),
            '/' => match_two_or_one(&mut iter,
                &[('=', TokenKind::SlashEqual),
                  ('/', TokenKind::LineComment),
                  ('*', TokenKind::BlockCommentStart)],
                        TokenKind::Slash),
            '%' => match_two_or_one(&mut iter,
                &[('=', TokenKind::PercentEqual)],
                        TokenKind::Percent),
            '!' => match_two_or_one(&mut iter,
                &[('=', TokenKind::BangEqual)],
                        TokenKind::Bang),
            '|' => match_two_or_one(&mut iter,
                &[('|', TokenKind::LogicalOr),
                  ('=', TokenKind::BarEqual)],
                        TokenKind::Bar),
            '&' => match_two_or_one(&mut iter,
                &[('&', TokenKind::LogicalAnd),
                  ('=', TokenKind::AmpersandEqual)],
                        TokenKind::Ampersand),
            '>' => match_two_or_one(&mut iter,
                &[('>', TokenKind::GreaterGreater),
                  ('=', TokenKind::GreaterEqual)],
                        TokenKind::Greater),
            '<' => match_two_or_one(&mut iter,
                &[('<', TokenKind::LessLess),
                  ('=', TokenKind::LessEqual)],
                        TokenKind::Less),
            ':' => match_two_or_one(&mut iter,
                &[(':', TokenKind::ColonColon)],
                        TokenKind::Colon),

            //---- Identifier
            c if c.is_alphabetic() => {
                let lexeme = iter
                    .by_ref()
                    .peeking_take_while(|&x| x.is_alphanumeric())
                    .collect::<String>();

                //---- Recognized keywords
                let kind = match lexeme.as_str() {
                    // Control flow
                    "while"  => {TokenKind::While}
                    "for"    => {TokenKind::For}
                    "if"     => {TokenKind::If}
                    "else"   => {TokenKind::Else}
                    "return"   => {TokenKind::Return}

                    // Types
                    "int"    => {TokenKind::TypeInt}
                    "char"   => {TokenKind::TypeChar}
                    "long"   => {TokenKind::TypeLong}
                    "float"  => {TokenKind::TypeFloat}
                    "double" => {TokenKind::TypeDouble}
                    "void"   => {TokenKind::TypeVoid}
                    _ => {TokenKind::Identifier}
                };

                Token{kind, lexeme}
            }

            //---- Numeric literals
            c if c.is_numeric() => {
                let mut lexeme = iter
                    .by_ref()
                    .peeking_take_while(|&x| x.is_numeric())
                    .collect::<String>();

                // If stopped at a '.' it's a float
                let kind = if iter.peek() == Some(&&'.') {
                    lexeme.push('.');
                    iter.next();
                    lexeme.extend(iter.by_ref().peeking_take_while(|&x| x.is_numeric()));

                    TokenKind::FloatLiteral
                }
                else {
                    TokenKind::IntegerLiteral
                };

                Token{kind, lexeme}
            }

            //---- String literals
            '"' => {
                iter.next();
                let word = iter
                    .by_ref()
                    .take_while(|&x| x != '"')
                    .collect::<String>();

                Token{kind: TokenKind::StringLiteral, lexeme: format!("\"{}\"", word) }
            }

            //---- Char literals
            '\''=> {
                let chr = iter.next().ok_or("Missing terminating \' character.")?;
                iter.next_if_eq(&'\'').ok_or("Missing terminating \' character.")?;
                Token{kind: TokenKind::StringLiteral, lexeme: format!("'{}'", chr) }
            }

            // Skip whitespace
            c if c.is_whitespace() => {
                iter.by_ref()
                    .peeking_take_while(|&x| x.is_whitespace())
                    .for_each(drop);

                Token{kind: TokenKind::Whitespace, lexeme: "".to_string()}
            }

            // Unrecognized
            _ => {
                let lexeme = c.to_string();

                iter.next();
                Token { kind: TokenKind::Unknown, lexeme }
            }
        });
    }

    Ok(tokens)
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    //---- Parentheses and Braces
    LParen, RParen,
    LCurly, RCurly,
    LSquare, RSquare,

    //---- Single character operators
    Plus, Minus, Star,
    Slash, Percent, Equal,
    Ampersand, Bar, Bang,
    Less, Greater,
    Comma, Semicolon,
    Colon, ColonColon, Dot,
    Question, Tilde, Caret,
    SingleQuote, DoubleQuote,

    //---- Double-character operators
    LessEqual, GreaterEqual,
    LessLess, GreaterGreater,
    EqualEqual, BangEqual,
    PlusEqual, MinusEqual,
    StarEqual, SlashEqual,
    PercentEqual,
    PlusPlus, MinusMinus,
    BarEqual, AmpersandEqual,
    LogicalOr, LogicalAnd,
    Arrow, LineComment,
    BlockCommentStart, BlockCommentEnd,

    // Keywords
    If, Else, While, For, Return,

    //---- Types
    TypeInt, TypeLong,
    TypeFloat, TypeDouble,
    TypeChar, TypeVoid,

    //---- Literals
    Identifier,
    IntegerLiteral, FloatLiteral, StringLiteral,

    //---- Whitespace
    Whitespace,

    //---- End of File
    EOF,

    //---- Unknown
    Unknown
}

#[cfg(test)]
mod lexer_tests {
    use super::*;

    fn assert_first_token(text: &str, expected_token: Token) {
        let vec = tokenize(text).unwrap();
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
}