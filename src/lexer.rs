use std::{iter::Peekable, str::Chars};

use anyhow::{Result, Context};
use itertools::Itertools;

pub struct Lexer<'a> {
    pub iter: Peekable<Chars<'a>>,
    tokens: Vec<Token>
}

impl<'a> Lexer<'a> {
    pub fn lex(input: &'a str) -> Result<Vec<Token>> {
        Self {
            iter: input.chars().peekable(),
            tokens:Vec::new()
        }.tokenize()
    }

    fn tokenize(mut self) -> Result<Vec<Token>> {
        while let Some(c) = self.iter.peek() {
            let token = match c {
                //---- Single-character tokens
                ';' => self.match_one(TokenKind::Semicolon),
                '.' => self.match_one(TokenKind::Dot),
                ',' => self.match_one(TokenKind::Comma),
                '?' => self.match_one(TokenKind::Question),
                '~' => self.match_one(TokenKind::Tilde),
                '^' => self.match_one(TokenKind::Caret),
                '(' => self.match_one(TokenKind::LParen),
                ')' => self.match_one(TokenKind::RParen),
                '{' => self.match_one(TokenKind::LCurly),
                '}' => self.match_one(TokenKind::RCurly),
                '[' => self.match_one(TokenKind::LSquare),
                ']' => self.match_one(TokenKind::RSquare),

                //---- Double-character tokens
                '=' => self.match_two_or_one(
                    &[('=', TokenKind::EqualEqual)],
                            TokenKind::Equal),
                '+' => self.match_two_or_one(
                    &[('+', TokenKind::PlusPlus),
                      ('=', TokenKind::PlusEqual)],
                            TokenKind::Plus),
                '-' => self.match_two_or_one(
                    &[('-', TokenKind::MinusMinus),
                      ('>', TokenKind::Arrow),
                      ('=', TokenKind::MinusEqual)],
                            TokenKind::Minus),
                '*' => self.match_two_or_one(
                    &[('=', TokenKind::StarEqual),
                      ('/', TokenKind::BlockCommentEnd)],
                            TokenKind::Star),
                '/' => self.match_two_or_one(
                    &[('=', TokenKind::SlashEqual),
                      ('/', TokenKind::LineComment),
                      ('*', TokenKind::BlockCommentStart)],
                            TokenKind::Slash),
                '%' => self.match_two_or_one(
                    &[('=', TokenKind::PercentEqual)],
                            TokenKind::Percent),
                '!' => self.match_two_or_one(
                    &[('=', TokenKind::BangEqual)],
                            TokenKind::Bang),
                '|' => self.match_two_or_one(
                    &[('|', TokenKind::LogicalOr),
                      ('=', TokenKind::BarEqual)],
                            TokenKind::Bar),
                '&' => self.match_two_or_one(
                    &[('&', TokenKind::LogicalAnd),
                      ('=', TokenKind::AmpersandEqual)],
                            TokenKind::Ampersand),
                '>' => self.match_two_or_one(
                    &[('>', TokenKind::GreaterGreater),
                      ('=', TokenKind::GreaterEqual)],
                            TokenKind::Greater),
                '<' => self.match_two_or_one(
                    &[('<', TokenKind::LessLess),
                      ('=', TokenKind::LessEqual)],
                            TokenKind::Less),
                ':' => self.match_two_or_one(
                    &[(':', TokenKind::ColonColon)],
                            TokenKind::Colon),

                //---- Identifier
                c if c.is_alphabetic() || *c == '_' => {
                    let lexeme = self.iter
                        .by_ref()
                        .peeking_take_while(|&x| x.is_alphanumeric() || x == '_')
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
                    let mut lexeme = self.iter
                        .by_ref()
                        .peeking_take_while(|&x| x.is_numeric())
                        .collect::<String>();

                    // If stopped at a '.' it's a float
                    let kind = if self.iter.peek() == Some(&&'.') {
                        lexeme.push('.');
                        self.iter.next();
                        lexeme.extend(self.iter.by_ref().peeking_take_while(|&x| x.is_numeric()));

                        TokenKind::FloatLiteral
                    }
                    else {
                        TokenKind::IntegerLiteral
                    };

                    Token{kind, lexeme}
                }

                //---- String literals
                '"' => {
                    // Consume the quote
                    self.iter.next();

                    // Grab that word
                    let mut word = self.iter
                        .by_ref()
                        .peeking_take_while(|&x| x != '"')
                        .collect::<String>();

                    self.iter.next().context("Missing terminating \" character.")?;

                    // Keep going if that quote was escaped
                    while word.ends_with("\\") {
                        word.push('"');
                        word.extend(self.iter.by_ref().take_while(|&x| x != '"'));
                    }

                    Token{kind: TokenKind::StringLiteral, lexeme: format!("\"{}\"", word) }
                }

                //---- Char literals
                '\''=> {
                    // Proceed past the first quote
                    self.iter.next();

                    // Grab the character
                    let chr = self.iter.next()
                        .context("Missing terminating \' character.")?;

                    // Ensure we got something up next
                    self.iter.peek()
                        .context("Missing terminating \' character.")?;
                    
                    // Make sure that character is an end quote
                    self.iter.next_if(|&c| c == '\'')
                        .context("Multi-character character constant.")?;

                    Token{kind: TokenKind::CharLiteral, lexeme: format!("'{}'", chr) }
                }

                // Skip whitespace
                c if c.is_whitespace() => {
                    self.iter.by_ref()
                        .peeking_take_while(|&x| x.is_whitespace())
                        .for_each(drop);

                    continue;
                }

                // Unrecognized
                _ => {
                    let lexeme = c.to_string();

                    self.iter.next();
                    Token { kind: TokenKind::Unknown, lexeme }
                }
            };

            self.tokens.push(token)
        }

        self.tokens.push(Token{kind: TokenKind::EOF, lexeme: "EOF".to_string()});
        Ok(self.tokens)
    }

    fn match_one(&mut self, kind: TokenKind) -> Token {
        let lexeme = self.iter.peek()
            .expect("The iterator should point to a valid char when this method is called.")
            .to_string();

        self.iter.next();
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
    fn match_two_or_one(&mut self, matches: &[(char, TokenKind)], otherwise: TokenKind) -> Token {
        let lexeme = self.iter.peek()
            .expect("The iterator should point to a valid char when this method is called.")
            .clone()
            .to_string();

        // Consume the first character, move to the second
        self.iter.next();
        for (second, kind) in matches {
            if self.iter.peek() == Some(&second) {
                self.iter.next();
                return Token{kind: kind.clone(), lexeme: format!("{}{}",lexeme,second)}
            }
        }

        Token{kind: otherwise, lexeme}
    }


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
    IntegerLiteral, FloatLiteral, 
    StringLiteral, CharLiteral,

    //---- End of File
    EOF,

    //---- Unknown
    Unknown
}

#[cfg(test)]
mod lexer_tests {
    use super::*;

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
