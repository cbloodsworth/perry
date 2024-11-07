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

pub fn tokenize(input: String) -> Result<Vec<Token>, &'static str> {
    let mut iter = input.chars().peekable();
    let mut tokens = Vec::new();
    while let Some(c) = iter.peek() {
        tokens.push(match c {
            //---- Single-character tokens
            ';' => match_one(&mut iter, TokenKind::Semicolon),
            '(' => match_one(&mut iter, TokenKind::LParen),
            ')' => match_one(&mut iter, TokenKind::RParen),
            '{' => match_one(&mut iter, TokenKind::LCurly),
            '}' => match_one(&mut iter, TokenKind::RCurly),
            '[' => match_one(&mut iter, TokenKind::RSquare),
            ']' => match_one(&mut iter, TokenKind::LSquare),

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
                  ('=', TokenKind::MinusEqual)],
                        TokenKind::Minus),
            '/' => match_two_or_one(&mut iter,
                &[('=', TokenKind::SlashEqual),
                  ('/', TokenKind::LineComment),
                  ('*', TokenKind::BlockCommentStart)],
                        TokenKind::Slash),
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

            //---- Identifier
            c if c.is_alphabetic() => {
                let lexeme = iter
                    .by_ref()
                    .peeking_take_while(|&x| x.is_alphanumeric())
                    .collect::<String>();

                //---- Recognized keywords
                let kind = match lexeme.as_str() {
                    "while"  => {TokenKind::While}
                    "for"    => {TokenKind::For}
                    "if"     => {TokenKind::If}
                    "else"   => {TokenKind::Else}
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

pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String
}

#[derive(Debug, Clone)]
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
    Colon, DoubleColon, Dot,
    Question, Tilde, Caret,
    SingleQuote, DoubleQuote,

    //---- Double-character operators
    LessEqual, GreaterEqual,
    LessLess, GreaterGreater,
    EqualEqual, BangEqual,
    PlusEqual, MinusEqual,
    StarEqual, SlashEqual,
    PlusPlus, MinusMinus,
    BarEqual, AmpersandEqual,
    LogicalOr, LogicalAnd,
    Arrow, LineComment,
    BlockCommentStart, BlockCommentEnd,

    // Keywords
    If, Else, While, For, Return, Int, Char, Void,

    //---- Types
    TypeInt, TypeFloat,
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