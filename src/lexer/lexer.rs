use std::{iter::{Enumerate, Peekable}, str::Chars};
use anyhow::{Result, Context};
use itertools::Itertools;

pub struct Lexer<'a> {
    pub iter: Peekable<Enumerate<Chars<'a>>>,
    tokens: Vec<Token>,
    line_number: usize,
    last_line_break: usize,
}

impl<'a> Lexer<'a> {
    pub fn lex(input: &'a str) -> Result<Vec<Token>> {
        Self {
            iter: input.chars().enumerate().peekable(),
            tokens:Vec::new(),
            line_number: 1,
            last_line_break: 0,
        }.tokenize()
    }

    fn tokenize(mut self) -> Result<Vec<Token>> {
        while let Some(&(i, c)) = self.iter.peek() {
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
                c if c.is_alphabetic() || c == '_' => {
                    let lexeme = self.iter
                        .by_ref()
                        .peeking_take_while(|&(_,char)| char.is_alphanumeric() || char == '_')
                        .map(|(_,c)| c)  // only collect characters, not indeces
                        .collect::<String>();

                    //---- Recognized keywords
                    let kind = match lexeme.as_str() {
                        // Control flow
                        "while"  => {TokenKind::While}
                        "for"    => {TokenKind::For}
                        "if"     => {TokenKind::If}
                        "else"   => {TokenKind::Else}
                        "return" => {TokenKind::Return}
                        "true"   => {TokenKind::True}
                        "false"  => {TokenKind::False}

                        // Types
                        "int"    => {TokenKind::TypeInt}
                        "char"   => {TokenKind::TypeChar}
                        "long"   => {TokenKind::TypeLong}
                        "float"  => {TokenKind::TypeFloat}
                        "double" => {TokenKind::TypeDouble}
                        "void"   => {TokenKind::TypeVoid}
                        _ => {TokenKind::Identifier}
                    };


                    Token {
                        kind,
                        lexeme,
                        line_number: self.line_number,
                        col_number: self.calc_col_num(i)
                    }
                }

                //---- Numeric literals
                c if c.is_numeric() => {
                    let mut lexeme = self.iter
                        .by_ref()
                        .peeking_take_while(|(_,c)| c.is_numeric())
                        .map(|(_,c)| c)  // only collect characters, not indeces
                        .collect::<String>();

                    // If stopped at a '.' it's a float (example: 3.14)
                    let kind = if self.iter.peek().is_some_and(|&(_, c)| c == '.') {
                        lexeme.push('.');
                        self.iter.next();

                        // Keep taking the numbers after the .
                        lexeme.extend(self.iter
                            .by_ref()
                            .map(|(_,c)| c)
                            .peekable()
                            .peeking_take_while(|c| c.is_numeric()));

                        TokenKind::FloatLiteral
                    }
                    else {
                        TokenKind::IntegerLiteral
                    };

                    Token {
                        kind,
                        lexeme,
                        line_number: self.line_number,
                        col_number: self.calc_col_num(i)
                    }
                }

                //---- String literals
                '"' => {
                    // Consume the quote
                    self.iter.next();

                    // Grab that word
                    let mut word = self.iter
                        .by_ref()
                        .peeking_take_while(|&(_,c)| c != '"')
                        .map(|(_,c)| c)
                        .collect::<String>();

                    self.iter.next().context("Missing terminating \" character.")?;

                    // Keep going if that quote was escaped
                    while word.ends_with("\\") {
                        word.push('"');
                        word.extend(self.iter.by_ref().map(|(_,c)| c).take_while(|c| c != &'"'));
                    }

                    Token {
                        kind: TokenKind::StringLiteral,
                        lexeme: format!("\"{}\"", word),
                        line_number: self.line_number,
                        col_number: self.calc_col_num(i)
                    }
                }

                //---- Char literals
                '\''=> {
                    // Proceed past the first quote
                    self.iter.next();

                    // Grab the character
                    let (_, chr) = self.iter.next()
                        .context("Missing terminating \' character.")?;

                    // Ensure we got something up next
                    self.iter.peek()
                        .context("Missing terminating \' character.")?;
                    
                    // Make sure that character is an end quote
                    self.iter.next_if(|&(_,c)| c == '\'')
                        .context("Multi-character character constant.")?;

                    Token {
                        kind: TokenKind::CharLiteral,
                        lexeme: format!("'{}'", chr),
                        line_number: self.line_number,
                        col_number: self.calc_col_num(i)
                    }
                }

                // Skip whitespace
                c if c.is_whitespace() => {
                    while self.iter.peek().is_some_and(|(_,c)| c.is_whitespace()) {
                        if let Some((_,'\n')) = self.iter.peek() {
                            self.line_number += 1;
                            self.last_line_break = i;
                        }
                        self.iter.next();
                    }
                    self.iter.by_ref()
                        .peeking_take_while(|(_,c)| c.is_whitespace())
                        .for_each(drop);

                    continue;
                }

                // Unrecognized
                _ => {
                    let lexeme = c.to_string();

                    self.iter.next();
                    Token {
                        kind: TokenKind::Unknown,
                        lexeme,
                        line_number: self.line_number,
                        col_number: self.calc_col_num(i)
                    }
                }
            };

            self.tokens.push(token)
        }

        Ok(self.tokens)
    }

    fn match_one(&mut self, kind: TokenKind) -> Token {
        let &(i, char) = self.iter
            .peek()
            .expect("The iterator should point to a valid char when this method is called.");
        let lexeme = char.to_string();

        self.iter.next();

        Token {
            kind,
            lexeme,
            line_number: self.line_number,
            col_number: self.calc_col_num(i)
        }
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
        let (i, first) = self.iter.peek()
            .cloned()
            .expect("The iterator should point to a valid char when this method is called.");

        // Consume the first character, move to the second
        self.iter.next();
        for (second, kind) in matches {
            if self.iter.peek().is_some_and(|&(_,c)| c == *second) {
                self.iter.next();
                return Token {
                    kind: kind.clone(),
                    lexeme: format!("{}{}",first,second),
                    line_number: self.line_number,
                    col_number: self.calc_col_num(i)
                }
            }
        }

        Token {
            kind: otherwise,
            lexeme: first.to_string(),
            line_number: self.line_number,
            col_number: self.calc_col_num(i)
        }
    }

    fn calc_col_num(&self, i: usize) -> usize {
        i - self.last_line_break + 1
    }

}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub line_number: usize,
    pub col_number: usize,
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
    True, False,

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
