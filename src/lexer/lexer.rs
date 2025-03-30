use anyhow::Context;
use itertools::Itertools;
use std::{
    iter::{Enumerate, Peekable},
    str::Chars,
};

pub struct Lexer<'a> {
    pub char_iter: Peekable<Enumerate<Chars<'a>>>,
    tokens: Vec<Token>,
    line_number: usize,
    last_line_break: usize,
}

#[derive(Debug)]
pub struct LexerError {
    message: String,
    line_number: usize,
    col_number: usize,
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}",
            self.message, self.line_number, self.col_number
        )
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            kind,
            lexeme,
            line_number,
            col_number,
        } = self;
        let lexeme = format!("{line_number}:{col_number}: [{lexeme}]");
        write!(f, "{0:<10}:{kind:?}", lexeme)
    }
}

impl<'a> Lexer<'a> {
    pub fn lex(input: &'a str) -> Result<Vec<Token>, LexerError> {
        Self {
            char_iter: input.trim().chars().enumerate().peekable(),
            tokens: Vec::new(),
            line_number: 1,
            last_line_break: 0,
        }
        .tokenize()
    }

    fn tokenize(mut self) -> Result<Vec<Token>, LexerError> {
        while let Some(&(i, c)) = self.char_iter.peek() {
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
                '=' => self.match_two_or_one(&[('=', TokenKind::EqualEqual)], TokenKind::Equal),
                '+' => self.match_two_or_one(
                    &[('+', TokenKind::PlusPlus), ('=', TokenKind::PlusEqual)],
                    TokenKind::Plus,
                ),
                '-' => self.match_two_or_one(
                    &[
                        ('-', TokenKind::MinusMinus),
                        ('>', TokenKind::Arrow),
                        ('=', TokenKind::MinusEqual),
                    ],
                    TokenKind::Minus,
                ),
                '*' => self.match_two_or_one(
                    &[
                        ('=', TokenKind::StarEqual),
                        ('/', TokenKind::BlockCommentEnd),
                    ],
                    TokenKind::Star,
                ),
                '/' => self.match_two_or_one(
                    &[
                        ('=', TokenKind::SlashEqual),
                        ('/', TokenKind::LineComment),
                        ('*', TokenKind::BlockCommentStart),
                    ],
                    TokenKind::Slash,
                ),
                '%' => self.match_two_or_one(&[('=', TokenKind::PercentEqual)], TokenKind::Percent),
                '!' => self.match_two_or_one(&[('=', TokenKind::BangEqual)], TokenKind::Bang),
                '|' => self.match_two_or_one(
                    &[('|', TokenKind::LogicalOr), ('=', TokenKind::BarEqual)],
                    TokenKind::Bar,
                ),
                '&' => self.match_two_or_one(
                    &[
                        ('&', TokenKind::LogicalAnd),
                        ('=', TokenKind::AmpersandEqual),
                    ],
                    TokenKind::Ampersand,
                ),
                '>' => self.match_two_or_one(
                    &[
                        ('>', TokenKind::GreaterGreater),
                        ('=', TokenKind::GreaterEqual),
                    ],
                    TokenKind::Greater,
                ),
                '<' => self.match_two_or_one(
                    &[('<', TokenKind::LessLess), ('=', TokenKind::LessEqual)],
                    TokenKind::Less,
                ),
                ':' => self.match_two_or_one(&[(':', TokenKind::ColonColon)], TokenKind::Colon),

                //---- Identifier
                c if c.is_alphabetic() || c == '_' => {
                    let lexeme = self.take_while(|c| c.is_alphanumeric() || c == '_');

                    //---- Recognized keywords
                    let kind = match lexeme.as_str() {
                        // Control flow
                        "while" => TokenKind::While,
                        "for" => TokenKind::For,
                        "if" => TokenKind::If,
                        "else" => TokenKind::Else,
                        "return" => TokenKind::Return,
                        "true" => TokenKind::True,
                        "false" => TokenKind::False,

                        // Types
                        "int" => TokenKind::TypeInt,
                        "char" => TokenKind::TypeChar,
                        "long" => TokenKind::TypeLong,
                        "float" => TokenKind::TypeFloat,
                        "double" => TokenKind::TypeDouble,
                        "void" => TokenKind::TypeVoid,
                        _ => TokenKind::Identifier,
                    };

                    Token {
                        kind,
                        lexeme,
                        line_number: self.line_number,
                        col_number: self.calc_col_num(i),
                    }
                }

                //---- Numeric literals
                c if c.is_numeric() => {
                    let mut lexeme = self
                        .char_iter
                        .by_ref()
                        .peeking_take_while(|(_, c)| c.is_numeric())
                        .map(|(_, c)| c) // only collect characters, not indeces
                        .collect::<String>();

                    // If stopped at a '.' it's a float (example: 3.14)
                    let kind = if self.char_iter.peek().is_some_and(|&(_, c)| c == '.') {
                        lexeme.push('.');
                        self.char_iter.next();

                        lexeme.push_str(&self.take_while(char::is_numeric));

                        TokenKind::FloatLiteral
                    } else {
                        TokenKind::IntegerLiteral
                    };

                    Token {
                        kind,
                        lexeme,
                        line_number: self.line_number,
                        col_number: self.calc_col_num(i),
                    }
                }

                //---- String literals
                '"' => {
                    // Consume the quote
                    self.char_iter.next();

                    // Grab that word
                    let mut word = self
                        .char_iter
                        .by_ref()
                        .peeking_take_while(|&(_, c)| c != '"')
                        .map(|(_, c)| c)
                        .collect::<String>();

                    if self.char_iter.next().is_none() {
                        return Err(self.into_err(i, "missing terminating \" character"));
                    }

                    // Keep going if that quote was escaped
                    while word.ends_with("\\") {
                        word.pop();
                        word.push('"');
                        word.extend(
                            self.char_iter
                                .by_ref()
                                .map(|(_, c)| c)
                                .take_while(|c| c != &'"'),
                        );
                    }

                    Token {
                        kind: TokenKind::StringLiteral,
                        lexeme: word,
                        line_number: self.line_number,
                        col_number: self.calc_col_num(i),
                    }
                }

                //---- Char literals
                '\'' => {
                    // Proceed past the first quote
                    self.char_iter.next();

                    // Grab the character
                    let (_, chr) = self
                        .char_iter
                        .next()
                        .ok_or_else(|| self.into_err(i, "missing terminating \' character"))?;

                    // Ensure we got something up next
                    if self.char_iter.peek().is_none() {
                        return Err(self.into_err(i, "missing terminating \' character"));
                    }

                    // Ensure that the char literal is one character in length
                    if self.char_iter.next_if(|&(_, c)| c == '\'').is_none() {
                        return Err(self.into_err(i, "multi-character character constant"));
                    }

                    Token {
                        kind: TokenKind::CharLiteral,
                        lexeme: format!("'{}'", chr),
                        line_number: self.line_number,
                        col_number: self.calc_col_num(i),
                    }
                }

                // Skip whitespace
                c if c.is_whitespace() => {
                    while self
                        .char_iter
                        .peek()
                        .is_some_and(|(_, c)| c.is_whitespace())
                    {
                        if let Some((_, '\n')) = self.char_iter.peek() {
                            self.line_number += 1;
                            self.last_line_break = i + 1; // one past so we skip the \n character
                        }
                        self.char_iter.next();
                    }
                    self.char_iter
                        .by_ref()
                        .peeking_take_while(|(_, c)| c.is_whitespace())
                        .for_each(drop);

                    continue;
                }

                // Unrecognized
                _ => {
                    let lexeme = c.to_string();

                    self.char_iter.next();
                    Token {
                        kind: TokenKind::Unknown,
                        lexeme,
                        line_number: self.line_number,
                        col_number: self.calc_col_num(i),
                    }
                }
            };

            self.tokens.push(token)
        }

        Ok(self.tokens)
    }

    fn take_while<F: Fn(char) -> bool>(&mut self, pred: F) -> String {
        // Keep taking the numbers after the .
        let mut buffer = String::new();
        while let Some(&(_, c)) = self.char_iter.peek() {
            if pred(c) {
                buffer.push(c)
            } else {
                break;
            }
            self.char_iter.next();
        }

        buffer
    }

    fn match_one(&mut self, kind: TokenKind) -> Token {
        let &(i, char) = self
            .char_iter
            .peek()
            .expect("The iterator should point to a valid char when this method is called.");
        let lexeme = char.to_string();

        self.char_iter.next();

        Token {
            kind,
            lexeme,
            line_number: self.line_number,
            col_number: self.calc_col_num(i),
        }
    }

    /// Matches either one or two characters, and returns the token.
    /// Changes state of iterator.
    ///
    /// There are cases where we want to lex two-character sequences, but
    ///     need to look ahead one character to determine what we are looking at.
    ///
    /// # Example:
    /// ```md
    /// !xxx <--- Unparsed characters
    /// ^--- If we are here, we need to look ahead to see if we are at an
    ///      Inequality '!=', or just LogicalNot '!'.
    ///
    /// ```
    ///
    fn match_two_or_one(&mut self, matches: &[(char, TokenKind)], otherwise: TokenKind) -> Token {
        let (i, first) = self
            .char_iter
            .peek()
            .cloned()
            .expect("The iterator should point to a valid char when this method is called.");

        // Consume the first character, move to the second
        self.char_iter.next();
        for (second, kind) in matches {
            if self.char_iter.peek().is_some_and(|&(_, c)| c == *second) {
                self.char_iter.next();
                return Token {
                    kind: kind.clone(),
                    lexeme: format!("{}{}", first, second),
                    line_number: self.line_number,
                    col_number: self.calc_col_num(i),
                };
            }
        }

        Token {
            kind: otherwise,
            lexeme: first.to_string(),
            line_number: self.line_number,
            col_number: self.calc_col_num(i),
        }
    }

    /// Calculates the current column number based on the number
    /// of characters since the last line break.
    fn calc_col_num(&self, i: usize) -> usize {
        i - self.last_line_break + 1
    }

    /// Takes the current lexer state and produces a LexerError.
    fn into_err(&self, idx: usize, msg: &str) -> LexerError {
        LexerError {
            message: msg.to_string(),
            line_number: self.line_number,
            col_number: self.calc_col_num(idx),
        }
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
    LParen,
    RParen,
    LCurly,
    RCurly,
    LSquare,
    RSquare,

    //---- Single character operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Equal,
    Ampersand,
    Bar,
    Bang,
    Less,
    Greater,
    Comma,
    Semicolon,
    Colon,
    ColonColon,
    Dot,
    Question,
    Tilde,
    Caret,

    //---- Double-character operators
    LessEqual,
    GreaterEqual,
    LessLess,
    GreaterGreater,
    EqualEqual,
    BangEqual,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    PercentEqual,
    PlusPlus,
    MinusMinus,
    BarEqual,
    AmpersandEqual,
    LogicalOr,
    LogicalAnd,
    Arrow,
    LineComment,
    BlockCommentStart,
    BlockCommentEnd,

    // Keywords
    If,
    Else,
    While,
    For,
    Return,
    True,
    False,

    //---- Types
    TypeInt,
    TypeLong,
    TypeFloat,
    TypeDouble,
    TypeChar,
    TypeVoid,

    //---- Literals
    Identifier,
    IntegerLiteral,
    FloatLiteral,
    StringLiteral,
    CharLiteral,

    //---- End of File
    #[allow(clippy::upper_case_acronyms)]
    EOF,

    //---- Unknown
    Unknown,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            TokenKind::LParen => "lparen",
            TokenKind::RParen => "rparen",
            TokenKind::LCurly => "lcurly",
            TokenKind::RCurly => "rcurly",
            TokenKind::LSquare => "lsquare",
            TokenKind::RSquare => "rsquare",
            TokenKind::Plus => "plus",
            TokenKind::Minus => "minus",
            TokenKind::Star => "star",
            TokenKind::Slash => "slash",
            TokenKind::Percent => "percent",
            TokenKind::Equal => "equal",
            TokenKind::Ampersand => "ampersand",
            TokenKind::Bar => "bar",
            TokenKind::Bang => "bang",
            TokenKind::Less => "less",
            TokenKind::Greater => "greater",
            TokenKind::Comma => "comma",
            TokenKind::Semicolon => "semicolon",
            TokenKind::Colon => "colon",
            TokenKind::ColonColon => "coloncolon",
            TokenKind::Dot => "dot",
            TokenKind::Question => "question",
            TokenKind::Tilde => "tilde",
            TokenKind::Caret => "caret",
            TokenKind::LessEqual => "lessequal",
            TokenKind::GreaterEqual => "greaterequal",
            TokenKind::LessLess => "lessless",
            TokenKind::GreaterGreater => "greatergreater",
            TokenKind::EqualEqual => "equalequal",
            TokenKind::BangEqual => "bangequal",
            TokenKind::PlusEqual => "plusequal",
            TokenKind::MinusEqual => "minusequal",
            TokenKind::StarEqual => "starequal",
            TokenKind::SlashEqual => "slashequal",
            TokenKind::PercentEqual => "percentequal",
            TokenKind::PlusPlus => "plusplus",
            TokenKind::MinusMinus => "minusminus",
            TokenKind::BarEqual => "barequal",
            TokenKind::AmpersandEqual => "ampersandequal",
            TokenKind::LogicalOr => "logicalor",
            TokenKind::LogicalAnd => "logicaland",
            TokenKind::Arrow => "arrow",
            TokenKind::LineComment => "linecomment",
            TokenKind::BlockCommentStart => "blockcommentstart",
            TokenKind::BlockCommentEnd => "blockcommentend",
            TokenKind::If => "if",
            TokenKind::Else => "else",
            TokenKind::While => "while",
            TokenKind::For => "for",
            TokenKind::Return => "return",
            TokenKind::True => "true",
            TokenKind::False => "false",
            TokenKind::TypeInt => "typeint",
            TokenKind::TypeLong => "typelong",
            TokenKind::TypeFloat => "typefloat",
            TokenKind::TypeDouble => "typedouble",
            TokenKind::TypeChar => "typechar",
            TokenKind::TypeVoid => "typevoid",
            TokenKind::Identifier => "identifier",
            TokenKind::IntegerLiteral => "integerliteral",
            TokenKind::FloatLiteral => "floatliteral",
            TokenKind::StringLiteral => "stringliteral",
            TokenKind::CharLiteral => "charliteral",
            TokenKind::EOF => "eof",
            TokenKind::Unknown => "unknown",
        };

        write!(f, "{name}")
    }
}
