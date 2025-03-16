use std::rc::Rc;

use crate::lexer::*;

type ParserResult<T> = std::result::Result<T, ParserError>;

#[derive (Debug, Clone)]
pub enum ASTNode {
    // Literals
    IntegerLiteral { token: Token, val: i64, },
    FloatLiteral   { token: Token, val: f64, },
    StringLiteral  { token: Token, val: String, },
    BoolLiteral    { token: Token, val: bool },

    Identifier     { token: Token, name: String, },
    UnaryExpr {
        op: Token,
        expr: Rc<ASTNode>
    },
    BinaryExpr {
        op: Token,
        left: Rc<ASTNode>,
        right: Rc<ASTNode>
    },
    Grouping {
        expr: Rc<ASTNode>,
        left_delim: Token,
        right_delim: Token,
    },
    Call {
        callee: Rc<ASTNode>,
        paren: Token,   // used for error generation
        args: Vec<Rc<ASTNode>>
    },
    Program {
        exprs: Vec<Rc<ASTNode>>
    },
}

impl std::fmt::Display for ASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display = match self {
            ASTNode::IntegerLiteral { token, val } => token.lexeme.clone(),
            ASTNode::FloatLiteral { token, val } => token.lexeme.clone(),
            ASTNode::StringLiteral { token, val } => token.lexeme.clone(),
            ASTNode::BoolLiteral { token, val } => token.lexeme.clone(),
            ASTNode::Identifier { token, name } => token.lexeme.clone(),
            ASTNode::UnaryExpr { op, expr } => format!("{}{}", op.lexeme, expr.as_ref()),
            ASTNode::BinaryExpr { op, left, right } => format!("{} {} {}", left.as_ref(), op.lexeme, right.as_ref()),
            ASTNode::Grouping { expr, left_delim, right_delim } => format!("{}{}{}", left_delim.lexeme, expr.as_ref(), right_delim.lexeme),
            ASTNode::Call { callee, paren, args } => {
                let args = args
                    .iter()
                    .map(|x| x.as_ref())
                    .map(ASTNode::to_string)
                    .collect::<Vec<String>>()
                    .join(", ");

                format!("{} ({args})", callee.as_ref())
            },
            ASTNode::Program { exprs } => {
                let exprs = exprs
                    .iter()
                    .map(|x| x.as_ref())
                    .map(ASTNode::to_string)
                    .collect::<Vec<String>>()
                    .join(";\n");

                exprs
            }
        };

        write!(f, "{display}")
    }
}


pub struct Parser {
    cursor: usize,
    tokens: Vec<Token>,
}

impl Parser {
    /// Returns true if we are done.
    fn end(&self) -> bool {
        self.cursor >= self.tokens.len() - 1 
    }

    /// Advances cursor. Returns new token.
    fn advance(&mut self) -> Option<&Token> {
        // Advance the cursor if we're not at the end
        self.cursor += 1;
        self.peek()
    }

    /// Returns the next token without advancing the cursor.
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.cursor)
    }

    /// Returns the next token's kind without advancing the cursor.
    fn peek_kind(&self) -> Option<&TokenKind> {
        self.tokens.get(self.cursor).map(|token| &token.kind)
    }

    /// Returns true if the next token's kind is what we pass.
    /// 
    /// Does not advance the cursor.
    fn next_is_kind(&self, kind: TokenKind) -> bool {
        return self.peek_kind() == Some(&kind)
    }

    /// Returns the token previous to the cursor.
    fn previous(&self) -> Option<&Token> {
        self.tokens.get(self.cursor - 1)
    }

    /// Returns the next token and advances the cursor if 
    /// the next token is one of the ones we expect.
    /// 
    /// Returns None otherwise, and leaves the cursor unchanged.
    fn eat_any_of(&mut self, targets: &[TokenKind]) -> Option<&Token> {
        for target in targets {
            if self.peek_kind() == Some(target) {
                self.advance();
                return self.previous() 
            }
        };

        None
    }

    /// Returns the next token and advances the cursor if 
    /// the next token is the one we expect.
    /// 
    /// Returns None otherwise, and leaves the cursor unchanged.
    fn eat_one(&mut self, target: TokenKind) -> Option<&Token> {
        if self.peek_kind() == Some(&target) {
            self.advance();
            return self.previous() 
        }
        None
    }
}

impl Parser {
    pub fn parse(input: Vec<Token>) -> ParserResult<ASTNode> {
        Self {
            cursor: 0,
            tokens: input,
        }.program()
    }

    fn program(&mut self) -> ParserResult<ASTNode> {
        // TODO: For now, a program is a single expression.
        let mut exprs = Vec::new();
        while self.peek().is_some() {
            exprs.push(Rc::new(self.call()?));
        }

        Ok(ASTNode::Program{ exprs })
    }

    fn call(&mut self) -> ParserResult<ASTNode> {
        let mut expr = self.primary()?;
        while let Some(lparen) = self.eat_one(TokenKind::LParen) {
            let lparen = lparen.clone();
            let mut args = Vec::<Rc<ASTNode>>::new();
            while !self.next_is_kind(TokenKind::RParen) {
                args.push(Rc::new(self.expression()?));
                if self.eat_one(TokenKind::Comma).is_none() {
                    break;
                }
            }

            let callee = Rc::new(expr);

            match self.eat_one(TokenKind::RParen) {
                Some(rparen) => expr = ASTNode::Call{ callee, paren: rparen.clone(), args },
                None => return Err(self.to_err_with_token("unmatched '('", lparen))
            }
        }

        Ok(expr)
    }

    fn expression(&mut self) -> ParserResult<ASTNode> {
        self.equality()
    }

    fn equality(&mut self) -> ParserResult<ASTNode> {
        let mut expr = self.comparison()?;
        while let Some(op) = self.eat_any_of(&[TokenKind::BangEqual, 
                                            TokenKind::EqualEqual]) {
            let op = op.clone();
            let left = Rc::new(expr.clone());
            let right = Rc::new(self.comparison()?);
            expr = ASTNode::BinaryExpr { op, left, right };
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> ParserResult<ASTNode> {
        let mut expr = self.term()?;
        while let Some(op) = self.eat_any_of(&[TokenKind::Greater, TokenKind::GreaterEqual, 
                                            TokenKind::Less, TokenKind::LessEqual]) {
            let op = op.clone();
            let left = Rc::new(expr.clone());
            let right = Rc::new(self.term()?);
            expr = ASTNode::BinaryExpr { op, left, right };
        }

        Ok(expr)
    }

    fn term(&mut self) -> ParserResult<ASTNode> {
        let mut expr = self.factor()?;
        while let Some(op) = self.eat_any_of(&[TokenKind::Minus, TokenKind::Plus]).cloned() {
            let left = Rc::new(expr.clone());
            let right = Rc::new(self.factor()?);
            expr = ASTNode::BinaryExpr { op, left, right };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> ParserResult<ASTNode> {
        let mut expr = self.unary()?;
        while let Some(op) = self.eat_any_of(&[TokenKind::Slash, TokenKind::Star]) {
            let op = op.clone();
            let left = Rc::new(expr.clone());
            let right = Rc::new(self.unary()?);
            expr = ASTNode::BinaryExpr { op, left, right };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ParserResult<ASTNode> {
        if let Some(op) = self.eat_any_of(&[TokenKind::Bang, TokenKind::Minus]) {
            let op = op.clone();
            let expr = Rc::new(self.unary()?);
            Ok(ASTNode::UnaryExpr { op, expr })
        }
        else { Ok(self.primary()?) }
    }

    fn primary(&mut self) -> ParserResult<ASTNode> {
        let curr_token = self.peek()
            .ok_or_else(|| self.to_err("expected PRIMARY token"))?
            .clone();

        self.advance();

        let node = match curr_token.kind {
            TokenKind::True => {
                ASTNode::BoolLiteral {val: true, token: curr_token}
            },
            TokenKind::False => {
                ASTNode::BoolLiteral {val: false, token: curr_token}
            },
            TokenKind::IntegerLiteral => {
                ASTNode::IntegerLiteral {
                    val: curr_token.lexeme.parse().map_err(|err| self.to_err(err))?, 
                    token: curr_token
                }
            },
            TokenKind::FloatLiteral => {
                ASTNode::FloatLiteral {
                    val: curr_token.lexeme.parse().map_err(|err| self.to_err(err))?, 
                    token: curr_token
                }
            },
            TokenKind::StringLiteral => {
                ASTNode::StringLiteral {val: curr_token.lexeme.clone(),  token: curr_token}
            },

            TokenKind::Identifier => {
                ASTNode::Identifier {name: curr_token.lexeme.clone(), token: curr_token}
            },

            // Parenthesized expressions, aka groupings
            TokenKind::LParen => {
                let left_delim = curr_token;

                let expr = Rc::new(self.expression()?);

                match self.eat_one(TokenKind::RParen).cloned() {
                    Some(right_delim) => ASTNode::Grouping { expr, left_delim, right_delim},
                    None => Err(self.to_err_with_token("unmatched '('", left_delim))?
                }
            }

            // Unimplemented
            _ => {
                return Err(self.to_err(format!("unimplemented tokenkind: {:?}", curr_token.kind)));
            }
        };

        Ok(node)
    }

    fn to_err<T: std::fmt::Display>(&self, msg: T) -> ParserError {
        ParserError {
            message: msg.to_string(),
            token: self.peek().cloned(),
        }
    }

    fn to_err_with_token<T: std::fmt::Display>(&self, msg: T, token: Token) -> ParserError {
        ParserError {
            message: msg.to_string(),
            token: Some(token),
        }
    }
}

#[derive(Debug)]
pub struct ParserError {
    pub message: String,
    pub token: Option<Token>,
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let msg = self.token
            .as_ref()
            .map_or_else(
                ||      self.message.to_string(),
                |token| format!("{}:{}:{}", self.message, token.line_number, token.col_number));

        write!(f, "{msg}")
    }
}