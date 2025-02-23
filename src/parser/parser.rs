use std::rc::Rc;
use anyhow::{Result, Error, Context, anyhow};

use crate::lexer::*;

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
        left: Token,
        right: Token,
    },
    Call {
        callee: Rc<ASTNode>,
        paren: Token,   // used for error generation
        args: Vec<Rc<ASTNode>>
    },
}


pub struct Parser {
    cursor: usize,
    tokens: Vec<Token>
}

impl Parser {
    /// Returns current position of the cursor.
    fn cursor(&self) -> usize {
        self.cursor
    }

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
    pub fn parse(input: &str) -> Result<ASTNode> {
        Self {
            cursor: 0,
            tokens: Lexer::lex(input)?
        }.program()
    }

    fn program(&mut self) -> Result<ASTNode> {
        // TODO: For now, a program is a single expression.
        let program = self.call()?;

        if !self.end() { Err(anyhow!("Expected EOF, got kind: {:?}", self.peek_kind())) } 
        else { Ok(program) }
    }

    fn call(&mut self) -> Result<ASTNode> {
        let mut expr = self.primary()?;
        while self.eat_one(TokenKind::LParen).is_some() {
            let mut args = Vec::<Rc<ASTNode>>::new();
            while !self.next_is_kind(TokenKind::RParen) {
                args.push(Rc::new(self.expression()?));
                if self.eat_one(TokenKind::Comma).is_none() {
                    break;
                }
            }

            let callee = Rc::new(expr);
            let paren = self.eat_one(TokenKind::RParen).cloned()
                .context("Expected ')' after argument list.")?;

            expr = ASTNode::Call{ callee, paren, args };
        }

        Ok(expr)
    }

    fn expression(&mut self) -> Result<ASTNode> {
        self.equality()
    }

    fn equality(&mut self) -> Result<ASTNode> {
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

    fn comparison(&mut self) -> Result<ASTNode> {
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

    fn term(&mut self) -> Result<ASTNode> {
        let mut expr = self.factor()?;
        while let Some(op) = self.eat_any_of(&[TokenKind::Minus, TokenKind::Plus]) {
            let op = op.clone();
            let left = Rc::new(expr.clone());
            let right = Rc::new(self.factor()?);
            expr = ASTNode::BinaryExpr { op, left, right };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<ASTNode> {
        let mut expr = self.unary()?;
        while let Some(op) = self.eat_any_of(&[TokenKind::Slash, TokenKind::Star]) {
            let op = op.clone();
            let left = Rc::new(expr.clone());
            let right = Rc::new(self.unary()?);
            expr = ASTNode::BinaryExpr { op, left, right };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<ASTNode, Error> {
        if let Some(op) = self.eat_any_of(&[TokenKind::Bang, TokenKind::Minus]) {
            let op = op.clone();
            let expr = Rc::new(self.unary()?);
            Ok(ASTNode::UnaryExpr { op, expr })
        }
        else { Ok(self.primary()?) }
    }

    fn primary(&mut self) -> Result<ASTNode, Error> {
        let token = self.peek().context("Expected PRIMARY token").cloned()?;
        let node = match token.kind {
            TokenKind::True              => ASTNode::BoolLiteral    {val: true,                  token},
            TokenKind::False             => ASTNode::BoolLiteral    {val: false,                 token},
            TokenKind::IntegerLiteral    => ASTNode::IntegerLiteral {val: token.lexeme.parse()?, token},
            TokenKind::FloatLiteral      => ASTNode::FloatLiteral   {val: token.lexeme.parse()?, token},
            TokenKind::StringLiteral     => ASTNode::StringLiteral  {val: token.lexeme.clone(),  token},

            TokenKind::Identifier        => ASTNode::Identifier     {name: token.lexeme.clone(), token},

            // Parenthesized expressions, aka groupings
            TokenKind::LParen            => {
                let left = self.peek()
                               .cloned()
                               .context("Internal error.")?;  // when would this even happen

                self.advance();

                let expr = Rc::new(self.expression()?);

                let right = self.eat_one(TokenKind::RParen).cloned()
                    .context("Expected ')' after expression.")?;

                // We are already on the next token, early return
                return Ok(ASTNode::Grouping { expr, left, right })
            }

            // Unimplemented
            _ => {
                return Err(anyhow!("Unimplemented TokenKind: {:?}", token.kind));
            }
        };

        self.advance();

        Ok(node)
    }
}


