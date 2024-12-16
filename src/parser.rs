use std::rc::Rc;

use anyhow::{Result, Error, Context, anyhow};

use crate::lexer::{self, Token, TokenKind};

#[derive (Debug, Clone)]
pub enum ASTNode {
    Integer (i32),
    Float (f32),
    String (String),
    Boolean (bool),
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
    }
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
        self.cursor >= self.tokens.len()
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
        self.tokens.get(self.cursor).and_then(|token| Some(&token.kind))
    }

    /// Returns the token previous to the cursor.
    fn previous(&self) -> Option<&Token> {
        self.tokens.get(self.cursor - 1)
    }

    /// Returns the next token and advances the cursor if 
    /// the next token is one of the ones we expect.
    /// 
    /// Returns None otherwise, and leaves the cursor unchanged.
    fn matches(&mut self, targets: &[TokenKind]) -> Option<&Token> {
        for target in targets {
            if self.peek_kind() == Some(target) {
                self.advance();
                return self.previous() 
            }
        };

        None
    }
}

impl Parser {
    pub fn parse(input: &str) -> Result<ASTNode> {
        Self {
            cursor: 0,
            tokens: lexer::Lexer::lex(input)?
        }.program()
    }

    fn program(&mut self) -> Result<ASTNode> {
        // TODO: For now, a program is a single expression.
        let program = self.expression()?;
        eprintln!("self.peek() = {:?}", self.peek());

        if !self.end() { Err(anyhow!("Expected EOF.")) } 
        else { Ok(program) }
    }

    /// <expression> ::= <assignment-expression>
    ///                | <expression> , <assignment-expression>
    fn expression(&mut self) -> Result<ASTNode> {
        self.equality()
    }

    fn equality(&mut self) -> Result<ASTNode> {
        let mut expr = self.comparison()?;
        while let Some(op) = self.matches(&[TokenKind::BangEqual, 
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
        while let Some(op) = self.matches(&[TokenKind::Greater, TokenKind::GreaterEqual, 
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
        while let Some(op) = self.matches(&[TokenKind::Minus, TokenKind::Plus]) {
            let op = op.clone();
            let left = Rc::new(expr.clone());
            let right = Rc::new(self.factor()?);
            expr = ASTNode::BinaryExpr { op, left, right };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<ASTNode> {
        let mut expr = self.unary()?;
        while let Some(op) = self.matches(&[TokenKind::Slash, TokenKind::Star]) {
            let op = op.clone();
            let left = Rc::new(expr.clone());
            let right = Rc::new(self.unary()?);
            expr = ASTNode::BinaryExpr { op, left, right };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<ASTNode, Error> {
        if let Some(op) = self.matches(&[TokenKind::Bang, TokenKind::Minus]) {
            let op = op.clone();
            let expr = Rc::new(self.unary()?);
            Ok(ASTNode::UnaryExpr { op, expr })
        }
        else { Ok(self.primary()?) }
    }

    fn primary(&mut self) -> Result<ASTNode, Error> {
        let token = self.peek().context("Expected PRIMARY token")?;
        let node = match token.kind {
            TokenKind::True              => ASTNode::Boolean(true),
            TokenKind::False             => ASTNode::Boolean(true),
            TokenKind::IntegerLiteral    => ASTNode::Integer(token.lexeme.parse()?),
            TokenKind::FloatLiteral      => ASTNode::Float(token.lexeme.parse()?),
            TokenKind::StringLiteral     => ASTNode::String(token.lexeme.clone()),
            TokenKind::LParen            => {
                let left = self.peek()
                               .cloned()
                               .context("Internal error.")?;

                self.advance();

                let expr = Rc::new(self.expression()?);

                let right = self.matches(&[TokenKind::RParen])
                                .cloned()
                                .context("Expected ')' after expression.")?;

                ASTNode::Grouping { expr, left, right }
            }
            _ => {
                ASTNode::Integer(-1)
            }
        };

        self.advance();

        Ok(node)
    }
}


