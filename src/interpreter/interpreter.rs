use std::{ops::Add, path::Display};

use crate::{parser, Lexer, Parser, parser::ASTNode};

type Result<T> = std::result::Result<T, InterpreterError>;

#[derive(Debug)]
pub enum InterpreterError {
    TypeMismatch(String),
    NameCollision(String),
    NameNotFound(String),
    RuntimeError(String),
}

impl std::fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO/implement: Add additional attributes to the error display
        let msg = match self {
            InterpreterError::TypeMismatch(s) => format!("type mismatch: {s}"),
            InterpreterError::NameCollision(s) => format!("name collision: {s}"),
            InterpreterError::NameNotFound(s) => format!("name not found: {s}"),
            InterpreterError::RuntimeError(s) => format!("runtime error: {s}"),
        };

        write!(f, "{msg}")
    }
}

pub trait Compile {
    type Output;

    fn from_ast(ast: ASTNode) -> Self::Output;

    fn from_source(source: &str) -> Self::Output {
        let tokens = Lexer::lex(source).unwrap_or_else(|err| panic!("LEXER ERROR: {err}"));
        let ast = Parser::parse(tokens).unwrap_or_else(|err| panic!("PARSER ERROR: {err}"));

        Self::from_ast(ast)
    }
}

pub struct Interpreter;

impl Compile for Interpreter {
    type Output = Result<Value>;

    fn from_ast(ast: ASTNode) -> Self::Output {
        let eval = Evaluator::new();
        eval.eval(ast)
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
}

impl Value {
    fn name(&self) -> String {
        match self {
            Value::Integer(_) => "integer",
            Value::Float(_) => "float",
            Value::String(_) => "string",
            Value::Boolean(_) => "boolean",
        }.to_owned()
    }
}

struct Evaluator {
    environment: std::collections::HashMap<String, Value>,
}

impl Evaluator {
    fn new() -> Self {
        Self {
            environment: std::collections::HashMap::new(),
        }
    }
    fn eval(&self, root: ASTNode) -> Result<Value> {
        use ASTNode::*;
        let val = match root {
            Program { exprs } => {
                // TODO/implement: should we really just print the last expression..?
                // TODO/implement: we shouldn't clone
                let last = exprs.last().cloned().unwrap();
                self.eval(*last)?
            }
            IntegerLiteral { token, val } => Value::Integer(val),
            FloatLiteral { token, val } => Value::Float(val),
            StringLiteral { token, val } => Value::String(val),
            BoolLiteral { token, val } => Value::Boolean(val),
            Identifier { token, name } => {
                self
                    .lookup(&name)
                    .ok_or(InterpreterError::NameNotFound(format!("couldn't find identifier {name} in this scope")))?
                    .to_owned()
            }
            UnaryExpr { op, expr } => {
                let val = self.eval(*expr)?;

                match op.kind {
                    parser::UnaryOpKind::Negate => negate(val)?,
                    parser::UnaryOpKind::LogicalNot => not(val)?,
                }
            }
            BinaryExpr { op, left, right } => {
                let left_val = self.eval(*left)?;
                let right_val = self.eval(*right)?;
                
                match op.kind {
                    crate::parser::BinaryOpKind::Plus => add(left_val, right_val)?,
                    crate::parser::BinaryOpKind::Minus => sub(left_val, right_val)?,
                    crate::parser::BinaryOpKind::Times => mult(left_val, right_val)?,
                    crate::parser::BinaryOpKind::Divide => div(left_val, right_val)?,
                    crate::parser::BinaryOpKind::NotEqual => ne(left_val, right_val)?,
                    crate::parser::BinaryOpKind::Equal => eq(left_val, right_val)?,
                }
            },
            Grouping {
                expr,
                left_delim: left,
                right_delim: right,
            } => {
                self.eval(*expr)?
            }
            Call {
                callee,
                paren,
                args,
            } => self.eval_call()?,
        };

        Ok(val)
    }

    fn lookup(&self, var: &str) -> Option<&Value> {
        self.environment.get(var)
    }

    fn eval_unary(&self) -> Result<Value> {
        todo!()
    }

    fn eval_binary(&self) -> Result<Value> {
        todo!()
    }

    fn eval_call(&self) -> Result<Value> {
        todo!()
    }
}

fn add(lhs: Value, rhs: Value) -> Result<Value> {
    use Value::*;
    let addition = match (lhs, rhs) {
        (Integer(l), Integer(r)) => Integer(l+r),
        (Float(l), Float(r)) => Float(l+r),

        // integer + float
        (Integer(l), Float(r)) => Float(l as f64 + r),
        (Float(l), Integer(r)) => Float(l + r as f64),

        // String concatenation
        (String(l), String(r)) => String(l+&r),

        (lhs, rhs) => {
            return Err(InterpreterError::TypeMismatch(
                    format!("could not add types {} and {}", lhs.name(), rhs.name())));
        }
    };

    Ok(addition)
}

fn sub(lhs: Value, rhs: Value) -> Result<Value> {
    use Value::*;
    let subtraction = match (lhs, rhs) {
        (Integer(l), Integer(r)) => Integer(l-r),
        (Float(l), Float(r)) => Float(l-r),

        // integer + float
        (Integer(l), Float(r)) => Float(l as f64 - r),
        (Float(l), Integer(r)) => Float(l - r as f64),

        (String(l), String(r)) => String(format!("{l}{}", r.clone().chars().rev().collect::<std::string::String>())),

        (lhs, rhs) => {
            return Err(InterpreterError::TypeMismatch(
                    format!("could not subtract types {} and {}", lhs.name(), rhs.name())));
        }
    };

    Ok(subtraction)
}

fn mult(lhs: Value, rhs: Value) -> Result<Value> {
    use Value::*;
    let multiplication = match (lhs, rhs) {
        (Integer(l), Integer(r)) => Integer(l*r),
        (Float(l), Float(r)) => Float(l*r),

        // integer + float
        (Integer(l), Float(r)) => Float((l as f64) * r),
        (Float(l), Integer(r)) => Float(l * (r as f64)),

        (lhs, rhs) => {
            return Err(InterpreterError::TypeMismatch(
                    format!("could not multiply types {} and {}", lhs.name(), rhs.name())));
        }
    };

    Ok(multiplication)
}

fn div(lhs: Value, rhs: Value) -> Result<Value> {
    use Value::*;
    let division = match (lhs, rhs) {
        (Integer(l), Integer(r)) => Integer(l/r),
        (Float(l), Float(r)) => Float(l/r),

        // integer + float
        (Integer(l), Float(r)) => Float(l as f64 / r),
        (Float(l), Integer(r)) => Float(l / r as f64),

        (lhs, rhs) => {
            return Err(InterpreterError::TypeMismatch(
                    format!("could not divide types {} and {}", lhs.name(), rhs.name())));
        }
    };

    Ok(division)
}

fn negate(val: Value) -> Result<Value> {
    use Value::*;
    let negation = match val {
        Integer(v) => Integer(-v),
        Float(v) => Float(-v),
        String(v) => String(v.chars().rev().collect()),
        _ => {
            return Err(InterpreterError::TypeMismatch(
                    format!("could not negate type {}", val.name())));
        }
    };

    Ok(negation)
}

fn not(val: Value) -> Result<Value> {
    use Value::*;
    let logical_not = match val {
        Value::Boolean(b) => Value::Boolean(!b),
        _ => {
            return Err(InterpreterError::TypeMismatch(
                format!("could not negate non-boolean type {}", val.name())));
        }
    };

    Ok(logical_not)
}

fn ne(lhs: Value, rhs: Value) -> Result<Value> {
    use Value::*;
    let inequality = match (lhs, rhs) {
        (Integer(l), Integer(r)) => l != r,
        (Float(l), Float(r)) => l != r,
        (String(l), String(r)) => l != r,
        (Boolean(l), Boolean(r)) => l != r,

        (lhs, rhs) => {
            return Err(InterpreterError::TypeMismatch(
                    format!("could not compare types {} and {}", lhs.name(), rhs.name())));
        }
    };

    Ok(Value::Boolean(inequality))
}

fn eq(lhs: Value, rhs: Value) -> Result<Value> {
    use Value::*;
    let equality = match (lhs, rhs) {
        (Integer(l), Integer(r)) => l == r,
        (Float(l), Float(r)) => l == r,
        (String(l), String(r)) => l == r,
        (Boolean(l), Boolean(r)) => l == r,

        (lhs, rhs) => {
            return Err(InterpreterError::TypeMismatch(
                    format!("could not compare types {} and {}", lhs.name(), rhs.name())));
        }
    };

    Ok(Value::Boolean(equality))
}


impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Value::Integer(i) => &format!("{i}"),
            Value::Float(f) => &format!("{f:?}"),
            Value::String(s) => s,
            Value::Boolean(b) => &format!("{b}"),
        };

        write!(f, "{s}")
    }
}