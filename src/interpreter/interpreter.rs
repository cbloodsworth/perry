use anyhow::anyhow;
use itertools::Itertools;

use crate::TokenLocation;
use crate::{parser::{self, ASTNode}, print_lex_results, print_parse_results, Lexer, Parser};


type Result<T> = std::result::Result<T, InterpreterError>;

#[derive(Debug)]
pub enum InterpreterError {
    TypeMismatch(String, TokenLocation),
    NameCollision(String, TokenLocation),
    NameNotFound(String, TokenLocation),
    RuntimeError(String),
}

macro_rules! runtime_error {
    ($($arg:tt)*) => {
        Err(InterpreterError::RuntimeError(format!($($arg)*)))
    };
}

impl std::fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO/implement: Add additional attributes to the error display
        let msg = match self {
            InterpreterError::TypeMismatch(s, l) => format!("type mismatch: {s} ({}:{})", l.0, l.1),
            InterpreterError::NameCollision(s, l) => format!("name collision: {s} ({}:{})", l.0, l.1),
            InterpreterError::NameNotFound(s, l) => format!("name not found: {s} ({}:{})", l.0, l.1),
            InterpreterError::RuntimeError(s) => s.to_owned(),
        };

        write!(f, "{msg}")
    }
}

pub struct Interpreter;

impl super::repl::RunCommand for Interpreter {
    fn run_cmd(cmd: &str) -> std::result::Result<String, String> {
        let cmd = &format!("({cmd})");
        crate::compile(cmd).map_err(|err| format!("{err}"))
    }

    fn run_cmd_debug(cmd: &str) -> std::result::Result<String, String> {
        let cmd = &format!("({cmd})");
        match crate::compile(cmd) {
            Ok(val) => {
                let mut info = String::new();

                if let Ok(lex_dbg_info) = print_lex_results(cmd) { 
                    info += &format!("\nLEXER INFO:\n{lex_dbg_info}\n"); 
                }
                if let Ok(parse_dbg_info) = print_parse_results(cmd) { 
                    info += &format!("\nPARSER INFO:\n{parse_dbg_info}\n"); 
                }

                Ok(format!("{info}OUTPUT:\n{val}"))
            }
            Err(err) => {
                let mut info = String::new();

                if let Ok(lex_dbg_info) = print_lex_results(cmd) { 
                    info += &format!("\nLEXER INFO:\n{lex_dbg_info}\n"); 
                }
                if let Ok(parse_dbg_info) = print_parse_results(cmd) { 
                    info += &format!("\nPARSER INFO:\n{parse_dbg_info}\n"); 
                }

                Err(format!("{info}\n{err}"))
            }
        }
    }
}

pub trait Compile {
    type Output;

    fn from_ast(ast: ASTNode) -> anyhow::Result<Self::Output>;

    fn from_source(source: &str) -> anyhow::Result<Self::Output> {
        let tokens = Lexer::lex(source).map_err(|err| anyhow!("LEXER ERROR: {err}"))?;
        let ast = Parser::parse(tokens).map_err(|err| anyhow!("PARSER ERROR: {err}"))?;

        Self::from_ast(ast)
    }
}

impl Compile for Interpreter {
    type Output = Value;

    fn from_ast(ast: ASTNode) -> anyhow::Result<Self::Output> {
        let eval = Evaluator::new();
        eval.eval(ast).map_err(|err| anyhow!(err))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),

    Array(Vec<Value>),
    Tuple(Vec<Value>),
}

impl Value {
    fn name(&self) -> String {
        match self {
            Value::Integer(_) => "integer",
            Value::Float(_) => "float",
            Value::String(_) => "string",
            Value::Boolean(_) => "boolean",
            Value::Array(_) => "array",
            Value::Tuple(_) => "array",
        }
        .to_owned()
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
        use ASTNode as Node;
        let val = match root {
            Node::Program { exprs } => {
                                // TODO/implement: should we really just print the last expression..?
                                // We probably want to evaluate all previous expressions first.
                                let last = exprs.into_vec().pop().unwrap();
                                self.eval(last)?
                            }
            Node::IntegerLiteral { val, .. } => Value::Integer(val),
            Node::FloatLiteral { val, .. } => Value::Float(val),
            Node::StringLiteral { val, .. } => Value::String(val),
            Node::BoolLiteral { val, .. } => Value::Boolean(val),
            Node::TupleLiteral { exprs, .. } => {
                let tuple = exprs
                    .iter()
                    .cloned()
                    .map(|expr| self.eval(expr))
                    .collect::<Result<Vec<_>>>()?;

                Value::Tuple(tuple)
            },
            Node::ArrayLiteral { exprs, left_delim, .. } => {
                let array = exprs
                    .iter()
                    .cloned()
                    .map(|expr| self.eval(expr))
                    .collect::<Result<Vec<_>>>()?;


                if array.iter().map(std::mem::discriminant).all_equal() {
                    Value::Array(array)
                } else {
                    return Err(InterpreterError::TypeMismatch("incompatible types in array".into(), left_delim.loc));
                }
            },
            Node::Identifier { token, name, } => self
                                .lookup(&name)
                                .ok_or(InterpreterError::NameNotFound(
                                    format!("couldn't find identifier {name} in this scope"), token.loc))?
                                .to_owned(),
            Node::UnaryExpr { op, expr } => {
                                let val = self.eval(*expr)?;

                                match op.kind {
                                    parser::UnaryOpKind::Negate => negate(val)?,
                                    parser::UnaryOpKind::LogicalNot => not(val)?,
                                }
                            }
            Node::BinaryExpr { op, left, right } => {
                                let loc = left.get_loc();
                                let left_val = self.eval(*left)?;
                                let right_val = self.eval(*right)?;

                                match op.kind {
                                    parser::BinaryOpKind::Plus => 
                                        add(left_val, right_val)
                                            .map_err(|err| InterpreterError::TypeMismatch(err.to_string(), loc))?,
                                    parser::BinaryOpKind::Minus => sub(left_val, right_val)?,
                                    parser::BinaryOpKind::Times => mult(left_val, right_val)?,
                                    parser::BinaryOpKind::Divide => div(left_val, right_val)?,
                                    parser::BinaryOpKind::NotEqual => ne(left_val, right_val)?,
                                    parser::BinaryOpKind::Equal => eq(left_val, right_val)?,
                                }
                            }
            Node::Grouping { expr, .. } => self.eval(*expr)?,
            Node::Call { callee, paren, args, } => todo!(),
        };

        Ok(val)
    }

    fn lookup(&self, var: &str) -> Option<&Value> {
        self.environment.get(var)
    }
}

fn add(lhs: Value, rhs: Value) -> Result<Value> {
    use Value::*;
    let addition = match (lhs, rhs) {
        (Integer(l), Integer(r)) => Integer(l + r),
        (Float(l), Float(r)) => Float(l + r),

        // integer + float
        (Integer(l), Float(r)) => Float(l as f64 + r),
        (Float(l), Integer(r)) => Float(l + r as f64),

        // String concatenation
        (String(l), String(r)) => String(l + &r),

        (lhs, rhs) => {
            return runtime_error!("could not add types {} and {}", lhs.name(), rhs.name());
        }
    };

    Ok(addition)
}

fn sub(lhs: Value, rhs: Value) -> Result<Value> {
    use Value::*;
    let subtraction = match (lhs, rhs) {
        (Integer(l), Integer(r)) => Integer(l - r),
        (Float(l), Float(r)) => Float(l - r),

        // integer + float
        (Integer(l), Float(r)) => Float(l as f64 - r),
        (Float(l), Integer(r)) => Float(l - r as f64),

        (String(l), String(r)) => String(format!(
            "{l}{}",
            r.clone().chars().rev().collect::<std::string::String>()
        )),

        (lhs, rhs) => {
            return runtime_error!( "could not subtract types {} and {}", lhs.name(), rhs.name());
        }
    };

    Ok(subtraction)
}

fn mult(lhs: Value, rhs: Value) -> Result<Value> {
    use Value::*;
    let multiplication = match (lhs, rhs) {
        (Integer(l), Integer(r)) => Integer(l * r),
        (Float(l), Float(r)) => Float(l * r),

        // integer + float
        (Integer(l), Float(r)) => Float((l as f64) * r),
        (Float(l), Integer(r)) => Float(l * (r as f64)),

        (lhs, rhs) => {
            return runtime_error!("could not multiply types {} and {}", lhs.name(), rhs.name());
        }
    };

    Ok(multiplication)
}

fn div(lhs: Value, rhs: Value) -> Result<Value> {
    match rhs {
        Integer(0) | Float(0.0) => return runtime_error!("division by zero"),
        _ => {}
    }

    use Value::*;
    let division = match (lhs, rhs) {
        (Integer(l), Integer(r)) => Integer(l / r),
        (Float(l), Float(r)) => Float(l / r),

        // integer + float
        (Integer(l), Float(r)) => Float(l as f64 / r),
        (Float(l), Integer(r)) => Float(l / r as f64),

        (lhs, rhs) => {
            return runtime_error!("could not divide types {} and {}", lhs.name(), rhs.name());
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
            return runtime_error!("could not negate type {}", val.name());
        }
    };

    Ok(negation)
}

fn not(val: Value) -> Result<Value> {
    use Value::*;
    let logical_not = match val {
        Boolean(b) => Boolean(!b),
        _ => return runtime_error!("could not negate non-boolean type {}", val.name()),
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
        (Array(l), Array(r)) => l != r,
        (Tuple(l), Tuple(r)) => l != r,

        (lhs, rhs) => {
            return runtime_error!("could not compare types {} and {}", lhs.name(), rhs.name());
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
        (Array(l), Array(r)) => l == r,
        (Tuple(l), Tuple(r)) => l == r,

        (lhs, rhs) => {
            return runtime_error!("could not compare types {} and {}", lhs.name(), rhs.name());
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

            Value::Array(a) => &format!("[{}]", a.iter().map(Value::to_string).join(", ")),
            Value::Tuple(t) => &format!("({})", t.iter().map(Value::to_string).join(", ")),
        };

        write!(f, "{s}")
    }
}
