use crate::parser::ASTNode;

type Result<T> = std::result::Result<T, InterpreterError>;

#[derive(Debug)]
pub enum InterpreterError {
    TypeError(String),
    NameError(String),
    RuntimeError(String),
}

impl std::fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

pub struct Interpreter {
    environment: std::collections::HashMap<String, Value>,
}

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
}

impl Interpreter {
    pub fn eval(&self, root: &ASTNode) -> Result<Value> {
        use ASTNode::*;
        match root {
            Program        { exprs }      => self.eval_program(),
            IntegerLiteral { token, val } => Ok(Value::Integer(*val)),
            FloatLiteral   { token, val } => Ok(Value::Float(*val)),
            StringLiteral  { token, val } => Ok(Value::String(val.clone())),
            BoolLiteral    { token, val } => Ok(Value::Boolean(*val)),
            Identifier     { token, name} => {
                self.lookup(name)
                    .ok_or(InterpreterError::NameError(
                        format!("Couldn't find identifier {} in this scope", name)))
            }

            UnaryExpr  { op, expr }            => self.eval_unary(),
            BinaryExpr { op, left, right }     => self.eval_binary(),
            Grouping   { expr, left_delim: left, right_delim: right }   => self.eval_grouping(),
            Call       { callee, paren, args } => self.eval_call(),
        }
    }

    fn lookup(&self, var: &str) -> Option<Value> {
        return self.environment.get(var).cloned();
    }
    
    fn eval_unary(&self) -> Result<Value> {
        todo!()
    }
    
    fn eval_binary(&self) -> Result<Value> {
        todo!()
    }
    
    fn eval_grouping(&self) -> Result<Value> {
        todo!()
    }
    
    fn eval_call(&self) -> Result<Value> {
        todo!()
    }
    
    fn eval_program(&self) -> std::result::Result<Value, InterpreterError> {
        todo!()
    }
}