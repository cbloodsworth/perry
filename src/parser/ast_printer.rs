use std::rc::Rc;

use super::ASTNode;

/// Pretty printer for the AST.
/// 
/// Performs a tree walk and does a nice print of the tree.
fn pretty_print(root: ASTNode) {

}

struct Node {
    s: String,
    c: Vec<Rc<Node>>,
}

// fn create_printnode_tree(root: ASTNode) -> Node {
//     use ASTNode::*;
//     let print_root = match root {
//         Program { exprs } => {
//             Node {
//                 s: root.to_string()
//             }
//             exprs
//                 .iter()
//                 .map()
//         }
//         IntegerLiteral { token, val } => Ok(Value::Integer(*val)),
//         FloatLiteral   { token, val } => Ok(Value::Float(*val)),
//         StringLiteral  { token, val } => Ok(Value::String(val.clone())),
//         BoolLiteral    { token, val } => Ok(Value::Boolean(*val)),
//         Identifier     { token, name} => {
//             self.lookup(name)
//                 .ok_or(InterpreterError::NameError(
//                     format!("Couldn't find identifier {} in this scope", name)))
//         }

//         UnaryExpr  { op, expr }            => self.eval_unary(),
//         BinaryExpr { op, left, right }     => self.eval_binary(),
//         Grouping   { expr, left_delim: left, right_delim: right }   => self.eval_grouping(),
//         Call       { callee, paren, args } => self.eval_call(),
//     };

//     print_root
// }