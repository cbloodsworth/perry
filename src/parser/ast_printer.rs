use std::{collections::VecDeque, rc::Rc};

use super::ASTNode;

/// Pretty printer for the AST.
///
/// Performs a tree walk and does a nice print of the tree.
trait TreeNode {
    fn get_children(&self) -> Vec<&Box<Self>>;
}

fn bfs<T, F>(root: &T, mut f: F)
where
    T: TreeNode,
    F: FnMut(&T),
{
    let mut queue = VecDeque::new();
    queue.push_back(root);

    while let Some(node) = queue.pop_front() {
        f(node);

        for child in node.get_children() {
            queue.push_back(child);
        }
    }
}

fn postorder<T, F>(root: &T, mut f: F)
where
    T: TreeNode,
    F: FnMut(&T),
{
    for child in root.get_children() {
        postorder(child.as_ref(), &mut f);
    }

    f(root);
}

fn preorder<T, F>(root: &T, mut f: F)
where
    T: TreeNode,
    F: FnMut(&T),
{
    f(root);

    for child in root.get_children() {
        postorder(child.as_ref(), &mut f);
    }
}

impl TreeNode for PrintNode {
    fn get_children(&self) -> Vec<&Box<Self>> {
        self.c.iter().collect()
    }
}

pub fn pretty_print(root: &ASTNode) {
    let print_root: PrintNode = create_printnode_tree(root.clone());
    let mut lines: Vec<Vec<String>> = Vec::new();
}

#[derive(Debug)]
struct PrintNode {
    s: String,
    c: Vec<Box<PrintNode>>,
}

/// Traverses the AST and decorates a new "printnode" tree, with just the names of each node and its value.
fn create_printnode_tree(root: ASTNode) -> PrintNode {
    use ASTNode::*;
    let print_root = match root {
        Program { exprs } => PrintNode {
            s: "program".to_string(),
            c: exprs
                .iter()
                .map(|x| Box::new(create_printnode_tree(x.as_ref().clone())))
                .collect(),
        },
        IntegerLiteral { token, val } => PrintNode {
            s: token.lexeme.clone(),
            c: Vec::new(),
        },
        FloatLiteral { token, val } => PrintNode {
            s: token.lexeme.clone(),
            c: Vec::new(),
        },
        StringLiteral { token, val } => PrintNode {
            s: token.lexeme.clone(),
            c: Vec::new(),
        },
        BoolLiteral { token, val } => PrintNode {
            s: token.lexeme.clone(),
            c: Vec::new(),
        },
        Identifier { token, name } => PrintNode {
            s: token.lexeme.clone(),
            c: Vec::new(),
        },

        UnaryExpr { op, expr } => PrintNode {
            s: op.token.kind.to_string(),
            c: vec![Box::new(create_printnode_tree(expr.as_ref().clone()))],
        },
        BinaryExpr { op, left, right } => PrintNode {
            s: op.token.kind.to_string(),
            c: vec![
                Box::new(create_printnode_tree(left.as_ref().clone())),
                Box::new(create_printnode_tree(right.as_ref().clone())),
            ],
        },
        Grouping {
            expr,
            left_delim,
            right_delim,
        } => create_printnode_tree(expr.as_ref().clone()),

        Call {
            callee,
            paren,
            args,
        } => PrintNode {
            s: format!("{}()", callee.as_ref().clone()),
            c: args
                .iter()
                .map(|x| Box::new(create_printnode_tree(x.as_ref().clone())))
                .collect(),
        },
    };

    print_root
}
