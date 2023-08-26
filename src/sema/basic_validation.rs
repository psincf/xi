use super::SemanticAnalizer;
use super::parser;

pub fn sema_validate_no_expr_in_top_file(sema: &mut SemanticAnalizer) {
    for node in &sema.ast.inner {
        match node {
            parser::Node::None => {},
            parser::Node::Expr(_) => { sema.err.push(format!("No expression allowed on file level")) },
            parser::Node::Statement(stmt) => {
                match stmt {
                    parser::Statement::Expr(_) => { sema.err.push(format!("No expression allowed on file level")) }
                    _ => {  }
                }
            },
        }
    }
}