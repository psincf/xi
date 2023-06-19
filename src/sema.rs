use std::collections::HashMap;

use super::parser::Ast;
use super::parser::Node;
use super::parser::Statement;

#[derive(Clone)]
pub struct Type {
    name: String,
}


#[derive(Default)]
pub struct SymTable {
    prec: Vec<HashMap<String, Type>>,
    symbols: HashMap<String, Type>,
    layers: HashMap<String, Box<SymTable>>,
}

impl SymTable {
    pub fn insert_symbol(&mut self, sym: String, ty: Type) -> Result<(), String> {
        for p in &self.prec {
            if p.get(&sym).is_some() {
                return Err("Already exist".to_string());
            }
        }
        if self.symbols.get(&sym).is_some() {
            return Err("Already exist".to_string());
        }

        self.symbols.insert(sym, ty);
        Ok(())
    }

    pub fn in_scope(&self, sym: String) -> Result<Type, String> {
        let mut to_return = Err(format!("no type: {}", sym));
        for p in &self.prec {
            if p.get(&sym).is_some() {
                if to_return.is_err() {
                    to_return = Ok(p.get(&sym).unwrap().clone());
                } else {
                    to_return = Err(format!("More than one: {}", sym))
                }
            }
        }

        if self.symbols.get(&sym).is_some() {
            if to_return.is_err() {
                to_return = Ok(self.symbols.get(&sym).unwrap().clone());
            } else {
                to_return = Err(format!("More than one: {}", sym))
            }
        }

        return to_return
    }
}

pub struct SemanticAnalizer<'a> {
    ast: &'a Ast,
    pub sym_table: SymTable,
    pub err: Vec<String>
}

impl<'a> SemanticAnalizer<'a> {
    pub fn new(ast: &'a Ast) -> Self {
        Self {
            ast,
            sym_table: SymTable::default(),
            err: Vec::new()
        }
    }

    pub fn sema(&mut self) {
        self.validate_no_expr_in_top_file();
        self.validate_type();
    }

    fn validate_no_expr_in_top_file(&mut self) {
        for node in &self.ast.inner {
            match node {
                Node::None => {},
                Node::Expr(_) => { self.err.push(format!("No expression allowed on file level")) },
                Node::Statement(stmt) => {
                    match stmt {
                        Statement::Expr(_) => { self.err.push(format!("No expression allowed on file level")) }
                        _ => {  }
                    }
                },
            }
        }
    }

    fn validate_type(&mut self) {
        //TODO
    }
}