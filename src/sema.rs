use std::collections::HashMap;

use super::parser;
use super::parser::Ast;
use super::parser::Node;
use super::parser::Statement;

#[derive(Clone, Debug)]
pub struct Ty {
    path: String,
    kind: Tykind
}

#[derive(Clone, Debug)]
pub enum Tykind {
    Struct(String, Vec<StructField>),
    Enum(String, Vec<EnumVariant>),
    Name(String)
}

#[derive(Clone, Debug)]
pub struct StructField {
    name: String,
    ty: Ty,
}

#[derive(Clone, Debug)]
pub struct EnumVariant {
    name: String,
    ty: Option<Ty>,
}

type TyId = usize;
type SymtableId = usize;

#[derive(Debug, Default)]
pub struct SymTable {
    id: SymtableId,
    prec: Vec<SymtableId>,
    symbols: HashMap<String, TyId>,
    layers: HashMap<String, SymtableId>,
}

impl SymTable {
    pub fn new(id: SymtableId, prec: Vec<SymtableId>) -> Self {
        Self {
            id,
            prec,
            symbols: HashMap::new(),
            layers: HashMap::new()
        }
    }

    pub fn insert_symbol(&mut self, sym: String, ty: TyId) -> Result<(), String> {
        if self.symbols.get(&sym).is_some() {
            return Err("Already exist".to_string());
        }

        self.symbols.insert(sym, ty);
        Ok(())
    }

    pub fn in_scope(&self, sym: String, sym_tables: &Vec<SymTable>) -> Result<TyId, String> {
        let mut to_return = Err(format!("no type: {}", sym));
        for &id in &self.prec {
            let sym_table_prec = &sym_tables[id];
            if sym_table_prec.symbols.get(&sym).is_some() {
                if to_return.is_err() {
                    to_return = Ok(sym_table_prec.symbols.get(&sym).unwrap().clone());
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

    pub fn new_layer(&mut self, layer_name: String, isolated: bool, sym_tables: &mut Vec<SymTable>) -> Result<(), String> {
        if self.layers.get(&layer_name).is_some() {
            panic!()
        } else {
            let mut prec_sym = Vec::new();
            if !isolated {
                prec_sym.clone_from_slice(&self.prec)
            }
            let id = sym_tables.len();

            let new_sym_table = SymTable {
                id,
                prec: prec_sym,
                symbols: HashMap::new(),
                layers: HashMap::new()
            };

            sym_tables.push(new_sym_table);

            self.layers.insert(layer_name, id);
        }

        panic!()
    }
}

pub struct SemanticAnalizer<'a> {
    ast: &'a Ast,
    pub types: Vec<Ty>,
    pub sym_tables: Vec<SymTable>,
    pub err: Vec<String>
}

impl<'a> SemanticAnalizer<'a> {
    pub fn new(ast: &'a Ast) -> Self {
        Self {
            ast,
            types: Vec::new(),
            sym_tables: Vec::new(),
            err: Vec::new()
        }
    }

    pub fn sema(&mut self) {
        self.sema_validate_no_expr_in_top_file();
        self.sema_insert_types_all_ast();
    }

    fn sema_validate_no_expr_in_top_file(&mut self) {
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

    fn sema_insert_types_all_ast(&mut self) {
        let sym_table = SymTable::new(0, Vec::new());
        self.sym_tables.push(sym_table);
        for node in &self.ast.inner {
            self.sema_insert_types("package".to_string(), 0, node);
        }
    }

    fn sema_insert_types(&mut self, actual_path: String, sym_table_id: SymtableId, node: &Node) {
        match node {
            Node::Statement(stmt) => {
                match stmt {
                    Statement::EnumDecl(enum_decl) => {
                        let name = &enum_decl.name;
                        let variants: Vec<EnumVariant> = enum_decl.variants.iter().map(|s| {
                            EnumVariant {
                                name: s.clone(),
                                ty: None
                            }
                        }).collect();

                        let ty = Ty {
                            path: actual_path.clone(),
                            kind: Tykind::Enum(name.clone(), variants)
                        };
                        
                        _ = self.insert_type(name, ty, sym_table_id);
                    }
                    Statement::StructDecl(struct_decl) => {
                        let name = &struct_decl.name;
                        let fields: Vec<StructField> = struct_decl.fields.iter().map(|f| {
                            let ty_kind = match &f.type_ {
                                parser::Type::Ident(s) => { Tykind::Name(s.clone()) },
                                parser::Type::None | parser::Type::Inferred => { panic!() }
                            };

                            let ty = Ty {
                                path: actual_path.clone(),
                                kind: ty_kind,
                            };

                            StructField {
                                name: f.name.clone(),
                                ty: ty
                            }
                        }).collect();
                        
                        let ty = Ty {
                            path: actual_path.clone(),
                            kind: Tykind::Struct(name.clone(), fields)
                        };

                        _ = self.insert_type(name, ty, sym_table_id);
                    }
                    _ => { //TODO
                    }
                }
            },
            Node::Expr(_) => unreachable!(),
            _ => {  }
        }
    }

    fn insert_type(&mut self, name: &String, ty: Ty, sym_table_id: SymtableId) -> Result<usize, ()> {
        let sym_table = &mut self.sym_tables[sym_table_id];
        if sym_table.symbols.get(name).is_some() {
            let existing_ty_id = sym_table.symbols.get(name).unwrap();
            let existing_ty = &self.types[*existing_ty_id];

            self.err.push(format!("{} already exist in {}", name, existing_ty.path));

            return Err(())
        } else {
            let ty_id = self.types.len();
            self.types.push(ty);
            let result = sym_table.insert_symbol(name.clone(), ty_id);
            if let Err(err) = result {
                self.err.push(err);

                return Err(())
            }

            return Ok(ty_id)
        }
    }
}