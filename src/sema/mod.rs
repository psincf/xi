mod basic_validation;
mod sym_first_pass;
mod sym_second_pass;

use std::collections::HashMap;

use super::parser;

#[derive(Clone, Debug)]
pub struct Ir {
    pub inner: Vec<Node>,
}

#[derive(Clone, Debug)]
pub enum Node {
    Expr(Expr),
    Statement(Statement),
    None
}

#[derive(Clone, Debug)]
pub enum Statement {
    StructDecl(TyId),
    EnumDecl(TyId),
    FnDecl(TyId),
    VarDecl(SymId, Expr),
    //ModDecl(ModeDecl),
    Expr(Expr)
}

#[derive(Clone, Debug)]
pub enum Sym {
    Ty(TyId),
    Value(Value)
}

#[derive(Clone, Debug)]
pub struct Value {
    ident: String,
    mutable: bool,
    ty: Ty,
}

#[derive(Clone, Debug)]
pub enum Operator {
    Equal,
    Add,
    Minus,
    Multiply,
    Divide,
    Dot,
}

#[derive(Clone, Debug)]
pub struct Operation {
    lhs: Box<Expr>,
    rhs: Box<Expr>,
    operator: Operator
}

#[derive(Clone, Debug)]
pub enum Lit {
    Int(i32),
    Float(f32),
    String(String)
}

#[derive(Clone, Debug)]
pub struct FnCall {
    ident: Box<Expr>,
    arguments: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Sym(SymId),
    Literal(Lit),
    FnCall(FnCall),
    Operation(Operation),
    Paren(Box<Expr>),
}

#[derive(Clone, Debug)]
pub struct Ty {
    kind: Tykind
}

impl Ty {
    pub fn get_path(&self) -> &str {
        match &self.kind {
            Tykind::Struct(s) => return &s.path,
            Tykind::Enum(e) => return &e.path,
            Tykind::Fn(f) => return &f.path,
            _ => return ""
        }
    }
}

#[derive(Clone, Debug)]
pub enum Tykind {
    Struct(Box<StructTy>),
    Enum(Box<EnumTy>),
    Fn(Box<FnTy>),
    Primitive(Primitive),
    Id(TyId),
    Name(String),
    Inferred,
    None
}

#[derive(Clone, Debug)]
pub enum Primitive {
    I32,
    F32,
    String
}

#[derive(Clone, Debug)]
pub struct Param {
    pub name: String,
    pub ty: Ty,
}

#[derive(Clone, Debug)]
pub struct StructTy {
    pub path: String,
    pub name: String,
    pub fields: Vec<StructField>
}

#[derive(Clone, Debug)]
pub struct EnumTy {
    pub path: String,
    pub name: String,
    pub variants: Vec<EnumVariant>,
}

#[derive(Clone, Debug)]
pub struct FnTy {
    pub path: String,
    name: String,
    paramaters: Vec<Param>,
    return_type: Ty,
    body: Vec<Node>,
    body_ast: &'static Vec<parser::Node>
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

pub type TyId = usize;
pub type SymId = usize;
pub type SymtableId = usize;

#[derive(Debug, Default)]
pub struct SymTable {
    id: SymtableId,
    prec: Vec<SymtableId>,
    symbols: HashMap<String, SymId>,
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

    pub fn get_symbol(&self, sym: &String) -> Result<SymId, String> {
        let sym_id_option = self.symbols.get(sym);
        if sym_id_option.is_some() {
            return Ok(*sym_id_option.unwrap())
        } else {
            return Err("No symbol".to_string())
        }
    }
}

pub struct SemanticAnalizer<'a> {
    ast: &'a parser::Ast,
    pub ir: Ir,
    pub types: Vec<Ty>,
    pub symbols: Vec<Sym>,
    pub sym_tables: Vec<SymTable>,
    pub err: Vec<String>
}

impl<'a> SemanticAnalizer<'a> {
    pub fn new(ast: &'a parser::Ast) -> Self {
        Self {
            ast,
            ir: Ir { inner: Vec::new() },
            types: Vec::new(),
            symbols: Vec::new(),
            sym_tables: Vec::new(),
            err: Vec::new()
        }
    }

    pub fn sema(&mut self) {
        basic_validation::sema_validate_no_expr_in_top_file(self);
        sym_first_pass::sema_insert_symbols_all_ast(self);
        sym_second_pass::sema_check_symbols_all_ast(self);
    }

    fn insert_new_layer_sym_table(&mut self, origin: SymtableId, layer_name: &String, isolated: bool) -> Result<SymtableId, String> {
        let id = self.sym_tables.len();
        let sym_table = &mut self.sym_tables[origin];

        if sym_table.layers.get(layer_name).is_some() {
            panic!()
        } else {
            let mut prec_sym = Vec::new();
            if !isolated {
                prec_sym.clone_from_slice(&sym_table.prec)
            }

            let new_sym_table = SymTable {
                id,
                prec: prec_sym,
                symbols: HashMap::new(),
                layers: HashMap::new()
            };
            sym_table.layers.insert(layer_name.clone(), id);
            drop(sym_table);

            self.sym_tables.push(new_sym_table);

            return Ok(id)
        }
    }

    fn insert_sym(&mut self, name: &String, sym: Sym, sym_table_id: SymtableId) -> Result<usize, ()> {
        let sym_table = &mut self.sym_tables[sym_table_id];
        if sym_table.symbols.get(name).is_some() {
            let existing_ty_id = sym_table.symbols.get(name).unwrap();
            let existing_ty = &self.types[*existing_ty_id];

            self.err.push(format!("{} already exist in {}", name, existing_ty.get_path()));

            return Err(())
        } else {
            let sym_id = self.symbols.len();
            self.symbols.push(sym);

            let result = sym_table.insert_symbol(name.clone(), sym_id);
            if let Err(err) = result {
                self.err.push(err);

                return Err(())
            }

            return Ok(sym_id)
        }

    }

    fn insert_value(&mut self, name: &String, value: Value, sym_table_id: SymtableId) -> Result<usize, ()> {
        let sym = Sym::Value(value);

        self.insert_sym(name, sym, sym_table_id)
    }

    fn insert_type(&mut self, name: &String, ty: Ty, sym_table_id: SymtableId) -> Result<usize, ()> {
        let ty_id = self.types.len();
        self.types.push(ty);

        let sym = Sym::Ty(ty_id);

        self.insert_sym(name, sym, sym_table_id)
    }

    fn get_sym_in_scope(&self, sym: &String, sym_table_id: usize) -> Result<SymId, ()> {
        let mut to_return = Err(());
        let sym_table = &self.sym_tables[sym_table_id];
        for &id in &sym_table.prec {
            let sym_table_prec = &self.sym_tables[id];
            if sym_table_prec.symbols.get(sym).is_some() {
                if to_return.is_err() {
                    to_return = Ok(sym_table_prec.symbols.get(sym).unwrap().clone());
                } else {
                    to_return = Err(())
                }
            }
        }

        if sym_table.get_symbol(sym).is_ok() {
            if to_return.is_err() {
                to_return = Ok(sym_table.get_symbol(sym).unwrap().clone());
            } else {
                to_return = Err(())
            }
        }

        return to_return
    }

    fn get_type_id(&self, name: &String, sym_table_id: SymtableId) -> TyId {
        let sym_table = &self.sym_tables[sym_table_id];
        let sym_id = sym_table.get_symbol(name).unwrap();
        let sym = &self.symbols[sym_id];

        match sym {
            Sym::Ty(ty_id) => {
                   return *ty_id
                }
            _ => { panic!() }
        }
    }
}