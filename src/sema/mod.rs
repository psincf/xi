mod basic_validation;
mod sym_first_pass;
mod sym_second_pass;

use std::collections::HashMap;

use super::pipeline::Library;
use super::parser;
use super::lexer::Span;

#[derive(Clone, Debug, Default)]
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
    FnDecl(FnDecl),
    VarDecl(VarDecl),
    //ModDecl(ModeDecl),
    Expr(Expr)
}

#[derive(Clone, Debug)]
pub struct VarDecl {
    id: SymId,
    expr: Expr
}

#[derive(Clone, Debug)]
pub struct FnDecl {
    id: SymId,
    body: Vec<Node>
}

#[derive(Clone, Debug)]
pub enum Sym {
    Ty(TyId),
    Value(Value),
    Namespace(String),
    External(String, Option<SymId>)
}

#[derive(Clone, Debug)]
pub struct Value {
    ident: String,
    mutable: bool,
    ty: Ty,
    span: Span,
    isolated: bool
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

impl Operator {
    pub fn from_op_ast(op: &parser::Operator) -> Self {
        match op {
            parser::Operator::Add => { return Self::Add },
            parser::Operator::Dot => { return Self::Dot },
            parser::Operator::Minus => { return Self::Minus },
            parser::Operator::Multiply => { return Self::Multiply },
            parser::Operator::Divide => { return Self::Divide },
            parser::Operator::Equal => { return Self::Equal }
        }
    }
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
    Unresolved(*const parser::Expr),
    None
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
    parameters: Vec<Param>,
    return_type: Ty,
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
    isolated: bool,
    id: SymtableId,
    prec: Vec<SymtableId>,
    symbols: HashMap<String, SymId>,
    layers: HashMap<String, SymtableId>,
}

impl SymTable {
    pub fn new(id: SymtableId, prec: Vec<SymtableId>) -> Self {
        Self {
            isolated: false,
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

#[derive(Default)]
pub struct SemaIr {
    pub ir: Ir,
    pub types: Vec<Ty>,
    pub symbols: Vec<Sym>,
    pub sym_tables: Vec<SymTable>,
}

pub struct SemanticAnalizer<'a> {
    ast: &'a parser::Ast,
    pub ir: Ir,
    pub types: Vec<Ty>,
    pub symbols: Vec<Sym>,
    pub sym_tables: Vec<SymTable>,
    pub libraries: &'a Vec<Library>,
    pub libraries_hash: &'a HashMap<String, usize>,
    pub err: Vec<String>
}

impl<'a> SemanticAnalizer<'a> {
    pub fn new(ast: &'a parser::Ast, libraries: &'a Vec<Library>, libraries_hash: &'a HashMap<String, usize>) -> Self {
        Self {
            ast,
            ir: Ir { inner: Vec::new() },
            types: Vec::new(),
            symbols: Vec::new(),
            sym_tables: Vec::new(),
            libraries,
            libraries_hash,
            err: Vec::new()
        }
    }

    pub fn sema(&mut self) -> SemaIr {
        basic_validation::sema_validate_no_expr_in_top_file(self);
        sym_first_pass::sema_insert_symbols_all_ast(self);
        sym_second_pass::sema_check_symbols_all_ast(self);

        let mut sema_ir = SemaIr::default();
        std::mem::swap(&mut sema_ir.ir, &mut self.ir);
        std::mem::swap(&mut sema_ir.types, &mut self.types);
        std::mem::swap(&mut sema_ir.symbols, &mut self.symbols);
        std::mem::swap(&mut sema_ir.sym_tables, &mut self.sym_tables);

        return sema_ir;
    }

    fn insert_new_layer_sym_table(&mut self, origin: SymtableId, layer_name: &String, isolated: bool) -> Result<SymtableId, String> {
        let id = self.sym_tables.len();
        let sym_table = &mut self.sym_tables[origin];

        if sym_table.layers.get(layer_name).is_some() {
            panic!()
        } else {
            let mut prec_sym = Vec::new();
            prec_sym.push(origin);

            let new_sym_table = SymTable {
                isolated,
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

    fn get_sym_ext_resolved(&self, sym: &'a Sym) -> &'a Sym {
        match sym {
            Sym::External(lib, sym_id) => {
                let lib_num = self.libraries_hash.get(lib).unwrap();
                let lib = &self.libraries[*lib_num];
                if sym_id.is_none() { return sym }

                let resolved_sym = &lib.sema_ir.as_ref().unwrap().symbols[sym_id.unwrap()];
                return self.get_sym_ext_resolved(resolved_sym);
            }
            _ => {
                return sym
            }
        }
        //TODO
    }

    fn get_sym_in_scope(&self, sym: &String, sym_table_id: usize) -> Result<SymId, String> {
        fn get_sym_inner(sema: &SemanticAnalizer, sym: &String, sym_table_id: usize, isolated_context: bool) -> Result<SymId, String> {
            let mut to_return = Err("".to_string());
            let sym_table = &sema.sym_tables[sym_table_id];

            for &id in &sym_table.prec {
                let new_isolated_context = sym_table.isolated || isolated_context;
                to_return = get_sym_inner(sema, sym, id, new_isolated_context);
            }
            if sym_table.get_symbol(sym).is_ok() {
                if to_return.is_err() {
                    if isolated_context {
                        let sym = sym_table.get_symbol(sym).unwrap();
                        match sema.get_sym_ext_resolved(&sema.symbols[sym]) {
                            Sym::Ty(_) => { to_return = Ok(sym.clone()); },
                            Sym::Namespace(_) => { to_return = Ok(sym.clone()); },
                            Sym::Value(value) => {
                                if !value.isolated {
                                    to_return = Ok(sym.clone());
                                }
                            }
                            Sym::External(lib, id) => {
                                assert!(id.is_none());
                                to_return = Ok(sym.clone());
                            }
                        }
                    } else {
                        to_return = Ok(sym_table.get_symbol(sym).unwrap().clone());
                    }
                } else {
                    to_return = Err("duplicate".to_string())
                }
            }

            return to_return
        }

        return get_sym_inner(self, sym, sym_table_id, false);

    }

    fn get_value_id(&self, name: &String, sym_table_id: SymtableId) -> SymId {
        let sym_table = &self.sym_tables[sym_table_id];
        let sym_id = sym_table.get_symbol(name).unwrap();
        let sym = &self.symbols[sym_id];

        match sym {
            Sym::Value(v) => {
                   return (sym_id)
                }
            _ => { panic!("{}", name) }
        }
    }

    fn get_value_mut(&mut self, sym_id: usize, sym_table_id: SymtableId) -> &mut Value {
        let sym_table = &self.sym_tables[sym_table_id];
        let sym = &mut self.symbols[sym_id];

        match sym {
            Sym::Value(v) => {
                   return v
                }
            _ => { panic!("{}", sym_id) }
        }
    }

    fn get_type_id(&self, name: &String, sym_table_id: SymtableId) -> TyId {
        let sym_table = &self.sym_tables[sym_table_id];
        let sym_id = sym_table.get_symbol(name).unwrap();
        let sym = &self.symbols[sym_id];

        match sym {
            Sym::Ty(ty_id) => {
                   return *ty_id
                }
            _ => { panic!("{}", name) }
        }
    }
}