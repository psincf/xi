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

type TyId = usize;
type SymId = usize;
type SymtableId = usize;

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
        self.sema_validate_no_expr_in_top_file();
        self.sema_insert_symbols_all_ast();
        self.sema_check_symbols_all_ast();
    }

    fn sema_validate_no_expr_in_top_file(&mut self) {
        for node in &self.ast.inner {
            match node {
                parser::Node::None => {},
                parser::Node::Expr(_) => { self.err.push(format!("No expression allowed on file level")) },
                parser::Node::Statement(stmt) => {
                    match stmt {
                        parser::Statement::Expr(_) => { self.err.push(format!("No expression allowed on file level")) }
                        _ => {  }
                    }
                },
            }
        }
    }

    fn sema_insert_symbols_all_ast(&mut self) {
        let sym_table = SymTable::new(self.sym_tables.len(), Vec::new());
        self.sym_tables.push(sym_table);
        for node in &self.ast.inner {
            self.sema_insert_symbols("package".to_string(), 0, node);
        }
    }

    fn sema_insert_symbols(&mut self, actual_path: String, sym_table_id: SymtableId, node: &parser::Node) {
        match node {
            parser::Node::Statement(stmt) => {
                match stmt {
                    parser::Statement::EnumDecl(enum_decl) => {
                        let name = &enum_decl.name;
                        let variants: Vec<EnumVariant> = enum_decl.variants.iter().map(|s| {
                            EnumVariant {
                                name: s.clone(),
                                ty: None
                            }
                        }).collect();

                        let ty = Ty {
                            kind: Tykind::Enum(Box::new(EnumTy{ path: actual_path.clone(), name: name.clone(), variants: variants }))
                        };
                        
                        _ = self.insert_type(name, ty, sym_table_id);
                    }
                    parser::Statement::StructDecl(struct_decl) => {
                        let name = &struct_decl.name;
                        let fields: Vec<StructField> = struct_decl.fields.iter().map(|f| {
                            let ty_kind = match &f.type_ {
                                parser::Type::Ident(s) => { Tykind::Name(s.clone()) },
                                parser::Type::None | parser::Type::Inferred => { panic!() }
                            };

                            let ty = Ty {
                                kind: ty_kind,
                            };

                            StructField {
                                name: f.name.clone(),
                                ty: ty
                            }
                        }).collect();
                        
                        let ty = Ty {
                            kind: Tykind::Struct(Box::new(StructTy { path: actual_path.clone(), name: name.clone(), fields: fields }))
                        };

                        _ = self.insert_type(name, ty, sym_table_id);
                    }
                    parser::Statement::FnDecl(fn_decl) => {
                        let name = &fn_decl.name;
                        let params: Vec<Param> = fn_decl.paramaters.iter().map(|p| {
                            let ty: String = match &p.type_{
                                parser::Type::Ident(id) => id.clone(),
                                _ => unreachable!()
                            };
                            Param {
                                name: p.name.clone(),
                                ty: Ty {
                                    kind: Tykind::Name(ty)
                                }
                            }
                        }).collect();

                        let return_type = match &fn_decl.return_type {
                            parser::Type::Ident(id) => Tykind::Name(id.clone()),
                            parser::Type::None => Tykind::None,
                            _ => unreachable!()
                        };

                        let body = &fn_decl.body;

                        let fn_ty = FnTy {
                            path: actual_path.clone(),
                            name: name.clone(),
                            paramaters: params,
                            return_type: Ty { kind: return_type },
                            body: Vec::new(),
                            body_ast: unsafe { std::mem::transmute(body) }
                        };

                        let ty = Ty {
                            kind: Tykind::Fn(Box::new(fn_ty))
                        };

                        _ = self.insert_type(name, ty, sym_table_id);

                        let symtable_id_fn = self.insert_new_layer_sym_table(sym_table_id, &fn_decl.name, false).unwrap();
                        for node in &fn_decl.body {
                            self.sema_insert_symbols(actual_path.clone() + "/#FN/" + &fn_decl.name, symtable_id_fn, node)
                        }
                    }
                    parser::Statement::VarDecl(var_decl) => {
                        let ident = &var_decl.ident;
                        let mutable = var_decl.mutable;
                        let ty = match &var_decl.ty {
                            parser::Type::Ident(i) => { Ty { kind: Tykind::Name(i.clone()) } }
                            parser::Type::Inferred => { Ty { kind: Tykind::Inferred } }
                            _ => panic!("{:?}", var_decl)
                        };

                        let value = Value { ident: ident.clone(), mutable, ty };
                        
                        _ = self.insert_value(ident, value, sym_table_id);
                    }
                    _ => { //TODO
                    }
                }
            },
            parser::Node::Expr(_) => unreachable!(),
            _ => {  }
        }
    }

    fn sema_check_symbols_all_ast(&mut self) {
        let mut ir_nodes = Vec::new();
        for node in &self.ast.inner {
            ir_nodes.push(
                self.sema_check_symbols("package".to_string(), 0, node)
            );
        }

        self.ir.inner = ir_nodes;
    }

    fn sema_check_symbols(&mut self, actual_path: String, sym_table_id: SymtableId, node: &parser::Node) -> Node {
        match node {
            parser::Node::Statement(stmt) => {
                match stmt {
                    parser::Statement::EnumDecl(e) => {
                        let enum_ty_id = self.get_type_id(&e.name, sym_table_id);
                        return Node::Statement(
                            Statement::EnumDecl(enum_ty_id)
                        )
                    },
                    parser::Statement::StructDecl(struct_decl) => {
                        let struct_ty_id = self.get_type_id(&struct_decl.name, sym_table_id);

                        let ty = unsafe {&mut (self as *mut Self).as_mut().unwrap().types[struct_ty_id] };
                        match &mut ty.kind {
                            Tykind::Struct(s_ty) => {
                                for field in s_ty.fields.iter_mut() {
                                    match field.ty.kind.clone() {
                                        Tykind::Inferred => panic!("Inferrence not implemented"),
                                        Tykind::Name(s) => {
                                            if get_primitive(&s).is_some() {
                                                field.ty.kind = Tykind::Primitive(get_primitive(&s).unwrap())
                                            } else {
                                                let id_field = self.get_sym_in_scope(&s, sym_table_id);
                                                match id_field {
                                                    Ok(id) => {
                                                        match &self.symbols[id] {
                                                            Sym::Ty(ty_id) => {
                                                                field.ty = Ty { kind: Tykind::Id(*ty_id) }
                                                            }
                                                            _ => panic!("{} is not a Type", s)
                                                        }
                                                    },
                                                    Err(()) => panic!("Type {} not found", s)
                                                }
                                            }
                                        }
                                        _ => unreachable!()
                                    }
                                }
                            }
                            _ => panic!()
                        }

                        return Node::Statement(
                            Statement::StructDecl(struct_ty_id)
                        )
                    },
                    parser::Statement::FnDecl(fn_decl) => {
                        let fndecl_ty_id = self.get_type_id(&fn_decl.name, sym_table_id);

                        let ty = unsafe {&mut (self as *mut Self).as_mut().unwrap().types[fndecl_ty_id] };
                        match &mut ty.kind {
                            Tykind::Fn(fn_ty) => {
                                for params in &mut fn_ty.paramaters {
                                    let mut new_ty_kind = params.ty.kind.clone();

                                    match &params.ty.kind {
                                        Tykind::Inferred => { unimplemented!() },
                                        Tykind::Name(n) => {
                                            if get_primitive(n).is_some() {
                                                new_ty_kind = Tykind::Primitive(get_primitive(n).unwrap())
                                            } else {
                                                let id_param = self.get_sym_in_scope(n, sym_table_id);
                                                match id_param {
                                                    Ok(id) => {
                                                        match &self.symbols[id] {
                                                            Sym::Ty(ty_id) => {
                                                                new_ty_kind = Tykind::Id(*ty_id)
                                                            }
                                                            _ => panic!("{} is not a Type", n)
                                                        }
                                                    },
                                                    Err(()) => panic!("Type {} not found", n)
                                                }
                                            }
                                        }
                                        _ => { unreachable!() }
                                    }

                                    params.ty.kind = new_ty_kind;
                                }
                            }
                            _ => unreachable!()
                        }



                        return Node::Statement(Statement::FnDecl(fndecl_ty_id))

                    }
                    _ => { Node::None }
                }
            }
            _ => { Node::None }
        }
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

fn get_primitive(s: &String) -> Option<Primitive> {
    match s.as_str() {
        "i32" => Some(Primitive::I32),
        "f32" => Some(Primitive::F32),
        "String" => Some(Primitive::String),
        _ => return None
    }
}