use crate::sema::{
    SemanticAnalizer,
    Sym,
    SymTable,
    SymtableId,
    EnumTy,
    Ty,
    Tykind,
    EnumVariant,
    StructField,
    Value,
    StructTy,
    FnTy,
    Param
};

use crate::parser;

pub fn sema_insert_symbols_all_ast(sema: &mut SemanticAnalizer) {
    let sym_table = SymTable::new(sema.sym_tables.len(), Vec::new());
    sema.sym_tables.push(sym_table);
    for (lib_name, _) in sema.libraries_hash {
        let sym = Sym::External(lib_name.clone(), None);
        sema.insert_sym(lib_name, sym, 0).unwrap();
    }
    for node in &sema.ast.inner {
        sema_insert_symbols(sema, "package".to_string(), 0, node);
    }
}

fn sema_insert_symbols(sema: &mut SemanticAnalizer, actual_path: String, sym_table_id: SymtableId, node: &parser::Node) {
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
                    
                    _ = sema.insert_type(name, ty, sym_table_id);
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

                    _ = sema.insert_type(name, ty, sym_table_id);
                }
                parser::Statement::FnDecl(fn_decl) => {
                    let name = &fn_decl.name;
                    let params: Vec<Param> = fn_decl.parameters.iter().map(|p| {
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
                        parameters: params,
                        return_type: Ty { kind: return_type },
                    };


                    let ty = Ty {
                        kind: Tykind::Fn(Box::new(fn_ty))
                    };
                    
                    let fn_value = Value {
                        ident: name.clone(),
                        mutable: false,
                        ty: ty,
                        span: fn_decl.span,
                        isolated: false
                    };

                    _ = sema.insert_value(name, fn_value, sym_table_id);

                    let symtable_id_fn = sema.insert_new_layer_sym_table(sym_table_id, &fn_decl.name, true).unwrap();
                    for node in &fn_decl.body {
                        sema_insert_symbols(sema, actual_path.clone() + "/#FN/" + &fn_decl.name, symtable_id_fn, node)
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

                    let value = Value { ident: ident.clone(), mutable, ty, span: var_decl.span, isolated: true };
                    
                    _ = sema.insert_value(ident, value, sym_table_id);
                }
                _ => { //TODO
                }
            }
        },
        parser::Node::Expr(_) => unreachable!(),
        _ => {  }
    }
}