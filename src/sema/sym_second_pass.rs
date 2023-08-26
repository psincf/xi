use crate::sema::{
    SemanticAnalizer,
    SymtableId,
    Ty,
    Tykind,
    Primitive,
    Node,
    Statement,
    Sym,
};

use crate::parser;

pub fn sema_check_symbols_all_ast(sema: &mut SemanticAnalizer) {
    let mut ir_nodes = Vec::new();
    for node in &sema.ast.inner {
        ir_nodes.push(
            sema_check_symbols(sema, "package".to_string(), 0, node)
        );
    }

    sema.ir.inner = ir_nodes;
}

fn sema_check_symbols(sema: &mut SemanticAnalizer, actual_path: String, sym_table_id: SymtableId, node: &parser::Node) -> Node {
    match node {
        parser::Node::Statement(stmt) => {
            match stmt {
                parser::Statement::EnumDecl(e) => {
                    let enum_ty_id = sema.get_type_id(&e.name, sym_table_id);
                    return Node::Statement(
                        Statement::EnumDecl(enum_ty_id)
                    )
                },
                parser::Statement::StructDecl(struct_decl) => {
                    let struct_ty_id = sema.get_type_id(&struct_decl.name, sym_table_id);

                    let ty = unsafe {&mut (sema as *mut SemanticAnalizer).as_mut().unwrap().types[struct_ty_id] };
                    match &mut ty.kind {
                        Tykind::Struct(s_ty) => {
                            for field in s_ty.fields.iter_mut() {
                                match field.ty.kind.clone() {
                                    Tykind::Inferred => panic!("Inferrence not implemented"),
                                    Tykind::Name(s) => {
                                        if get_primitive(&s).is_some() {
                                            field.ty.kind = Tykind::Primitive(get_primitive(&s).unwrap())
                                        } else {
                                            let id_field = sema.get_sym_in_scope(&s, sym_table_id);
                                            match id_field {
                                                Ok(id) => {
                                                    match &sema.symbols[id] {
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
                    let fndecl_ty_id = sema.get_type_id(&fn_decl.name, sym_table_id);

                    let ty = unsafe {&mut (sema as *mut SemanticAnalizer).as_mut().unwrap().types[fndecl_ty_id] };
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
                                            let id_param = sema.get_sym_in_scope(n, sym_table_id);
                                            match id_param {
                                                Ok(id) => {
                                                    match &sema.symbols[id] {
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

fn get_primitive(s: &String) -> Option<Primitive> {
    match s.as_str() {
        "i32" => Some(Primitive::I32),
        "f32" => Some(Primitive::F32),
        "String" => Some(Primitive::String),
        _ => return None
    }
}