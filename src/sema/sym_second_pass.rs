use crate::sema::{
    FnDecl,
    SemanticAnalizer,
    SymtableId,
    Ty,
    Tykind,
    Primitive,
    Node,
    Operation,
    Operator,
    Statement,
    Sym,
    VarDecl
};

use crate::parser;

use super::Expr;

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
                                                Err(err) => panic!("Type {} not found", s)
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
                    let fndecl_value_id = sema.get_value_id(&fn_decl.name, sym_table_id);

                    let fndecl_value = unsafe { &mut *(sema as *mut SemanticAnalizer) }.get_value_mut(fndecl_value_id, sym_table_id);

                    let ty = &mut fndecl_value.ty;
                    match &mut ty.kind {
                        Tykind::Fn(fn_ty) => {
                            for params in &mut fn_ty.parameters {
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
                                                Err(err) => panic!("Type {} not found", n)
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

                    let mut body = Vec::new();

                    for body_node in &fn_decl.body {
                        let sym_table = &sema.sym_tables[sym_table_id];
                        let new_sim_table_id = *sym_table.layers.get(&fn_decl.name).unwrap();
                        let new_node = sema_check_symbols(sema, actual_path.clone(), new_sim_table_id, body_node);
                        body.push(new_node)
                    }

                    let fn_decl_final = FnDecl {
                        id: fndecl_value_id,
                        body: body
                    };
                    return Node::Statement(Statement::FnDecl(fn_decl_final))

                },

                parser::Statement::VarDecl(var_decl) => {
                    let vardecl_value_id = sema.get_value_id(&var_decl.ident, sym_table_id);
                    let vardecl_value = unsafe { &mut *(sema as *mut SemanticAnalizer) }.get_value_mut(vardecl_value_id, sym_table_id);

                    match &var_decl.ty {
                        parser::Type::Ident(ident) => { 
                            if get_primitive(&ident).is_some() {
                                vardecl_value.ty.kind = Tykind::Primitive(get_primitive(&ident).unwrap());
                            } else {
                                let id_ty = sema.get_sym_in_scope(&ident, sym_table_id).unwrap();
                                match sema.symbols[id_ty] {
                                    Sym::Ty(ty_id) => {
                                        vardecl_value.ty.kind = Tykind::Id(ty_id)
                                    }
                                    _ => { panic!("{} is not a Type", ident) }
                                }
                            }
                        }
                        parser::Type::Inferred => { vardecl_value.ty.kind = Tykind::Inferred },
                        _ => { unreachable!() }
                    }

                    let expr = sema_check_symbols_expr(sema, actual_path, sym_table_id, &var_decl.expr);

                    let var_decl_final = VarDecl {
                        id: vardecl_value_id,
                        expr: expr
                    };

                    return Node::Statement(Statement::VarDecl(var_decl_final))

                }
                parser::Statement::Expr(expr) => {
                    let new_expr = sema_check_symbols_expr(sema, actual_path, sym_table_id, expr);

                    return Node::Statement(Statement::Expr(new_expr))
                }
                _ => { Node::None }
            }
        }
        _ => { Node::None }
    }
}

fn sema_check_symbols_expr(sema: &mut SemanticAnalizer, actual_path: String, sym_table_id: SymtableId, expr: &parser::Expr) -> Expr {
    match &expr {
        parser::Expr::Operation(operation) => {
            let lhs = sema_check_symbols_expr(sema, actual_path.clone(), sym_table_id, &operation.lhs);
            let rhs;
            match operation.operator {
                parser::Operator::Dot => {
                    rhs = sema_check_dot_rhs(sema, &lhs, &operation.rhs);
                }
                _ => {
                    rhs = sema_check_symbols_expr(sema, actual_path.clone(), sym_table_id, &operation.rhs);
                }
            }

            let operator = Operator::from_op_ast(&operation.operator);
            
            return Expr::Operation(Operation {
                lhs: Box::new(lhs),
                rhs:Box::new(rhs),
                operator
            })
        }
        parser::Expr::Ident(ident) => {
            let id_sym;
            let id_sym_res = sema.get_sym_in_scope(&ident, sym_table_id);
            match id_sym_res {
                Err(err) => {
                    panic!("{} is not found -> Err: {}", ident, err)
                }
                Ok(id) => { id_sym = id }
            }
            match &sema.symbols[id_sym] {
                Sym::Ty(_ty_id) => {
                    panic!("{} is a Type, not a value", ident)
                }
                Sym::Value(_v) => {
                    return Expr::Sym(id_sym)
                }
                Sym::External(lib, id) => {
                    return Expr::Sym(id_sym)
                }
                _ => {
                    unimplemented!()
                }
            }
        }
        _ => {
            return Expr::None
        }
    }
}

fn sema_check_dot_rhs(sema: &mut SemanticAnalizer, lhs: &Expr, expr: &parser::Expr) -> Expr {
    return Expr::Unresolved(expr)
}

fn get_primitive(s: &String) -> Option<Primitive> {
    match s.as_str() {
        "i32" => Some(Primitive::I32),
        "f32" => Some(Primitive::F32),
        "str" => Some(Primitive::String),
        _ => return None
    }
}