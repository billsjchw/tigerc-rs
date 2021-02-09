use crate::{ast::{Def, Expr}, symtab::SymbolTable};

pub fn find_esc(expr: &mut Expr) {
    let mut vars = SymbolTable::new();
    handle_expr(expr, 0, &mut vars);
}

struct Var<'a> {
    depth: i32,
    esc: &'a mut bool,
}

fn handle_expr<'a>(expr: &'a mut Expr, depth: i32, vars: &mut SymbolTable<String, Var<'a>>) {
    match *expr {
        Expr::Array {
            ref mut size, ref mut init, ..
        } => {
            handle_expr(&mut **size, depth, vars);
            handle_expr(&mut **init, depth, vars);
        }
        Expr::Assign {
            ref mut lvalue,
            ref mut rvalue,
            ..
        } => {
            handle_expr(&mut **lvalue, depth, vars);
            handle_expr(&mut **rvalue, depth, vars);
        }
        Expr::Attr { ref mut record, .. } => {
            handle_expr(&mut **record, depth, vars);
        }
        Expr::BinOp {
            ref mut lhs, ref mut rhs, ..
        } => {
            handle_expr(&mut **lhs, depth, vars);
            handle_expr(&mut **rhs, depth, vars);
        }
        Expr::Call { ref mut args, .. } => {
            for arg in args.iter_mut() {
                handle_expr(arg, depth, vars);
            }
        }
        Expr::For {
            ref cnt,
            ref mut low,
            ref mut high,
            ref mut body,
            ref mut esc,
            ..
        } => {
            handle_expr(&mut **low, depth, vars);
            handle_expr(&mut **high, depth, vars);
            vars.enter_scope();
            *esc = false;
            vars.insert(cnt.clone(), Var { depth, esc });
            handle_expr(&mut **body, depth, vars);
            vars.exit_scope();
        }
        Expr::Ident {
            ref ident,
            ..
        } => {
            if let Some(var) = vars.get_mut(ident) {
                if var.depth < depth {
                    *var.esc = true;
                }
            }
        }
        Expr::If {
            ref mut test,
            ref mut then,
            ref mut orelse,
            ..
        } => {
            handle_expr(&mut **test, depth, vars);
            handle_expr(&mut **then, depth, vars);
            handle_expr(&mut **orelse, depth, vars);
        }
        Expr::Index {
            ref mut array,
            ref mut index,
            ..
        } => {
            handle_expr(&mut **array, depth, vars);
            handle_expr(&mut **index, depth, vars);
        }
        Expr::Let {
            ref mut defs,
            ref mut body,
            ..
        } => {
            vars.enter_scope();
            for def in defs.iter_mut() {
                handle_def(def, depth, vars);
            }
            handle_expr(&mut **body, depth, vars);
            vars.exit_scope();
        }
        Expr::Record {
            ref mut elems,
            ..
        } => {
            for (_, init) in elems.iter_mut() {
                handle_expr(init, depth, vars);
            }
        }
        Expr::Seq {
            ref mut exprs,
            ref mut expr,
            ..
        } => {
            for expr in exprs.iter_mut() {
                handle_expr(expr, depth, vars);
            }
            handle_expr(&mut **expr, depth, vars);
        }
        Expr::UnOp {
            ref mut expr,
            ..
        } => {
            handle_expr(&mut **expr, depth, vars);
        }
        Expr::While {
            ref mut test,
            ref mut body,
            ..
        } => {
            handle_expr(&mut **test, depth, vars);
            handle_expr(&mut **body, depth, vars);
        }
        _ => (),
    }
}

fn handle_def<'a>(def: &'a mut Def, depth: i32, vars: &mut SymbolTable<String, Var<'a>>) {
    match *def {
        Def::Func {
            ref params,
            ref mut escs,
            ref mut body,
            ..
        } => {
            vars.enter_scope();
            for ((ident, _), esc) in params.iter().zip(escs.iter_mut()) {
                *esc = false;
                vars.insert(ident.clone(), Var { depth: depth + 1, esc });
            }
            handle_expr(&mut **body, depth + 1, vars);
            vars.exit_scope();
        }
        Def::Var {
            ref ident,
            ref mut esc,
            ref mut init,
            ..
        } => {
            handle_expr(&mut **init, depth, vars);
            *esc = false;
            vars.insert(ident.clone(), Var { depth, esc });
        }
        _ => (),
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::{Def, Expr}, parser};

    use super::find_esc;

    #[test]
    fn test_find_esc() {
        let mut expr = parser::parse(
            "\
            let\n\
                var x := 1\n\
                var y := 2\n\
                function get_x(): int = (\n\
                    x\n\
                )\n\
            in\n\
                y\n\
            end\n\
            "
        ).unwrap();
        find_esc(&mut *expr);
        assert_eq!(
            expr,
            Box::new(Expr::Let {
                loc: (0, 64),
                defs: vec![
                    Def::Var {
                        loc: (4, 14),
                        ident: String::from("x"),
                        esc: true,
                        type_: None,
                        init: Box::new(Expr::Integer {
                            loc: (13, 14),
                            value: 1,
                        }),
                    },
                    Def::Var {
                        loc: (15, 25),
                        ident: String::from("y"),
                        esc: false,
                        type_: None,
                        init: Box::new(Expr::Integer {
                            loc: (24, 25),
                            value: 2,
                        }),
                    },
                    Def::Func {
                        loc: (26, 55),
                        ident: String::from("get_x"),
                        params: vec![],
                        escs: vec![],
                        ret_type: Some(String::from("int")),
                        body: Box::new(Expr::Seq {
                            loc: (50, 55),
                            exprs: vec![],
                            expr: Box::new(Expr::Ident {
                                loc: (52, 53),
                                ident: String::from("x"),
                            }),
                        }),
                    },
                ],
                body: Box::new(Expr::Seq {
                    loc: (59, 60),
                    exprs: vec![],
                    expr: Box::new(Expr::Ident {
                        loc: (59, 60),
                        ident: String::from("y"),
                    }),
                }),
            }),
        );
    }
}
