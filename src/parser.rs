use crate::tiger;
use crate::{ast::Expr, error::Error};

pub fn parse(prog: &str) -> Result<Box<Expr>, Error> {
    Ok(tiger::ExprParser::new()
        .parse(prog)
        .map_err(|err| Error::ParsingError(err.to_string()))?)
}

#[cfg(test)]
mod tests {
    use super::parse;
    use crate::ast::{BinOp, Def, Expr, Type, UnOp};

    #[test]
    fn test_parse_arithmetic() {
        assert_eq!(
            parse("a + b * c & d[e + f] < -g.h1_J").unwrap(),
            Box::new(Expr::BinOp {
                loc: (0, 30),
                lhs: Box::new(Expr::BinOp {
                    loc: (0, 9),
                    lhs: Box::new(Expr::Ident {
                        loc: (0, 1),
                        ident: String::from("a"),
                    }),
                    op: BinOp::Add,
                    rhs: Box::new(Expr::BinOp {
                        loc: (4, 9),
                        lhs: Box::new(Expr::Ident {
                            loc: (4, 5),
                            ident: String::from("b"),
                        }),
                        op: BinOp::Mul,
                        rhs: Box::new(Expr::Ident {
                            loc: (8, 9),
                            ident: String::from("c"),
                        }),
                    }),
                }),
                op: BinOp::And,
                rhs: Box::new(Expr::BinOp {
                    loc: (12, 30),
                    lhs: Box::new(Expr::Index {
                        loc: (12, 20),
                        array: Box::new(Expr::Ident {
                            loc: (12, 13),
                            ident: String::from("d"),
                        }),
                        index: Box::new(Expr::BinOp {
                            loc: (14, 19),
                            lhs: Box::new(Expr::Ident {
                                loc: (14, 15),
                                ident: String::from("e"),
                            }),
                            op: BinOp::Add,
                            rhs: Box::new(Expr::Ident {
                                loc: (18, 19),
                                ident: String::from("f"),
                            }),
                        }),
                    }),
                    op: BinOp::LT,
                    rhs: Box::new(Expr::UnOp {
                        loc: (23, 30),
                        op: UnOp::Neg,
                        expr: Box::new(Expr::Attr {
                            loc: (24, 30),
                            record: Box::new(Expr::Ident {
                                loc: (24, 25),
                                ident: String::from("g"),
                            }),
                            attr: String::from("h1_J"),
                        }),
                    }),
                }),
            }),
        );
    }

    #[test]
    fn test_parse_seq_expr() {
        assert_eq!(
            parse("(1; 2 + 3;)").unwrap(),
            Box::new(Expr::Seq {
                loc: (0, 11),
                exprs: vec![
                    Expr::Integer {
                        loc: (1, 2),
                        value: 1,
                    },
                    Expr::BinOp {
                        loc: (4, 9),
                        lhs: Box::new(Expr::Integer {
                            loc: (4, 5),
                            value: 2,
                        }),
                        op: BinOp::Add,
                        rhs: Box::new(Expr::Integer {
                            loc: (8, 9),
                            value: 3,
                        }),
                    },
                ],
                expr: Box::new(Expr::Empty { loc: (10, 10) }),
            }),
        );
    }

    #[test]
    fn test_parse_if_expr() {
        assert_eq!(
            parse("if 0 > 1 then (2) else (3)").unwrap(),
            Box::new(Expr::If {
                loc: (0, 26),
                test: Box::new(Expr::BinOp {
                    loc: (3, 8),
                    lhs: Box::new(Expr::Integer {
                        loc: (3, 4),
                        value: 0,
                    }),
                    op: BinOp::GT,
                    rhs: Box::new(Expr::Integer {
                        loc: (7, 8),
                        value: 1,
                    }),
                }),
                then: Box::new(Expr::Seq {
                    loc: (14, 17),
                    exprs: vec![],
                    expr: Box::new(Expr::Integer {
                        loc: (15, 16),
                        value: 2,
                    }),
                }),
                orelse: Box::new(Expr::Seq {
                    loc: (23, 26),
                    exprs: vec![],
                    expr: Box::new(Expr::Integer {
                        loc: (24, 25),
                        value: 3,
                    }),
                }),
            }),
        );
    }

    #[test]
    fn test_parse_while_expr() {
        assert_eq!(
            parse("while 0 do (1; break;)").unwrap(),
            Box::new(Expr::While {
                loc: (0, 22),
                test: Box::new(Expr::Integer {
                    loc: (6, 7),
                    value: 0,
                }),
                body: Box::new(Expr::Seq {
                    loc: (11, 22),
                    exprs: vec![
                        Expr::Integer {
                            loc: (12, 13),
                            value: 1,
                        },
                        Expr::Break { loc: (15, 20) },
                    ],
                    expr: Box::new(Expr::Empty { loc: (21, 21) }),
                }),
            }),
        );
    }

    #[test]
    fn test_parse_for_expr() {
        assert_eq!(
            parse("for i := 0 to 1 do (a := a * 2; continue;)").unwrap(),
            Box::new(Expr::For {
                loc: (0, 42),
                cnt: String::from("i"),
                esc: true,
                low: Box::new(Expr::Integer {
                    loc: (9, 10),
                    value: 0,
                }),
                high: Box::new(Expr::Integer {
                    loc: (14, 15),
                    value: 1,
                }),
                body: Box::new(Expr::Seq {
                    loc: (19, 42),
                    exprs: vec![
                        Expr::Assign {
                            loc: (20, 30),
                            lvalue: Box::new(Expr::Ident {
                                loc: (20, 21),
                                ident: String::from("a"),
                            }),
                            rvalue: Box::new(Expr::BinOp {
                                loc: (25, 30),
                                lhs: Box::new(Expr::Ident {
                                    loc: (25, 26),
                                    ident: String::from("a"),
                                }),
                                op: BinOp::Mul,
                                rhs: Box::new(Expr::Integer {
                                    loc: (29, 30),
                                    value: 2,
                                }),
                            }),
                        },
                        Expr::Continue { loc: (32, 40) },
                    ],
                    expr: Box::new(Expr::Empty { loc: (41, 41) }),
                }),
            }),
        );
    }

    #[test]
    fn test_parse_array_expr() {
        assert_eq!(
            parse("IntArray {0; 3}").unwrap(),
            Box::new(Expr::Array {
                loc: (0, 15),
                type_: String::from("IntArray"),
                size: Box::new(Expr::Integer {
                    loc: (13, 14),
                    value: 3,
                }),
                init: Box::new(Expr::Integer {
                    loc: (10, 11),
                    value: 0,
                }),
            }),
        );
    }

    #[test]
    fn test_parse_record_expr() {
        assert_eq!(
            parse("Student {id: 1, name: \"tom\"}").unwrap(),
            Box::new(Expr::Record {
                loc: (0, 28),
                type_: String::from("Student"),
                elems: vec![
                    (
                        String::from("id"),
                        Expr::Integer {
                            loc: (13, 14),
                            value: 1,
                        },
                    ),
                    (
                        String::from("name"),
                        Expr::String {
                            loc: (22, 27),
                            value: String::from("tom"),
                        },
                    ),
                ],
            }),
        );
    }

    #[test]
    fn test_parse_let_expr() {
        assert_eq!(
            parse(
                "let\n\
                     var i := 0\n\
                     var s: string := \"test\"\n\
                     type IntArray = array of int\n\
                     type Student = {id: int, name: string}\n\
                     function zero() = (0)\n\
                     function add_one(x: int): int = (x + 1)\n\
                 in\n\
                     0;\n\
                     add_one(i)\n\
                 end\n"
            )
            .unwrap(),
            Box::new(Expr::Let {
                loc: (0, 189),
                defs: vec![
                    Def::Var {
                        loc: (4, 14),
                        ident: String::from("i"),
                        esc: true,
                        type_: None,
                        init: Box::new(Expr::Integer {
                            loc: (13, 14),
                            value: 0,
                        }),
                    },
                    Def::Var {
                        loc: (15, 38),
                        ident: String::from("s"),
                        esc: true,
                        type_: Some(String::from("string")),
                        init: Box::new(Expr::String {
                            loc: (32, 38),
                            value: String::from("test"),
                        }),
                    },
                    Def::Type {
                        loc: (39, 67),
                        ident: String::from("IntArray"),
                        type_: Box::new(Type::Array {
                            loc: (55, 67),
                            elem_type: String::from("int"),
                        }),
                    },
                    Def::Type {
                        loc: (68, 106),
                        ident: String::from("Student"),
                        type_: Box::new(Type::Record {
                            loc: (83, 106),
                            attrs: vec![
                                (String::from("id"), String::from("int")),
                                (String::from("name"), String::from("string")),
                            ],
                        }),
                    },
                    Def::Func {
                        loc: (107, 128),
                        ident: String::from("zero"),
                        ret_type: None,
                        params: vec![],
                        escs: vec![],
                        body: Box::new(Expr::Seq {
                            loc: (125, 128),
                            exprs: vec![],
                            expr: Box::new(Expr::Integer {
                                loc: (126, 127),
                                value: 0,
                            }),
                        }),
                    },
                    Def::Func {
                        loc: (129, 168),
                        ident: String::from("add_one"),
                        ret_type: Some(String::from("int")),
                        params: vec![(String::from("x"), String::from("int"))],
                        escs: vec![true],
                        body: Box::new(Expr::Seq {
                            loc: (161, 168),
                            exprs: vec![],
                            expr: Box::new(Expr::BinOp {
                                loc: (162, 167),
                                lhs: Box::new(Expr::Ident {
                                    loc: (162, 163),
                                    ident: String::from("x"),
                                }),
                                op: BinOp::Add,
                                rhs: Box::new(Expr::Integer {
                                    loc: (166, 167),
                                    value: 1,
                                }),
                            }),
                        }),
                    },
                ],
                body: Box::new(Expr::Seq {
                    loc: (172, 185),
                    exprs: vec![Expr::Integer {
                        loc: (172, 173),
                        value: 0,
                    }],
                    expr: Box::new(Expr::Call {
                        loc: (175, 185),
                        callee: String::from("add_one"),
                        args: vec![Expr::Ident {
                            loc: (183, 184),
                            ident: String::from("i"),
                        }],
                    }),
                }),
            }),
        );
    }
}
