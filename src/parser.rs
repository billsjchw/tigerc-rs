use crate::ast::Expr;
use crate::tiger;

pub fn parse(prog: &str) -> Box<Expr> {
    tiger::ExprParser::new().parse(prog).unwrap()
}

#[cfg(test)]
mod tests {
    use super::parse;
    use crate::ast::{BinOp, Expr, UnOp};

    #[test]
    fn test_parse_arithmetic() {
        assert_eq!(
            parse("a + b * c & d[0] < -e.f1_G"),
            Box::new(Expr::BinOp {
                loc: (0, 26),
                lhs: Box::new(Expr::BinOp {
                    loc: (0, 9),
                    lhs: Box::new(Expr::Ident {
                        loc: (0, 1),
                        name: String::from("a"),
                    }),
                    op: BinOp::Add,
                    rhs: Box::new(Expr::BinOp {
                        loc: (4, 9),
                        lhs: Box::new(Expr::Ident {
                            loc: (4, 5),
                            name: String::from("b"),
                        }),
                        op: BinOp::Mul,
                        rhs: Box::new(Expr::Ident {
                            loc: (8, 9),
                            name: String::from("c"),
                        }),
                    }),
                }),
                op: BinOp::And,
                rhs: Box::new(Expr::BinOp {
                    loc: (12, 26),
                    lhs: Box::new(Expr::Index {
                        loc: (12, 16),
                        array: Box::new(Expr::Ident {
                            loc: (12, 13),
                            name: String::from("d"),
                        }),
                        index: Box::new(Expr::Integer {
                            loc: (14, 15),
                            value: 0,
                        }),
                    }),
                    op: BinOp::LT,
                    rhs: Box::new(Expr::UnOp {
                        loc: (19, 26),
                        op: UnOp::Neg,
                        expr: Box::new(Expr::Attr {
                            loc: (20, 26),
                            record: Box::new(Expr::Ident {
                                loc: (20, 21),
                                name: String::from("e"),
                            }),
                            attr: String::from("f1_G"),
                        }),
                    }),
                }),
            }),
        );
    }

    #[test]
    fn test_parse_seq_expr() {
        assert_eq!(
            parse("(1; 2 + 3;)"),
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
            parse("if 0 > 1 then (2) else (3)"),
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
                body: Box::new(Expr::Seq {
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
            parse("while 0 do (1; break;)"),
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
            parse("for i := 0 to 1 do (a := a * 2;)"),
            Box::new(Expr::For {
                loc: (0, 32),
                var: String::from("i"),
                low: Box::new(Expr::Integer {
                    loc: (9, 10),
                    value: 0,
                }),
                high: Box::new(Expr::Integer {
                    loc: (14, 15),
                    value: 1,
                }),
                body: Box::new(Expr::Seq {
                    loc: (19, 32),
                    exprs: vec![Expr::Assign {
                        loc: (20, 30),
                        var: Box::new(Expr::Ident {
                            loc: (20, 21),
                            name: String::from("a"),
                        }),
                        expr: Box::new(Expr::BinOp {
                            loc: (25, 30),
                            lhs: Box::new(Expr::Ident {
                                loc: (25, 26),
                                name: String::from("a"),
                            }),
                            op: BinOp::Mul,
                            rhs: Box::new(Expr::Integer {
                                loc: (29, 30),
                                value: 2,
                            }),
                        }),
                    }],
                    expr: Box::new(Expr::Empty { loc: (31, 31) }),
                }),
            }),
        );
    }

    #[test]
    fn test_parse_array_expr() {
        assert_eq!(
            parse("IntArray {0; 3}"),
            Box::new(Expr::Array {
                loc: (0, 15),
                typ: String::from("IntArray"),
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
            parse("Student {id: 1, name: \"tom\"}"),
            Box::new(Expr::Record {
                loc: (0, 28),
                typ: String::from("Student"),
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
}
