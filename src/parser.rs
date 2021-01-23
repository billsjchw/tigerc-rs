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
            parse("1 + 2 * 3 & 0 < -1"),
            Box::new(Expr::BinOp {
                loc: (0, 18),
                lhs: Box::new(Expr::BinOp {
                    loc: (0, 9),
                    lhs: Box::new(Expr::Integer {
                        loc: (0, 1),
                        value: 1,
                    }),
                    op: BinOp::Add,
                    rhs: Box::new(Expr::BinOp {
                        loc: (4, 9),
                        lhs: Box::new(Expr::Integer {
                            loc: (4, 5),
                            value: 2,
                        }),
                        op: BinOp::Mul,
                        rhs: Box::new(Expr::Integer {
                            loc: (8, 9),
                            value: 3,
                        }),
                    }),
                }),
                op: BinOp::And,
                rhs: Box::new(Expr::BinOp {
                    loc: (12, 18),
                    lhs: Box::new(Expr::Integer {
                        loc: (12, 13),
                        value: 0,
                    }),
                    op: BinOp::LT,
                    rhs: Box::new(Expr::UnOp {
                        loc: (16, 18),
                        op: UnOp::Neg,
                        expr: Box::new(Expr::Integer {
                            loc: (17, 18),
                            value: 1,
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
            parse("for i := 0 to 1 do ()"),
            Box::new(Expr::For {
                loc: (0, 21),
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
                    loc: (19, 21),
                    exprs: vec![],
                    expr: Box::new(Expr::Empty { loc: (20, 20) }),
                }),
            }),
        );
    }
}
