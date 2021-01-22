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
}
