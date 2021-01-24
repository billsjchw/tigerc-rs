#[derive(Debug, PartialEq)]
pub enum Expr {
    Ident {
        loc: (usize, usize),
        name: String,
    },
    Attr {
        loc: (usize, usize),
        record: Box<Expr>,
        attr: String,
    },
    Index {
        loc: (usize, usize),
        array: Box<Expr>,
        index: Box<Expr>,
    },
    Integer {
        loc: (usize, usize),
        value: i64,
    },
    String {
        loc: (usize, usize),
        value: String,
    },
    Call {
        loc: (usize, usize),
        func: String,
        args: Vec<Expr>,
    },
    BinOp {
        loc: (usize, usize),
        lhs: Box<Expr>,
        op: BinOp,
        rhs: Box<Expr>,
    },
    UnOp {
        loc: (usize, usize),
        op: UnOp,
        expr: Box<Expr>,
    },
    Record {
        loc: (usize, usize),
        typ: String,
        elems: Vec<(String, Expr)>,
    },
    Seq {
        loc: (usize, usize),
        exprs: Vec<Expr>,
        expr: Box<Expr>,
    },
    Assign {
        loc: (usize, usize),
        var: Box<Expr>,
        expr: Box<Expr>,
    },
    If {
        loc: (usize, usize),
        test: Box<Expr>,
        body: Box<Expr>,
        orelse: Box<Expr>,
    },
    While {
        loc: (usize, usize),
        test: Box<Expr>,
        body: Box<Expr>,
    },
    For {
        loc: (usize, usize),
        var: String,
        low: Box<Expr>,
        high: Box<Expr>,
        body: Box<Expr>,
    },
    Let {
        loc: (usize, usize),
        decs: Vec<Def>,
        body: Box<Expr>,
    },
    Array {
        loc: (usize, usize),
        typ: String,
        size: Box<Expr>,
        init: Box<Expr>,
    },
    Break {
        loc: (usize, usize),
    },
    Nil {
        loc: (usize, usize),
    },
    Empty {
        loc: (usize, usize),
    },
}

#[derive(Debug, PartialEq)]
pub enum Def {
    Func {
        loc: (usize, usize),
        name: String,
        typ: String,
        params: Vec<(String, String)>,
        body: Box<Expr>,
    },
    Var {
        loc: (usize, usize),
        name: String,
        typ: Option<String>,
        init: Box<Expr>,
    },
    Type {
        loc: (usize, usize),
        name: String,
        typ: Box<Type>,
    },
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Ident {
        loc: (usize, usize),
        name: String
    },
    Record {
        loc: (usize, usize),
        attrs: Vec<(String, String)>,
    },
    Array {
        loc: (usize, usize),
        typ: String,
    },
}

#[derive(Debug, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Eq,
    NE,
    LT,
    LE,
    GT,
    GE,
}

#[derive(Debug, PartialEq)]
pub enum UnOp {
    Pos,
    Neg,
    Not,
}
