#[derive(Debug, PartialEq)]
pub enum Variable {
    Identifier {
        name: String,
    },
    Attribute {
        var: Box<Variable>,
        attrname: String,
    },
    Subscript {
        var: Box<Variable>,
        expr: Box<Expression>,
    },
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Variable {
        var: Box<Variable>,
    },
    Integer {
        value: i64,
    },
    String {
        value: String,
    },
    Call {
        funcname: String,
        args: Vec<Expression>,
    },
    BinOp {
        lhs: Box<Expression>,
        op: BinaryOperator,
        rhs: Box<Expression>,
    },
    UnOp {
        op: UnaryOperator,
        expr: Box<Expression>,
    },
    Record {
        typename: String,
        elems: Vec<(String, Expression)>,
    },
    Sequence {
        exprs: Vec<Expression>,
        expr: Box<Expression>,
    },
    Assign {
        var: Variable,
        expr: Box<Expression>,
    },
    If {
        test: Box<Expression>,
        body: Box<Expression>,
        orelse: Box<Expression>,
    },
    While {
        test: Box<Expression>,
        body: Box<Expression>,
    },
    For {
        idname: String,
        low: Box<Expression>,
        high: Box<Expression>,
        body: Box<Expression>,
    },
    Let {
        decs: Vec<Declaration>,
        body: Box<Expression>,
    },
    Array {
        typename: String,
        size: Box<Expression>,
        init: Box<Expression>,
    },
    Break,
    Nil,
    Unit,
}

#[derive(Debug, PartialEq)]
pub enum Declaration {
    Function {
        name: String,
        typename: String,
        params: Vec<(String, String)>,
        body: Box<Expression>,
    },
    Variable {
        name: String,
        typename: Option<String>,
        init: Box<Expression>,
    },
    Type {
        name: String,
        ty: Box<Type>,
    },
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Identifier {
        name: String,
    },
    Record {
        attrs: Vec<(String, String)>,
    },
    Array {
        typename: String,
    },
}

#[derive(Debug, PartialEq)]
pub enum BinaryOperator {
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
pub enum UnaryOperator {
    Pos,
    Neg,
}
