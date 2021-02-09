#[derive(Debug)]
pub enum Error {
    ParsingError(String),
    UndefFunc((usize, usize)),
    UndefType((usize, usize)),
    UndefVar((usize, usize)),
    ParamMismatch((usize, usize)),
    RetMismatch((usize, usize)),
    AssignMismatch((usize, usize)),
    AliasCycle((usize, usize)),
    TypeNotArray((usize, usize)),
    ArrayInitMismatch((usize, usize)),
    ArraySizeNotInteger((usize, usize)),
    ArrayNotArray((usize, usize)),
    IndexNotInteger((usize, usize)),
    VarInitMismatch((usize, usize)),
    ForBoundNotInteger((usize, usize)),
    AssignToReadonly((usize, usize)),
    WhileTestNotInteger((usize, usize)),
    IfTestNotInteger((usize, usize)),
    IfMismatch((usize, usize)),
    BreakOutsideLoop((usize, usize)),
    ContinueOutsideLoop((usize, usize)),
}
