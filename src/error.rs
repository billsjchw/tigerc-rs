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
}
