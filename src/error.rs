#[derive(Debug)]
pub enum Error {
    ParsingError(String),
    UndefFunc((usize, usize)),
    UndefType((usize, usize)),
    ParamMismatch((usize, usize)),
    RetMismatch((usize, usize)),
}
