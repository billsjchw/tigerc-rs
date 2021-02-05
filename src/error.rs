#[derive(Debug)]
pub enum Error {
    ParsingError(String),
    UndefFunc((usize, usize)),
    ParamMismatch((usize, usize)),
}
