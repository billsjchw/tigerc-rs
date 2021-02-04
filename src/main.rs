use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub tiger);
pub mod parser;
pub mod util;
pub mod ast;
pub mod error;

fn main() {
    println!("Hello, world!");
}
