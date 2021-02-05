use clap::Clap;
use compiler::Compiler;
use fs::File;
use lalrpop_util::lalrpop_mod;
use std::{fs, process};
use std::io::prelude::*;

lalrpop_mod!(pub tiger);
pub mod ast;
pub mod compiler;
pub mod error;
pub mod parser;
pub mod util;
pub mod symtab;

#[derive(Clap)]
struct Opts {
    input: String,
    #[clap(short, long, default_value = "tiger.o")]
    output: String,
}

fn main() {
    let opts = Opts::parse();
    let prog = fs::read_to_string(&opts.input).expect("Failed to read the input file");
    let compiler = Compiler::new();
    let bytes = compiler.compile(&prog[..]).unwrap_or_else(|err| {
        println!("{:?}", err);
        process::exit(1);
    });
    let mut output = File::create(&opts.output).expect("Failed to create the output file");
    output.write_all(&bytes[..]).expect("Failed to write the output file");
}
