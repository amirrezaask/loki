#![allow(clippy::needless_return)]
#![allow(warnings, unused)]
mod parser;
mod stack;
mod lexer;
mod typer;
mod sema;
mod utils;
mod compliation;
mod ir;
mod errors;
mod bytecode;
mod generic_ds;
mod c_backend;
mod tests;
// mod symbol_table;
use anyhow::Result;
use std::env;

const ABOUT: &str = "loki compiler 0.2";
const USAGE: &str =
    "Usage: loki [filename] [flags]\n\t--emit-cpp: emits CPP code using C code backend.\n";

pub fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("{}\n{}", ABOUT, USAGE);
        return Ok(());
    }
    let args = args[1..].to_vec();
    let filename = &args[0];
    let mut c = compliation::Compilation::new(&filename)?;
    // p.compile_file(filename, code_gen::Backend::CPP)?;
    // Ok(())
    Ok(())
}
