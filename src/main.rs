mod code_gen;
mod parser;
mod lexer;
mod compiler;
mod ast;
mod node_manager;
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
    let mut c = compiler::Compiler::new();
    c.compile_file(filename, code_gen::Backend::CPP)?;
    Ok(())
}
