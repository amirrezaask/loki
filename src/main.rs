mod code_gen;
mod parser;
mod tokenizer;
mod compiler;
mod ast;
mod symbol_table;
use anyhow::Result;
use std::env;

const about: &'static str = "loki compiler 0.2";
const usage: &'static str =
    "Usage: loki [filename] [flags]\n\t--emit-cpp: emits CPP code using C code backend.\n";

pub fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("{}\n{}", about, usage);
        return Ok(());
    }
    let args = args[1..].to_vec();
    let filename = &args[0];
    let c = compiler::Compiler::new();
    c.compile_file(filename, code_gen::Backend::CPP)?;
    Ok(())
}
