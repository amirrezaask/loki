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
use anyhow::Result;
use std::env;

const ABOUT: &str = "loki compiler 0.2";
const USAGE: &str =
    "Usage: loki [filename]\n";

pub fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("{}\n{}", ABOUT, USAGE);
        return Ok(());
    }
    let args = args[1..].to_vec();
    let mut verbose = false;
    if args.contains(&"-v".to_string()) || args.contains(&"--verbose".to_string()) {
        verbose = true;
    }
    let mut debug = false;
    if args.contains(&"--debug".to_string()) {
        debug = true;
    }
    let filename = &args[0];
    let mut c = compliation::Compilation::new(&filename, verbose, debug)?;
    Ok(())
}
