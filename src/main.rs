#![allow(clippy::needless_return)]
#![allow(warnings, unused)]
mod bytecode;
mod c_backend;
mod compliation;
mod errors;
mod hash_list;
mod ir;
mod lexer;
mod parser;
mod sema;
mod stack;
mod tests;
mod typer;
mod utils;
use anyhow::Result;
use std::env;

const ABOUT: &str = "loki compiler 0.2";
const USAGE: &str =
    "Usage: loki [filename]\n\n--debug: enables debug output of the bytecode in the terminal and also preserves the generated backend code.\n-v|--verbose: Show compiler timings";

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
