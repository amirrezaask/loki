mod ast;
mod backend;
mod cmd;
mod errors;
mod parser;
mod passes;
mod src_discovery;
mod tokenizer;
use anyhow::Result;

pub fn main() -> Result<()> {
    let matches = cmd::parse_cli();
    match matches.subcommand() {
        Some(("emit", arg_matches)) => cmd::emit(arg_matches)?,
        Some(("run", arg_matches)) => cmd::run(arg_matches)?,
        Some(("compile", arg_matches)) => cmd::compile(arg_matches)?,
        _ => unreachable!(),
    };
    Ok(())
}
