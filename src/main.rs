mod backend;
mod cmd;
mod errors;
mod parser;
mod passes;
mod tokenizer;
mod new_parser;
mod src_discovery;
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
