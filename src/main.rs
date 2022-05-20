mod backend;
mod parser;
mod errors;
mod passes;
mod pipeline;
mod cmd;
use anyhow::{Result};


use clap::{ArgMatches, Arg, Command, arg};

pub fn parse_cli() -> ArgMatches {
    Command::new("loki")
        .arg_required_else_help(true)
        .author("amirrezaask, raskarpour@gmail.com")
        .version("0.1.0")
        .about("Loki Compiler")
        .arg(Arg::new("debug").long("debug"))
        .arg(
            Arg::new("backend")
                .required(false)
                .short('b')
                .long("backend")
                .takes_value(true)
                .default_value("c")
                .help("Backend to use for codegen"),
        )
        .subcommand(
            Command::new("emit")
                .about("emits backend generated code")
                .arg(arg!([NAME])),
        )
        .subcommand(
            Command::new("compile")
                .about("compiles your code using selected backend")
                .arg(arg!([NAME])),
        )
        .subcommand(
            Command::new("run")
                .about("compiles and then runs your code")
                .arg(arg!([NAME])),
        )
        .get_matches()
}

pub fn main() -> Result<()> {
    let matches = parse_cli();
    match matches.subcommand() {
        Some(("emit", arg_matches)) => cmd::emit(arg_matches)?,
        Some(("run", arg_matches)) => cmd::run(arg_matches)?,
        Some(("compile", arg_matches)) => cmd::compile(arg_matches)?,
        _ => unreachable!(),
    };
    Ok(())
}

