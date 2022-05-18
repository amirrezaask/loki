mod backend;
mod parser;
mod errors;
mod cmd;
use anyhow::{Result};

use crate::backend::{c::C, CodeGen, Compiler};
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
        .get_matches()
}

pub fn main() -> Result<()> {
    let matches = parse_cli();
    let backend = matches.value_of("backend").expect("need a backend");
    let debug = matches.is_present("debug");
    if debug {
        println!("DEBUG MODE!");
    }
    // println!("generating code using {}", backend);
    match matches.subcommand() {
        Some(("emit", arg_matches)) => cmd::emit(backend, arg_matches, debug)?,
        Some(("compile", arg_matches)) => cmd::compile(backend, arg_matches, debug)?,
        _ => unreachable!(),
    };
    Ok(())
}

