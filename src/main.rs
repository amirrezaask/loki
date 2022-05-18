mod backend;
mod parser;
mod errors;

use anyhow::{Error, Result};
use backend::Compiler;
use clap::{arg, Arg, ArgMatches, Command};

use backend::CodeGen;

use crate::backend::c::C;

fn get_backend(backend: &str, node: parser::Node) -> Result<impl CodeGen> {
    if backend.to_lowercase() == "c" {
        Ok(C {
            arch: "".to_string(),
            os: "".to_string(),
            ast: node,
        })
    } else {
        Err(Error::msg(format!("no backend with {} found", backend)))
    }
}

fn parse_cli() -> ArgMatches {
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
fn emit(backend: &str, arg_matches: &ArgMatches, debug: bool) -> Result<()> {
    let file_name = arg_matches.value_of("NAME").expect("filename needed");
    let (_, ast) = parser::module(std::fs::read_to_string(file_name).unwrap())?;
    if debug {
        println!("{:?}", ast);
    }
    let back = get_backend(backend, ast)?;
    let output = back.generate()?;

    println!("{}", output);

    Ok(())
}
fn compile(backend: &str, arg_matches: &ArgMatches, debug: bool) -> Result<()> {
    let file_name = arg_matches.value_of("NAME").expect("filename needed");
    let debug = arg_matches.is_present("debug");
    let (_, ast) = parser::module(std::fs::read_to_string(file_name).unwrap())?;
    if debug {
        println!("{:?}", ast);
    }
    let back = get_backend(backend, ast)?;
    let output = back.generate()?;

    let bin = file_name.split(".").nth(0).unwrap();
    let generated_file_name = format!("./{}.c", bin);
    std::fs::write(generated_file_name.clone(), output)?;
    C::compile(&generated_file_name, bin);
    std::fs::remove_file(generated_file_name)?;
    Ok(())
}

fn main() -> Result<()> {
    let matches = parse_cli();
    let backend = matches.value_of("backend").expect("need a backend");
    let debug = matches.is_present("debug");
    if debug {
        println!("DEBUG MODE!");
    }
    // println!("generating code using {}", backend);
    match matches.subcommand() {
        Some(("emit", arg_matches)) => emit(backend, arg_matches, debug)?,
        Some(("compile", arg_matches)) => compile(backend, arg_matches, debug)?,
        _ => unreachable!(),
    };
    Ok(())
}
