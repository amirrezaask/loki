mod parser;
mod backend;

use anyhow::Result;
use clap::{arg, Arg, ArgMatches, Command};
use backend::c;
use backend::CodeGen;

use crate::backend::c::C;

fn parse_cli() -> ArgMatches {
    Command::new("loki")
        .author("amirrezaask, raskarpour@gmail.com")
        .version("0.1.0")
        .about("Loki Compiler")
        .arg(
            Arg::new("backend")
                .required(false)
                .short('b')
                .long("backend")
                .takes_value(true)
                .help("Backend to use for codegen"),
        )
        .subcommand(Command::new("gen").arg(arg!([NAME])))
        .get_matches()
}

fn gen(backend: &str, arg_matches: &ArgMatches) -> Result<()> {
    let file_name = arg_matches.value_of("NAME").expect("filename needed");
    // println!("compiling {}", file_name);
    let (_, ast) = parser::module(std::fs::read_to_string(file_name).unwrap())?;
    // println!("{:?}", ast);

    let c_code_gen = C{arch: "".to_string(), os:"".to_string(), ast};
    let output = c_code_gen.generate()?;

    println!("{}", output);

    Ok(())
}
fn main() -> Result<()> {
    let matches = parse_cli();
    let backend = matches.value_of("backend").expect("need a backend");
    // println!("generating code using {}", backend);
    match matches.subcommand() {
        Some(("gen", arg_matches)) => gen(backend, arg_matches),
        _ => unreachable!(),
    };
    // println!("{:?}", mod::_struct("struct {\n\tname: string,\n\tage: int\n}".to_string())?);
    Ok(())
}
