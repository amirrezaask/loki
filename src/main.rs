mod parser;
mod backend;

use std::fs::remove_file;
use std::time;

use anyhow::Result;
use backend::Compiler;
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
        .subcommand(Command::new("emit").about("emits backend generated code").arg(arg!([NAME])))
        .subcommand(Command::new("compile").about("compiles your code using selected backend").arg(arg!([NAME])))
        .get_matches()
}
fn emit(backend: &str, arg_matches: &ArgMatches) -> Result<()> {
    let file_name = arg_matches.value_of("NAME").expect("filename needed");
    // println!("compiling {}", file_name);
    let (_, ast) = parser::module(std::fs::read_to_string(file_name).unwrap())?;
    println!("{:?}", ast);

    let c_code_gen = C{arch: "".to_string(), os:"".to_string(), ast};
    let output = c_code_gen.generate()?;

    println!("{}", output);

    Ok(())
}
fn compile(backend: &str, arg_matches: &ArgMatches) -> Result<()> {
    let file_name = arg_matches.value_of("NAME").expect("filename needed");
    // println!("compiling {}", file_name);
    let (_, ast) = parser::module(std::fs::read_to_string(file_name).unwrap())?;
    // println!("{:?}", ast);

    let c_code_gen = C{arch: "".to_string(), os:"".to_string(), ast};
    let output = c_code_gen.generate()?;
    let bin = file_name.split(".").nth(0).unwrap();
    let generated_file_name = format!("./{}.c", bin);
    std::fs::write(generated_file_name.clone(), output)?;
    C::compile(&generated_file_name, bin);
    std::fs::remove_file(generated_file_name)?;
    Ok(())
}

fn main() -> Result<()> {
    // println!("{:?}", parser::fn_def("fn(a: int) string {\n\tprint(a);\n\t}".to_string()));
    let matches = parse_cli();
    let backend = matches.value_of("backend").expect("need a backend");
    // println!("generating code using {}", backend);
    match matches.subcommand() {
        Some(("emit", arg_matches)) => emit(backend, arg_matches)?,
        Some(("compile", arg_matches)) => compile(backend, arg_matches)?,
        _ => unreachable!(),
    };
    // println!("{:?}", mod::_struct("struct {\n\tname: string,\n\tage: int\n}".to_string())?);
    Ok(())
}
