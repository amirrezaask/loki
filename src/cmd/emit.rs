use crate::backend::{c::C, CodeGen, Compiler};
use crate::parser;
use clap::{ArgMatches, Arg, Command, arg};
use anyhow::Result;
use super::core::get_backend;

pub fn emit(backend: &str, arg_matches: &ArgMatches, debug: bool) -> Result<()> {
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