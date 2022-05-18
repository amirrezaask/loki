use crate::backend::{c::C, CodeGen, Compiler};
use crate::parser;
use clap::{ArgMatches};
use anyhow::Result;

pub fn compile(backend: &str, arg_matches: &ArgMatches, _debug: bool) -> Result<()> {
    let file_name = arg_matches.value_of("NAME").expect("filename needed");
    let debug = arg_matches.is_present("debug");
    let (_, ast) = parser::module(std::fs::read_to_string(file_name).unwrap())?;
    if debug {
        println!("{:?}", ast);
    }
    let back = super::core::get_backend(backend, ast)?;
    let output = back.generate()?;

    let bin = file_name.split(".").nth(0).unwrap();
    let generated_file_name = format!("./{}.c", bin);
    std::fs::write(generated_file_name.clone(), output)?;
    C::compile(&generated_file_name, bin);
    std::fs::remove_file(generated_file_name)?;
    Ok(())
}