
use crate::{parser};
use clap::{ArgMatches};
use anyhow::Result;
use crate::passes::Passes;
use super::core::*;

pub fn emit(arg_matches: &ArgMatches) -> Result<()> {
    let file_name = get_input_file_name(arg_matches);
    let (_, ast) = parser::module(std::fs::read_to_string(file_name).unwrap())?;
    let p = Passes::new();
    let ast = p.run(ast)?;
    if_debug_print_ast(arg_matches, &ast);

    let target_code = generate_target_code(arg_matches, &ast)?;

    println!("{}", target_code);
    Ok(())
}