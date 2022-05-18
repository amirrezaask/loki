use crate::backend::{c::C, Compiler};
use crate::parser;
use clap::{ArgMatches};
use anyhow::Result;

use super::core::{get_input_file_name, if_debug_print_ast, generate_target_code, save_target_code, get_output_binary_name};

pub fn compile(arg_matches: &ArgMatches) -> Result<()> {
    let file_name = get_input_file_name(arg_matches);
    let (_, ast) = parser::module(std::fs::read_to_string(file_name).unwrap())?;
    if_debug_print_ast(arg_matches, &ast);

    let target_code = generate_target_code(arg_matches, &ast)?;
    let target_code_file = save_target_code(arg_matches, target_code)?;
    let binary_name = get_output_binary_name(arg_matches);
    C::compile(&target_code_file, binary_name);
    std::fs::remove_file(target_code_file)?;
    Ok(())
}