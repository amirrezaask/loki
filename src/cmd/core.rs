use crate::backend::{c::C, CodeGen};
use crate::parser::{self, Node};

use anyhow::Result;
use clap::ArgMatches;

const DEFAULT_BACKEND: &str = "c";

pub fn get_target_code_generator(backend: &str, node: parser::Node) -> Result<impl CodeGen> {
    if backend.to_lowercase() == "c" {
        Ok(C {
            arch: "".to_string(),
            os: "".to_string(),
            ast: node,
        })
    } else {
        Err(anyhow::Error::msg(format!("no backend with {} found", backend)))
    }
}

pub fn get_backend_name(arg_matches: &ArgMatches) -> &str {
    arg_matches.value_of("backend").unwrap_or(DEFAULT_BACKEND)
}


pub fn get_input_file_name(arg_matches: &ArgMatches) -> &str {
    arg_matches.value_of("NAME").expect("input file name missing")
}

pub fn if_debug_print_ast(arg_matches: &ArgMatches, ast: &Node) {
    let debug = arg_matches.is_present("debug");
    if debug {
        println!("{:?}", ast);
    }
}

pub fn generate_target_code(arg_matches: &ArgMatches, ast: &Node) -> Result<String> {
    let back = super::core::get_target_code_generator(get_backend_name(arg_matches), ast.clone())?;
    back.generate()
}

pub fn get_output_binary_name(arg_matches: &ArgMatches) -> &str {
    get_input_file_name(arg_matches).split('.').next().unwrap()
}

pub fn save_target_code(arg_matches: &ArgMatches, target_code: String) -> Result<String>{
    let bin = get_output_binary_name(arg_matches);
    let output_file_name = format!("./{}.c", bin);
    std::fs::write(output_file_name.clone(), target_code)?;

    Ok(output_file_name)
}