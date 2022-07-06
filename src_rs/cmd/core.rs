use crate::backend::{c::C, CodeGen};
use crate::parser::{self, Node};

use anyhow::Result;
use clap::ArgMatches;

const DEFAULT_BACKEND: &str = "c";
use clap::{arg, Arg, Command};

pub fn parse_cli() -> ArgMatches {
    Command::new("loki")
        .arg_required_else_help(true)
        .author("amirrezaask, raskarpour@gmail.com")
        .version("0.1.0")
        .about("Loki Compiler")
        .arg(Arg::new("debug").long("debug"))
        .arg(
            Arg::new("include")
                .required(false)
                .short('I')
                .takes_value(true)
                .multiple_occurrences(true)
                .default_value(".")
                .help("paths to use for looking up loki source files."),
        )
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

pub fn get_target_code_generator(backend: &str, node: parser::Node) -> Result<impl CodeGen> {
    if backend.to_lowercase() == "c" {
        Ok(C {
            arch: "".to_string(),
            os: "".to_string(),
            ast: node,
        })
    } else {
        Err(anyhow::Error::msg(format!(
            "no backend with {} found",
            backend
        )))
    }
}

pub fn get_backend_name() -> String {
    parse_cli().value_of("backend").unwrap_or(DEFAULT_BACKEND).to_string()
}

pub fn get_input_file_name(arg_matches: &ArgMatches) -> &str {
    arg_matches
        .value_of("NAME")
        .expect("input file name missing")
}

pub fn get_include_paths() -> Vec<String> {
    match parse_cli().values_of("include") {
        Some(val) => {
            val.map(|v| v.to_string()).collect()
        },
        None => {
            vec![".".to_string()]
        }
    }
}

pub fn if_debug_print_ast(ast: &Node) {
    let debug = parse_cli().is_present("debug");
    if debug {
        println!("{:?}", ast);
    }
}

pub fn generate_target_code(arg_matches: &ArgMatches, ast: &Node) -> Result<String> {
    let back = super::core::get_target_code_generator(get_backend_name().as_str(), ast.clone())?;
    back.generate()
}

pub fn get_output_binary_name(arg_matches: &ArgMatches) -> &str {
    get_input_file_name(arg_matches).split('.').next().unwrap()
}

pub fn save_target_code(arg_matches: &ArgMatches, target_code: String) -> Result<String> {
    let bin = get_output_binary_name(arg_matches);
    let output_file_name = format!("./{}.c", bin);
    std::fs::write(output_file_name.clone(), target_code)?;

    Ok(output_file_name)
}
