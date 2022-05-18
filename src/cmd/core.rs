use crate::backend::{c::C, CodeGen, Compiler};
use crate::parser;
use clap::{ArgMatches, Arg, Command, arg};
use anyhow::Result;

pub fn get_backend(backend: &str, node: parser::Node) -> Result<impl CodeGen> {
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