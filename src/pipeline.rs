use crate::{parser, backend::CodeGen, backend::Compiler};
use anyhow::Result;

pub struct Pipeline<C> where C: Compiler + CodeGen {
    passes: Vec<Box<dyn Fn(parser::Node) -> Result<parser::Node>>>,
    backend: C,
}
