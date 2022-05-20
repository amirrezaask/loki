mod type_infer;
mod imports;

pub use imports::*;
pub use type_infer::*;

use anyhow::Result;
use crate::parser;

pub struct Passes;

impl Passes {
    pub fn new() -> Self {
        Self
    }

    pub fn run(&self, mut ast: parser::Node) -> Result<parser::Node> {
        ast = self.include_imports(ast)?;

        Ok(ast)
    }
}