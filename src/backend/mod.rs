use anyhow::Result;

use crate::parser::Node;

pub mod c;

pub trait CodeGen {
    fn generate(node: Node) -> Result<String>;
}
