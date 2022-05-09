use anyhow::Result;

use crate::parser::Node;
pub trait CodeGen {
    fn generate(node: Node) -> Result<String>;
}
