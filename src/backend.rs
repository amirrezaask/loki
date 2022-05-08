use anyhow::Result;

use crate::parser::ASTNode;
pub trait CodeGen {
    fn generate(node: ASTNode) -> Result<String>;
}
