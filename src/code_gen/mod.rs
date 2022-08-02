use super::parser::{Node, AST};
use anyhow::Result;

pub mod cpp;

pub trait Repr {
    fn repr(node: &Node) -> Result<String>;
}

pub enum Backend {
    CPP,
}
