use anyhow::Result;
use crate::ast::*;

pub mod cpp;

pub trait Repr {
    fn repr(node: &AstNode) -> Result<String>;
}

pub enum Backend {
    CPP,
}
