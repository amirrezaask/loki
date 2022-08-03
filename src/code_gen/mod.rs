use anyhow::Result;
use crate::ast::*;

pub mod cpp;

pub trait Repr {
    fn repr(node: &Node) -> Result<String>;
}

pub enum Backend {
    CPP,
}
