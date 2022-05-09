use anyhow::Result;

use crate::parser::Node;

pub mod c;

pub trait CodeGen {
    fn generate(&self) -> Result<String>;
}

pub trait Repr<T> where T: CodeGen {
    fn repr(&self) -> String;
}
