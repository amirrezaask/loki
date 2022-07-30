use super::parser::{Node, AST};
use anyhow::Result;

pub mod c;

pub trait Repr {
    fn repr(node: &Node) -> Result<String>;
}

pub enum Backend {
    C,
    Go,
    JS,
    Loki,
}

pub fn generate(backend: Backend, ast: AST) -> Result<String> {
    match backend {
        Backend::C => {
            let mut c_code_gen = c::C::new(ast);
            c_code_gen.generate()
        }

        _ => {
            panic!("not implemented yet");
        }
    }
}
