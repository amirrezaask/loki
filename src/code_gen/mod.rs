use super::parser::{Node, AST};
use anyhow::Result;

pub mod cpp;

pub trait Repr {
    fn repr(node: &Node) -> Result<String>;
}

pub enum Backend {
    CPP,
    Go,
    JS,
    Loki,
}

pub fn generate(backend: Backend, ast: AST) -> Result<String> {
    match backend {
        Backend::CPP => {
            let mut c_code_gen = cpp::CPP::new(ast);
            c_code_gen.generate()
        }

        _ => {
            panic!("not implemented yet");
        }
    }
}
