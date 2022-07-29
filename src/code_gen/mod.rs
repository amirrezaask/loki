use super::parser::AST;
use anyhow::Result;

mod c;

pub enum Backend {
    C,
    Go,
    JS,
    Loki,
}

pub fn generate(backend: Backend, ast: &AST) -> Result<String> {
    match backend {
        Backend::C => c::generate(ast),

        _ => {
            panic!("not implemented yet");
        }
    }
}
