use crate::parser::{module, Node};
use anyhow::Result;

fn parse_module(path: &str) -> Result<Node> {
    let (_, parsed_module) = module(std::fs::read_to_string(path)?)?;
    Ok(parsed_module)
}

pub fn include_imports(mut ast: Node) -> Result<Node> {
    match ast {
        Node::Block(ref mut stmts) => {
            for stmt in stmts.iter_mut() {
                if let Node::Import(i) = stmt {
                    if let Node::Str(path) = &i.path {
                        if !path.contains("c:") {
                            let m = parse_module(path.as_str())?;
                            *stmt = m;
                        }
                    } else {
                        unreachable!()
                    }
                }
            }
        }
        _ => unreachable!(),
    }
    Ok(ast)
}
