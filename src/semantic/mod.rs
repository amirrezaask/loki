use std::collections::HashMap;
use anyhow::Result;

use super::parser::{ Node, AST };


struct SymbolTable {
    ast: AST,
    table: HashMap<String, Node>,
}

impl SymbolTable {
    pub fn new(ast: AST) -> Result<Self> {
        let mut table = HashMap::<String, Node>::new();
        for node in ast.top_level.iter() {
            match node {
                Node::Decl(decl) => {
                    let name_str: String = if let Node::Ident(idx) = *decl.name {
                        let src_range = &ast.tokens[idx];
                        let name_str: String = ast.src[src_range.loc.0..=src_range.loc.1].to_string();
                        name_str
                    } else {
                        unreachable!();
                    };

                    
                    table.insert(name_str, node.clone());
                }

                _ => {
                    unreachable!();
                }
            }
        }

        Ok(Self {
            table, ast
        })
    }
   
}


