#![allow(clippy::single_match)]

use std::{collections::HashMap, cell::RefCell};

use serde::Serialize;
use anyhow::Result;


use crate::ast::{AstNode, NodeID, AstNodeType, AstNodeData};

// this struct will hold all nodes data of your whole compilation.
#[derive(Debug, PartialEq, Clone, Serialize, Default)]
pub struct AstNodeManager {
    pub nodes: HashMap<NodeID, AstNode>,
    unknowns: HashMap<NodeID, ()>,
}

impl AstNodeManager {
    pub fn new() -> Self {
        Self { nodes: HashMap::new(), unknowns: HashMap::new() }
    }

    // on duplicate node id error.
    pub fn add_node(&mut self, node: AstNode) -> Result<()> {
        if node.infered_type == AstNodeType::Unknown {
            self.unknowns.insert(node.id.clone(), ());
        }
        
        match self.nodes.insert(node.id.clone(), node) {
            None => Ok(()),
            Some(n) => Ok(())
        }
    }

    pub fn get_node(&self, id: NodeID) -> AstNode {
        match self.nodes.get(&id) {
            Some(n) => n.clone(),
            None => panic!("node {} does not exist", id),
        }
    }

    pub fn find_ident_ast_type(&self, ident: String) -> (NodeID, AstNodeType) {
        for (node_id, node) in self.nodes.iter() {
            match node.data {
                AstNodeData::Def(ref def) => {
                    if def.name == ident {
                        let def_expr = self.get_node(def.expr.clone());
                        return (def.expr.clone(), def_expr.infered_type);
                    }
                }
                AstNodeData::Decl(ref name) => {
                    if name.clone() == ident {
                        return (node.id.clone(), node.infered_type.clone());
                    }
                }

                _ => {}
            }
        }

        panic!("undeclared identifier: {:?}", ident);
    }

    pub fn infer_types(&mut self) -> Result<()> {
        for (unknown_id, _) in self.unknowns.clone() {
            let mut unknown_node = self.nodes.get(&unknown_id).unwrap().clone();
            match unknown_node.data {
                AstNodeData::Ident(ref ident) => {
                    let (ref_id, ty) = self.find_ident_ast_type(ident.clone());
                   
                    if ty == AstNodeType::Unknown {
                        /*
                            a := 1
                            b := a
                            c := b
                         */
                        self.unknowns.insert(ref_id, ());
                        self.unknowns.insert(unknown_id.clone(), ());
                        continue;
                    }
                    unknown_node.infered_type = ty;
                    self.add_node(unknown_node)?;

                }

                _ => {}
            }
        }


        Ok(())
    }
}