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

    pub fn find_ident_def(&self, ident: String) -> NodeID {
        for (node_id, node) in self.nodes.iter() {
            match node.data {
                AstNodeData::Def(ref def) => {
                    if def.name == ident {
                        return node_id.clone();
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
                    let def_id = self.find_ident_def(ident.clone());
                    let def = self.get_node(def_id);
                    if !def.is_def() {
                        panic!("nemidoonam");
                    }
                    let def_expr_id = def.get_def().expr;
                    let def_expr = self.get_node(def_expr_id.clone());
                    if def_expr.infered_type == AstNodeType::Unknown {
                        /*
                            a := 1
                            b := a
                            c := b
                         */
                        self.unknowns.insert(def_expr_id, ());
                        self.unknowns.insert(unknown_id.clone(), ());
                        continue;
                    }
                    unknown_node.infered_type = def_expr.infered_type;
                    self.add_node(unknown_node)?;

                }

                _ => {}
            }
        }


        Ok(())
    }
}