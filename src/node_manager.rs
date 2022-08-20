#![allow(clippy::single_match)]

use std::{collections::HashMap, cell::RefCell};

use serde::Serialize;
use anyhow::Result;


use crate::ast::{AstNode, NodeID, AstNodeType, AstNodeData, AstTag};

// this struct will hold all nodes data of your whole compilation.
#[derive(Debug, PartialEq, Clone, Serialize, Default)]
pub struct AstNodeManager {
    pub nodes: HashMap<NodeID, AstNode>,
    unknowns: Vec<NodeID>,
}

impl AstNodeManager {
    pub fn new() -> Self {
        Self { nodes: HashMap::new(), unknowns: vec![] }
    }

    // on duplicate node id error.
    pub fn add_node(&mut self, node: AstNode) -> Result<()> {
        if node.infered_type == AstNodeType::Unknown {
            self.unknowns.push(node.id.clone());
        }
        
        match self.nodes.insert(node.id.clone(), node) {
            None => Ok(()),
            Some(n) => Ok(())
        }
    }
    fn add_unknown(&mut self, id: NodeID) {
        if !self.unknowns.contains(&id) {
            self.unknowns.push(id);
        }
    }

    pub fn add_type_inference(&mut self, id: &NodeID, type_infer: AstNodeType) {
        let mut node = self.nodes.get_mut(id).unwrap();
        node.infered_type = type_infer;

        match self.unknowns.iter().position(|i| i.clone() == id.clone()) {
            Some(idx) => {
                self.unknowns.remove(idx);
            }
            None => {}
        }
    }

    pub fn add_tag(&mut self, id: &NodeID, tag: AstTag) {
        let node = self.nodes.get_mut(id).unwrap();
        node.tags.push(tag);

    }

    pub fn get_node(&self, id: NodeID) -> AstNode {
        match self.nodes.get(&id) {
            Some(n) => n.clone(),
            None => panic!("node {} does not exist", id),
        }
    }

    pub fn find_ident_ast_type(&self, ident: String) -> AstNodeType {
        for (node_id, node) in self.nodes.iter() {
            match node.data {
                AstNodeData::Def(ref def) => {
                    if def.name == ident {
                        let def_expr = self.get_node(def.expr.clone());
                        if def_expr.infered_type == AstNodeType::Unknown {
                            continue;
                        }
                        return def_expr.infered_type;
                    }
                }
                AstNodeData::Ident(ref name) => {
                    if node.infered_type == AstNodeType::Unknown {
                        continue;
                    }
                    if ident == name.clone() {
                        return node.infered_type.clone();
                    }
                }
                AstNodeData::Decl(ref name) => {
                    if node.infered_type == AstNodeType::Unknown {
                        continue;
                    }
                    if name.clone() == ident {
                        return node.infered_type.clone();
                    }
                }

                _ => {}
            }
        }

        panic!("undeclared identifier: {:?}", ident);
    }

    pub fn infer_types(&mut self) -> Result<()> {
        let mut counter = 0;
        loop {
            // if counter > 100 {
            //     break;
            // }
            if self.unknowns.is_empty() {
                break;
            }
            counter += 1;
            for (idx, unknown_id) in self.unknowns.clone().iter().enumerate() {
                let unknown_node = self.nodes.get(unknown_id).unwrap();
                self.unknowns.remove(idx);
                if unknown_node.infered_type != AstNodeType::Unknown {
                    break;
                }
                if let AstNodeData::Ident(ref ident) = unknown_node.data {
                    // println!("lookiing for {:?}", unknown_node);
                    let ty = self.find_ident_ast_type(ident.clone());
                    self.add_type_inference(unknown_id, ty.clone());
                    break
                }
            }
        }


        println!("unknowns: {:?}", self.unknowns);


       


        Ok(())
    }
}