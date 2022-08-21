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
        if node.is_unknown() {
            self.unknowns.push(node.id.clone());
        }

        
        match self.nodes.insert(node.id.clone(), node) {
            None => Ok(()),
            Some(n) => Ok(())
        }
    }
    fn add_unknown(&mut self, id: &NodeID) {
        if !self.unknowns.contains(&id) {
            self.unknowns.push(id.clone());
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
        for (_, node) in self.nodes.iter() {
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

        return AstNodeType::Unknown;
    }

    pub fn infer_types(&mut self) -> Result<()> {
        println!("unknowns => {:?}", self.unknowns);
        //@Refactor: possible usage of Queue like datastructure will make this code much simpler.
        loop {
            if self.unknowns.is_empty() {
                break;
            }
            for (idx, unknown_id) in self.unknowns.clone().iter().enumerate() {
                let unknown_node = self.nodes.get(unknown_id).unwrap();
                self.unknowns.remove(idx);
                if !unknown_node.infered_type.is_unknown() {
                    break;
                }
                if let AstNodeData::Ident(ref ident) = unknown_node.data {
                    let ty = self.find_ident_ast_type(ident.clone());
                    if ty.is_unknown() {
                        self.add_unknown(unknown_id);
                        break;
                    }
                    self.add_type_inference(unknown_id, ty.clone());
                    break
                }

                if let AstNodeData::PointerTo(ref object) = unknown_node.data {
                    let pointee = self.get_node(object.clone());
                    if pointee.infered_type.is_unknown() {
                        self.add_unknown(&object.clone());
                        self.add_unknown(unknown_id);
                        break;
                    } else {
                        self.add_type_inference(unknown_id, AstNodeType::Pointer(Box::new(pointee.infered_type)));
                        break
                    }
                }

                // if let AstNodeData::NamespaceAccess(ref ns) = unknown_node.data {
                //     let namespace_access = ns.clone();
                //     let namespace = self.get_node(namespace_access.namespace.clone());
                //     let field = self.get_node(namespace_access.field.clone()); // TODO: need scope in this one
                //     if namespace.is_unknown() {
                //         self.add_unknown(namespace.id.clone());
                //         self.add_unknown(unknown_id.clone());
                //         break;
                //     }
                //     if field.is_unknown() {
                //         self.add_unknown(field.id.clone());
                //         self.add_unknown(unknown_id.clone());
                //         break;
                //     }
                //     self.add_type_inference(&namespace_access.namespace, namespace.infered_type);
                //     self.add_type_inference(&namespace_access.field, field.infered_type);


                // }
            }
        }


        println!("unknowns: {:?}", self.unknowns);


       


        Ok(())
    }
}