#![allow(clippy::single_match)]

use std::collections::HashMap;

use serde::Serialize;
use anyhow::Result;


use crate::ast::{AstNode, NodeID, AstNodeType, AstNodeData, AstTag, AstOperation};

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

        if node.infered_type.is_unknown() {
            self.unknowns.push(node.id.clone());
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
                    let name_node = self.get_node(def.name.clone());
                    if name_node.get_ident() == ident {
                        let def_expr = self.get_node(def.expr.clone());
                        if def_expr.is_unknown() {
                            continue;
                        }
                        return def_expr.infered_type;
                    }
                }
                AstNodeData::Ident(ref name) => {
                    if node.infered_type.is_unknown() {
                        continue;
                    }
                    if ident == name.clone() {
                        return node.infered_type.clone();
                    }
                }
                AstNodeData::Decl(ref name_id) => {
                    let name_node = self.get_node(name_id.clone());
                    if node.infered_type.is_unknown() {
                        continue;
                    }
                    if name_node.get_ident().clone() == ident {
                        return node.infered_type.clone();
                    }
                }

                _ => {}
            }
        }

        return AstNodeType::Unknown;
    }

    pub fn infer_types(&mut self) -> Result<()> {
        loop {
            if self.unknowns.is_empty() {
                break;
            }
            let unknown_id = self.unknowns.get(0).unwrap().clone(); // treating like a queue, always get first element, remove it and process it.
            let unknown_node = self.nodes.get(&unknown_id).unwrap();
            self.unknowns.remove(0);
            if !unknown_node.infered_type.is_unknown() {
                continue;
            }
            if let AstNodeData::Ident(ref ident) = unknown_node.data {
                let ty = self.find_ident_ast_type(ident.clone());
                if ty.is_unknown() {
                    self.add_unknown(&unknown_id);
                    continue;
                }
                self.add_type_inference(&unknown_id, ty.clone());
                continue
            }
            
            if let AstNodeData::Deref(ref object) = unknown_node.data {
                let pointer = self.get_node(object.clone());
                if pointer.infered_type.is_unknown() {
                    self.add_unknown(&object.clone());
                    self.add_unknown(&unknown_id);
                    continue;
                } else {
                    if !pointer.infered_type.is_pointer() {
                        panic!("deref needs a pointer: {:?}", pointer);
                    }
                    self.add_type_inference(&unknown_id, pointer.infered_type.get_pointer_pointee());
                    continue
                }
            }

            if let AstNodeData::PointerTo(ref object) = unknown_node.data {
                let pointee = self.get_node(object.clone());
                if pointee.infered_type.is_unknown() {
                    self.add_unknown(&object.clone());
                    self.add_unknown(&unknown_id);
                    continue;
                } else {
                    self.add_type_inference(&unknown_id, AstNodeType::Pointer(Box::new(pointee.infered_type)));
                    continue
                }
            }

            if let AstNodeData::NamespaceAccess(ref ns) = unknown_node.data {
                let namespace_access = ns.clone();
                let namespace = self.get_node(namespace_access.namespace.clone());
                let mut namespace_ty = namespace.infered_type.clone();
                if namespace_ty.is_unknown() {
                    namespace_ty = self.find_ident_ast_type(namespace.get_ident());
                }

                let field = self.get_node(namespace_access.field.clone()); // TODO: need scope in this one
                let mut field_ty = field.infered_type.clone();
                if field_ty.is_unknown() {
                    field_ty = self.find_ident_ast_type(field.get_ident());
                }
                if namespace_ty.is_unknown() {
                    self.add_unknown(&namespace.id.clone());
                    self.add_unknown(&unknown_id.clone());
                }
                
                if field_ty.is_unknown() {
                    self.add_unknown(&field.id.clone());
                    self.add_unknown(&unknown_id.clone());
                }

                if namespace_ty.is_unknown() || field_ty.is_unknown() {
                    continue;
                }

                self.add_type_inference(&namespace_access.namespace, namespace_ty);
                self.add_type_inference(&namespace_access.field, field_ty.clone());
                self.add_type_inference(&unknown_id, field_ty.clone());
                continue;

            }
            if let AstNodeData::BinaryOperation(ref bop) = unknown_node.data {
                match bop.operation {
                    AstOperation::Equal 
                    | AstOperation::NotEqual
                    | AstOperation::Greater
                    | AstOperation::GreaterEqual
                    | AstOperation::Less
                    | AstOperation::LessEqual 
                    
                    => {
                        self.add_type_inference(&unknown_id, AstNodeType::Bool);
                    }
                    AstOperation::Sum => {
                        // we should check both sides, first they should be same type
                        // then we use their type as node type.
                    }
                    AstOperation::Divide => {}
                    AstOperation::Modulu => {}
                    AstOperation::Subtract => {}
                    AstOperation::Multiply => {}
                }
            }

            // TODO: Initialize, InitializeArray, FnCall, BinaryOperation
        }


        // println!("unknowns: {:?}", self.unknowns);


       


        Ok(())
    }
}
