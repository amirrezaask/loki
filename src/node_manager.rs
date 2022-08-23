#![allow(clippy::single_match)]

use std::collections::HashMap;

use serde::Serialize;
use anyhow::Result;


use crate::ast::{AstNode, NodeID, AstNodeType, AstNodeData, AstTag, AstOperation, Scope, ScopeID, ScopeType};

pub type FilePath = String;

// this struct will hold all data of your whole compilation.
#[derive(Debug, PartialEq, Clone, Serialize, Default)]
pub struct AstNodeManager {
    pub nodes: HashMap<NodeID, AstNode>,
    pub scopes: Vec<Scope>,
    pub scope_stack: Vec<ScopeID>,
    pub scope_nodes: HashMap<ScopeID, Vec<NodeID>>,
    pub file_loads: HashMap<String, Vec<String>>,
    unknowns: Vec<NodeID>,
}

impl AstNodeManager {
    pub fn new() -> Self {
        Self { nodes: HashMap::new(),
               unknowns: vec![],
               scopes: vec![],
               scope_stack: Vec::new(),
               scope_nodes: HashMap::new(),
               file_loads: HashMap::new(),
        }
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

    pub fn push_to_scope_stack(&mut self, id: ScopeID) {
        self.scope_stack.push(id);
    }

    pub fn remove_from_scope_stack(&mut self) {
        self.scope_stack.pop();
    }

    pub fn top_of_scope_stack(&self) -> ScopeID {
        return match self.scope_stack.last() {
            Some(i) => *i,
            None => -1,
        }
    }
    pub fn add_scope(&mut self, scope_ty: ScopeType, start: isize, end: isize) -> ScopeID {
        self.scopes.push(Scope { scope_type: scope_ty, parent: self.top_of_scope_stack(), start, end });
        self.scope_stack.push((self.scopes.len()-1) as isize);
        self.scope_nodes.insert(self.top_of_scope_stack(), vec![]);
        return (self.scopes.len()-1) as isize;
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

    pub fn add_scope_to_node(&mut self, id: &NodeID, scope: ScopeID) {
        let mut node = self.nodes.get_mut(id).unwrap();
        node.scope = scope;
        let scope_nodes = self.scope_nodes.get_mut(&scope).unwrap();
        scope_nodes.push(id.clone());
    }

    pub fn add_scope_to_def_or_decl(&mut self, id: &NodeID, scope: ScopeID) {
        let def_node = self.get_node(id.clone());
        let name = def_node.get_name_for_defs_and_decls(self).unwrap();
        self.add_scope_to_node(&name, scope.clone());
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

    fn get_relevant_scopes(&self, scope_id: ScopeID) -> Vec<ScopeID> {
        let mut scope_id = scope_id;
        if scope_id <= 0 {
            return vec![scope_id];
        }
        let mut relevant_scopes: Vec<isize> = vec![scope_id];
        loop {
            if scope_id == -1 {
                break;
            }
            let scope = &self.scopes[scope_id as usize];
            relevant_scopes.push(scope.parent);
            scope_id = scope.parent;
        }

        return relevant_scopes;
    }

    fn get_file_root_scope_id(&self, file: &str) -> ScopeID {
        for (idx, scope) in self.scopes.iter().enumerate() {
            match scope.scope_type {
                ScopeType::File(ref name) => {
                    if name == file {
                        return idx as isize;
                    }
                }
                _ => {}
            }
        }

        panic!("root scope of file {:?} not found ", file);
    }
    
    pub fn resolve_loads(&mut self, file: String, mut loads: Vec<String>) {
        let file_root_scope_id = self.get_file_root_scope_id(&file);
        for load in &mut loads {
            let load_root_scope_id = self.get_file_root_scope_id(&load);
            let mut nodes = self.scope_nodes.get(&load_root_scope_id).unwrap().clone(); // TODO: filter just the nodes that matter
            self.scope_nodes.get_mut(&file_root_scope_id).unwrap().append(&mut nodes);
        }
    }

    pub fn find_ident_ast_type(&self, ident: String, scope_id: ScopeID) -> AstNodeType {
        let scopes = self.get_relevant_scopes(scope_id);
        for scope in scopes {
            let nodes = self.scope_nodes.get(&scope).unwrap();
            for node_id in nodes.iter() {
                //TODO: check node is defined before the given ident
                let node = self.get_node(node_id.clone());
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
                let ty = self.find_ident_ast_type(ident.clone(), unknown_node.scope);
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
                    namespace_ty = self.find_ident_ast_type(namespace.get_ident(), unknown_node.scope);
                }

                let field = self.get_node(namespace_access.field.clone()); // TODO: need scope in this one
                let mut field_ty = field.infered_type.clone();
                if field_ty.is_unknown() {
                    field_ty = self.find_ident_ast_type(field.get_ident(), unknown_node.scope);
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
