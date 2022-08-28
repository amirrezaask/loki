#![allow(clippy::single_match)]

use std::collections::HashMap;

use anyhow::Result;
use anyhow::anyhow;
use rand::distributions::Alphanumeric;
use rand::distributions::DistString;
use serde::Serialize;

use crate::ast::{
    Ast, AstNode, AstNodeData, AstNodeType, AstOperation, AstTag, NodeID, Scope, ScopeID, ScopeType,
};

pub type FilePath = String;

// this struct will hold all data of your whole compilation.
#[derive(Debug, Clone, Serialize, Default)]
pub struct Context {
    pub nodes: HashMap<NodeID, AstNode>,
    pub scopes: Vec<Scope>,
    pub scope_stack: Vec<ScopeID>,
    pub scope_nodes: HashMap<ScopeID, Vec<NodeID>>,
    pub file_loads: HashMap<String, Vec<String>>,
    unknowns: Vec<NodeID>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            nodes: HashMap::new(),
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
            Some(n) => Ok(()),
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
        };
    }
    pub fn set_scope_owner(&mut self, scope_id: ScopeID, owner: NodeID) {
        let scope = &mut self.scopes.get_mut(scope_id as usize).unwrap();
        scope.owner_node = owner;
    }
    pub fn add_scope(&mut self, scope_ty: ScopeType, start: isize, end: isize) -> ScopeID {
        self.scopes.push(Scope {
            scope_type: scope_ty,
            owner_node: "".to_string(),
            parent: self.top_of_scope_stack(),
            start,
            end,
        });
        self.scope_stack.push((self.scopes.len() - 1) as isize);
        self.scope_nodes.insert(self.top_of_scope_stack(), vec![]);
        return (self.scopes.len() - 1) as isize;
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

    pub fn remove_node_from_scope(&mut self, node: &AstNode) -> Result<()> {
        let scope_nodes = self.scope_nodes.get_mut(&node.scope).unwrap();
        if scope_nodes.contains(&node.id) {
            let index = scope_nodes.iter().position(|x| x == &node.id).unwrap();
            scope_nodes.remove(index);
        }

        Ok(())
    }

    pub fn add_scope_to_node(&mut self, id: &NodeID, scope: ScopeID) {
        let mut node = self.nodes.get_mut(id).unwrap();
        node.scope = scope;
        let scope_nodes = self.scope_nodes.get_mut(&scope).unwrap();
        scope_nodes.push(id.clone());
    }

    pub fn add_scope_to_def_or_decl(&mut self, id: &NodeID, scope: ScopeID) -> Result<()> {
        let def_node = self.get_node(id.clone())?;
        let name = def_node.get_name_for_defs_and_decls(self).unwrap();
        self.add_scope_to_node(&name, scope.clone());
        Ok(())
    }

    pub fn add_tag(&mut self, id: &NodeID, tag: AstTag) {
        let node = self.nodes.get_mut(id).unwrap();
        node.tags.push(tag);
    }

    pub fn get_node(&self, id: NodeID) -> Result<AstNode> {
        match self.nodes.get(&id) {
            Some(n) => Ok(n.clone()),
            None => Err(anyhow!("node {} does not exist", id)),
        }
    }
    

    fn get_relevant_scopes(&self, scope_id: ScopeID) -> Vec<ScopeID> {
        let mut scope_id = scope_id;
        if scope_id <= 0 {
            return vec![];
        }
        let mut relevant_scopes: Vec<isize> = vec![scope_id];
        loop {
            if scope_id == -1 {
                break;
            }
            let scope = &self.scopes[scope_id as usize];
            if scope.parent != -1 {
                relevant_scopes.push(scope.parent);
            }
            scope_id = scope.parent;
        }
        return relevant_scopes;
    }

    fn get_file_root_scope_id(&self, file: &str) -> Result<ScopeID> {
        for (idx, scope) in self.scopes.iter().enumerate() {
            match scope.scope_type {
                ScopeType::File(ref name) => {
                    if name == file {
                        return Ok(idx as isize);
                    }
                }
                _ => {}
            }
        }

        Err(anyhow!("root scope of file {:?} not found ", file))
    }

    pub fn resolve_loads(&mut self, file: String, mut loads: Vec<String>) -> Result<()> {
        let file_root_scope_id = self.get_file_root_scope_id(&file)?;
        for load in &mut loads {
            let load_root_scope_id = self.get_file_root_scope_id(&load)?;
            let mut nodes = self.scope_nodes.get(&load_root_scope_id).unwrap().clone(); // TODO: filter just the nodes that matter
            self.scope_nodes
                .get_mut(&file_root_scope_id)
                .unwrap()
                .append(&mut nodes);
        }

        Ok(())
    }

    pub fn find_ident_ast_type(&self, ident: String, scope_id: ScopeID) -> Result<AstNodeType> {
        let scopes = self.get_relevant_scopes(scope_id);
        for scope in scopes {
            let nodes = self.scope_nodes.get(&scope).unwrap();
            for node_id in nodes.iter() {
                //TODO: check node is defined before the given ident
                let node = self.get_node(node_id.clone())?;
                match node.data {
                    AstNodeData::Def{mutable, name, expr} => {
                        let name_node = self.get_node(name.clone())?;
                        if name_node.get_ident()? == ident {
                            let def_expr = self.get_node(expr.clone())?;
                            if def_expr.is_unknown() {
                                continue;
                            }
                            return Ok(def_expr.infered_type);
                        }
                    }
                    AstNodeData::Ident(ref name) => {
                        if node.infered_type.is_unknown() {
                            continue;
                        }
                        if ident == name.clone() {
                            return Ok(node.infered_type.clone());
                        }
                    }
                    AstNodeData::Decl(ref name_id) => {
                        let name_node = self.get_node(name_id.clone())?;
                        if node.infered_type.is_unknown() {
                            continue;
                        }
                        if name_node.get_ident()?.clone() == ident {
                            return Ok(node.infered_type.clone());
                        }
                    }

                    _ => {}
                }
            }
        }

        return Ok(AstNodeType::Unknown);
    }

    fn is_defined(&self, ident: String) -> Result<bool> {
        for (node_id, node) in self.nodes.iter() {
            match node.data {
                AstNodeData::Decl(ref id) => {
                    let ident_node = self.get_node(id.clone())?;
                    if ident_node.get_ident()? == ident {
                        return Ok(true);
                    }
                }

                AstNodeData::Def{mutable, name: ref name, expr: ref expr} => {
                    let ident_node = self.get_node(name.clone())?;
                    if ident_node.get_ident()? == ident {
                        return Ok(true);
                    }
                }

                _ => {}
            }
        }

        return Ok(false);
    }

    fn infer_types(&mut self) -> Result<()> {
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
                if !self.is_defined(ident.clone())? {
                    return Err(anyhow!("In file {} undeclared ident: {} at line: {} column: {}", ident, unknown_node.filename, unknown_node.line, unknown_node.col));
                }
                let ty = self.find_ident_ast_type(ident.clone(), unknown_node.scope)?;
                if ty.is_unknown() {
                    self.add_unknown(&unknown_id);
                    continue;
                }
                self.add_type_inference(&unknown_id, ty.clone());
                continue;
            }

            if let AstNodeData::Deref(ref object) = unknown_node.data {
                let pointer = self.get_node(object.clone())?;
                if pointer.infered_type.is_unknown() {
                    self.add_unknown(&object.clone());
                    self.add_unknown(&unknown_id);
                    continue;
                } else {
                    if !pointer.infered_type.is_pointer() {
                        return Err(anyhow!("In file {} dereferencing operation needs a pointer but got: {:?} at line: {} column: {}", unknown_node.filename, pointer.infered_type, unknown_node.line, unknown_node.col))
                    }
                    self.add_type_inference(
                        &unknown_id,
                        pointer.infered_type.get_pointer_pointee()?,
                    );
                    continue;
                }
            }

            if let AstNodeData::PointerTo(ref object) = unknown_node.data {
                let pointee = self.get_node(object.clone())?;
                if pointee.infered_type.is_unknown() {
                    self.add_unknown(&object.clone());
                    self.add_unknown(&unknown_id);
                    continue;
                } else {
                    self.add_type_inference(
                        &unknown_id,
                        AstNodeType::Pointer(Box::new(pointee.infered_type)),
                    );
                    continue;
                }
            }
            if let AstNodeData::NamespaceAccess{ namespace: namespace_id, field: field_id } = &unknown_node.data {
                let namespace_id = namespace_id.clone().to_owned();
                let field_id = field_id.clone().to_owned();
                // let namespace_access = ns.clone();
                let namespace = self.get_node(namespace_id.clone())?;
                let mut namespace_ty = namespace.infered_type.clone();
                if namespace_ty.is_unknown() {
                    namespace_ty =
                        self.find_ident_ast_type(namespace.get_ident()?, unknown_node.scope)?;
                }

                let field = self.get_node(field_id.clone())?; // TODO: need scope in this one
                let mut field_ty = field.infered_type.clone();
                if field_ty.is_unknown() {
                    field_ty = self.find_ident_ast_type(field.get_ident()?, unknown_node.scope)?;
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

                self.add_type_inference(&namespace_id, namespace_ty);
                self.add_type_inference(&field_id.clone(), field_ty.clone());
                self.add_type_inference(&unknown_id, field_ty.clone());
                continue;
            }
            if let AstNodeData::BinaryOperation {ref operation, ref left, ref right } = unknown_node.data {
                match operation {
                    AstOperation::Equal
                    | AstOperation::NotEqual
                    | AstOperation::Greater
                    | AstOperation::GreaterEqual
                    | AstOperation::Less
                    | AstOperation::BinaryAnd
                    | AstOperation::BinaryOr
                    | AstOperation::LessEqual => {
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

            // TODO: Initialize, InitializeArray, FnCall
        }

        Ok(())
    }
    fn type_check(&mut self) -> Result<()> {
        Ok(())
    }
    fn lower_initialize(&mut self, mut ast: &Ast) -> Result<()> {
        Ok(())
    }

    fn lower_enum(&mut self, mut ast: &mut Ast) -> Result<()> {
        for node_id in &ast.top_level {
            

        }
        Ok(())
    }

    fn sema_check(&self, mut ast: &Ast) -> Result<()> {
        Ok(())
    }
    
    pub fn process(&mut self, mut asts: Vec<Ast>) -> Result<Vec<Ast>> {
        self.infer_types()?;
        self.type_check()?;
        for mut ast in &mut asts {
            self.sema_check(&ast)?;
            self.lower_enum(&mut ast)?;
            self.lower_initialize(&ast)?;
        }

        Ok(asts)
    }
}
