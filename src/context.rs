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
}

impl Context {
    pub fn new() -> Self {
        Self {
            nodes: HashMap::new(),
        }
    }

    // on duplicate node id error.
    pub fn add_node(&mut self, node: AstNode) -> Result<()> {
        match self.nodes.insert(node.id.clone(), node) {
            None => Ok(()),
            Some(n) => Ok(()),
        }
    }

    

    pub fn add_type_inference(&mut self, id: &NodeID, type_infer: AstNodeType) {
        let mut node = self.nodes.get_mut(id).unwrap();
        node.infered_type = type_infer;
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
}

