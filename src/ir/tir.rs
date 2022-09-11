use std::collections::HashMap;

use serde::Serialize;
use anyhow::Result;

use super::hir::HIR;
use super::hir::Index;
use super::hir::AstTag;
use super::hir::Node;
use super::hir::NodeData;
use super::lexer::Token;

pub struct TypedNode {
    pub id: Index,
    pub data: NodeData,
    pub type_information: Type,
    pub parent_block: Index,
    pub tags: Vec<AstTag>,

    pub line: usize,
    pub col: usize,
    pub filename: String,
}

type BitSize = i64;
#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum Type {
    Unknown,
    NoType,

    SignedInt(BitSize),
    UnsignedInt(BitSize),
    Float(BitSize),

    Bool,

    IntPtr,
    UintPtr,

    Char,
    String,

    Array(u64, Box<Type>),
    DynamicArray(Box<Type>),

    Initialize(Box<Type>),

    Struct {
        fields: Vec<(String, Type)>, // name: type
    },
    Enum {
        variants: Vec<String>,
    },

    TypeRef {
        name: String,
        actual_ty: Box<Type>,
    },

    Pointer(Box<Type>),

    FnType(Vec<Type>, Box<Type>),

    NamespaceAccess {
        ns: Box<Type>,
        field: Box<Type>
    },

    CVarArgs,
    CString,

    Void,
}

pub struct TypedIR {
    pub filename: String,
    pub tokens: Vec<Token>,
    pub root: Index,
    pub nodes: HashMap<Index, TypedNode>,
    untyped_nodes: HashMap<Index, Node>,
}


impl TypedIR {
    pub fn type_root(&mut self, root_index: Index) -> Result<()> {
        

        unimplemented!()
    }




    pub fn new(ir: HIR) -> Result<Self> {
        let mut tir = TypedIR {
            filename: ir.filename,
            tokens: ir.tokens.clone(),
            root: 0,
            nodes: HashMap::new(),
            untyped_nodes: ir.nodes,
        };
        tir.type_root(ir.root)?;
        Ok(tir)
    }
}