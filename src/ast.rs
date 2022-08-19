use std::{collections::HashMap};

use crate::node_manager::AstNodeManager;
use crate::lexer::{Token, TokenType};
use anyhow::Result;
use serde::Serialize;
pub type NodeID = String;


#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum AstNodeType {
    Unknown,
    NoType,
    SignedInt(BitSize),
    UnsignedInt(BitSize),
    Float(BitSize),

    Bool,

    Char,
    String,

    Array(i8, Box<AstNodeType>),
    DynamicArray(Box<AstNodeType>),

    TypeName(String),

    Initialize(Box<AstNodeType>),

    TypeDefStruct,
    TypeDefEnum,
    TypeDefUnion,

    Pointer(Box<AstNodeType>),
    Deref(Box<AstNodeType>),

    FnType(Vec<AstNodeType>, Box<AstNodeType>),

    Void,

}

impl AstNodeType {
    pub fn new(node: &AstNode) -> Self {
        match node.data {
            AstNodeData::Ident(ref ident) => {
                return AstNodeType::TypeName(ident.clone());
            }
            
            AstNodeData::UintTy(bitsize) => AstNodeType::UnsignedInt(bitsize),
            AstNodeData::IntTy(bitsize) => AstNodeType::SignedInt(bitsize),
            AstNodeData::CharTy => AstNodeType::Char,
            AstNodeData::StringTy => AstNodeType::String,
            AstNodeData::BoolTy => AstNodeType::Bool,
            AstNodeData::FloatTy(bitsize) => AstNodeType::Float(bitsize),
            AstNodeData::VoidTy => AstNodeType::Void,
            _ => {
                AstNodeType::Unknown
            }

        }
    }
    pub fn make_fn_signature(node_manager: &AstNodeManager, args: &Vec<NodeID>, ret: &AstNode) -> Self {
        
        let args_ty: Vec<AstNodeType> = args.iter().map(|arg| {
            let arg = node_manager.get_node(arg.clone());
            arg.infered_type.clone()
        } ).collect();

        return AstNodeType::FnType(args_ty, Box::new(AstNodeType::new(&ret)));
    }
    pub fn is_type_def(&self) -> bool {
        match self {
            AstNodeType::TypeDefStruct | AstNodeType::TypeDefUnion | AstNodeType::TypeDefEnum => true,
            _ => return false
        }
    }

    pub fn is_unknown(&self) -> bool {
        match self {
            AstNodeType::Unknown => true,
            _ => return false
        }

    }
    pub fn is_fn_def(&self) -> bool {
        match self {
            AstNodeType::FnType(_, _) => true,
            _ => return false
        }
    }
    pub fn get_fn_ret_ty(&self) -> AstNodeType {
        match self {
            AstNodeType::FnType(_, sign) => *(sign.clone()),
            _ => unreachable!()
        }
    }
    pub fn is_type_def_enum(&self) -> bool {
        match self {
            AstNodeType::TypeDefEnum => true,
            _ => return false
        }
    }
}
#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum AstTag {
    Foreign,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct NamespaceAccess {
    pub namespace: NodeID,
    pub field: NodeID,
    pub namespace_is_enum: bool,
    pub namespace_is_pointer: bool,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct AstFnSignature {
    pub args: Vec<NodeID>,
    pub ret: NodeID
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct AstFnDef {
    pub sign: AstFnSignature,
    pub body: Vec<NodeID>,    
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum AstOperation {
    Sum,
    Subtract,
    Divide,
    Modulu,
    Multiply,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Equal,
    NotEqual,
    MemberAcces,
}
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct AstBinaryOperation {
    pub operation: AstOperation,
    pub left: NodeID,
    pub right: NodeID
}
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct AstCaseBlock {
    pub cases: Vec<(NodeID, Vec<NodeID>)>,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct AstDef {
    pub mutable: bool,
    pub name: String,
    pub expr: NodeID,
}

pub type BitSize = usize;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct AstNode {
    pub id: NodeID,
    pub data: AstNodeData,
    pub infered_type: AstNodeType,
    pub tags: Vec<AstTag>
}



#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum AstNodeData {
    //top level items
    CompilerFlags(String),
    Load(String),
    Host(String),
    Def(AstDef),
    Decl(String),
    Assign(NodeID, NodeID),

    // Type defs
    IntTy(BitSize), // bitsize
    UintTy(BitSize), 

    FloatTy(BitSize),
    BoolTy,
    StringTy,
    CharTy,
    VoidTy,
    ArrayTy(u64, AstNodeType), // len ty
    FnType(AstFnSignature),


    Struct(Vec<NodeID>),
    Enum(bool, Vec<(NodeID, Option<NodeID>)>),

    //Expressions
    Uint(u64),
    Int(i64),
    StringLiteral(String),
    Float(f64),
    Bool(bool),
    Char(char),
    Ident(String),
    
    BinaryOperation(AstBinaryOperation),

    NamespaceAccess(NamespaceAccess),
    
    Initialize(NodeID, Vec<(NodeID, NodeID)>),
    InitializeArray(Option<NodeID>, Vec<NodeID>),
    
    FnDef(AstFnDef),
    FnCall(NodeID, Vec<NodeID>),
    
    PointerTo(NodeID),
    Deref(NodeID),
    
    If(AstCaseBlock),
    
    For(NodeID, NodeID, NodeID, Vec<NodeID>),
    ForIn(NodeID, NodeID, Vec<NodeID>),
    While(NodeID, Vec<NodeID>),
    
    Break,
    Continue,

    Return(NodeID),
}

impl AstNode {
    pub fn get_ident(&self) -> String {
        if let AstNodeData::Ident(ident) = &self.data {
            return ident.to_string();
        } else {
            panic!("expected the node to be a identifier but : {:?}", self);
        }
    }
    pub fn is_ident(&self) -> bool {
        if let AstNodeData::Ident(_) = self.data {
            return true;
        }
        return false;

    }
    pub fn is_enum(&self) -> bool {
        if let AstNodeData::Enum(_, _) = self.data {
            return true;
        }
        return false;
    }

    pub fn is_def(&self) -> bool {
        if let AstNodeData::Def(_) = self.data {
            return true;
        }
        return false;
    }

    pub fn get_def(&self) -> AstDef {
        if let AstNodeData::Def(ref def) = self.data {
            return def.clone();
        }
        panic!("expected AstNodeData::Def got: {:?}", self);
    }

    pub fn is_initialize(&self) -> bool {
        if let AstNodeData::Initialize(_, _) = self.data {
            return true;
        }
        return false;
    }

    // pub fn get_pointer_to_value(&self) -> AstNodeData {
    //     if let AstNodeData::PointerTo(ref obj) = self.data {
    //         return obj.data.clone();
    //     }
    //     panic!("expected a pointer to node found: {:?}", self);
    // }

    pub fn extract_uint(&self) -> u64 {
        if let AstNodeData::Uint(u) = self.data {
            return u;
        }
        unreachable!();
    }

    pub fn extract_if(&self) -> &AstCaseBlock {
        if let AstNodeData::If(ref ast_if) = self.data {
            return ast_if;
        }
        unreachable!();
    }
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub filename: String,
    pub src: String,
    pub tokens: Vec<Token>,
    pub top_level: Vec<NodeID>,
}

impl Ast {
    pub fn new(filename: String, src: String, tokens: Vec<Token>, top_level:Vec<NodeID>, node_manager: &mut AstNodeManager) -> Result<Self> {
        Ok(Self {
            filename, src,tokens, top_level
        })
    }
}
