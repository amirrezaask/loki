use std::collections::HashMap;
use std::default;

use crate::lexer::{Token, TokenType};
use anyhow::anyhow;
use crate::context::Context;
use anyhow::Result;
use serde::Serialize;
pub type NodeID = String;
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct NamespaceAccessType {
    pub namespace: Box<AstNodeType>,
    pub field: Box<AstNodeType>,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum AstNodeType {
    Unknown,
    NoType,

    LoadedFile,
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

    Struct,
    Enum,
    Union,

    Pointer(Box<AstNodeType>),

    FnType(Vec<AstNodeType>, Box<AstNodeType>),

    NamespaceAccess(NamespaceAccessType),

    CVarArgs,
    CString,

    Void,
}

impl AstNodeType {
    pub fn new(node: &AstNode, compiler: &Context) -> Result<Self> {
        match node.data {
            AstNodeData::Ident(ref ident) => {
                return Ok(AstNodeType::TypeName(ident.clone()));
            }

            AstNodeData::UintTy(bitsize) => Ok(AstNodeType::UnsignedInt(bitsize)),
            AstNodeData::IntTy(bitsize) => Ok(AstNodeType::SignedInt(bitsize)),
            AstNodeData::CharTy => Ok(AstNodeType::Char),
            AstNodeData::StringTy => Ok(AstNodeType::String),
            AstNodeData::BoolTy => Ok(AstNodeType::Bool),
            AstNodeData::FloatTy(bitsize) => Ok(AstNodeType::Float(bitsize)),
            AstNodeData::CVarArgs => Ok(AstNodeType::CVarArgs),
            AstNodeData::CString => Ok(AstNodeType::CString),
            AstNodeData::Deref(ref obj) => { // TODO: this is a temporary fix, this should be handled in parser and this would become PointerTo
                let pointee = compiler.get_node(obj.clone())?;
                return Ok(AstNodeType::Pointer(Box::new(pointee.infered_type)));
            }
            AstNodeData::FnType { args: ref fn_args, ret: ref ret } => {
                let mut args: Vec<AstNodeType> = vec![];
                for decl in fn_args.iter() {
                    let decl_node = compiler.get_node(decl.clone())?;
                    args.push(decl_node.infered_type);
                }
                let ret = compiler.get_node(ret.clone())?.infered_type;

                return Ok(AstNodeType::FnType(args, Box::new(ret)));
            }
            AstNodeData::VoidTy => Ok(AstNodeType::Void),
            _ => Ok(AstNodeType::Unknown),
        }
    }
    pub fn make_fn_signature(
        compiler: &Context,
        args: &Vec<NodeID>,
        ret: &AstNode,
    ) -> Result<Self> {
        let args_ty: Vec<AstNodeType> = args
            .iter()
            .map(|arg| {
                let arg = compiler.get_node(arg.clone()).unwrap();
                arg.infered_type.clone()
            })
            .collect();

        return Ok(AstNodeType::FnType(args_ty, Box::new(AstNodeType::new(&ret, compiler)?)));
    }
    pub fn is_type_def(&self) -> bool {
        match self {
            AstNodeType::Struct | AstNodeType::Union | AstNodeType::Enum => true,
            _ => return false,
        }
    }

    pub fn is_enum(&self) -> bool {
        match self {
            AstNodeType::Enum => true,
            _ => return false,
        }
    }
    pub fn is_type_name(&self) -> bool {
        match self {
            AstNodeType::TypeName(_)=> true,
            _ => false,
        }
    }
    pub fn get_type_name(&self) -> String {
        match self {
            AstNodeType::TypeName(name)=> name.to_string(),
            _ => panic!("{:?} is not type name", self),
        }
    }

    pub fn get_pointer_pointee(&self) -> Result<AstNodeType> {
        match self {
            AstNodeType::Pointer(obj) => {
                return Ok(*obj.clone());
            }

            _ => {
                return Err(anyhow!("expected pointer found: {:?}", self));
            }
        }
    }

    pub fn is_unknown(&self) -> bool {
        match self {
            AstNodeType::Unknown => true,
            AstNodeType::Pointer(obj) => {
                if obj.is_unknown() {
                    return true;
                }
                return false;
            }
            _ => return false,
        }
    }
    pub fn is_fn_def(&self) -> bool {
        match self {
            AstNodeType::FnType(_, _) => true,
            _ => return false,
        }
    }
    pub fn get_fn_ret_ty(&self) -> AstNodeType {
        match self {
            AstNodeType::FnType(_, sign) => *(sign.clone()),
            _ => unreachable!(),
        }
    }
    pub fn is_type_def_enum(&self) -> bool {
        match self {
            AstNodeType::Enum => true,
            _ => return false,
        }
    }

    pub fn is_pointer(&self) -> bool {
        match self {
            AstNodeType::Pointer(_) => true,
            _ => return false,
        }
    }
}
#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum AstTag {
    Foreign,
    IsUsedInNamespaceAccess,
    IsUsedInInitialize,
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
    BinaryAnd,
    BinaryOr,
}
pub type BitSize = usize;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum ScopeType {
    Unknown,

    // all use expect_block, they all know where block starts and expect_block knows where it will end.
    Function,
    While,
    For,
    ForIn,
    If,
    Else,

    Struct,
    Enum,

    File(String),
}

pub type TokenIndex = isize;
pub type ScopeID = isize;
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Scope {
    pub scope_type: ScopeType,
    pub parent: ScopeID,
    pub owner_node: NodeID,
    pub start: TokenIndex,
    pub end: TokenIndex,
}
impl Default for Scope {
    fn default() -> Self {
        Self {
            scope_type: ScopeType::Unknown,
            owner_node: "".to_string(),
            parent: -1,
            start: -1,
            end: -1,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct AstNode {
    pub id: NodeID,
    pub data: AstNodeData,
    pub infered_type: AstNodeType,
    pub scope: ScopeID,
    pub tags: Vec<AstTag>,
    
    pub line: usize,
    pub col: usize,
    pub filename: String,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum AstNodeData {
    //top level items
    CompilerFlags(String),
    Load(String),
    Host(String),
    Def{
        mutable: bool,
        name: NodeID,
        expr: NodeID,
    },
    Decl(NodeID),
    Assign{lhs: NodeID, rhs: NodeID},

    CVarArgs,
    CString,

    // Type defs
    IntTy(BitSize), // bitsize
    UintTy(BitSize),

    FloatTy(BitSize),
    BoolTy,
    StringTy,
    CharTy,
    VoidTy,
    ArrayTy {
        length: u64, 
        elem_ty: AstNodeType
    },
    FnType {
        args: Vec<NodeID>,
        ret: NodeID,
    },

    Namespace(Vec<NodeID>),

    Struct(Vec<NodeID>),
    Enum(Vec<NodeID>),

    //Expressions
    Unsigned(u64),
    Signed(i64),
    StringLiteral(String),
    Float(f64),
    Bool(bool),
    Char(char),
    Ident(String),

    BinaryOperation {
        operation: AstOperation,
        left: NodeID,
        right: NodeID,
    },

    NamespaceAccess {
        namespace: NodeID,
        field: NodeID,
    },

    Initialize{ty: NodeID, fields: Vec<(NodeID, NodeID)>},
    InitializeArray(Option<NodeID>, Vec<NodeID>),

    FnDef{
        sign: NodeID,
        body: NodeID,
    },
    
    FnCall{fn_name: NodeID, args: Vec<NodeID>},

    PointerTo(NodeID),
    Deref(NodeID),

    If {
        cases: Vec<(NodeID, NodeID)>,
    },

    For { start: NodeID, cond: NodeID, cont: NodeID, body: NodeID},
    ForIn{ iterator: NodeID, iterable: NodeID, body: NodeID },
    While{ cond: NodeID, body: NodeID },

    Break,
    Continue,

    Return(NodeID),
}

impl AstNode {
    pub fn get_ident(&self) -> Result<String> {
        if let AstNodeData::Ident(ident) = &self.data {
            return Ok(ident.to_string());
        } else {
            Err(anyhow!("expected the node to be a identifier but : {:?}", self))
        }
    }
    pub fn is_ident(&self) -> bool {
        if let AstNodeData::Ident(_) = self.data {
            return true;
        }
        return false;
    }

    pub fn is_unknown(&self) -> bool {
        return self.infered_type.is_unknown();
    }

    pub fn get_block(&self) -> Result<Vec<NodeID>> {
        if let AstNodeData::Namespace(ref nodes) = self.data {
            return Ok(nodes.clone());
        }
        Err(anyhow!("expected AstNodeData::Namespace got: {:?}", self))
    }

    pub fn is_pointer(&self) -> bool {
        if let AstNodeData::PointerTo(_) = self.data {
            return true;
        }
        return false;
    }
    pub fn is_def(&self) -> bool {
        if let AstNodeData::Def{mutable: _, name: _, expr: _} = self.data {
            return true;
        }
        return false;
    }
    pub fn is_decl(&self) -> bool {
        if let AstNodeData::Decl(_) = self.data {
            return true;
        }
        return false;
    }
    pub fn get_def(&self) -> Result<(bool, NodeID, NodeID)> {
        if let AstNodeData::Def{mutable, name: ref name, expr: ref expr} = self.data {
            return Ok((mutable,  name.clone(), expr.clone()));
        }
        Err(anyhow!("expected AstNodeData::Def got: {:?}", self))
    }
    pub fn get_decl(&self) -> Result<NodeID> {
        if let AstNodeData::Decl(ref ident_node_id) = self.data {
            return Ok(ident_node_id.clone());
        }
        Err(anyhow!("expected AstNodeData::Decl got: {:?}", self))
    }
    pub fn get_fn_signature(&self) -> Result<(Vec<NodeID>, NodeID)> {
        if let AstNodeData::FnType {args, ret} = &self.data {
            return Ok((args.clone(), ret.clone()));
        }
        Err(anyhow!("expected AstNodeData::Decl got: {:?}", self))
 
    }
    pub fn get_name_for_defs_and_decls(&self, compiler: &Context) -> Option<NodeID> {
        match &self.data {
            AstNodeData::Def{mutable, name, expr} => {
                return Some(name.clone());
            }

            AstNodeData::Decl(ident_id) => {
                return Some(ident_id.clone());
            }
            _ => None,
        }
    }

    pub fn add_node_to_block(&mut self, id: NodeID) -> Result<()> {
        match &mut self.data {
            
            _ => unimplemented!(),

        }
        Ok(())
    }



    pub fn get_enum_variants(&self) -> Result<Vec<NodeID>> {
        if let AstNodeData::Enum(ref variants) = self.data {
            return Ok(variants.clone());
        }
        Err(anyhow!("expected AstNodeData::Enum got: {:?}", self))
    }

    pub fn is_initialize(&self) -> bool {
        if let AstNodeData::Initialize{ty: _, fields: _} = self.data {
            return true;
        }
        return false;
    }

    // pub fn get_pointer_to_value(&self) -> AstNodeData {
    //     if let AstNodeData::PointerTo(ref obj) = self.data {
    //         return obj.data.clone();
    //     }
    //     return Err(anyhow!("expected a pointer to node found: {:?}", self)));
    // }

    pub fn extract_uint(&self) -> u64 {
        if let AstNodeData::Unsigned(u) = self.data {
            return u;
        }
        unreachable!();
    }

    pub fn extract_if(&self) -> &Vec<(NodeID, NodeID)> {
        if let AstNodeData::If { ref cases } = self.data {
            return cases;
        }
        unreachable!();
    }
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub filename: String,
    pub src: String,
    pub tokens: Vec<Token>,
    pub top_level: NodeID,
}

impl Ast {
    pub fn new(
        filename: String,
        src: String,
        tokens: Vec<Token>,
        top_level: NodeID,
        compiler: &mut Context,
    ) -> Result<Self> {
        Ok(Self {
            filename,
            src,
            tokens,
            top_level,
        })
    }
}
