use std::collections::HashMap;
use std::default;

use crate::lexer::{Token, TokenType};
use crate::compiler::Compiler;
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
    pub fn new(node: &AstNode, compiler: &Compiler) -> Self {
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
            AstNodeData::CVarArgs => AstNodeType::CVarArgs,
            AstNodeData::CString => AstNodeType::CString,
            AstNodeData::Deref(ref obj) => { // TODO: this is a temporary fix, this should be handled in parser and this would become PointerTo
                let pointee = compiler.get_node(obj.clone());
                return AstNodeType::Pointer(Box::new(pointee.infered_type));
            }
            AstNodeData::FnType(ref sign) => {
                let mut args: Vec<AstNodeType> = vec![];
                for decl in sign.args.iter() {
                    let decl_node = compiler.get_node(decl.clone());
                    args.push(decl_node.infered_type);
                }
                let ret = compiler.get_node(sign.ret.clone()).infered_type;

                return AstNodeType::FnType(args, Box::new(ret));
            }
            AstNodeData::VoidTy => AstNodeType::Void,
            _ => AstNodeType::Unknown,
        }
    }
    pub fn make_fn_signature(
        node_manager: &Compiler,
        args: &Vec<NodeID>,
        ret: &AstNode,
    ) -> Self {
        let args_ty: Vec<AstNodeType> = args
            .iter()
            .map(|arg| {
                let arg = node_manager.get_node(arg.clone());
                arg.infered_type.clone()
            })
            .collect();

        return AstNodeType::FnType(args_ty, Box::new(AstNodeType::new(&ret, node_manager)));
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

    pub fn get_pointer_pointee(&self) -> AstNodeType {
        match self {
            AstNodeType::Pointer(obj) => {
                return *obj.clone();
            }

            _ => {
                panic!("expected pointer found: {:?}", self);
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
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct NamespaceAccess {
    pub namespace: NodeID,
    pub field: NodeID,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct AstFnSignature {
    pub args: Vec<NodeID>,
    pub ret: NodeID,
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
    BinaryAnd,
    BinaryOr,
}
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct AstBinaryOperation {
    pub operation: AstOperation,
    pub left: NodeID,
    pub right: NodeID,
}
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct AstCaseBlock {
    pub cases: Vec<(NodeID, Vec<NodeID>)>,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct AstDef {
    pub mutable: bool,
    pub name: NodeID,
    pub expr: NodeID,
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

    File(String),
}

pub type TokenIndex = isize;
pub type ScopeID = isize;
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Scope {
    pub scope_type: ScopeType,
    pub parent: ScopeID,
    pub start: TokenIndex,
    pub end: TokenIndex,
}
impl Default for Scope {
    fn default() -> Self {
        Self {
            scope_type: ScopeType::Unknown,
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
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum AstNodeData {
    //top level items
    CompilerFlags(String),
    Load(String),
    Host(String),
    Def(AstDef),
    Decl(NodeID),
    Assign(NodeID, NodeID),

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

    pub fn is_unknown(&self) -> bool {
        return self.infered_type.is_unknown();
    }

    pub fn is_pointer(&self) -> bool {
        if let AstNodeData::PointerTo(_) = self.data {
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

    pub fn get_name_for_defs_and_decls(&self, node_manager: &Compiler) -> Option<NodeID> {
        match &self.data {
            AstNodeData::Def(def) => {
                return Some(def.name.clone());
            }

            AstNodeData::Decl(ident_id) => {
                return Some(ident_id.clone());
            }
            _ => None,
        }
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
    pub fn new(
        filename: String,
        src: String,
        tokens: Vec<Token>,
        top_level: Vec<NodeID>,
        node_manager: &mut Compiler,
    ) -> Result<Self> {
        Ok(Self {
            filename,
            src,
            tokens,
            top_level,
        })
    }
}
