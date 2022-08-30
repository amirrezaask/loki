use std::collections::HashMap;
use std::default;

use crate::{lexer::{Token, TokenType}};
use anyhow::anyhow;
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

    Struct {
        fields: HashMap<String, AstNodeType>, // name: type 
    },
    Enum {
        variants: Vec<String>,
    },
    Union,

    Pointer(Box<AstNodeType>),

    FnType(Vec<AstNodeType>, Box<AstNodeType>),

    NamespaceAccess(NamespaceAccessType),

    CVarArgs,
    CString,

    Void,
}

impl AstNodeType {
    pub fn new(node: &AstNode, ast: &Ast) -> Result<Self> {
        match node.data {
            AstNodeData::Ident(ref ident) => {
                return Ok(AstNodeType::TypeName(ident.clone()));
            }

            AstNodeData::StructTy(ref decls) => {
                let mut fields = HashMap::<String, AstNodeType>::new();
                for decl_id in decls {
                    let decl = ast.get_node(decl_id.clone())?;
                    let field_id = decl.get_decl()?;
                    let field = ast.get_node(field_id)?;
                    fields.insert(field.get_ident()?.clone(), decl.infered_type);
                }

                Ok(AstNodeType::Struct{fields})
            }

            AstNodeData::EnumTy(ref vs) => {
                let mut variants = Vec::<String>::new();
                for decl_id in vs {
                    let decl = ast.get_node(decl_id.clone())?;
                    let variant_id = decl.get_decl()?;
                    let variant = ast.get_node(variant_id)?;
                    variants.push(variant.get_ident()?.clone());
                }

                Ok(AstNodeType::Enum{variants})
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
                let pointee = ast.get_node(obj.clone())?;
                return Ok(AstNodeType::Pointer(Box::new(pointee.infered_type)));
            }
            AstNodeData::FnType { args: ref fn_args, ret: ref ret } => {
                let mut args: Vec<AstNodeType> = vec![];
                for decl in fn_args.iter() {
                    let decl_node = ast.get_node(decl.clone())?;
                    args.push(decl_node.infered_type);
                }
                let ret = ast.get_node(ret.clone())?.infered_type;

                return Ok(AstNodeType::FnType(args, Box::new(ret)));
            }
            AstNodeData::VoidTy => Ok(AstNodeType::Void),
            _ => Ok(AstNodeType::Unknown),
        }
    }
    pub fn from_struct_fields(decls: Vec<NodeID>, ast: &Ast) -> Result<AstNodeType> {
        let mut fields = HashMap::<String, AstNodeType>::new();
        for decl_id in decls {
            let decl = ast.get_node(decl_id.clone())?;
            let field_id = decl.get_decl()?;
            let field = ast.get_node(field_id)?;
            fields.insert(field.get_ident()?.clone(), decl.infered_type);
        }

        Ok(AstNodeType::Struct{fields})
    }
    pub fn from_enum_variants(vs: Vec<NodeID>, ast: &Ast) -> Result<AstNodeType> {
        let mut variants = Vec::<String>::new();
        for decl_id in vs {
            let decl = ast.get_node(decl_id.clone())?;
            let variant_id = decl.get_decl()?;
            let variant = ast.get_node(variant_id)?;
            variants.push(variant.get_ident()?.clone());
        }

        Ok(AstNodeType::Enum{variants})
    }
    pub fn make_fn_signature(
        ast: &Ast,
        args: &Vec<NodeID>,
        ret: &AstNode,
    ) -> Result<Self> {
        let args_ty: Vec<AstNodeType> = args
            .iter()
            .map(|arg| {
                let arg = ast.get_node(arg.clone()).unwrap();
                arg.infered_type.clone()
            })
            .collect();

        return Ok(AstNodeType::FnType(args_ty, Box::new(AstNodeType::new(&ret, ast)?)));
    }
    pub fn is_type_def(&self) -> bool {
        match self {
            AstNodeType::Struct{fields: _} | AstNodeType::Union | AstNodeType::Enum{variants: _} => true,
            _ => return false,
        }
    }
    pub fn is_struct(&self) -> bool {
        match self {
            AstNodeType::Struct{fields: _}  => true,
            _ => return false,
        }
    }
    pub fn get_struct_fields(&self) -> HashMap<String, AstNodeType> {
        match self {
            AstNodeType::Struct{ref fields} => fields.clone(),
            _ => panic!("expected struct type found: {:?}", self),
        }
    }
    
    pub fn get_enum_variants(&self) -> Vec<String> {
        match self {
            AstNodeType::Enum{ref variants} => variants.clone(),
            _ => panic!("expected enum type found: {:?}", self),
        }
    }
    pub fn is_enum(&self) -> bool {
        match self {
            AstNodeType::Enum{variants: _} => true,
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
            AstNodeType::Enum{variants:_} => true,
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
    pub parent_block: NodeID,
    pub scope: ScopeID,
    pub tags: Vec<AstTag>,
    
    pub line: usize,
    pub col: usize,
    pub filename: String,
}

pub enum AstBlockType {
    File,
    NotImportant,
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

    Block{
        is_file_root: bool,
        nodes: Vec<NodeID>
    },

    StructTy(Vec<NodeID>),
    EnumTy(Vec<NodeID>),

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

    pub fn block_is_file_root(&self) -> Result<bool> {
        if let AstNodeData::Block{nodes: _, is_file_root} = self.data {
            return Ok(is_file_root);
        }
        Err(anyhow!("expected AstNodeData::Block got: {:?}", self))
    }

    pub fn get_block(&self) -> Result<Vec<NodeID>> {
        if let AstNodeData::Block{nodes: ref nodes, is_file_root: _} = self.data {
            return Ok(nodes.clone());
        }
        
        Err(anyhow!("expected AstNodeData::Namespace got: {:?}", self))
    }

    pub fn is_fn_call(&self) -> bool {
        if let AstNodeData::FnCall{fn_name: _, args: _} = self.data {
            return true;
        }
        return false;
    }
    pub fn is_fn_def(&self) -> bool {
        if let AstNodeData::FnDef{sign: _, body: _} = self.data {
            return true;
        }
        return false;
    }
    pub fn is_deref(&self) -> bool {
        if let AstNodeData::Deref(_) = self.data {
            return true;
        }
        return false;
    }
    pub fn get_deref_expr(&self) -> Result<NodeID> {
        if let AstNodeData::Deref(ref expr_id) = self.data {
            return Ok(expr_id.clone());
        }
        return Err(anyhow!("expected a deref expression got {:?}", self));
    }
    pub fn get_pointer_expr(&self) -> Result<NodeID> {
        if let AstNodeData::PointerTo(ref expr_id) = self.data {
            return Ok(expr_id.clone());
        }
        return Err(anyhow!("expected a pointer to expression got {:?}", self));
    }
    pub fn is_namespace_access(&self) -> bool {
        if let AstNodeData::NamespaceAccess{namespace: _, field: _} = self.data {
            return true;
        }
        return false;
    }

    pub fn get_namespace_ns_id(&self) -> Result<NodeID> {
        if let AstNodeData::NamespaceAccess{ref namespace, field: _} = self.data {
            return Ok(namespace.clone());
        }
        Err(anyhow!("expected AstNodeData::Namespace got: {:?}", self))
    }

    pub fn get_namespace_field_id(&self) -> Result<NodeID> {
        if let AstNodeData::NamespaceAccess{namespace: _, ref field} = self.data {
            return Ok(field.clone());
        }
        Err(anyhow!("expected AstNodeData::Namespace got: {:?}", self))
    }
    pub fn get_fn_call_fn_name(&self) -> Result<NodeID> {
        if let AstNodeData::FnCall{ref fn_name, args: _} = self.data {
            return Ok(fn_name.clone());
        }
        Err(anyhow!("expected AstNodeData::FnCall got: {:?}", self))
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
    pub fn get_name_for_defs_and_decls(&self) -> Option<NodeID> {
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
        if let AstNodeData::EnumTy(ref variants) = self.data {
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

#[derive(Debug, Clone, Serialize)]
pub struct Ast {
    pub filename: String,
    pub src: String,
    pub tokens: Vec<Token>,
    pub top_level: NodeID,
    pub nodes: HashMap<NodeID, AstNode>,
}

impl Ast {
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
    pub fn sema_check(&self) {}
    fn find_identifier_type(&self, start_block: NodeID, index_in_block: isize, ident: String) -> Result<AstNodeType> {
        let block = self.get_node(start_block)?;
        let block_nodes = block.get_block()?; 
        for (index, node_id) in block_nodes.iter().enumerate() {
            if index_in_block >0 && index > index_in_block as usize && !block.block_is_file_root()? {
                break;
            }

            let node = self.get_node(node_id.clone())?;
            if node.is_def() {
                let (_, name_id, expr_id) = node.get_def()?;
                let name = self.get_node(name_id)?.get_ident()?;
                if name == ident  {
                    let expr = self.get_node(expr_id)?;
                    return Ok(expr.infered_type);
                }
            }

            if node.is_decl() {
                let name_id = node.get_decl()?;
                let name_node = self.get_node(name_id)?;
                let name = name_node.get_ident()?;

                if name == ident {
                    return Ok(name_node.infered_type);
                }
            }
        }

        // if we reach here we didn't find type of that identifier in the block

        return self.find_identifier_type(block.parent_block, -1, ident);
    }
    fn infer_type_expr(&mut self, expr_id: NodeID, index_in_block: usize) -> Result<()> {
        let expr = self.get_node(expr_id.clone())?;
        if expr.is_ident() {
            let expr_ident = expr.get_ident()?;
            let infered_type = self.find_identifier_type(expr.parent_block.clone(), index_in_block as isize, expr_ident)?;
            self.add_type_inference(&expr_id.clone(), infered_type.clone());
        }

        if expr.is_fn_def() {
            if let AstNodeData::FnDef{sign: _, ref body } = expr.data {
                self.infer_types_block(body.to_string())?;
            } else {
                unreachable!()
            }
        }

        if expr.is_deref() {
            let deref_expr_id = expr.get_deref_expr()?;
            let infered_type = self.infer_type_expr(deref_expr_id.clone(), index_in_block)?;
            let deref_expr = self.get_node(deref_expr_id)?;
            self.add_type_inference(&expr_id, deref_expr.infered_type)
        }

        if expr.is_pointer() {
            let pointer_expr_id = expr.get_pointer_expr()?;
            let infered_type = self.infer_type_expr(pointer_expr_id.clone(), index_in_block)?;
            let pointer_expr = self.get_node(pointer_expr_id)?;
            self.add_type_inference(&expr_id, AstNodeType::Pointer(Box::new(pointer_expr.infered_type)))
        }


        if expr.is_fn_call() {
            let fn_name_id = expr.get_fn_call_fn_name()?;
            let fn_name = self.get_node(fn_name_id.to_string())?.get_ident()?;
            let infered_type = self.find_identifier_type(expr.parent_block.clone(), index_in_block as isize, fn_name)?;
            self.add_type_inference(&expr_id.clone(), infered_type.clone());
        }

        if expr.is_namespace_access() {
            let ns_id = expr.get_namespace_ns_id()?;
            let field_id = expr.get_namespace_field_id()?;
            let field = self.get_node(field_id)?.get_ident()?;
            let ns_identifier = self.get_node(ns_id.clone())?.get_ident()?;
            let ns_ty = self.find_identifier_type(expr.parent_block.clone(), index_in_block as isize, ns_identifier.clone())?;
            if !ns_ty.is_type_def() {
                return Err(anyhow!(". operator can only be used for structs and enums but you used {:?}", ns_ty));
            }
            
            if ns_ty.is_struct() {
                let mut infered_type = AstNodeType::Unknown;
                for (name, ty) in ns_ty.get_struct_fields() {
                    if name == field {
                        infered_type = ty;
                    }
                }
                if infered_type.is_unknown() {
                    return Err(anyhow!("struct {:?} has no field named {}", ns_ty.clone(), field));
                }
                self.add_type_inference(&expr_id.clone(), infered_type.clone());

            } else if ns_ty.is_enum() {
                let mut infered_type = AstNodeType::Unknown;
                for name in ns_ty.get_enum_variants() {
                    if name == field {
                        infered_type = AstNodeType::UnsignedInt(64);
                    }
                }
                if infered_type.is_unknown() {
                    return Err(anyhow!("no enum {} has no variant named {}", ns_identifier.clone(), field));
                }
                self.add_type_inference(&expr_id.clone(), AstNodeType::UnsignedInt(64));
                self.add_type_inference(&ns_id, ns_ty);
            }
        }

        Ok(())
    }
    
    fn infer_type_def(&mut self, node_id: NodeID, index_in_block: usize) -> Result<()> {
        let node = self.get_node(node_id.to_string())?;
        let (_, name_id, expr_id) = node.get_def()?;
        let name = self.get_node(name_id.clone())?.get_ident()?;
        self.infer_type_expr(expr_id.clone(), index_in_block)?;
        let expr = self.get_node(expr_id)?;
        self.add_type_inference(&name_id.clone(), expr.infered_type);
        Ok(())
    }

    fn infer_types_block(&mut self, block_id: NodeID) -> Result<()> {
        let mut block = self.get_node(block_id.to_string())?.get_block()?;
        for (index, node_id) in block.iter().enumerate() {
            let node = self.get_node(node_id.to_string())?;
            if node.is_def() {
                self.infer_type_def(node.id.clone(), index)?;
                continue;
            }
        }

        Ok(())
    }
    pub fn infer_types(&mut self) -> Result<()> {
        self.infer_types_block(self.top_level.to_string())?;
        Ok(())
    }
    pub fn type_check(&self) {}


    pub fn new(
        filename: String,
        src: String,
        tokens: Vec<Token>,
        top_level_block: NodeID,
        nodes: HashMap<NodeID, AstNode>
    ) -> Result<Self> {
        Ok(Self {
            filename,
            src,
            tokens,
            top_level: top_level_block,
            nodes
        })
    }
}
