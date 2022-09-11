use std::collections::HashMap;
use std::default;
use std::ops::Deref;

use crate::lexer::{Token, TokenType};
use crate::utils;
use anyhow::anyhow;
use anyhow::Result;
use rand::distributions::Alphanumeric;
use rand::distributions::DistString;
use serde::Serialize;
pub type NodeID = String;

pub enum EntityValue {
    Node(AstNode),
    Type(Type),
}

pub struct Entity {
    idx: NodeID,
    value: EntityValue
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct NamespaceAccessType {
    pub namespace: Box<Type>,
    pub field: Box<Type>,
}
#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum BlockType {
    Unknown,
    ForC,
    ForIn,
    While,
    Function,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum Type {
    Unknown,
    NoType,

    LoadedFile,
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
    Union,

    TypeRef {
        name: String,
        actual_ty: Box<Type>,
    },

    Pointer(Box<Type>),

    FnType(Vec<Type>, Box<Type>),

    NamespaceAccess(NamespaceAccessType),

    CVarArgs,
    CString,

    Void,
}

impl Type {
    pub fn new(node: &AstNode, ast: &Ast) -> Result<Self> {
        match node.data {
            AstNodeData::Ident(ref ident) => {
                let ty = ast.find_identifier_type(
                    node.parent_block.clone(),
                    -1,
                    ident.clone(),
                    &vec![],
                )?;
                return Ok(ty);
            }

            AstNodeData::StructTy(ref decls) => {
                let mut fields = Vec::<(String, Type)>::new();
                for decl_id in decls {
                    let decl = ast.get_node(decl_id.clone())?;
                    let (field_id, _) = decl.get_decl()?;
                    let field = ast.get_node(field_id)?;
                    fields.push((field.get_ident()?.clone(), decl.type_information));
                }

                Ok(Type::Struct { fields })
            }

            AstNodeData::EnumTy(ref vs) => {
                let mut variants = Vec::<String>::new();
                for decl_id in vs {
                    let decl = ast.get_node(decl_id.clone())?;
                    let (variant_id, _) = decl.get_decl()?;
                    let variant = ast.get_node(variant_id)?;
                    variants.push(variant.get_ident()?.clone());
                }

                Ok(Type::Enum { variants })
            }
            AstNodeData::UintTy(bitsize) => Ok(Type::UnsignedInt(bitsize)),
            AstNodeData::IntTy(bitsize) => Ok(Type::SignedInt(bitsize)),
            AstNodeData::CharTy => Ok(Type::Char),
            AstNodeData::StringTy => Ok(Type::String),
            AstNodeData::BoolTy => Ok(Type::Bool),
            AstNodeData::FloatTy(bitsize) => Ok(Type::Float(bitsize)),
            AstNodeData::CVarArgs => Ok(Type::CVarArgs),
            AstNodeData::CString => Ok(Type::CString),
            AstNodeData::Deref(ref obj) => {
                // TODO: this is a temporary fix, this should be handled in parser and this would become PointerTo
                let pointee = ast.get_node(obj.clone())?;
                return Ok(Type::Pointer(Box::new(pointee.type_information)));
            }
            AstNodeData::PointerOf(ref obj) => {
                let pointee = ast.get_node(obj.clone())?;
                return Ok(Type::Pointer(Box::new(pointee.type_information)));
            }
            AstNodeData::FnType {
                args: ref fn_args,
                ret: ref ret,
            } => {
                let mut args: Vec<Type> = vec![];
                for decl in fn_args.iter() {
                    let decl_node = ast.get_node(decl.clone())?;
                    args.push(decl_node.type_information);
                }
                let ret = ast.get_node(ret.clone())?.type_information;

                return Ok(Type::FnType(args, Box::new(ret)));
            }
            AstNodeData::VoidTy => Ok(Type::Void),
            AstNodeData::PointerTy(ref node) => {
                let n = ast.get_node(node.clone())?;
                Ok(Type::Pointer(Box::new(n.type_information)))
            }
            AstNodeData::ArrayTy {
                length,
                ref elem_ty,
            } => Ok(Type::Array(length, Box::new(elem_ty.clone()))),
            _ => Ok(Type::Unknown),
        }
    }
    pub fn is_iterable(&self) -> bool {
        match self {
            Type::Array(_, _) | Type::DynamicArray(_) => true,
            _ => false,
        }
    }
    pub fn is_array(&self) -> bool {
        match self {
            Type::Array(_, _) | Type::DynamicArray(_) => true,
            _ => false,
        }
    }
    pub fn get_array_elem_type(&self) -> Result<Type> {
        match self {
            Type::Array(_, ty) | Type::DynamicArray(ty) => Ok(ty.deref().clone()),
            _ => Err(anyhow!("expected array type or dynamic array type but: {:?}", self)),
        }
    }
    pub fn from_struct_fields(decls: Vec<NodeID>, ast: &Ast) -> Result<Type> {
        let mut fields = Vec::<(String, Type)>::new();
        for decl_id in decls {
            let decl = ast.get_node(decl_id.clone())?;
            let (field_id, _) = decl.get_decl()?;
            let field = ast.get_node(field_id)?;
            fields.push((field.get_ident()?.clone(), decl.type_information));
        }

        Ok(Type::Struct { fields })
    }
    pub fn from_enum_variants(vs: Vec<NodeID>, ast: &Ast) -> Result<Type> {
        let mut variants = Vec::<String>::new();
        for decl_id in vs {
            let decl = ast.get_node(decl_id.clone())?;
            let (variant_id, _) = decl.get_decl()?;
            let variant = ast.get_node(variant_id)?;
            variants.push(variant.get_ident()?.clone());
        }

        Ok(Type::Enum { variants })
    }
    pub fn is_bool(&self) -> bool {
        match self {
            Type::Bool => true,
            _ => false,
        }
    }
    pub fn make_fn_signature(ast: &Ast, args: &Vec<NodeID>, ret: &AstNode) -> Result<Self> {
        let args_ty: Vec<Type> = args
            .iter()
            .map(|arg| {
                let arg = ast.get_node(arg.clone()).unwrap();
                arg.type_information.clone()
            })
            .collect();

        return Ok(Type::FnType(args_ty, Box::new(Type::new(&ret, ast)?)));
    }
    pub fn is_type_def(&self) -> bool {
        match self {
            Type::Struct { fields: _ } | Type::Union | Type::Enum { variants: _ } => true,
            _ => return false,
        }
    }
    pub fn is_type_ref(&self) -> bool {
        match self {
            Type::TypeRef {
                name: _,
                actual_ty: _,
            } => true,
            _ => return false,
        }
    }
    pub fn get_actual_ty_type_ref(&self) -> Result<Type> {
        match self {
            Type::TypeRef {
                name: _,
                actual_ty: ref ty,
            } => Ok(*ty.clone()),
            _ => return Err(anyhow!("expected type ref found: {:?}", self)),
        }
    }
    pub fn get_array_ty_size(&self) -> Result<u64> {
        match self {
            Type::Array(size, _) => Ok(*size),
            _ => return Err(anyhow!("expected type array found: {:?}", self)),
        }
    }
    pub fn get_array_ty_elem_ty(&self) -> Result<Type> {
        match self {
            Type::Array(_, ref elem_ty) => Ok(*elem_ty.clone()),
            _ => return Err(anyhow!("expected type array found: {:?}", self)),
        }
    }

    pub fn is_struct(&self) -> bool {
        match self {
            Type::Struct { fields: _ } => true,
            _ => return false,
        }
    }
    pub fn get_struct_fields(&self) -> Result<Vec<(String, Type)>> {
        match self {
            Type::Struct { ref fields } => Ok(fields.clone()),
            Type::TypeRef { name: _, actual_ty } => {
                return actual_ty.get_struct_fields();
            }
            _ => Err(anyhow!("expected struct type found: {:?}", self)),
        }
    }

    pub fn get_enum_variants(&self) -> Result<Vec<String>> {
        match self {
            Type::Enum { ref variants } => Ok(variants.clone()),
            _ => Err(anyhow!("expected enum type found: {:?}", self)),
        }
    }
    pub fn is_enum_def(&self) -> bool {
        match self {
            Type::Enum { variants: _ } => true,
            _ => return false,
        }
    }

    pub fn get_pointer_pointee(&self) -> Result<Type> {
        match self {
            Type::Pointer(obj) => {
                return Ok(*obj.clone());
            }

            _ => {
                return Err(anyhow!("expected pointer found: {:?}", self));
            }
        }
    }

    pub fn is_unknown(&self) -> bool {
        match self {
            Type::Unknown => true,
            Type::Pointer(obj) => {
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
            Type::FnType(_, _) => true,
            _ => return false,
        }
    }
    pub fn get_fn_ret_ty(&self) -> Type {
        match self {
            Type::FnType(_, sign) => *(sign.clone()),
            _ => unreachable!(),
        }
    }
    pub fn get_fn_args(&self) -> Vec<Type> {
        match self {
            Type::FnType(ref args, sign) => args.clone(),
            _ => unreachable!(),
        }
    }
    pub fn is_type_def_enum(&self) -> bool {
        match self {
            Type::Enum { variants: _ } => true,
            _ => return false,
        }
    }

    pub fn is_pointer(&self) -> bool {
        match self {
            Type::Pointer(_) => true,
            _ => return false,
        }
    }
}
#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum AstTag {
    Foreign,
    IsUsedInNamespaceAccess,
    IsUsedInInitialize,
    NoCodeGen,
    CompilerFunctionCall,
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
    Not,
}
pub type BitSize = usize;

pub type TokenIndex = isize;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct AstNode {
    pub id: NodeID,
    pub data: AstNodeData,
    pub type_information: Type,
    pub parent_block: NodeID,
    pub index_in_block: usize,
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
    Size(NodeID),
    Cast(NodeID, Type),
    Def {
        mutable: bool,
        name: NodeID,
        expr: NodeID,
    },
    Decl {
        name: NodeID,
        ty: NodeID,
    },
    Assign {
        lhs: NodeID,
        rhs: NodeID,
    },

    CVarArgs,
    CString,

    // Type defs
    IntTy(BitSize), // bitsize
    UintTy(BitSize),

    FloatTy(BitSize),
    BoolTy,
    IntPtrTy,
    UintPtrTy,
    StringTy,
    CharTy,
    VoidTy,
    PointerTy(NodeID),
    ArrayTy {
        length: u64,
        elem_ty: Type,
    },
    FnType {
        args: Vec<NodeID>,
        ret: NodeID,
    },

    Block {
        ty: BlockType,
        is_file_root: bool,
        nodes: Vec<NodeID>,
        symbols: HashMap<String, Type>
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
    Paren(NodeID),

    Not(NodeID),

    ArrayIndex {
        arr: NodeID,
        idx: NodeID,
    },

    BinaryOperation {
        operation: AstOperation,
        left: NodeID,
        right: NodeID,
    },

    NamespaceAccess {
        namespace: NodeID,
        field: NodeID,
    },

    Initialize {
        ty: NodeID,
        fields: Vec<(NodeID, NodeID)>,
    },
    InitializeArray {
        elements: Vec<NodeID>,
    },

    FnDef {
        sign: NodeID,
        body: NodeID,
    },

    FnCall {
        fn_name: NodeID,
        args: Vec<NodeID>,
    },

    PointerOf(NodeID),
    Deref(NodeID),

    If {
        cases: Vec<(NodeID, NodeID)>,
    },

    For {
        start: NodeID,
        cond: NodeID,
        cont: NodeID,
        body: NodeID,
    },
    ForIn {
        iterator: NodeID,
        iterable: NodeID,
        body: NodeID,
    },
    While {
        cond: NodeID,
        body: NodeID,
    },

    Break,
    Continue,

    Return(NodeID),
}

impl AstNode {
    pub fn get_ident(&self) -> Result<String> {
        if let AstNodeData::Ident(ident) = &self.data {
            return Ok(ident.to_string());
        } else {
            Err(anyhow!(
                "expected the node to be a identifier but : {:?}",
                self
            ))
        }
    }
    pub fn is_ident(&self) -> bool {
        if let AstNodeData::Ident(_) = self.data {
            return true;
        }
        return false;
    }

    pub fn is_struct_def(&self) -> bool {
        if let AstNodeData::StructTy(_) = self.data {
            return true;
        }
        return false;
    }

   
    pub fn is_unknown(&self) -> bool {
        return self.type_information.is_unknown();
    }

    pub fn block_is_file_root(&self) -> Result<bool> {
        if let AstNodeData::Block {
            ty: _,
            nodes: _,
            symbols: _,
            is_file_root,
        } = self.data
        {
            return Ok(is_file_root);
        }
        Err(anyhow!("expected AstNodeData::Block got: {:?}", self))
    }
    pub fn get_fn_call(&self) -> Result<(NodeID, Vec<NodeID>)> {
        match self.data {
            AstNodeData::FnCall {
                fn_name: ref name,
                args: ref args,
            } => Ok((name.clone(), args.clone())),
            _ => return Err(anyhow!("expected ast node fn call found: {:?}", self)),
        }
    }
    pub fn get_load(&self) -> Result<String> {
        match self.data {
            AstNodeData::Load(ref path) => Ok(path.clone()),
            _ => return Err(anyhow!("expected ast node load found: {:?}", self)),
        }
    }
    pub fn get_block(&self) -> Result<Vec<NodeID>> {
        if let AstNodeData::Block {
            nodes: ref nodes,
            ty: _,
            symbols: _,
            is_file_root: _,
        } = self.data
        {
            return Ok(nodes.clone());
        }

        Err(anyhow!("expected AstNodeData::Namespace got: {:?}", self))
    }

    pub fn is_fn_call(&self) -> bool {
        if let AstNodeData::FnCall {
            fn_name: _,
            args: _,
        } = self.data
        {
            return true;
        }
        return false;
    }
    pub fn is_fn_def(&self) -> bool {
        if let AstNodeData::FnDef { sign: _, body: _ } = self.data {
            return true;
        }
        return false;
    }
    pub fn is_if(&self) -> bool {
        if let AstNodeData::If {cases: _} = self.data {
            return true;
        }
        return false;
    }
    pub fn get_if(&self) -> Result<Vec<(NodeID, NodeID)>> {
        if let AstNodeData::If{ref cases} = self.data {
            return Ok(cases.clone());
        }
        return Err(anyhow!("expected a if statement got {:?}", self));
    }
    pub fn get_while(&self) -> Result<(NodeID, NodeID)> {
        if let AstNodeData::While{ref cond, ref body} = self.data {
            return Ok((cond.clone(), body.clone()));
        }
        return Err(anyhow!("expected a while statement got {:?}", self));
    }
    pub fn get_for(&self) -> Result<(NodeID, NodeID, NodeID, NodeID)> {
        if let AstNodeData::For{ref start, ref cond, ref cont, ref body} = self.data {
            return Ok((start.clone(), cond.clone(), cont.clone(), body.clone()));
        }
        return Err(anyhow!("expected a for statement got {:?}", self));
    }
    pub fn get_for_in(&self) -> Result<(NodeID, NodeID, NodeID)> {
        if let AstNodeData::ForIn{ref iterable, ref iterator, ref body} = self.data {
            return Ok((iterator.clone(), iterable.clone(), body.clone()));
        }
        return Err(anyhow!("expected a for in statement got {:?}", self));
    }
    
    pub fn get_pointer_expr(&self) -> Result<NodeID> {
        if let AstNodeData::PointerOf(ref expr_id) = self.data {
            return Ok(expr_id.clone());
        }
        return Err(anyhow!("expected a pointer to expression got {:?}", self));
    }
    pub fn is_namespace_access(&self) -> bool {
        if let AstNodeData::NamespaceAccess {
            namespace: _,
            field: _,
        } = self.data
        {
            return true;
        }
        return false;
    }

    pub fn is_initialize_array(&self) -> bool {
        if let AstNodeData::InitializeArray {
            elements: _,
        } = self.data
        {
            return true;
        }
        return false;
    }
    pub fn get_initialize_fields(&self) -> Result<Vec<(NodeID, NodeID)>> {
        if let AstNodeData::Initialize { ty: _, ref fields } = self.data {
            return Ok(fields.clone());
        }
        Err(anyhow!("expected AstNodeData::Initialize got: {:?}", self))
    }

    pub fn get_initialize_type_name(&self) -> Result<NodeID> {
        if let AstNodeData::Initialize { ref ty, fields: _ } = self.data {
            return Ok(ty.clone());
        }
        Err(anyhow!("expected AstNodeData::Initialize got: {:?}", self))
    }

    pub fn get_namespace_ns_id(&self) -> Result<NodeID> {
        if let AstNodeData::NamespaceAccess {
            ref namespace,
            field: _,
        } = self.data
        {
            return Ok(namespace.clone());
        }
        Err(anyhow!("expected AstNodeData::Namespace got: {:?}", self))
    }

    pub fn get_namespace_field_id(&self) -> Result<NodeID> {
        if let AstNodeData::NamespaceAccess {
            namespace: _,
            ref field,
        } = self.data
        {
            return Ok(field.clone());
        }
        Err(anyhow!("expected AstNodeData::Namespace got: {:?}", self))
    }
    pub fn get_array_elems(&self) -> Result<Vec<NodeID>> {
        if let AstNodeData::InitializeArray {
            ref elements,
        } = self.data
        {
            return Ok(elements.clone());
        }
        Err(anyhow!("expected AstNodeData::FnCall got: {:?}", self))
    }
    
    pub fn get_fn_call_fn_name(&self) -> Result<NodeID> {
        if let AstNodeData::FnCall {
            ref fn_name,
            args: _,
        } = self.data
        {
            return Ok(fn_name.clone());
        }
        Err(anyhow!("expected AstNodeData::FnCall got: {:?}", self))
    }
    pub fn is_pointer(&self) -> bool {
        if let AstNodeData::PointerOf(_) = self.data {
            return true;
        }
        return false;
    }
    pub fn is_def(&self) -> bool {
        if let AstNodeData::Def {
            mutable: _,
            name: _,
            expr: _,
        } = self.data
        {
            return true;
        }
        return false;
    }
    pub fn is_decl(&self) -> bool {
        if let AstNodeData::Decl{name: _, ty: _} = self.data {
            return true;
        }
        return false;
    }
    pub fn is_load(&self) -> bool {
        if let AstNodeData::Load(_) = self.data {
            return true;
        }
        return false;
    }
    pub fn get_def(&self) -> Result<(bool, NodeID, NodeID)> {
        if let AstNodeData::Def {
            mutable,
            name: ref name,
            expr: ref expr,
        } = self.data
        {
            return Ok((mutable, name.clone(), expr.clone()));
        }
        Err(anyhow!("expected AstNodeData::Def got: {:?}", self))
    }
    pub fn get_decl(&self) -> Result<(NodeID, NodeID)> {
        if let AstNodeData::Decl{name: ref ident_node_id, ref ty} = self.data {
            return Ok((ident_node_id.clone(), ty.clone()));
        }
        Err(anyhow!("expected AstNodeData::Decl got: {:?}", self))
    }
    pub fn get_fn_signature(&self) -> Result<(Vec<NodeID>, NodeID)> {
        if let AstNodeData::FnType { args, ret } = &self.data {
            return Ok((args.clone(), ret.clone()));
        }
        Err(anyhow!("expected AstNodeData::Decl got: {:?}", self))
    }
    pub fn get_name_for_defs_and_decls(&self) -> Option<NodeID> {
        match &self.data {
            AstNodeData::Def {
                mutable,
                name,
                expr,
            } => {
                return Some(name.clone());
            }

            AstNodeData::Decl{name: ident_id, ty: _ } => {
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
        if let AstNodeData::Initialize { ty: _, fields: _ } = self.data {
            return true;
        }
        return false;
    }

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

    pub fn add_type_inference(&mut self, id: &NodeID, type_infer: Type) {
        let mut node = self.nodes.get_mut(id).unwrap();
        node.type_information = type_infer;
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
    fn get_file_loads_from_block(&self, block_id: NodeID) -> Result<Vec<NodeID>> {
        let mut loads = vec![];
        let block = self.get_node(block_id.clone())?;
        for node_id in block.get_block()? {
            let node = self.get_node(node_id.clone())?;
            if node.is_load() {
                loads.push(node_id);
            }
        }

        Ok(loads)
    }

    pub fn add_block_ty(&mut self, node_id: NodeID, block_ty: BlockType) -> Result<()> {
        let block = self.nodes.get_mut(&node_id.clone()).unwrap();
        if let AstNodeData::Block { symbols:_, ref mut ty, is_file_root, nodes: _ } = block.data {
            *ty = block_ty;
            return Ok(());
        }

        Err(anyhow!("expected a block node found: {:?}", block))
    }

    fn find_identifier_type(
        &self,
        start_block: NodeID,
        index_in_block: isize,
        ident: String,
        other_asts: &Vec<Ast>,
    ) -> Result<Type> {
        let block = self.get_node(start_block.clone())?;
        if let AstNodeData::Block { ty: _, is_file_root, nodes, symbols } = block.data {
            match symbols.get(&ident) {
                Some(ty) => {
                    if ty.is_struct() {
                        return Ok(Type::TypeRef { name: ident, actual_ty: Box::new(ty.clone()) })
                    }
                    return Ok(ty.clone());
                },
                None => {},
            };
        }
        
        // if we reach here we didn't find type of that identifier in the block
        if block.parent_block != "" {
            return self.find_identifier_type(block.parent_block, -1, ident, other_asts);
        }

        if block.parent_block == "" {
            // we are at root of the file so we should check file loads.
            let load_nodes = self.get_file_loads_from_block(start_block.clone())?;
            for node_id in &load_nodes {
                let load_node = self.get_node(node_id.clone())?;
                let path = load_node.get_load()?;
                for ast in other_asts {
                    if ast.filename == utils::find_abs_path_to_file(&path)? || ast.filename == utils::find_abs_path_to_file(&format!("{}.loki", path).to_string())?  {
                        let ty = ast.find_identifier_type(
                            ast.top_level.clone(),
                            -1,
                            ident.clone(),
                            other_asts,
                        )?;
                        if !ty.is_unknown() {
                            return Ok(ty);
                        }
                    }
                }
            }
        }

        return Ok(Type::Unknown);
    }
    pub fn block_remove_node_by_id(&mut self, block_id: NodeID, node_id: NodeID) -> Result<usize> {
        let block = self.nodes.get_mut(&block_id.clone()).unwrap();
        if let AstNodeData::Block { symbols:_, is_file_root, ref mut nodes, ty:_ } = block.data {
            let node_idx = nodes.iter().position(|id| id == &node_id).unwrap();
            nodes.remove(node_idx);
            return Ok(node_idx);
        }
        return Err(anyhow!("expected a block node got : {:?}", self));
    }

    pub fn block_insert_at_index(&mut self, block_id:NodeID, idx: usize, node: AstNode) -> Result<()> {
        self.nodes.insert(node.id.clone(), node.clone());
        let block = self.nodes.get_mut(&block_id.clone()).unwrap();
        if let AstNodeData::Block { symbols: _, is_file_root, ref mut nodes, ty:_ } = block.data {
            nodes.insert(idx, node.id);
            return Ok(());
        }
        return Err(anyhow!("expected a block node got : {:?}", self));
    }
    pub fn block_get_node_idx(&self, block_id: NodeID, node_id: NodeID) -> Result<usize> {
        let block = self.nodes.get(&block_id.clone()).unwrap();
        if let AstNodeData::Block { symbols:_, is_file_root, ref nodes, ty:_ } = block.data {
            let node_idx = nodes.iter().position(|id| id == &node_id).unwrap();
            return Ok(node_idx);
        }
        return Err(anyhow!("expected a block node got : {:?}", self));
    }
    fn type_expression(
        &mut self,
        expr_id: NodeID,
        index_in_block: usize,
        other_asts: &Vec<Ast>,
    ) -> Result<()> {
        let expr = self.get_node(expr_id.clone())?;
        match expr.data {
            AstNodeData::IntPtrTy => {}
            AstNodeData::UintPtrTy => {}
            AstNodeData::Size(_) => {},
            AstNodeData::Cast(_, _) => {},
            AstNodeData::CompilerFlags(_) => {},
            AstNodeData::Host(_) => {},
            AstNodeData::Def { mutable, name, expr } => {},
            AstNodeData::Decl { name, ty } => {},
            AstNodeData::Assign { lhs, rhs } => {},
            AstNodeData::If { cases } => {},
            AstNodeData::For { start, cond, cont, body } => {},
            AstNodeData::ForIn { iterator, iterable, body } => {},
            AstNodeData::While { cond, body } => {},
            AstNodeData::Break => {},
            AstNodeData::Continue => {},
            AstNodeData::Return(_) => {},

            AstNodeData::Load(_) => {},
            
            AstNodeData::CVarArgs => {},
            AstNodeData::CString => {},
            AstNodeData::IntTy(_) => {},
            AstNodeData::UintTy(_) => {},
            AstNodeData::FloatTy(_) => {},
            AstNodeData::BoolTy => {},
            AstNodeData::StringTy => {},
            AstNodeData::CharTy => {},
            AstNodeData::VoidTy => {},
            AstNodeData::ArrayTy { length, elem_ty } => {
            },
            AstNodeData::PointerTy(ref pointee_id) => {
                self.type_expression(pointee_id.clone(), index_in_block, other_asts)?;
                let pointee = self.get_node(pointee_id.clone())?;
                self.add_type_inference(&expr_id.clone(), Type::Pointer(Box::new(pointee.type_information)));
            },
            AstNodeData::FnType { ref args, ref ret } => {
                for arg in args {
                    self.type_decl(arg.clone(), index_in_block, other_asts)?;
                }
                self.type_expression(ret.clone(), index_in_block, other_asts)?;
                let arg_types: Vec<Type> = args.iter().map(|arg_id| self.get_node(arg_id.clone()).unwrap()).map(|arg| arg.type_information).collect();
                for (idx, arg) in args.iter().enumerate() {
                    let arg = self.get_node(arg.clone())?;
                    let (name, _) = arg.get_decl()?;
                    let ident = self.get_node(name)?.get_ident()?;
                }
                self.add_type_inference(&expr.id.clone(), Type::FnType(arg_types, Box::new(self.get_node(ret.clone())?.type_information)));
            },
            AstNodeData::Block { ty, is_file_root, nodes, symbols } => {},
            AstNodeData::StructTy(_) => {},
            AstNodeData::EnumTy(_) => {},

            AstNodeData::Unsigned(_) => {},
            AstNodeData::Signed(_) => {},
            AstNodeData::StringLiteral(_) => {},
            AstNodeData::Float(_) => {},
            AstNodeData::Bool(_) => {},
            AstNodeData::Char(_) => {},
            AstNodeData::Ident(ref ident) => {
                let infered_type = self.find_identifier_type(
                    expr.parent_block.clone(),
                    index_in_block as isize,
                    ident.clone(),
                    other_asts,
                )?;
                self.add_type_inference(&expr_id.clone(), infered_type.clone());
            },
            AstNodeData::Paren(ref expr_id) => {
                self.type_expression(expr_id.clone(), index_in_block, other_asts)?;
            }
            AstNodeData::Not(ref expr_id) => {
                self.type_expression(expr_id.clone(), index_in_block, other_asts)?;
                let expr = self.get_node(expr_id.clone())?;
                if !expr.type_information.is_bool() {
                    return Err(self.report_error(format!("not needs a boolean typed expression but got {:?}", expr), expr));
                }

                self.add_type_inference(expr_id, Type::Bool);
            },
            AstNodeData::ArrayIndex { arr, idx } => {
                self.type_expression(arr, index_in_block, other_asts)?;
                self.type_expression(idx, index_in_block, other_asts)?;
            },
            AstNodeData::BinaryOperation { operation, left: ref left_id, right: ref right_id } => {
                let left = self.get_node(left_id.clone())?;
                self.type_expression(left_id.clone(), index_in_block, other_asts)?;
                let right = self.get_node(right_id.clone())?;
                self.type_expression(right_id.clone(), index_in_block, other_asts)?;
            },
            AstNodeData::NamespaceAccess { namespace: ref ns_id, field: ref field_id } => {
                // expresion is a namespace access, namespace part can be any expression but field is always an identifier.
                let field = self.get_node(field_id.clone())?.get_ident()?;
                self.type_expression(ns_id.clone(), index_in_block, other_asts)?;
                let ns = self.get_node(ns_id.clone())?;
                let ns_ty = self.get_node(ns_id.clone())?.type_information;
                if !ns_ty.is_type_def()
                    && !(ns_ty.is_type_ref() && ns_ty.get_actual_ty_type_ref()?.is_type_def())
                    && !(ns_ty.is_pointer() && ns_ty.get_pointer_pointee()?.is_type_def())
                    && !(ns_ty.is_pointer()
                        && ns_ty.get_pointer_pointee()?.is_type_ref()
                        && ns_ty
                            .get_pointer_pointee()?
                            .get_actual_ty_type_ref()?
                            .is_type_def())
                {
                    return Err(anyhow!(
                        ". operator can only be used for structs and enums but you used {:?} in expression: {:?}",
                        ns_ty, ns,
                    ));
                }

                if let Type::Struct { ref fields } = ns_ty {
                    let mut infered_type = Type::Unknown;
                    for (name, ty) in fields.iter() {
                        if name == &field {
                            infered_type = ty.clone();
                        }
                    }
                    if infered_type.is_unknown() {
                        return Err(anyhow!(
                            "struct {:?} has no field named {}",
                            ns_ty.clone(),
                            field
                        ));
                    }
                    self.add_type_inference(&expr_id.clone(), infered_type.clone());
                } else if let Type::Enum { ref variants } = ns_ty {
                    let mut infered_type = Type::Unknown;
                    for name in variants.iter() {
                        if name == &field {
                            infered_type = Type::UnsignedInt(64);
                        }
                    }
                    if infered_type.is_unknown() {
                        return Err(anyhow!(
                            "enum {:?} has no variant named {}",
                            ns_ty.clone(),
                            field
                        ));
                    }
                    self.add_type_inference(&expr_id.clone(), Type::UnsignedInt(64));
                    self.add_type_inference(&ns_id, ns_ty);
                } else if let Type::Pointer(ref pointee_ty) = ns_ty {
                    self.add_type_inference(&ns_id, Type::Pointer(pointee_ty.clone()));
                    if let Type::Struct{ref fields} = pointee_ty.deref() {
                        let mut infered_type = Type::Unknown;
                        for (name, ty) in pointee_ty.get_struct_fields()? {
                            if name == field {
                                infered_type = ty;
                            }
                        }
                        if infered_type.is_unknown() {
                            return Err(anyhow!(
                                "struct {:?} has no field named {}",
                                pointee_ty.clone(),
                                field
                            ));
                        }
                        self.add_type_inference(&expr_id.clone(), infered_type.clone());
                    }

                    if let Type::TypeRef { ref name, ref actual_ty } = pointee_ty.deref() {
                        if pointee_ty.get_actual_ty_type_ref()?.is_struct() {
                            let mut infered_type = Type::Unknown;
                            for (name, ty) in
                                pointee_ty.get_actual_ty_type_ref()?.get_struct_fields()?
                            {
                                if name == field {
                                    infered_type = ty;
                                }
                            }
                            if infered_type.is_unknown() {
                                return Err(anyhow!(
                                    "struct {:?} has no field named {}",
                                    pointee_ty.clone(),
                                    field
                                ));
                            }
                            self.add_type_inference(&expr_id.clone(), infered_type.clone());
                        } else {
                            return Err(anyhow!("you can only use a pointer to struct type as a namespace but you used {:?}", pointee_ty.clone()));
                        }
                    }
                }   
            },
            AstNodeData::Initialize { ty: ref type_name_id, ref fields } => {
                let type_name = self.get_node(type_name_id.clone())?.get_ident()?;
                let infered_type = self.find_identifier_type(
                    expr.parent_block.clone(),
                    index_in_block as isize,
                    type_name.clone(),
                    other_asts,
                )?;
                self.add_type_inference(
                    &expr_id,
                    infered_type.clone(),
                );
            },
            AstNodeData::InitializeArray { elements: ref arr_elems } => {
                for elem_id in arr_elems {
                    self.type_expression(elem_id.clone(), index_in_block, other_asts)?;
                    let elem = self.get_node(elem_id.clone())?;
                }
                let elem_id = &arr_elems[0];
                let elem = self.get_node(elem_id.clone())?;
                
                self.add_type_inference(&expr.id, Type::Array(arr_elems.len() as u64, Box::new(elem.type_information)));
            },
            AstNodeData::FnDef { sign: ref sign_id, body: ref body_id } => {
                let sign = self.get_node(sign_id.clone())?;
                let (args, ret) = sign.get_fn_signature()?;
                for arg in &args {
                    self.type_decl(arg.clone(), index_in_block, other_asts)?;
                }
                self.type_expression(ret.clone(), index_in_block, other_asts)?;
                let arg_types: Vec<Type> = args.iter().map(|arg_id| self.get_node(arg_id.clone()).unwrap()).map(|arg| arg.type_information).collect();
                for (idx, arg) in args.iter().enumerate() {
                    let arg = self.get_node(arg.clone())?;
                    let (name, _) = arg.get_decl()?;
                    let ident = self.get_node(name)?.get_ident()?;
                    let block = self.nodes.get_mut(body_id).unwrap();
                    if let AstNodeData::Block { ty: _ , is_file_root, nodes, symbols } = &mut block.data {
                        symbols.insert(ident, arg_types[idx].clone());
                    }
                }
                self.type_block(body_id.clone(), other_asts)?;

                let ret_ty = self.get_node(ret.clone())?.type_information;
                self.add_type_inference(&expr.id.clone(), Type::FnType(arg_types, Box::new(ret_ty)));
            },
            AstNodeData::FnCall { fn_name: _, args: _ } => {
                self.type_fn_call(expr_id.clone(), index_in_block, other_asts)?;
            },
            AstNodeData::PointerOf(ref pointee_expr_id) => {
                let pointer_expr_id = expr.get_pointer_expr()?;
                let infered_type =
                    self.type_expression(pointer_expr_id.clone(), index_in_block, other_asts)?;
                let pointer_expr = self.get_node(pointer_expr_id)?;
                self.add_type_inference(
                    &expr_id,
                    Type::Pointer(Box::new(pointer_expr.type_information)),
                )
            },
            AstNodeData::Deref(ref deref_expr_id) => {
                let infered_type =
                    self.type_expression(deref_expr_id.clone(), index_in_block, other_asts)?;
                let deref_expr = self.get_node(deref_expr_id.clone())?;
                self.add_type_inference(&expr_id, deref_expr.type_information.get_pointer_pointee()?)
            },

            
        }
        Ok(())
    }
    fn type_decl(&mut self, node_id: NodeID, index_in_block: usize, other_asts: &Vec<Ast>) -> Result<()> {
        let decl = self.get_node(node_id)?;
        let (ident_id, ty_id) = decl.get_decl()?;
        let ident = self.get_node(ident_id.clone())?.get_ident()?;
        let ty = self.get_node(ty_id.clone())?;
        self.type_expression(ty_id.clone(), index_in_block, other_asts)?;
        let ty = self.get_node(ty_id.clone())?;
        self.add_type_inference(&ident_id.clone(), ty.type_information.clone());
        self.add_type_inference(&decl.id.clone(), ty.type_information.clone());
        let block = self.nodes.get_mut(&decl.parent_block).unwrap();
        if let AstNodeData::Block { ty: _, is_file_root, nodes, symbols } = &mut block.data {
            symbols.insert(ident, ty.type_information);
        }
        Ok(())
    }
    
    fn type_definition(
        &mut self,
        node_id: NodeID,
        index_in_block: usize,
        other_asts: &Vec<Ast>,
    ) -> Result<()> {
        let node = self.get_node(node_id.to_string())?;
        let (_, name_id, expr_id) = node.get_def()?;
        let name = self.get_node(name_id.clone())?.get_ident()?;
        self.type_expression(expr_id.clone(), index_in_block, other_asts)?;
        let expr = self.get_node(expr_id)?;
        self.add_type_inference(&name_id.clone(), expr.type_information.clone());
        let block = self.nodes.get_mut(&node.parent_block).unwrap();
        if let AstNodeData::Block { ty, is_file_root, nodes, symbols } = &mut block.data {
            symbols.insert(name, expr.type_information);
        }
        Ok(())
    }

    fn type_fn_call(
        &mut self,
        node_id: NodeID,
        index_in_block: usize,
        other_asts: &Vec<Ast>,
    ) -> Result<()> {
        let fn_call_node = self.get_node(node_id.clone())?;
        let (fn_name_id, args) = fn_call_node.get_fn_call()?;
        for (idx, arg_id) in args.iter().enumerate() {
            self.type_expression(arg_id.clone(), index_in_block, other_asts)?;
        }
        let mut fn_ty: Option<Type> = None;
        let mut fn_name: Option<String> = None;
        // type compiler functions based on arguments.
        if fn_call_node.tags.contains(&AstTag::CompilerFunctionCall) {
            let arg_types: Vec<Type> = args.iter().map(|arg_id| self.get_node(arg_id.clone()).unwrap()).map(|arg| arg.type_information).collect(); 
            if fn_name_id == "#cast" {
                fn_ty = Some(Type::FnType(arg_types.clone(), Box::new(arg_types[1].clone())));
            } else if fn_name_id == "#size" {
                fn_ty = Some(Type::FnType(arg_types.clone(), Box::new(Type::UnsignedInt(64))))
            }
            fn_name = Some(fn_name_id);
        } else {
            fn_name = Some(self.get_node(fn_name_id.to_string())?.get_ident()?);
            fn_ty = Some(self.find_identifier_type(fn_call_node.parent_block.clone(), index_in_block as isize, fn_name.as_ref().unwrap().clone(), other_asts)?);
        }

        let fn_ty = fn_ty.unwrap();
        let fn_name = fn_name.unwrap();

        if fn_ty.is_unknown() && !fn_call_node.tags.contains(&AstTag::CompilerFunctionCall) {
            return Err(self.report_error(format!("unknown function call: {}", fn_name), fn_call_node));
        }
        let args_ty = fn_ty.get_fn_args();
        let ret_ty = fn_ty.get_fn_ret_ty();
        
        self.add_type_inference(&node_id.clone(), ret_ty);
        // for (idx, arg_id) in args.iter().enumerate() {
        //     let arg = self.get_node(arg_id.clone())?;
        //     if (args_ty[idx] != arg.type_information) || (args_ty[idx] == Type::Pointer(Box::new(Type::Char)) && arg.type_information == Type::String) {
        //         return Err(anyhow!("in file {} at {}:{} argument {} of function {} type mismatch, expected {:?} found {:?}", fn_call_node.filename, fn_call_node.line, fn_call_node.col , idx+1, fn_name, args_ty[idx], arg.type_information));
        //     }
        // }

    
        Ok(())
    }

    fn type_block(&mut self, block_id: NodeID, other_asts: &Vec<Ast>) -> Result<()> {
        let mut block = self.get_node(block_id.to_string())?.get_block()?;
        for (index, node_id) in block.iter().enumerate() {
            let node = self.get_node(node_id.to_string())?;
            match node.data {
                AstNodeData::UintPtrTy => {},
                AstNodeData::IntPtrTy => {},
                AstNodeData::Size(_) => todo!(),
                AstNodeData::Cast(_, _) => todo!(),
                AstNodeData::Paren(_) => {}
                AstNodeData::Decl { name, ty } => {
                    self.type_decl(node_id.clone(), index, other_asts)?;
                },

                AstNodeData::FnCall { fn_name, args } => {
                    self.type_fn_call(node_id.clone(), index, other_asts)?;
                },
                AstNodeData::Assign { lhs, rhs } => {
                    self.type_expression(lhs.clone(), index, other_asts)?;
                    self.type_expression(rhs.clone(), index, other_asts)?;
                },
                AstNodeData::Def { mutable, name, expr } => {
                    self.type_definition(node.id.clone(), index, other_asts)?;
                },
                AstNodeData::If { ref cases } => {
                    for case in cases {
                        self.type_expression(case.0.clone(), index, other_asts)?;
                        let cond = self.get_node(case.0.clone())?;
                        if !cond.type_information.is_bool() {
                            return Err(self.report_error(format!("if condition must be a boolean but {:?}", cond.type_information), node));
                        }
                        self.type_block(case.1.clone(), other_asts)?;
                    }
                },
                AstNodeData::For { start: ref start_id, cond: ref cond_id, cont: ref cont_id, ref body } => {
                    self.type_definition(start_id.clone(), index, other_asts)?;
                    self.type_expression(cond_id.clone(), index, other_asts)?;
                    let cond = self.get_node(cond_id.clone())?;
                    if !cond.type_information.is_bool() {
                        return Err(self.report_error(format!("for loop condition must be a boolean but {:?}", cond.type_information), node));
                    }
                    self.type_block(body.clone(), other_asts)?;
                },
                AstNodeData::ForIn { iterator: ref iterator_id, iterable: ref iterable_id, ref body } => {
                    self.type_expression(iterable_id.clone(), index, other_asts)?;
                    let mut iterator = self.get_node(iterator_id.clone())?;
                    let iterable = self.get_node(iterable_id.clone())?;
                    if iterable.type_information.is_unknown() {
                        return Err(self.report_error(format!("unknown type for iterable {:?} in for in statement: {:?}", iterable, node), node))
                    }
                    if !iterable.type_information.is_iterable() {
                        return Err(self.report_error(format!("for in statement needs iterable to be either an array or dynamic array but found: {:?}", iterable.type_information), node))
                    }
                    if iterator.is_unknown() {
                        iterator.type_information = iterable.type_information.get_array_elem_type()?;
                        self.nodes.insert(iterator.id.clone(), iterator.clone());
                    }
                    if iterator.type_information != iterable.type_information.get_array_elem_type()? {
                        return Err(self.report_error(format!("for in iterable element type and iterator must use same type: {:?} vs {:?}", iterator.type_information, iterable.type_information.get_array_elem_type()?), node))
                    }
                    let block = self.nodes.get_mut(&body.clone()).unwrap();
                    if let AstNodeData::Block { ty: _ , is_file_root, nodes, symbols } = &mut block.data {
                        symbols.insert(iterator.get_ident()?, iterator.type_information.clone());
                    }
                    self.type_block(body.clone(), other_asts)?;
                },
                AstNodeData::While { cond: ref cond_id, ref body } => {
                    self.type_expression(cond_id.clone(), index, other_asts)?;
                    let cond = self.get_node(cond_id.clone())?;
                    if !cond.type_information.is_bool() {
                        return Err(self.report_error(format!("while condition must be a boolean but {:?}", cond.type_information), node));
                    }
                    self.type_block(body.clone(), other_asts)?;
                },

                AstNodeData::Return(ref expr_id) => {
                    self.type_expression(expr_id.clone(), index, other_asts)?;
                },




                AstNodeData::CompilerFlags(_) => {},
                AstNodeData::Load(_) => {},
                AstNodeData::Host(_) => {},
                
                
                AstNodeData::CVarArgs => {},
                AstNodeData::CString => {},
                AstNodeData::IntTy(_) => {},
                AstNodeData::UintTy(_) => {},
                AstNodeData::FloatTy(_) => {},
                AstNodeData::BoolTy => {},
                AstNodeData::StringTy => {},
                AstNodeData::CharTy => {},
                AstNodeData::VoidTy => {},
                AstNodeData::ArrayTy { length, elem_ty } => {},
                AstNodeData::FnType { args, ret } => {},
                AstNodeData::Block { ty, is_file_root, nodes, symbols } => {},
                AstNodeData::StructTy(_) => {},
                AstNodeData::EnumTy(_) => {},
                AstNodeData::Unsigned(_) => {},
                AstNodeData::Signed(_) => {},
                AstNodeData::StringLiteral(_) => {},
                AstNodeData::Float(_) => {},
                AstNodeData::Bool(_) => {},
                AstNodeData::Char(_) => {},
                AstNodeData::Ident(_) => {},
                AstNodeData::Not(_) => {},
                AstNodeData::ArrayIndex { arr, idx } => {},
                AstNodeData::BinaryOperation { operation, left, right } => {},
                AstNodeData::NamespaceAccess { namespace, field } => {},
                AstNodeData::Initialize { ty, fields } => {},
                AstNodeData::InitializeArray { elements } => {},
                AstNodeData::FnDef { sign, body } => {},
                
                AstNodeData::PointerOf(_) => {},
                AstNodeData::Deref(_) => {},
                AstNodeData::Break => {},
                AstNodeData::Continue => {},
                AstNodeData::PointerTy(_) => {},

            }
        }

        Ok(())
    }
    pub fn fill_root_symbols(&mut self, other_asts: &Vec<Ast>) -> Result<()> {
        let mut block = self.get_node(self.top_level.clone())?;
        let mut symbols = HashMap::<String, Type>::new();
        for (index, node_id) in block.get_block()?.iter().enumerate() {
            let node = self.get_node(node_id.clone())?;
            if let AstNodeData::Def { mutable, ref name, expr: ref expr_id } = node.data {
                let name = self.get_node(name.clone())?;
                self.type_definition(node.id.clone(), index, other_asts)?;
                let expr = self.get_node(expr_id.clone())?;
                symbols.insert(name.get_ident()?, expr.type_information);
            }

            if let AstNodeData::Decl { ref name, ref ty } = &node.data {
                self.type_decl(node.id.clone(), index, other_asts)?;
                let name = self.get_node(name.clone())?;
                symbols.insert(name.get_ident()?, name.type_information);
            }
        }
        let mut block = self.nodes.get_mut(&self.top_level.clone()).unwrap();
        if let AstNodeData::Block { ref ty, is_file_root, ref nodes, symbols: ref mut block_symbols } = &mut block.data {
            for (key, value) in symbols {
                block_symbols.insert(key, value);
            }
        }
        Ok(())
    }
    pub fn type_ast(&mut self, other_asts: &Vec<Ast>) -> Result<()> {
        self.fill_root_symbols(other_asts)?;
        self.type_block(self.top_level.to_string(), other_asts)?;
        Ok(())
    }
    fn lower_initialize(&mut self) -> Result<()> {
        let node_ids: Vec<NodeID> = self.nodes.keys().map(|id| id.clone()).collect();
        for node_id in &node_ids {
            let node = self.get_node(node_id.clone());
            if node.is_err() {
                // we are going to look into something we deleted in previous loops
                continue;
            }
            let node = node.unwrap();
            if let AstNodeData::Def { mutable, name: ref ident_id, expr: ref expr_id } = node.data {
                let expr = self.get_node(expr_id.clone())?;
                let ident = self.get_node(ident_id.clone())?;
                if expr.is_initialize() {
                    self.nodes.remove(&node.id.clone());
                    let mut idx = self.block_remove_node_by_id(node.parent_block.clone(), node.id.clone())?;
                    self.block_insert_at_index(node.parent_block.clone(), idx, AstNode { 
                        id: node.id.clone(),
                        data: AstNodeData::Decl {
                            name: ident_id.clone(),
                            ty: "".to_string(),
                        },
                        type_information: expr.type_information.clone(),
                        parent_block: node.parent_block.clone(),
                        tags: vec![],
                        line: node.line,
                        col: node.col, 
                        filename: node.filename.clone(),
                        index_in_block: idx, 
                    })?;
                    let field_values = expr.get_initialize_fields()?;
                    idx += 1;
                    for fv in field_values {
                        let lhs_ns = AstNode {
                            id: utils::generate_node_id(),
                            data: AstNodeData::Ident(ident.get_ident()?),
                            type_information: ident.type_information.clone(),
                            parent_block: node.parent_block.clone(),
                            tags: vec![],
                            line: node.line,
                            col: node.col,
                            filename: node.filename.clone(),
                            index_in_block: idx,
                        };
                        self.nodes.insert(lhs_ns.id.clone(), lhs_ns.clone());
                        let field = self.get_node(fv.0)?;
                        let value = self.get_node(fv.1)?;

                        let lhs_field = AstNode {
                            id: utils::generate_node_id(),
                            data: AstNodeData::Ident(field.get_ident()?),
                            type_information: value.type_information.clone(),
                            parent_block: node.parent_block.clone(),
                            tags: vec![],
                            line: node.line,
                            col: node.col,
                            filename: node.filename.clone(),
                            index_in_block: idx,
                        };
                        self.nodes.insert(lhs_field.id.clone(), lhs_field.clone());

                        let lhs = AstNode {
                            id: utils::generate_node_id(),
                            data: AstNodeData::NamespaceAccess { namespace: lhs_ns.id.clone(), field: lhs_field.id.clone() },
                            type_information: value.type_information.clone(),
                            parent_block: node.parent_block.clone(),
                            tags: vec![],
                            line: node.line,
                            col: node.col,
                            filename: node.filename.clone(),
                            index_in_block: idx,
                        };
                        self.nodes.insert(lhs.id.clone(), lhs.clone());

                        self.block_insert_at_index(node.parent_block.clone(), idx, AstNode { 
                            id: utils::generate_node_id(),
                            data: AstNodeData::Assign { lhs: lhs.id.clone(), rhs: value.id.clone() },
                            type_information: expr.type_information.clone(),
                            parent_block: node.parent_block.clone(),
                            tags: vec![],
                            line: node.line,
                            col: node.col, 
                            filename: node.filename.clone(),
                            index_in_block: idx,
                        })?;
                        idx += 1;
                        // BUG: 
                        self.nodes.remove(&expr.id.clone());
                    } 
                }
            }
        }
        let node_ids: Vec<NodeID> = self.nodes.keys().map(|id| id.clone()).collect();

        for node_id in &node_ids {
            let node = self.get_node(node_id.clone())?;
            if !node.is_initialize() {
                continue;
            }
            let mut idx = node.index_in_block;
            self.block_insert_at_index(node.parent_block.clone(), idx, AstNode { 
                id: utils::generate_node_id(),
                data: AstNodeData::Decl {
                    name: node.id.clone(),
                    ty: "".to_string(),
                },
                type_information: node.type_information.clone(),
                parent_block: node.parent_block.clone(),
                tags: vec![],
                line: node.line,
                col: node.col, 
                filename: node.filename.clone(),
                index_in_block: idx, 
            })?;
            idx += 1;
            let field_values = node.get_initialize_fields()?;
            for fv in field_values {
                let lhs_ns = AstNode {
                    id: utils::generate_node_id(),
                    data: AstNodeData::Ident(node.id.clone()),
                    type_information: node.type_information.clone(),
                    parent_block: node.parent_block.clone(),
                    tags: vec![],
                    line: node.line,
                    col: node.col,
                    filename: node.filename.clone(),
                    index_in_block: idx,
                };
                self.nodes.insert(lhs_ns.id.clone(), lhs_ns.clone());
                let field = self.get_node(fv.0)?;
                let value = self.get_node(fv.1)?;

                let lhs_field = AstNode {
                    id: utils::generate_node_id(),
                    data: AstNodeData::Ident(field.get_ident()?),
                    type_information: value.type_information.clone(),
                    parent_block: node.parent_block.clone(),
                    tags: vec![],
                    line: node.line,
                    col: node.col,
                    filename: node.filename.clone(),
                    index_in_block: idx,
                };
                self.nodes.insert(lhs_field.id.clone(), lhs_field.clone());

                let lhs = AstNode {
                    id: utils::generate_node_id(),
                    data: AstNodeData::NamespaceAccess { namespace: lhs_ns.id.clone(), field: lhs_field.id.clone() },
                    type_information: value.type_information.clone(),
                    parent_block: node.parent_block.clone(),
                    tags: vec![],
                    line: node.line,
                    col: node.col,
                    filename: node.filename.clone(),
                    index_in_block: idx,
                };
                self.nodes.insert(lhs.id.clone(), lhs.clone());

                self.block_insert_at_index(node.parent_block.clone(), idx, AstNode { 
                    id: utils::generate_node_id(),
                    data: AstNodeData::Assign { lhs: lhs.id.clone(), rhs: value.id.clone() },
                    type_information: Type::NoType,
                    parent_block: node.parent_block.clone(),
                    tags: vec![],
                    line: node.line,
                    col: node.col, 
                    filename: node.filename.clone(),
                    index_in_block: idx,
                })?;
                idx += 1;
            } 
            self.nodes.insert(node.id.clone(), AstNode {
                id: node.id.clone(),
                data: AstNodeData::Ident(node.id.clone()),
                type_information: node.type_information.clone(),
                parent_block: node.parent_block.clone(),
                index_in_block: node.index_in_block,
                tags: node.tags.clone(),
                line: node.line,
                col: node.col,
                filename: node.filename.clone(),
            });
        }
        Ok(())
    }
    fn report_error(&self, msg: String, related_node: AstNode) -> anyhow::Error {
        return anyhow::format_err!("In file: {}, {}, at line: {}, column: {}", related_node.filename, msg, related_node.line, related_node.col);
    }

    fn lower_enums(&mut self) -> Result<()> {
        let node_ids: Vec<NodeID> = self.nodes.keys().map(|id| id.clone()).collect();
        for node_id in node_ids {
            let node = self.get_node(node_id.clone())?;
            if node.is_def() {
                let (_, def_name_id, expr_id) = node.get_def()?;
                let expr = self.get_node(expr_id.clone())?;
                let def_name = self.get_node(def_name_id.clone())?.get_ident()?;
                if expr.type_information.is_enum_def() {
                    let idx = self.block_remove_node_by_id(node.parent_block.clone(), node_id.to_string())?;
                    for (variant_idx, variant) in expr.get_enum_variants()?.iter().enumerate() {
                        let expr = AstNode {
                            id: utils::generate_node_id(),
                            data: AstNodeData::Unsigned(variant_idx as u64),
                            type_information: Type::UnsignedInt(64),
                            parent_block: node.parent_block.clone(),
                            tags: vec![],
                            line: node.line,
                            col: node.col,
                            filename: node.filename.clone(),
                            index_in_block: idx,
                        };
                        let (variant_name_idx, _) = self.get_node(variant.clone())?.get_decl()?;
                        let variant_name = self.get_node(variant_name_idx)?.get_ident()?;
                        let name = AstNode {
                            id: utils::generate_node_id(),
                            data: AstNodeData::Ident(format!("LOKI_ENUM_{}_{}", def_name, variant_name)),
                            type_information: Type::UnsignedInt(64),
                            parent_block: node.parent_block.clone(),
                            tags: vec![],
                            line: node.line,
                            col: node.col,
                            filename: node.filename.clone(),
                            index_in_block: idx,
                        };
                        self.nodes.insert(expr.id.clone(), expr.clone());
                        self.nodes.insert(name.id.clone(), name.clone());
                        self.block_insert_at_index(node.parent_block.clone(), idx, AstNode {
                            id: utils::generate_node_id(),
                            data: AstNodeData::Def { mutable: false, name: name.id.clone(), expr: expr.id.clone()},
                            type_information: Type::NoType,
                            parent_block: node.parent_block.clone(),
                            tags: vec![],
                            line: node.line,
                            col: node.col,
                            filename: node.filename.clone(),
                            index_in_block: idx,
                        })?;
                    }
                }
                if expr.is_namespace_access() {
                    let ns_id = expr.get_namespace_ns_id()?;
                    let field_id = expr.get_namespace_field_id()?;
                    let ns = self.get_node(ns_id.clone())?;
                    let field = self.get_node(field_id.clone())?;
                    if ns.type_information.is_enum_def() {
                        let new_enum_ref = AstNode {
                            id: expr.id.clone(),
                            data: AstNodeData::Ident(format!("LOKI_ENUM_{}_{}", ns.get_ident()?, field.get_ident()?)),
                            type_information: Type::UnsignedInt(64),
                            parent_block: node.parent_block,
                            tags: vec![],
                            line: node.line,
                            col: node.col,
                            filename: node.filename,
                            index_in_block: 0,
                        };
                        self.nodes.insert(expr.id.clone(), new_enum_ref);
                    }
                }    
            }
            
        }

        Ok(())
    }
    pub fn lower_features(&mut self) -> Result<()> {
        self.lower_initialize()?;
        self.lower_enums()?;

        Ok(())
    }

    pub fn new(
        filename: String,
        src: String,
        tokens: Vec<Token>,
        top_level_block: NodeID,
        nodes: HashMap<NodeID, AstNode>,
    ) -> Result<Self> {
        Ok(Self {
            filename,
            src,
            tokens,
            top_level: top_level_block,
            nodes,
        })
    }
}