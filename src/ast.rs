use std::collections::HashMap;
use std::default;

use crate::lexer::{Token, TokenType};
use anyhow::anyhow;
use anyhow::Result;
use serde::Serialize;
pub type NodeID = String;
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct NamespaceAccessType {
    pub namespace: Box<Type>,
    pub field: Box<Type>,
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
                    let field_id = decl.get_decl()?;
                    let field = ast.get_node(field_id)?;
                    fields.push((field.get_ident()?.clone(), decl.type_information));
                }

                Ok(Type::Struct { fields })
            }

            AstNodeData::EnumTy(ref vs) => {
                let mut variants = Vec::<String>::new();
                for decl_id in vs {
                    let decl = ast.get_node(decl_id.clone())?;
                    let variant_id = decl.get_decl()?;
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
            AstNodeData::ArrayTy {
                length,
                ref elem_ty,
            } => Ok(Type::Array(length, Box::new(elem_ty.clone()))),
            _ => Ok(Type::Unknown),
        }
    }
    pub fn from_struct_fields(decls: Vec<NodeID>, ast: &Ast) -> Result<Type> {
        let mut fields = Vec::<(String, Type)>::new();
        for decl_id in decls {
            let decl = ast.get_node(decl_id.clone())?;
            let field_id = decl.get_decl()?;
            let field = ast.get_node(field_id)?;
            fields.push((field.get_ident()?.clone(), decl.type_information));
        }

        Ok(Type::Struct { fields })
    }
    pub fn from_enum_variants(vs: Vec<NodeID>, ast: &Ast) -> Result<Type> {
        let mut variants = Vec::<String>::new();
        for decl_id in vs {
            let decl = ast.get_node(decl_id.clone())?;
            let variant_id = decl.get_decl()?;
            let variant = ast.get_node(variant_id)?;
            variants.push(variant.get_ident()?.clone());
        }

        Ok(Type::Enum { variants })
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
            _ => Err(anyhow!("expected struct type found: {:?}", self)),
        }
    }

    pub fn get_enum_variants(&self) -> Result<Vec<String>> {
        match self {
            Type::Enum { ref variants } => Ok(variants.clone()),
            _ => Err(anyhow!("expected enum type found: {:?}", self)),
        }
    }
    pub fn is_enum(&self) -> bool {
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

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct AstNode {
    pub id: NodeID,
    pub data: AstNodeData,
    pub type_information: Type,
    pub parent_block: NodeID,
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
    Def {
        mutable: bool,
        name: NodeID,
        expr: NodeID,
    },
    Decl(NodeID),
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
    StringTy,
    CharTy,
    VoidTy,
    ArrayTy {
        length: u64,
        elem_ty: Type,
    },
    FnType {
        args: Vec<NodeID>,
        ret: NodeID,
    },

    Block {
        is_file_root: bool,
        nodes: Vec<NodeID>,
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

    PointerTo(NodeID),
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

    pub fn is_unknown(&self) -> bool {
        return self.type_information.is_unknown();
    }

    pub fn block_is_file_root(&self) -> Result<bool> {
        if let AstNodeData::Block {
            nodes: _,
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
        if let AstNodeData::PointerTo(_) = self.data {
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
        if let AstNodeData::Decl(_) = self.data {
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
    pub fn get_decl(&self) -> Result<NodeID> {
        if let AstNodeData::Decl(ref ident_node_id) = self.data {
            return Ok(ident_node_id.clone());
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

    fn find_identifier_type(
        &self,
        start_block: NodeID,
        index_in_block: isize,
        ident: String,
        other_asts: &Vec<Ast>,
    ) -> Result<Type> {
        let block = self.get_node(start_block.clone())?;
        let block_nodes = block.get_block()?;
        for (index, node_id) in block_nodes.iter().enumerate() {
            if index_in_block > 0
                && index > index_in_block as usize
                && !block.block_is_file_root()?
            {
                break;
            }

            let node = self.get_node(node_id.clone())?;
            if node.is_def() {
                let (_, name_id, expr_id) = node.get_def()?;
                let name = self.get_node(name_id)?.get_ident()?;
                if name == ident {
                    let expr = self.get_node(expr_id)?;
                    return Ok(expr.type_information);
                }
            }

            if node.is_decl() {
                let name_id = node.get_decl()?;
                let name_node = self.get_node(name_id)?;
                let name = name_node.get_ident()?;

                if name == ident {
                    return Ok(name_node.type_information);
                }
            }
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
                    if ast.filename == path {
                        return ast.find_identifier_type(
                            ast.top_level.clone(),
                            -1,
                            ident.clone(),
                            other_asts,
                        );
                    }
                }
            }
        }

        return Ok(Type::Unknown);
    }
    fn infer_type_expr(
        &mut self,
        expr_id: NodeID,
        index_in_block: usize,
        other_asts: &Vec<Ast>,
    ) -> Result<()> {
        let expr = self.get_node(expr_id.clone())?;
        if expr.is_ident() {
            let expr_ident = expr.get_ident()?;
            let infered_type = self.find_identifier_type(
                expr.parent_block.clone(),
                index_in_block as isize,
                expr_ident,
                other_asts,
            )?;
            self.add_type_inference(&expr_id.clone(), infered_type.clone());
        }

        if expr.is_fn_def() {
            if let AstNodeData::FnDef { sign: _, ref body } = expr.data {
                self.infer_types_block(body.to_string(), other_asts)?;
            } else {
                unreachable!()
            }
        }

        if expr.is_deref() {
            let deref_expr_id = expr.get_deref_expr()?;
            let infered_type =
                self.infer_type_expr(deref_expr_id.clone(), index_in_block, other_asts)?;
            let deref_expr = self.get_node(deref_expr_id)?;
            self.add_type_inference(&expr_id, deref_expr.type_information.get_pointer_pointee()?)
        }

        if expr.is_pointer() {
            let pointer_expr_id = expr.get_pointer_expr()?;
            let infered_type =
                self.infer_type_expr(pointer_expr_id.clone(), index_in_block, other_asts)?;
            let pointer_expr = self.get_node(pointer_expr_id)?;
            self.add_type_inference(
                &expr_id,
                Type::Pointer(Box::new(pointer_expr.type_information)),
            )
        }

        if expr.is_fn_call() {
            let fn_name_id = expr.get_fn_call_fn_name()?;
            let fn_name = self.get_node(fn_name_id.to_string())?.get_ident()?;
            let infered_type = self.find_identifier_type(
                expr.parent_block.clone(),
                index_in_block as isize,
                fn_name,
                other_asts,
            )?;
            self.add_type_inference(&expr_id.clone(), infered_type.clone());
        }

        if expr.is_initialize() {
            let type_name_id = expr.get_initialize_type_name()?;
            let type_name = self.get_node(type_name_id.clone())?.get_ident()?;
            let infered_type = self.find_identifier_type(
                expr.parent_block.clone(),
                index_in_block as isize,
                type_name.clone(),
                other_asts,
            )?;
            self.add_type_inference(
                &expr_id,
                Type::TypeRef {
                    name: type_name.clone(),
                    actual_ty: Box::new(infered_type.clone()),
                },
            );
        }

        if expr.is_initialize_array() {
            let arr_elems = expr.get_array_elems()?;
            for elem_id in &arr_elems {
                self.infer_type_expr(elem_id.clone(), index_in_block, other_asts)?;
                let elem = self.get_node(elem_id.clone())?;
            }
            let elem_id = &arr_elems[0];
            let elem = self.get_node(elem_id.clone())?;
            
            self.add_type_inference(&expr.id, Type::Array(arr_elems.len() as u64, Box::new(elem.type_information)));
        }

        if expr.is_namespace_access() {
            // expresion is a namespace access, namespace part can be any expression but field is always an identifier.
            let ns_id = expr.get_namespace_ns_id()?;
            let field_id = expr.get_namespace_field_id()?;
            let field = self.get_node(field_id)?.get_ident()?;
            let ns = self.get_node(ns_id.clone())?;
            self.infer_type_expr(ns_id.clone(), index_in_block, other_asts)?;
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
                    ". operator can only be used for structs and enums but you used {:?}",
                    ns_ty
                ));
            }

            if ns_ty.is_struct() {
                let mut infered_type = Type::Unknown;
                for (name, ty) in ns_ty.get_struct_fields()? {
                    if name == field {
                        infered_type = ty;
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
            } else if ns_ty.is_enum() {
                let mut infered_type = Type::Unknown;
                for name in ns_ty.get_enum_variants()? {
                    if name == field {
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
            } else if ns_ty.is_pointer() {
                let pointee_ty = ns_ty.get_pointer_pointee()?;
                self.add_type_inference(&ns_id, Type::Pointer(Box::new(pointee_ty.clone())));
                if pointee_ty.is_struct() {
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

                if pointee_ty.is_type_ref() {
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
        }
        Ok(())
    }
    fn infer_type_def(
        &mut self,
        node_id: NodeID,
        index_in_block: usize,
        other_asts: &Vec<Ast>,
    ) -> Result<()> {
        let node = self.get_node(node_id.to_string())?;
        let (_, name_id, expr_id) = node.get_def()?;
        let name = self.get_node(name_id.clone())?.get_ident()?;
        self.infer_type_expr(expr_id.clone(), index_in_block, other_asts)?;
        let expr = self.get_node(expr_id)?;
        self.add_type_inference(&name_id.clone(), expr.type_information);
        Ok(())
    }

    fn infer_type_fn_call(
        &mut self,
        node_id: NodeID,
        index_in_block: usize,
        other_asts: &Vec<Ast>,
    ) -> Result<()> {
        let fn_call_node = self.get_node(node_id.clone())?;
        let (fn_name, args) = fn_call_node.get_fn_call()?;
        // check fn_name ?
        for arg_id in &args {
            self.infer_type_expr(arg_id.clone(), index_in_block, other_asts)?;
        }

        Ok(())
    }

    fn infer_types_block(&mut self, block_id: NodeID, other_asts: &Vec<Ast>) -> Result<()> {
        let mut block = self.get_node(block_id.to_string())?.get_block()?;
        for (index, node_id) in block.iter().enumerate() {
            let node = self.get_node(node_id.to_string())?;
            if node.is_def() {
                self.infer_type_def(node.id.clone(), index, other_asts)?;
                continue;
            }
            if node.is_fn_call() {
                self.infer_type_fn_call(node_id.clone(), index, other_asts)?;
                continue;
            }
        }

        Ok(())
    }
    pub fn infer_types(&mut self, other_asts: &Vec<Ast>) -> Result<()> {
        self.infer_types_block(self.top_level.to_string(), other_asts)?;
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
