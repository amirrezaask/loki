use std::{collections::HashMap};

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
    pub fn make_fn_signature(args: &Vec<AstNode>, ret: &AstNode) -> Self {
        let args_ty: Vec<AstNodeType> = args.iter().map(|arg| arg.infered_type.clone()).collect();

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
pub struct AstNode {
    pub id: NodeID,
    pub data: AstNodeData,
    pub infered_type: AstNodeType,
    pub tags: Vec<AstTag>
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct NamespaceAccess {
    pub namespace: Box<AstNode>,
    pub field: Box<AstNode>,
    pub namespace_is_enum: bool,
    pub namespace_is_pointer: bool,
}
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct AstFnSignature {
    pub args: Vec<AstNode>,
    pub ret: Box<AstNode>
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct AstFnDef {
    pub sign: AstFnSignature,
    pub body: Vec<AstNode>,    
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
    pub left: Box<AstNode>,
    pub right: Box<AstNode>
}
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct AstCaseBlock {
    pub cases: Vec<(AstNode, Vec<AstNode>)>,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct AstDef {
    pub mutable: bool,
    pub name: String,
    pub expr: Box<AstNode>,
}

pub type BitSize = usize;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum AstNodeData {
    //top level items
    CompilerFlags(String),
    Load(String),
    Host(String),
    Def(AstDef),
    Decl(String),
    Assign(Box<AstNode>, Box<AstNode>),

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


    Struct(Vec<AstNode>),
    Enum(bool, Vec<(AstNode, Option<AstNode>)>),

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
    
    Initialize(Box<AstNode>, Vec<(AstNode, AstNode)>),
    InitializeArray(Option<Box<AstNode>>, Vec<AstNode>),
    
    FnDef(AstFnDef),
    FnCall(Box<AstNode>, Vec<AstNode>),
    
    PointerTo(Box<AstNode>),
    Deref(Box<AstNode>),
    
    If(AstCaseBlock),
    
    For(Box<AstNode>, Box<AstNode>, Box<AstNode>, Vec<AstNode>),
    ForIn(Option<Box<AstNode>>, Box<AstNode>, Vec<AstNode>),
    While(Box<AstNode>, Vec<AstNode>),
    
    Break,
    Continue,

    Return(Box<AstNode>),
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

    pub fn is_initialize(&self) -> bool {
        if let AstNodeData::Initialize(_, _) = self.data {
            return true;
        }
        return false;
    }

    pub fn get_pointer_to_value(&self) -> AstNodeData {
        if let AstNodeData::PointerTo(ref obj) = self.data {
            return obj.data.clone();
        }
        panic!("expected a pointer to node found: {:?}", self);
    }

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

type File = String;
type Scope = Vec<usize>;
type Name = String;

#[derive(Debug, Clone)]
pub enum SymbolType {
    Primitive(AstNode),
    TypeRef(AstNode),
    TypeDef(AstNode)
}
impl SymbolType {
    pub fn new(node: &AstNode) -> Self {
        match node.data {
            AstNodeData::Enum(_, _) | AstNodeData::Struct(_) | AstNodeData::FnType(_)=> {
                SymbolType::TypeDef(node.clone())
            }

            AstNodeData::Ident(_) => {
                SymbolType::TypeRef(node.clone())
            }
            AstNodeData::IntTy(_) |
            AstNodeData::UintTy(_) |
            AstNodeData::FloatTy(_) |
            AstNodeData::BoolTy |
            AstNodeData::StringTy |
            AstNodeData::CharTy |
            AstNodeData::VoidTy 
                => {
                    SymbolType::Primitive(node.clone())
                }

            _ => {
                panic!("cannot create symbol type: {:?}", node);
            }
        }
    }

    pub fn get_node(&self) -> AstNode {
        match self {
            SymbolType::TypeDef(ref node) => {
                return node.clone();
            }

            SymbolType::TypeRef(ref node) => {
                return node.clone();
            }

            SymbolType::Primitive(node) => {
                return node.clone();
            }
        }
    }

    pub fn is_type_ref(&self) -> bool {
        if let SymbolType::TypeRef(_) = self {
            return true;
        }

        return false;
    }

    pub fn is_type_def(&self) -> bool {
        if let SymbolType::TypeDef(_) = self {
            return true;
        }

        return false;
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct SymbolTable {
    pub file_scopes: HashMap<File, Scope>,
    pub file_loads: HashMap<File, Vec<File>>,
    pub file_scope_defs: HashMap<(File, Scope), Vec<(Name, AstNodeType)>>,
}
impl SymbolTable {
    fn add_symbol_to_scope(&mut self, name: &str, ty: &AstNodeType, file: &File, scope: &Scope) {
        if let std::collections::hash_map::Entry::Vacant(e) = self
            .file_scope_defs.entry((file.to_string(), scope.clone())) {
            e.insert(vec![(name.to_string(), ty.clone())]);
        } else {
            let val = self
                .file_scope_defs
                .get_mut(&(file.to_string(), scope.clone()));
            val.unwrap().push((name.to_string(), ty.clone()));
        }
    }
    pub fn new() -> Self {
        Self {
            file_scopes: HashMap::new(),
            file_scope_defs: HashMap::new(),
            file_loads: HashMap::new(),
        }
    }

    /*
    lookup checks current file defenitions and tries from scope passed into it and goes up.
     */
    fn lookup(
        &self,
        name: Name,
        file: &File,
        scope: &Scope,
        less_than_this: Option<usize>,
    ) -> Option<AstNodeType> {
        let mut scope = scope.clone();
        loop {
            let defs = self
                .file_scope_defs
                .get(&(file.clone(), scope.clone()));
            if defs.is_none() {
                scope.pop();
                continue;
            }
            let defs = defs.unwrap();
            for (idx, def) in defs.iter().enumerate() {
                if def.0 == name.to_string() {
                    if less_than_this.is_some() && idx > less_than_this.unwrap() {
                        return Some(def.1.clone());
                    }
                    return Some(def.1.clone());
                }
            }
            if scope.len() == 0 {
                break;
            }
            scope.pop();
        }

        // not in the current file. let's check loads
        let loads = self.file_loads.get(file);
        // println!("loads are: {:?}", loads);

        match loads {
            Some(files) => {
                for load_file in files {
                    let defs = self
                        .file_scope_defs
                        .get(&(load_file.clone(), vec![])) // getting top level defs of the `load_file`
                        .unwrap();
                    for def in defs.iter() {
                        if def.0 == name.to_string() {
                            return Some(def.1.clone());
                        }
                    }
                }
            }
            None => {

            }
        }

        None
    }

    fn infer_types_for_block(
        &mut self,
        file: &File,
        block: &mut Vec<AstNode>,
        scope: &Scope,
    ) -> Result<()> {
        for (idx, node) in block.iter_mut().enumerate() {
            match node.data {
                AstNodeData::Def(ref mut def) => {
                    self.infer_types_for_def(file, def, scope.to_vec(), idx)?;
                }
                AstNodeData::Decl(ref name) => {
                    self.add_symbol_to_scope(name, &node.infered_type, file, scope);
                }
                AstNodeData::Assign(ref mut lhs, ref mut rhs) => {
                    if let AstNodeData::NamespaceAccess(ref mut cf) = rhs.data {
                        if let AstNodeData::Ident(ref ident) = cf.namespace.data {
                            let container_ty = self.lookup(ident.clone(), file, &scope, Some(idx));
                            if container_ty.is_none() {
                                // error
                                panic!("cannot infer type of: {:?}", cf.namespace);
                            }
                            let container_ty = container_ty.unwrap();
                            if container_ty.is_type_def() && !container_ty.is_type_def_enum() {
                                panic!("cannot use a not enum def as container for a field access: {:?}", container_ty);
                            }
                            if container_ty.is_type_def_enum() {
                                cf.namespace_is_enum = true;
                            }
                        }
    
                    }
                    
                }

                _ => {}
            }
        }

        Ok(())
    }

    fn infer_types_for_def(
        &mut self,
        file: &File,
        def: &mut AstDef,
        scope: Scope,
        idx: usize,
    ) -> Result<()> {
        match def.expr.data {
            AstNodeData::Struct(_) 
            | AstNodeData::Enum(_, _)
            | AstNodeData::Uint(_)
            | AstNodeData::Int(_)
            | AstNodeData::Char(_)
            | AstNodeData::Bool(_)
            | AstNodeData::Float(_)
            | AstNodeData::StringLiteral(_)
                => {
                    if def.expr.infered_type == AstNodeType::Unknown {
                        unreachable!();
                    }
            }

            AstNodeData::Ident(ref ident) => {
                // look it up
                let ty = self.lookup(ident.clone(), file, &scope, Some(idx));
                if ty.is_none() {
                    panic!("unknown identifier: {}", def.name);
                }
                def.expr.infered_type = ty.unwrap();

            }
            AstNodeData::PointerTo(ref obj) => {
                if obj.is_ident() {
                    let ty = self.lookup(obj.get_ident(), file, &scope, Some(idx));
                    if ty.is_none() {
                        panic!("unknown identifier: {}", obj.get_ident());
                    }

                    def.expr.infered_type = AstNodeType::Pointer(Box::new(ty.unwrap()));
                }
            }
            AstNodeData::NamespaceAccess(ref mut cf) => {
                if def.expr.infered_type != AstNodeType::Unknown { // user explicitly said the type
                    return Ok(());
                }
                match cf.namespace.data {
                    AstNodeData::Ident(ref ident) => {

                        let container_ty = self.lookup(ident.clone(), file, &scope, Some(idx));
                        if container_ty.is_none() {
                            panic!("cannot infer type of: {:?}", cf.namespace);
                        }
                        let container_ty = container_ty.unwrap();
                        if container_ty.is_type_def_enum() {
                            cf.namespace_is_enum = true;
                        }
                        def.expr.infered_type = container_ty;
                    }

                    AstNodeData::Initialize(ref ty, _) => {
                        let container_ty = self.lookup(ty.get_ident(), file, &scope, Some(idx));
                        if container_ty.is_none() {
                            panic!("cannot infer type of: {:?}", ty);
                        }
                        let container_ty = container_ty.unwrap();
                        if container_ty.is_type_def_enum() {
                            cf.namespace_is_enum = true;
                        }
                        def.expr.infered_type = container_ty;
                    }

                    AstNodeData::FnCall(ref name, _) => {
                        let fn_sign = self.lookup(name.get_ident(), file, &scope, Some(idx));
                        if fn_sign.is_none() {
                            panic!("cannot infer type of function: {:?}", name);
                        }
                        let fn_sign = fn_sign.unwrap();
                        if !fn_sign.is_fn_def() {
                            panic!("Compiler bug, expected a function type but found: {:?}", fn_sign);
                        }
                        let fn_ret_ty = fn_sign.get_fn_ret_ty();
                        if fn_ret_ty.is_type_def_enum() {
                            cf.namespace_is_enum = true;
                        }
                        def.expr.infered_type = fn_ret_ty;                    
                    }
                    
                    _ => {
                        
                    }
                }
            }

            AstNodeData::FnCall(ref name, _) => {
                let fn_sign = self.lookup(name.get_ident(), file, &scope, Some(idx));
                if fn_sign.is_none() {
                    panic!("cannot infer type of function: {:?}", name);
                }
                let fn_sign = fn_sign.unwrap();
                if !fn_sign.is_fn_def() {
                    panic!("Compiler bug, expected a function type but found: {:?}", fn_sign);
                }
                let fn_ret_ty = fn_sign.get_fn_ret_ty();
                def.expr.infered_type = fn_ret_ty;                    
            }

            AstNodeData::FnDef(ref mut fn_def) => {
                let mut new_scope = scope.clone();
                new_scope.push(idx);
                self.add_symbol_to_scope(
                    &def.name,
                    &def.expr.infered_type,
                    file,
                    &scope,
                );
                for arg in &fn_def.sign.args {
                    self.add_symbol_to_scope(
                        &arg.get_ident(),
                        &arg.infered_type,
                        file,
                        &new_scope,
                    );
                }
                self.infer_types_for_block(file, &mut fn_def.body, &new_scope)?;
            }

            _ => {}
        }

        self.add_symbol_to_scope(&def.name, &def.expr.infered_type, file, &scope);
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub filename: String,
    pub src: String,
    pub tokens: Vec<Token>,
    pub top_level: Vec<AstNode>,
}

impl Ast {
    // pub fn get_childes(&self, parent_id: NodeID) -> Vec<Node> {
    //     let nodes = vec![];
    //     for top_level in &self.top_level {
    //         match top_level.data {
    //             NodeData::Def(ref def) => {
    //                 match def.expr.data {
    //                     NodeData::Struct(_) => todo!(),
    //                     NodeData::Enum(_, _) => todo!(),
    //                     NodeData::FnPrototype(_, _) => todo!(),
    //                     NodeData::FnDef(_, _) => todo!(),
    //                     NodeData::If(_, _, _) => todo!(),
    //                     NodeData::For(_, _, _, _) => todo!(),
    //                     NodeData::ForIn(_, _, _) => todo!(),
    //                     NodeData::While(_, _) => todo!(),
    //                     NodeData::Break => todo!(),
    //                     NodeData::Continue => todo!(),
    //                     NodeData::Return(_) => todo!(),
    //                 }

    //             },
    //             _ => {}
    //         }
    //     }

    //     nodes
    // }
    pub fn get_compiler_flags(&self) -> Vec<String> {
        let mut flags = Vec::<String>::new();
        for node in self.top_level.iter() {
            if let AstNodeData::CompilerFlags(ref flag) = node.data {
                flags.push(flag.clone());
            }
        }
        return flags;
    }
    

    pub fn add(
        &mut self,
        st: &mut SymbolTable,
        filename: &str,
    ) -> Result<()> {
        let file = filename.to_string();
        for (idx, node) in self.top_level.iter_mut().enumerate() {
            match node.data {
                AstNodeData::Def(ref mut def) => {
                    st.infer_types_for_def(&file, def, vec![], idx)?;
                }

                AstNodeData::Load(ref path) => {
                    if st.file_loads.contains_key(filename) {
                        st.file_loads.get_mut(filename).unwrap().push(path.clone());
                    } else {
                        st.file_loads.insert(filename.to_string(), vec![path.clone()]);
                    }
                }

                _ => {
                    
                }
            }
        }

        Ok(())
    }

    pub fn new(
        filename: String,
        src: String,
        tokens: Vec<Token>,
        top_level: Vec<AstNode>,
        st: &mut SymbolTable,
    ) -> Result<Self> {
        let mut ast = Self {
            filename: filename.clone(),
            src: src.clone(),
            tokens: tokens.clone(),
            top_level,
        };
        Ok(ast)
    }
}
