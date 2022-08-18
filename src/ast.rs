use std::{collections::HashMap};

use crate::lexer::{Token, Type};
use anyhow::Result;
use serde::Serialize;

pub type TokenIndex = usize;

pub type NodeID = String;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Def {
    pub mutable: bool,
    pub name: String,
    pub expr: Box<Node>,
}
#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum AstNodeType {
    Unknown,
    NoType,
    SignedInt(i32),
    UnsignedInt(i32),
    Float(i8),

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

    Ref(Box<AstNodeType>),
    Deref(Box<AstNodeType>),

    FnType(Vec<AstNodeType>, Box<AstNodeType>),

    Void,

}

impl AstNodeType {
    pub fn new(node: &Node) -> Self {
        match node.data {
            NodeData::Ident(ref ident) => {
                return AstNodeType::TypeName(ident.clone());
            }
            
            NodeData::UintTy => AstNodeType::UnsignedInt(64),
            NodeData::Uint8Ty => AstNodeType::UnsignedInt(8),
            NodeData::Uint16Ty => AstNodeType::UnsignedInt(16),
            NodeData::Uint32Ty => AstNodeType::UnsignedInt(32),
            NodeData::Uint64Ty => AstNodeType::UnsignedInt(64),
            NodeData::Uint128Ty => AstNodeType::UnsignedInt(128),
            NodeData::IntTy => AstNodeType::SignedInt(64),
            NodeData::Int8Ty => AstNodeType::SignedInt(8),
            NodeData::Int16Ty => AstNodeType::SignedInt(16),
            NodeData::Int32Ty => AstNodeType::SignedInt(32),
            NodeData::Int64Ty => AstNodeType::SignedInt(64),
            NodeData::Int128Ty => AstNodeType::SignedInt(128),
            NodeData::CharTy => AstNodeType::Char,
            NodeData::StringTy => AstNodeType::String,
            NodeData::BoolTy => AstNodeType::Bool,
            NodeData::FloatTy => AstNodeType::Float(64),
            NodeData::VoidTy => AstNodeType::Void,
            _ => {
                AstNodeType::Unknown
            }

        }
    }
    pub fn make_fn_signature(args: &Vec<Node>, ret: &Node) -> Self {
        let args_ty: Vec<AstNodeType> = args.iter().map(|arg| AstNodeType::new(arg)).collect();

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

    pub fn is_type_def_enum(&self) -> bool {
        match self {
            AstNodeType::TypeDefEnum => true,
            _ => return false
        }
    }
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Node {
    pub id: NodeID,
    pub data: NodeData,
    pub type_annotation: AstNodeType,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct ContainerField {
    pub container: Box<Node>,
    pub field: Box<Node>,
    pub container_is_enum: bool,
}
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct FnSignature {
    pub args: Vec<Node>,
    pub ret: Box<Node>
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct FnDef {
    pub sign: FnSignature,
    pub body: Vec<Node>,    
}
#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum NodeData {
    //top level items
    CompilerFlags(String),
    Load(String),
    Host(String),
    Def(Def),
    Decl(String),
    Assign(Box<Node>, Box<Node>),

    // Type defs
    IntTy,
    Int8Ty,
    Int16Ty,
    Int32Ty,
    Int64Ty,
    Int128Ty,

    UintTy,
    Uint8Ty,
    Uint16Ty,
    Uint32Ty,
    Uint64Ty,
    Uint128Ty,

    FloatTy,
    BoolTy,
    StringTy,
    CharTy,
    VoidTy,
    ArrayTy(u64, AstNodeType), // len ty
    FnType(FnSignature),


    Struct(Vec<Node>),
    Enum(bool, Vec<(Node, Option<Node>)>),

    //Expressions
    Uint(u64),
    Int(i64),
    StringLiteral(String),
    Float(f64),
    Bool(bool),
    Char(char),
    Ident(String),
    
    TEXT(String),
    
    Sum(Box<Node>, Box<Node>),
    Subtract(Box<Node>, Box<Node>),
    Multiply(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),
    Mod(Box<Node>, Box<Node>),
    ContainerField(ContainerField),
    
    Initialize(Option<Box<Node>>, Vec<(Node, Node)>),
    InitializeArray(Option<Box<Node>>, Vec<Node>),
    
    FnDef(FnDef),
    FnCall(Box<Node>, Vec<Node>),
    
    Cmp(Type, Box<Node>, Box<Node>), // op, lhs, rhs
    Ref(Box<Node>),
    Deref(Box<Node>),
    
    If(Box<Node>, Vec<Node>, Option<Vec<Node>>),
    
    For(Box<Node>, Box<Node>, Box<Node>, Vec<Node>),
    ForIn(Option<Box<Node>>, Box<Node>, Vec<Node>),
    While(Box<Node>, Vec<Node>),
    
    Break,
    Continue,

    Return(Box<Node>),
}

impl Node {
    pub fn get_ident(&self) -> String {
        if let NodeData::Ident(ident) = &self.data {
            return ident.to_string();
        } else {
            unreachable!()
        }
    }
    pub fn is_ident(&self) -> bool {
        if let NodeData::Ident(_) = self.data {
            return true;
        }
        return false;

    }
    pub fn is_enum(&self) -> bool {
        if let NodeData::Enum(_, _) = self.data {
            return true;
        }
        return false;
    }

    pub fn is_initialize(&self) -> bool {
        if let NodeData::Initialize(_, _) = self.data {
            return true;
        }
        return false;
    }

    pub fn extract_uint(&self) -> u64 {
        if let NodeData::Uint(u) = self.data {
            return u;
        }
        unreachable!();
    }
}

type File = String;
type Scope = Vec<usize>;
type Name = String;

#[derive(Debug, Clone)]
pub enum SymbolType {
    Primitive(Node),
    TypeRef(Node),
    TypeDef(Node)
}
impl SymbolType {
    pub fn new(node: &Node) -> Self {
        match node.data {
            NodeData::Enum(_, _) | NodeData::Struct(_) | NodeData::FnType(_)=> {
                SymbolType::TypeDef(node.clone())
            }

            NodeData::Ident(_) => {
                SymbolType::TypeRef(node.clone())
            }
            NodeData::IntTy |
            NodeData::Int8Ty |
            NodeData::Int16Ty |
            NodeData::Int32Ty |
            NodeData::Int64Ty |
            NodeData::Int128Ty |
            NodeData::UintTy |
            NodeData::Uint8Ty |
            NodeData::Uint16Ty |
            NodeData::Uint32Ty |
            NodeData::Uint64Ty |
            NodeData::Uint128Ty |
            NodeData::FloatTy |
            NodeData::BoolTy |
            NodeData::StringTy |
            NodeData::CharTy |
            NodeData::VoidTy 
                => {
                    SymbolType::Primitive(node.clone())
                }

            _ => {
                panic!("cannot create symbol type: {:?}", node);
            }
        }
    }

    pub fn get_node(&self) -> Node {
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

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub file_scopes: HashMap<File, Scope>,
    pub file_loads: HashMap<File, Vec<File>>,
    pub file_scope_defs: HashMap<(File, Scope), Vec<(Name, AstNodeType)>>,
}
impl SymbolTable {
    fn add_def_to_scope(&mut self, name: &str, ty: &AstNodeType, file: &File, scope: &Scope) {
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

    fn lookup_pos(&self, name: Name, file: &File, scope: &Scope) -> Option<usize> {
        let defs = self
            .file_scope_defs
            .get(&(file.clone(), scope.clone()))
            .unwrap();
        for (idx, def) in defs.iter().enumerate() {
            if def.0 == name {
                return Some(idx);
            }
        }

        None
    }

    fn infer_type_block(
        &mut self,
        file: &File,
        block: &mut Vec<Node>,
        scope: &Scope,
    ) -> Result<()> {
        for (idx, node) in block.iter_mut().enumerate() {
            match node.data {
                NodeData::Def(ref mut def) => {
                    self.infer_type_def(file, def, scope.to_vec(), idx)?;
                }
                NodeData::Decl(ref name) => {
                    self.add_def_to_scope(name, &node.type_annotation, file, scope);
                }
                NodeData::Assign(ref mut lhs, ref mut rhs) => {
                    if let NodeData::ContainerField(ref mut cf) = rhs.data {
                        if let NodeData::Ident(ref ident) = cf.container.data {
                            // println!("st: {:?}", self);
                            // println!("cf: {:?}", cf);
                            // should check from symbol table the type, it should be a 
                            let container_ty = self.lookup(ident.clone(), file, &scope, Some(idx));
                            if container_ty.is_none() {
                                // error
                                panic!("cannot infer type of: {:?}", cf.container);
                            }
                            let container_ty = container_ty.unwrap();
                            if container_ty.is_type_def() && !container_ty.is_type_def_enum() {
                                panic!("cannot use a not enum def as container for a field access: {:?}", container_ty);
                            }
                            if container_ty.is_type_def_enum() {
                                cf.container_is_enum = true;
                            }
                        }
    
                    }
                    
                }

                _ => {}
            }
        }

        Ok(())
    }

    fn infer_type_def(
        &mut self,
        file: &File,
        def: &mut Def,
        scope: Scope,
        idx: usize,
    ) -> Result<()> {
        match def.expr.data {
            NodeData::Struct(_) | NodeData::Enum(_, _) => {
                self.add_def_to_scope(&def.name, &def.expr.type_annotation, file, &scope)
            }
            NodeData::Uint(_)
            | NodeData::Int(_)
            | NodeData::Char(_)
            | NodeData::Bool(_)
            | NodeData::Float(_)
            | NodeData::StringLiteral(_)
                => {
                    if def.expr.type_annotation == AstNodeType::Unknown {
                        unreachable!();
                    }
                    self.add_def_to_scope(&def.name, &def.expr.type_annotation.clone(), file, &scope)
            }

            NodeData::Ident(ref ident) => {
                // look it up
                let ty = self.lookup(ident.clone(), file, &scope, Some(idx));
                if ty.is_none() {
                    panic!("cannot guess {:?} type", def.name);
                }
                def.expr.type_annotation = ty.unwrap();

            }
            NodeData::ContainerField(ref mut cf) => {
                if def.expr.type_annotation != AstNodeType::Unknown { // user explicitly said the type
                    return Ok(());
                }
                match cf.container.data {
                    NodeData::Ident(ref ident) => {
                        // println!("st: {:?}", self);
                        // println!("cf: {:?}", cf);
                        // should check from symbol table the type, it should be a 
                        let container_ty = self.lookup(ident.clone(), file, &scope, Some(idx));
                        if container_ty.is_none() {
                            // error
                            panic!("cannot infer type of: {:?}", cf.container);
                        }
                        let container_ty = container_ty.unwrap();
                        // if container_ty.is_type_def() && !container_ty.get_node().is_enum() {
                        //     panic!("cannot use a not enum def as container for a field access: {:?}", container_ty);
                        // }
                        if container_ty.is_type_def_enum() {
                            cf.container_is_enum = true;
                        }
                    }

                    NodeData::Initialize(ref op_ty, _) => {
                    }

                    NodeData::FnCall(_, _) => {}

                    _ => {
                        
                    }
                }
            }
            NodeData::Cmp(_, _, _) => {
                def.expr.type_annotation = AstNodeType::Bool;
            }

            // NodeData::FnCall(ref mut proto, _) => {
            //     if def.expr.type_annotation.is_none() {
            //         if let NodeData::FnPrototype(_, ref ret) = proto.data {
            //             def.expr.type_annotation = Box::new(Some(ret.deref().clone()));
            //         }
            //     }
            // }

            NodeData::FnDef(ref mut fn_def) => {
                let mut new_scope = scope.clone();
                new_scope.push(idx);
                self.add_def_to_scope(
                    &def.name,
                    &def.expr.type_annotation,
                    file,
                    &scope,
                );
                self.infer_type_block(file, &mut fn_def.body, &new_scope)?;
            }

            _ => {}
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub filename: String,
    pub src: String,
    pub tokens: Vec<Token>,
    pub top_level: Vec<Node>,
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
            if let NodeData::CompilerFlags(ref flag) = node.data {
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
                NodeData::Def(ref mut def) => {
                    st.infer_type_def(&file, def, vec![], idx)?;
                }

                NodeData::Load(ref path) => {
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
        top_level: Vec<Node>,
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
