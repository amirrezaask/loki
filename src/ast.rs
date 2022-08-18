use std::{collections::HashMap, ops::Deref};

use crate::lexer::{Token, Type};
use anyhow::Result;

pub type TokenIndex = usize;
pub type NodeID = String;

#[derive(Debug, PartialEq, Clone)]
pub struct Def {
    pub mutable: bool,
    pub name: Box<Node>,
    pub expr: Box<Node>,
}
#[derive(Debug, PartialEq, Clone)]
pub enum AstType {
    Unknown,
    SignedInt(i32),
    UnsignedInt(i32),
    Float(i8),

    Bool,

    Char,
    String,

    Array(i8, Box<AstType>),
    DynamicArray(Box<AstType>),

    TypeName(String),

    Initialize(Box<AstType>),

    TypeDefStruct,
    TypeDefEnum,
    TypeDefUnion,

    Ref(Box<AstType>),
    Deref(Box<AstType>),

    Void,

}

impl AstType {
    pub fn new(node: &Node) -> Self {
        match node.data {
            NodeData::Ident(ref ident) => {
                return AstType::TypeName(ident.clone());
            }
            NodeData::UintTy => AstType::UnsignedInt(64),
            NodeData::Uint8Ty => AstType::UnsignedInt(8),
            NodeData::Uint16Ty => AstType::UnsignedInt(16),
            NodeData::Uint32Ty => AstType::UnsignedInt(32),
            NodeData::Uint64Ty => AstType::UnsignedInt(64),
            NodeData::Uint128Ty => AstType::UnsignedInt(128),
            NodeData::IntTy => AstType::SignedInt(64),
            NodeData::Int8Ty => AstType::SignedInt(8),
            NodeData::Int16Ty => AstType::SignedInt(16),
            NodeData::Int32Ty => AstType::SignedInt(32),
            NodeData::Int64Ty => AstType::SignedInt(64),
            NodeData::Int128Ty => AstType::SignedInt(128),
            NodeData::CharTy => AstType::Char,
            NodeData::StringTy => AstType::String,
            NodeData::BoolTy => AstType::Bool,
            NodeData::FloatTy => AstType::Float(64),
            NodeData::VoidTy => AstType::Void,
            _ => {
                AstType::Unknown
            }

        }
    }
    pub fn is_type_def(&self) -> bool {
        match self {
            AstType::TypeDefStruct | AstType::TypeDefUnion | AstType::TypeDefEnum => true,
            _ => return false
        }
    }

    pub fn is_type_def_enum(&self) -> bool {
        match self {
            AstType::TypeDefEnum => true,
            _ => return false
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Node {
    pub id: NodeID,
    pub data: NodeData,
    pub type_annotation: AstType
}

#[derive(Debug, PartialEq, Clone)]
pub struct ContainerField {
    pub container: Box<Node>,
    pub field: Box<Node>,
    pub container_is_enum: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub enum NodeData {
    //top level items
    CCompilerFlag(TokenIndex),
    Load(TokenIndex),
    Host(TokenIndex),
    Def(Def),
    Decl(Box<Node>, AstType),
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
    ArrayTy(Box<Node>, Box<Node>), // len ty

    Struct(Vec<Node>),
    Enum(bool, Vec<(Node, Option<Node>)>),

    //Expressions
    Uint(TokenIndex),
    Int(TokenIndex),
    StringLiteral(TokenIndex),
    Float(TokenIndex),
    True(TokenIndex),
    False(TokenIndex),
    Char(TokenIndex),
    Ident(String),
    TEXT(String),
    Sum(Box<Node>, Box<Node>),
    Subtract(Box<Node>, Box<Node>),
    Multiply(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),
    Mod(Box<Node>, Box<Node>),
    ContainerField(ContainerField),
    FnPrototype(Vec<Node>, Box<Node>),
    FnDef(Box<Node>, Vec<Node>),
    FnCall(Box<Node>, Vec<Node>),
    If(Box<Node>, Box<Vec<Node>>, Option<Vec<Node>>),
    Initialize(Option<Box<Node>>, Vec<(Node, Node)>),
    InitializeArray(Option<Box<Node>>, Vec<Node>),
    Cmp(Type, Box<Node>, Box<Node>), // op, lhs, rhs
    Ref(Box<Node>),
    Deref(Box<Node>),
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
            NodeData::Enum(_, _) | NodeData::Struct(_) | NodeData::FnPrototype(_, _)=> {
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
    pub file_scope_defs: HashMap<(File, Scope), Vec<(Name, AstType)>>,
}
impl SymbolTable {
    fn add_def_to_scope(&mut self, name: &str, ty: &AstType, file: &File, scope: &Scope) {
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
    ) -> Option<AstType> {
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
                NodeData::Decl(ref name, ref ty) => {
                    self.add_def_to_scope(name.get_ident().as_str(), &ty, file, scope);
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
                self.add_def_to_scope(&def.name.get_ident(), &def.expr.type_annotation, file, &scope)
            }
            NodeData::Uint(_)
            | NodeData::Int(_)
            | NodeData::Char(_)
            | NodeData::True(_)
            | NodeData::False(_)
            | NodeData::Float(_)
            | NodeData::StringLiteral(_)
                => {
                    if def.expr.type_annotation == AstType::Unknown {
                        unreachable!();
                    }
                    self.add_def_to_scope(&def.name.get_ident(), &def.expr.type_annotation.clone(), file, &scope)
            }

            NodeData::Ident(ref ident) => {
                // look it up
                let ty = self.lookup(ident.clone(), file, &scope, Some(idx));
                if ty.is_none() {
                    panic!("cannot guess {:?} type", def.name);
                }
                // println!(
                //     "checking {:?} type guessing this {:?}",
                //     def.name.get_ident(),
                //     ty
                // );
                def.expr.type_annotation = ty.unwrap();

            }
            NodeData::ContainerField(ref mut cf) => {
                if def.expr.type_annotation != AstType::Unknown { // user explicitly said the type
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
                def.expr.type_annotation = AstType::Bool;
            }

            // NodeData::FnCall(ref mut proto, _) => {
            //     if def.expr.type_annotation.is_none() {
            //         if let NodeData::FnPrototype(_, ref ret) = proto.data {
            //             def.expr.type_annotation = Box::new(Some(ret.deref().clone()));
            //         }
            //     }
            // }

            NodeData::FnDef(ref proto, ref mut body) => {
                let mut new_scope = scope.clone();
                new_scope.push(idx);
                self.add_def_to_scope(
                    &def.name.get_ident(),
                    &proto.type_annotation,
                    file,
                    &scope,
                );
                self.infer_type_block(file, body, &new_scope)?;
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
    pub fn get_compiler_flags(&self) -> Vec<String> {
        let mut flags = Vec::<String>::new();
        for node in self.top_level.iter() {
            if let NodeData::CCompilerFlag(flag) = node.data {
                flags.push(self.get_src_for_token(flag).unwrap().to_string());
            }
        }
        return flags;
    }
    pub fn get_src_for_token(&self, tok_idx: usize) -> Result<&str> {
        let src_range = &self.tokens[tok_idx];
        Ok(&self.src[src_range.loc.0..=src_range.loc.1])
    }

    pub fn add(
        &mut self,
        st: &mut SymbolTable,
        filename: &str,
        src: &str,
        tokens: &Vec<Token>,
    ) -> Result<()> {
        let file = filename.to_string();
        for (idx, node) in self.top_level.iter_mut().enumerate() {
            match node.data {
                NodeData::Def(ref mut def) => {
                    st.infer_type_def(&file, def, vec![], idx)?;
                }

                NodeData::Load(path_idx) => {
                    let path = src[tokens[path_idx].loc.0..=tokens[path_idx].loc.1].to_string();
                    if st.file_loads.contains_key(filename) {
                        st.file_loads.get_mut(filename).unwrap().push(path);
                    } else {
                        st.file_loads.insert(filename.to_string(), vec![path]);
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
