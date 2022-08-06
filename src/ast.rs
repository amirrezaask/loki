use std::{collections::HashMap, ops::Deref};

use crate::tokenizer::{Token, Type};
use anyhow::Result;

pub type TokenIndex = usize;
pub type NodeID = String;

#[derive(Debug, PartialEq, Clone)]
pub struct Def {
    pub mutable: bool,
    pub name: Box<Node>,
    pub ty: Box<Option<Node>>,
    pub expr: Box<Node>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Node {
    pub id: NodeID,
    pub data: NodeData,
}

#[derive(Debug, PartialEq, Clone)]
pub enum NodeData {
    //top level items
    Load(TokenIndex),
    Host(TokenIndex),
    Def(Def),
    Decl(Box<Node>, Box<Node>),
    Assign(Box<Node>, Box<Node>),

    // Type defs
    IntTy(TokenIndex),
    Int8Ty(TokenIndex),
    Int16Ty(TokenIndex),
    Int32Ty(TokenIndex),
    Int64Ty(TokenIndex),
    Int128Ty(TokenIndex),

    UintTy(TokenIndex),
    Uint8Ty(TokenIndex),
    Uint16Ty(TokenIndex),
    Uint32Ty(TokenIndex),
    Uint64Ty(TokenIndex),
    Uint128Ty(TokenIndex),

    FloatTy(TokenIndex),
    BoolTy(TokenIndex),
    StringTy(TokenIndex),
    CharTy(TokenIndex),
    VoidTy(TokenIndex),
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
    ContainerField(Box<Node>, Box<Node>),
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
    pub fn get_ident(&self)-> String {
        if let NodeData::Ident(ident) = &self.data {
            return ident.to_string();
        } else {
            unreachable!()
        }
    }
}

type File = String;
type Scope = Vec<usize>;
type Name = String;

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub file_scopes: HashMap<File, Scope>,
    pub file_scope_defs: HashMap<(File, Scope), Vec<(Name, Option<Node>)>>
}
impl SymbolTable {
    fn add_def_to_scope(&mut self, name: &str, ty: Option<Node>, file: &File, scope: &Scope) {
        if self.file_scope_defs.contains_key(&(file.to_string(), scope.clone())) {
            let val = self.file_scope_defs.get_mut(&(file.to_string(), scope.clone()));
            val.unwrap().push((name.to_string(), ty.clone()));
        } else {
            self.file_scope_defs.insert((file.to_string(), scope.clone()), vec![(name.to_string(), ty.clone())]);
        }
    }
    pub fn new() -> Self {
        Self { file_scopes:HashMap::new(), file_scope_defs: HashMap::new() }
    }

    fn lookup(&self, name: Name, file: &File, scope: &Scope, less_than_this: Option<usize>) -> Option<Node> {
        let defs = self.file_scope_defs.get(&(file.clone(), scope.clone())).unwrap();
        for (idx,def) in defs.iter().enumerate() {
            if def.0 == name.to_string() {
                if less_than_this.is_some() {
                    if idx > less_than_this.unwrap() {
                        return def.1.clone();
                    }
                }
                return def.1.clone();
            }
        }

        None
    }

    fn lookup_pos(&self, name: Name, file: &File, scope: &Scope) -> Option<usize> {
        let defs = self.file_scope_defs.get(&(file.clone(), scope.clone())).unwrap();
        for (idx, def) in defs.iter().enumerate() {
            if def.0 == name {
                return Some(idx);
            }
        }

        None
    }

    fn fill_for_block(&mut self, file: &File, block: &mut Vec<Node>, scope: &Scope) -> Result<()> {
        for (idx, node) in block.iter_mut().enumerate() {
            match node.data {
                NodeData::Def(ref mut def) => {
                    self.fill_for_def(file, def, scope.to_vec(), idx)?;
                }
                
                _ => {}
            }
        }

        Ok(())
    }

    fn fill_for_def(&mut self, file: &File, def: &mut Def, scope: Scope, idx: usize) -> Result<()> {
        match def.expr.data {
            NodeData::Uint(_) |
            NodeData::Int(_) |
            NodeData::Char(_) |
            NodeData::True(_) | 
            NodeData::False(_) |
            NodeData::Float(_) |
            NodeData::StringLiteral(_) => {
                self.add_def_to_scope(&def.name.get_ident(), def.ty.deref().clone(), file, &scope)
            }

            NodeData::Ident(ref ident) => {
                if def.ty.is_none() {
                    // look it up
                    let ty = self.lookup(ident.clone(), file, &scope, Some(idx));
                    if ty.is_none() {
                        println!("cannot guess {:?} type", def.name);
                        self.add_def_to_scope(&def.name.get_ident(), None, file, &scope);
                        return Ok(());
                    }
                    println!("checking {:?} type guessing this {:?}", def.name.get_ident(), ty);
                    def.ty = Box::new(Some(ty.clone().unwrap()));
                }
                
            }

            NodeData::FnCall(ref mut proto, _) => {
                if def.ty.is_none() {
                    if let NodeData::FnPrototype(_, ref ret) = proto.data {
                        def.ty = Box::new(Some(ret.deref().clone()));
                    }
                }
            }

            NodeData::FnDef(ref proto, ref mut body) => {
                let mut new_scope = scope.clone();
                new_scope.push(idx);
                self.add_def_to_scope(&def.name.get_ident(), Some(*proto.clone()), file, &new_scope);
                self.fill_for_block(file, body, &new_scope)?;
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
    pub fn get_src_for_token(&self, tok_idx: usize) -> Result<&str> {
        let src_range = &self.tokens[tok_idx];
        Ok(&self.src[src_range.loc.0..=src_range.loc.1])
    }

    pub fn add(&mut self, st: &mut SymbolTable, filename: &str, src: &str, tokens: &Vec<Token>) -> Result<()> {
        let file = filename.to_string();
        for (idx, node) in self.top_level.iter_mut().enumerate() {
            if let NodeData::Def(ref mut def) = node.data {
                st.fill_for_def(&file, def, vec![], idx)?;
            }
        }

        Ok(())
    }
    

    pub fn new(
        filename: String,
        src: String,
        tokens: Vec<Token>,
        mut top_level: Vec<Node>,
        st: &mut SymbolTable,
    ) -> Result<Self> {
        let mut ast = Self {
            filename: filename.clone(),
            src: src.clone(),
            tokens: tokens.clone(),
            top_level,
        };
        ast.add(st, &filename, &src, &tokens)?;
        Ok(ast)
    }
}
