use std::{ops::Deref, collections::HashMap};

use crate::{
    tokenizer::{Token, Type},
};
use anyhow::Result;

pub type TokenIndex = usize;
pub type NodeID = i64;

#[derive(Debug, PartialEq, Clone)]
pub struct Decl {
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
    Decl(Decl),
    Def(Box<Node>, Box<Node>),
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

    Struct(Vec<(Node, Node)>),
    Enum(bool, Vec<(Node, Option<Node>)>),

    //Expressions
    Uint(TokenIndex),
    Int(TokenIndex),
    StringLiteral(TokenIndex),
    Float(TokenIndex),
    True(TokenIndex),
    False(TokenIndex),
    Char(TokenIndex),
    Ident(TokenIndex),
    TEXT(String),
    Sum(Box<Node>, Box<Node>),
    Subtract(Box<Node>, Box<Node>),
    Multiply(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),
    Mod(Box<Node>, Box<Node>),
    ContainerField(Box<Node>, Box<Node>),
    FnDef(Vec<(Node, Node)>, Box<Node>, Vec<Node>),
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
/*  
    file(name: string) -> vec<id> (top_levels)
    def(id) -> expr (id)
    fn_def(id) -> stmt
    if(id) -> 
*/

enum SymbolDatabaseKey {
    File(String),
    NodeID(NodeID),
}

enum SymbolDatabaseTypes {
    UserDefined(NodeID), //structs, enums, unions, arrays
    Int,
    Int8,
    Int16,
    Int32,
    Int64,
    Int128,

    Uint,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Uint128,

    Float,
    Bool,
    String,
    Char,
    Void,
}

enum SymbolDatabaseValue {
    Null, // stil not parsed completely
    Stmts(Vec<NodeID>),
    Expr(NodeID, Option<SymbolDatabaseTypes>)
}

pub struct SymbolDatabase {
    data: HashMap<SymbolDatabaseKey, SymbolDatabaseValue>
}
    
#[derive(Debug, Clone)]
pub struct Ast {
    pub src: String,
    pub tokens: Vec<Token>,
    pub top_level: Vec<Node>,
}

impl Ast {
    pub fn get_src_for_token(&self, tok_idx: usize) -> Result<&str> {
        let src_range = &self.tokens[tok_idx];
        Ok(&self.src[src_range.loc.0..=src_range.loc.1])
    }

    pub fn get_name_for_ident(&self, node: Node) -> Result<&str> {
        match node.data {
            NodeData::Ident(ident) => {
                return self.get_src_for_token(ident);
            }

            _ => {
                unreachable!()
            }
        }
    }
}
