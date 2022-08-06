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
    Ident(TokenIndex),
    TEXT(String),
    Sum(Box<Node>, Box<Node>),
    Subtract(Box<Node>, Box<Node>),
    Multiply(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),
    Mod(Box<Node>, Box<Node>),
    ContainerField(Box<Node>, Box<Node>),
    FnDef(Vec<Node>, Box<Node>, Vec<Node>),
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

fn extract_decl(node: &Node) -> (Box<Node>, Box<Node>) {
    match &node.data {
        NodeData::Decl(lhs, rhs) => {
            return (lhs.clone(), rhs.clone())
        }

        _ => {
            unreachable!()
        }
    }
}
/*
    file(name: string) -> vec<id> (top_levels)
    def(id) -> expr (id)
    fn_def(id) -> stmt
    if(id) ->
*/
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum SymbolDatabaseKey {
    File(String),
    NodeID(NodeID),
}

#[derive(Debug, Clone)]
enum SymbolDatabaseTypes {
    StructUserDefined(NodeID), //structs, enums, unions, arrays
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

impl SymbolDatabaseTypes {
    pub fn from_def(node: &Node) -> SymbolDatabaseTypes {
        match node.data {
            NodeData::UintTy(_) => SymbolDatabaseTypes::Uint,
            NodeData::Uint8Ty(_) => SymbolDatabaseTypes::Uint8,
            NodeData::Uint16Ty(_) => SymbolDatabaseTypes::Uint16,
            NodeData::Uint32Ty(_) => SymbolDatabaseTypes::Uint32,
            NodeData::Uint64Ty(_) => SymbolDatabaseTypes::Uint64,
            NodeData::Uint128Ty(_) => SymbolDatabaseTypes::Uint128,
            NodeData::IntTy(_) => SymbolDatabaseTypes::Int,
            NodeData::Int8Ty(_) => SymbolDatabaseTypes::Int8,
            NodeData::Int16Ty(_) => SymbolDatabaseTypes::Int16,
            NodeData::Int32Ty(_) => SymbolDatabaseTypes::Int32,
            NodeData::Int64Ty(_) => SymbolDatabaseTypes::Int64,
            NodeData::Int128Ty(_) => SymbolDatabaseTypes::Int128,
            NodeData::CharTy(_) => SymbolDatabaseTypes::Char,
            NodeData::Float(_) => SymbolDatabaseTypes::Float,
            NodeData::BoolTy(_) => SymbolDatabaseTypes::Bool,
            NodeData::StringTy(_) => SymbolDatabaseTypes::String,
            NodeData::Uint(_) => SymbolDatabaseTypes::Uint,
            NodeData::Int(_) => SymbolDatabaseTypes::Int,

            _ => {
                println!("cannot handle from( {:?} ) to symbol database type", node);
                unreachable!()
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum SymbolDatabaseValue {
    Null,                                      // stil not parsed completely
    TopLevels(Vec<NodeID>),                    // just for files.
    Stmts(Vec<NodeID>),                        // for functions, if, for, while, switch
    Expr(NodeID, Option<SymbolDatabaseTypes>), // for definitions
    Struct(Vec<NodeID>),
    Decl(NodeID, SymbolDatabaseTypes),
    FnDef(Vec<NodeID>, SymbolDatabaseTypes, Vec<NodeID>)
}

impl SymbolDatabaseValue {
    pub fn addIfTopLevel(&mut self, id: NodeID) {
        match self {
            Self::TopLevels(v) => {
                v.push(id);
            }
            _ => {
                println!(
                    "trying to add top level but value is not top level. {:?}",
                    self
                );
                unreachable!()
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolDatabase {
    pub data: HashMap<SymbolDatabaseKey, SymbolDatabaseValue>,
}

impl SymbolDatabase {
    pub fn add(&mut self, key: SymbolDatabaseKey, value: SymbolDatabaseValue) {
        self.data.insert(key, value);
    }
    pub fn get_mut(&mut self, key: &SymbolDatabaseKey) -> Option<&mut SymbolDatabaseValue> {
        return self.data.get_mut(key);
    }
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub filename: String,
    pub src: String,
    pub tokens: Vec<Token>,
    pub top_level: Vec<Node>,
    pub database: SymbolDatabase,
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

    pub fn new(
        filename: String,
        src: String,
        tokens: Vec<Token>,
        top_level: Vec<Node>,
    ) -> Result<Self> {
        let db = Self::create_symbol_database(filename.clone(), &src, &tokens, &top_level)?;
        Ok(Self {
            filename,
            src,
            tokens,
            top_level,
            database: db,
        })
    }

    pub fn create_symbol_database(
        filename: String,
        src: &str,
        tokens: &Vec<Token>,
        top_level: &Vec<Node>,
    ) -> Result<SymbolDatabase> {
        let mut sd = SymbolDatabase {
            data: HashMap::new(),
        };
        // addin file as it's own entry.
        sd.add(
            SymbolDatabaseKey::File(filename.clone()),
            SymbolDatabaseValue::TopLevels(vec![]),
        );

        for node in top_level {
            let mut file_top_levels = sd
                .get_mut(&SymbolDatabaseKey::File(filename.clone()))
                .unwrap();
            file_top_levels.addIfTopLevel(node.id.clone());
            println!("got node: {:?}", node);
            match &node.data {
                NodeData::Def(def) => match &def.expr.data {
                    NodeData::Uint(_)
                    | NodeData::Int(_)
                    | NodeData::Float(_)
                    | NodeData::BoolTy(_)
                    | NodeData::CharTy(_)
                    | NodeData::StringTy(_) => sd.add(
                        SymbolDatabaseKey::NodeID(node.id.clone()),
                        SymbolDatabaseValue::Expr(
                            def.expr.id.clone(),
                            Some(SymbolDatabaseTypes::from_def(&def.expr)),
                        ),
                    ),

                    NodeData::Struct(fields) => {
                        let mut struct_field_ids = Vec::<NodeID>::new();
                        for f in fields {
                            struct_field_ids.push(f.id.clone());
                            let decl = extract_decl(f);
                            sd.add(SymbolDatabaseKey::NodeID(f.id.clone()), SymbolDatabaseValue::Decl(decl.0.id, SymbolDatabaseTypes::from_def(&decl.1)));
                        }
                        sd.add(
                            SymbolDatabaseKey::NodeID(node.id.clone()),
                            SymbolDatabaseValue::Struct(struct_field_ids),

                        );

                    }

                    NodeData::FnDef(args, ret, body) => {
                        let mut arg_ids = Vec::<NodeID>::new();
                        for f in args {
                            arg_ids.push(f.id.clone());
                            let decl = extract_decl(f);
                            sd.add(SymbolDatabaseKey::NodeID(f.id.clone()), SymbolDatabaseValue::Decl(decl.0.id, SymbolDatabaseTypes::from_def(&decl.1)));
                        }
                        sd.add(SymbolDatabaseKey::NodeID(node.id.clone()), SymbolDatabaseValue::FnDef(arg_ids, SymbolDatabaseTypes::from_def(ret), vec![]));
                    }


                    _ => {}
                },

                _ => {}
            }
        }

        Ok(sd)
    }
}
