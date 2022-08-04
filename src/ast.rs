use std::ops::Deref;

use crate::{
    symbol_table::{self, SymbolTable, SymbolType},
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
    FieldAccess(Vec<Node>),
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

#[derive(Debug, Clone)]
pub struct AST {
    pub src: String,
    pub tokens: Vec<Token>,
    pub top_level: Vec<Node>,
}

impl AST {
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

    fn infer_block_types(st: &SymbolTable, mut block: Vec<Node>) -> Result<Vec<Node>> {
        for stmt in &mut block {
            match &stmt.data {
                NodeData::Decl(decl) => {
                    let new_decl = Self::infer_decl_types(st, decl.clone())?;
                    stmt.data = NodeData::Decl(new_decl);
                }
                NodeData::If(args, then, _else) => { //TODO handle else
                    let new_then = Self::infer_block_types(st, *then.clone())?;
                    stmt.data = NodeData::If(args.clone(), Box::new(new_then), _else.clone());
                }
                _ => {}
            }
        }
        Ok(block)
    }

    fn infer_decl_types(st: &SymbolTable, mut decl: Decl) -> Result<Decl> {
        match *decl.ty {
            None => match decl.expr.data {
                NodeData::FnDef(args, ret, block) => {
                    let new_block = Self::infer_block_types(st, block.clone())?;
                    decl.expr.data = NodeData::FnDef(args, ret, new_block);

                    return Ok(decl);
                }
                _ => {
                    let infered_ty = st.lookup_by_id(decl.name.id);
                    if infered_ty.is_none() {
                        return Ok(decl);
                    }
                    let infered_ty = infered_ty.unwrap();
                    match &infered_ty.ty {
                        SymbolType::Array(size, elem_ty) => {
                            let size = Node {
                                id: -1, data: NodeData::Uint(0),
                            };

                            let elem_ty = Node {
                                id: -1, data: NodeData::Uint(0), //TODO Fixme
                            };
                            
                            decl.ty = Box::new(Some(Node {
                                id: -1,
                                data: NodeData::ArrayTy(Box::new(size), Box::new(elem_ty)),
                            }));
                            return Ok(decl);
                        }
                        SymbolType::Name(name) => {
                            decl.ty = Box::new(Some(Node {
                                id: -1,
                                data: NodeData::TEXT(name.clone()),
                            }));
                            return Ok(decl);
                        }
                        SymbolType::Uint => {
                            decl.ty = Box::new(Some(Node {
                                id: -1,
                                data: NodeData::UintTy(0),
                            }));
                            return Ok(decl);
                        }
                        SymbolType::Int => {
                            decl.ty = Box::new(Some(Node {
                                id: -1,
                                data: NodeData::IntTy(0),
                            }));
                            return Ok(decl);
                        }
                        SymbolType::Float => {
                            decl.ty = Box::new(Some(Node {
                                id: -1,
                                data: NodeData::FloatTy(0),
                            }));
                            return Ok(decl);
                        }
                        SymbolType::Bool => {
                            decl.ty = Box::new(Some(Node {
                                id: -1,
                                data: NodeData::BoolTy(0),
                            }));
                            return Ok(decl);
                        }
                        SymbolType::Char => {
                            decl.ty = Box::new(Some(Node {
                                id: -1,
                                data: NodeData::CharTy(0),
                            }));
                            return Ok(decl);
                        }
                        SymbolType::String => {
                            decl.ty = Box::new(Some(Node {
                                id: -1,
                                data: NodeData::StringTy(0),
                            }));
                            return Ok(decl);
                        }

                        SymbolType::Type => {
                            return Ok(decl);
                        }
                    }
                }
            },

            _ => {
                return Ok(decl);
            }
        }
    }

    pub fn infer_types(&mut self, st: &SymbolTable) -> Result<()> {
        for node in &mut self.top_level {
            match &node.data {
                NodeData::Decl(decl) => {
                    let infered_decl = Self::infer_decl_types(st, decl.clone())?;
                    node.data = NodeData::Decl(infered_decl);
                }
            
                _ => {}
            }
        }

        Ok(())
    }
}
