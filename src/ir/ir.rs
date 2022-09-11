use std::collections::HashMap;
use std::default;
use std::ops::Deref;

use crate::ir::lexer::{Token, TokenType};
use super::typer::Type;
use crate::utils;
use anyhow::anyhow;
use anyhow::Result;
use rand::Rng;
use rand::distributions::Alphanumeric;
use rand::distributions::DistString;
use serde::Serialize;
type BitSize = u8;

pub type Index = u64;

pub fn new_index() -> Index {
    let mut rng = rand::thread_rng();
    let id: u64 = rng.gen();
    return id;
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum AstTag {
    Foreign,
    IsUsedInNamespaceAccess,
    NoCodeGen,
    CompilerFunctionCall,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Node {
    pub id: Index,
    pub data: NodeData,
    pub type_information: Option<Type>,
    pub parent_block: Option<Index>,
    pub tags: Vec<AstTag>,

    pub line: usize,
    pub col: usize,
    pub filename: String,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum NodeData {
   Statement(Statement),
   TypeDefinition(TypeDefinition),
   Expression(Expression),
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum TypeDefinition {
    CVarArgs,
    CString,
    IntPtr,
    UintPtr,

    Int(BitSize), // bitsize
    Uint(BitSize),
    Float(BitSize),
    Bool,
    String,
    Char,
    Void,
    Pointer(Index),
    Array {
        length: Index,
        elem_ty: Index,
    },
    Function {
        args: Vec<Index>,
        ret: Index,
    },
    Struct(Vec<Index>),
    Enum(Vec<Index>),
}


#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum Expression {
    // Literal values
    Unsigned(u64),
    Signed(i64),
    StringLiteral(String),
    Float(f64),
    Bool(bool),
    Char(char),
    Identifier(String),
    
    Paren(Index),


    UnaryOperation {
        operator: UnaryOperation,
        expr: Index,
    },

    ArrayIndex {
        arr: Index,
        idx: Index,
    },

    BinaryOperation {
        operation: BinaryOperation,
        left: Index,
        right: Index,
    },

    NamespaceAccess {
        namespace: Index,
        field: Index,
    },

    Initialize {
        ty: Index,
        fields: Vec<(Index, Index)>,
    },
    InitializeArray {
        elements: Vec<Index>,
    },

    Function {
        args: Vec<Index>,
        ret_ty: Index,
        body: Index,
    },

    FunctionCall {
        fn_name: Index,
        args: Vec<Index>,
    },

    PointerOf(Index),
    Deref(Index),
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum BinaryOperation {
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

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum UnaryOperation {
    Not,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum Statement {
    Load(String),
    Host(String),
    Def {
        mutable: bool,
        name: Index,
        expr: Index,
    },
    Decl {
        name: Index,
        ty: Index,
    },
    Assign {
        lhs: Index,
        rhs: Index,
    },

    Scope {
        // this is for cases when we want to reach from an entity inside a block to it's owner like the if node that owns a block.
        owner: Index, 
        is_file_root: bool,
        stmts: Vec<Index>,
    },

    If {
        cases: Vec<(Index, Index)>,
    },

    For {
        start: Index,
        cond: Index,
        cont: Index,
        body: Index,
    },
    ForIn {
        iterator: Index,
        iterable: Index,
        body: Index,
    },
    While {
        cond: Index,
        body: Index,
    },

    Break,
    Continue,

    Goto(Index),

    Return(Index),
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct IR {
    pub filename: String,
    pub tokens: Vec<Token>,
    pub root: Index,
    pub nodes: HashMap<Index, Node>,
}

impl IR {
    pub fn get_node(&self, index: Index) -> Result<Node> {
        return Ok(self.nodes.get(&index).unwrap().clone());
    }

    pub fn add_tag(&mut self, index: Index, tag: AstTag) {
        let node = self.nodes.get_mut(&index).unwrap();
        node.tags.push(tag);
    }
}