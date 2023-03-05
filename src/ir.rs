use std::collections::HashMap;
use std::default;
use std::ops::Deref;

use super::typer::Type;
use crate::compliation::Dependency;
use crate::errors::Result;
use crate::lexer::{Token, TokenType};
use crate::stack::Stack;
use crate::utils;
use rand::distributions::Alphanumeric;
use rand::distributions::DistString;
use rand::Rng;
use serde::Serialize;
type BitSize = i64;

pub type NodeIndex = u64;

pub fn new_index() -> NodeIndex {
    let mut rng = rand::thread_rng();
    let id: u64 = rng.gen();
    return id;
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum AstTag {
    Foreign,
    NoCodeGen,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Node {
    pub id: NodeIndex,
    pub data: NodeData,
    pub type_information: Option<Type>,
    pub parent_block: Option<NodeIndex>,
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

impl Node {
    pub fn get_identifier(&self) -> Result<String> {
        match self.data {
            NodeData::Expression(Expression::Identifier(ref ident)) => return Ok(ident.clone()),
            _ => {
                unreachable!()
            }
        }
    }
    pub fn is_literal(&self) -> bool {
        match self.data {
            NodeData::Expression(Expression::Unsigned(_)) => true,
            NodeData::Expression(Expression::Signed(_)) => true,
            NodeData::Expression(Expression::Float(_)) => true,
            _ => false,
        }
    }
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
    Pointer(NodeIndex),
    Array {
        length: NodeIndex,
        elem_ty: NodeIndex,
    },
    Function {
        args: Vec<NodeIndex>,
        ret: NodeIndex,
    },
    Struct(Vec<NodeIndex>),
    Enum(Vec<NodeIndex>),
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

    Paren(NodeIndex),

    UnaryOperation {
        operator: UnaryOperation,
        expr: NodeIndex,
    },

    ArrayIndex {
        arr: NodeIndex,
        idx: NodeIndex,
    },

    BinaryOperation {
        operation: BinaryOperation,
        left: NodeIndex,
        right: NodeIndex,
    },

    NamespaceAccess {
        namespace: NodeIndex,
        field: NodeIndex,
    },

    Initialize {
        ty: NodeIndex,
        fields: Vec<(NodeIndex, NodeIndex)>,
    },
    InitializeArray {
        ty: NodeIndex,
        elements: Vec<NodeIndex>,
    },

    Function {
        args: Vec<NodeIndex>,
        ret_ty: NodeIndex,
        body: NodeIndex,
    },

    FunctionCall {
        fn_name: NodeIndex,
        args: Vec<NodeIndex>,
    },

    PointerOf(NodeIndex),
    Deref(NodeIndex),

    Cast(NodeIndex, NodeIndex),
    SizeOf(NodeIndex),
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
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
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
        name: NodeIndex,
        ty: Option<NodeIndex>,
        expr: NodeIndex,
    },
    Decl {
        name: NodeIndex,
        ty: NodeIndex,
    },
    Assign {
        lhs: NodeIndex,
        rhs: NodeIndex,
    },

    Scope {
        // this is for cases when we want to reach from an entity inside a block to it's owner like the if node that owns a block.
        owner: NodeIndex,
        is_file_root: bool,
        stmts: Vec<NodeIndex>,
    },

    If {
        cases: Vec<(NodeIndex, NodeIndex)>,
    },

    For {
        start: NodeIndex,
        cond: NodeIndex,
        cont: NodeIndex,
        body: NodeIndex,
    },
    ForIn {
        iterator: NodeIndex,
        iterable: NodeIndex,
        body: NodeIndex,
    },
    While {
        cond: NodeIndex,
        body: NodeIndex,
    },

    Break,
    Continue,

    Goto(NodeIndex),

    Return(NodeIndex),
}

#[derive(Debug, Clone)]
pub struct IR {
    pub filename: String,
    pub file_source: String,
    pub tokens: Vec<Token>,
    pub root: NodeIndex,
    pub registered_indexes: Vec<NodeIndex>,
    pub nodes: HashMap<NodeIndex, Node>,
    pub scoped_symbols: HashMap<NodeIndex, HashMap<String, Type>>,
    pub exported_symbols: HashMap<String, Type>,
    pub dependencies: Vec<Dependency>,
    pub type_checked: bool,
}

impl IR {
    pub fn delete_nodes_after_index(&mut self, index: usize) {
        let to_delete = self.registered_indexes[index + 1..].to_vec();
        for id in &to_delete {
            self.nodes.remove(id);
        }

        self.registered_indexes = self.registered_indexes[..index].to_vec();
    }
    pub fn any_unknowns(&self) -> bool {
        for (_, node) in self.nodes.iter() {
            if node.type_information.is_none() {
                return true;
            }
        }
        return false;
    }
    pub fn get_loads(&self) -> Vec<String> {
        let root_node = self.nodes.get(&self.root).unwrap();
        let mut loads = vec![];
        match root_node.data {
            NodeData::Statement(Statement::Scope {
                owner,
                is_file_root,
                ref stmts,
            }) => {
                for stmt in stmts {
                    let stmt_node = self.nodes.get(stmt).unwrap();
                    match stmt_node.data {
                        NodeData::Statement(Statement::Load(ref rel_path)) => {
                            loads.push(rel_path.clone())
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }

        return loads;
    }
    pub fn print_node(&self, idx: NodeIndex) {
        println!("{:?}", self.get_node(idx).unwrap());
    }
    pub fn add_exported_symbol(&mut self, identifier_index: NodeIndex, ty: Type) {
        let identifier = self.get_node(identifier_index).unwrap();
        if let NodeData::Expression(Expression::Identifier(ident)) = identifier.data {
            self.exported_symbols.insert(ident, ty);
        }
    }
    pub fn add_symbol_to_scope(&mut self, scope: NodeIndex, identifier_index: NodeIndex, ty: Type) {
        let identifier = self.get_node(identifier_index).unwrap();
        if let NodeData::Expression(Expression::Identifier(ident)) = identifier.data {
            match self.scoped_symbols.get_mut(&scope) {
                Some(ss) => {
                    ss.insert(ident, ty);
                    return;
                }
                None => {
                    let mut hm: HashMap<String, Type> = HashMap::new();
                    hm.insert(ident, ty);
                    self.scoped_symbols.insert(scope, hm);
                }
            }
        }
    }
    pub fn get_node(&self, index: NodeIndex) -> Result<Node> {
        return Ok(self.nodes.get(&index).unwrap().clone());
    }

    pub fn add_tag(&mut self, index: NodeIndex, tag: AstTag) {
        let node = self.nodes.get_mut(&index).unwrap();
        node.tags.push(tag);
    }
}
