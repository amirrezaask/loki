use std::{collections::HashMap, ops::Deref};
use anyhow::Result;

use super::parser::{ Node, AST , Decl };

#[derive(Debug, Clone)]
pub struct SymbolTable<'a> {
    pub asts: &'a Vec<AST>,
    pub symbols: Vec<(String, SymbolMetadata)>,
}
#[derive(Clone, Debug)]
enum SymbolType {
    Index(SymbolIndex),
    Uint,
    Int,
    Float,
    Bool,
    Char,
    String,
    Type,
}
impl SymbolType {
    pub fn to_node(&self) -> Node {
        match self {
            SymbolType::Uint => Node::UintTy(0),
            SymbolType::Int => Node::IntTy(0),
            SymbolType::Float => Node::FloatTy(0),
            SymbolType::Bool => Node::BoolTy(0),
            SymbolType::Char => Node::CharTy(0),
            SymbolType::String => Node::StringTy(0),
            SymbolType::Index(si) => {
                unreachable!();
            },
            _ => {
                unreachable!();
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct SymbolMetadata {
    index: SymbolIndex,
    ty: SymbolType
}

type Symbol = String;

#[derive(Clone, Debug)]
struct SymbolIndex {
    v: Vec<usize>,
}

impl SymbolIndex {
    pub fn new(v: Vec<usize>) -> Self {
        Self {v}
    }

    pub fn new_index_with_push(&self, idx: usize) -> Self {
        let mut v = self.clone();
        v.v.push(idx);
        return v;
    }
    
}

impl<'a> SymbolTable<'a> {

    pub fn lookup(&self, sym: &str) -> Option<&SymbolMetadata> {
        //TODO: This should handle shadowed variables, choose closest one to the current scope.
        
        for (name, md) in self.symbols.iter() {
            if name == sym {
                return Some(md);
            }
        }
        None
    }

    fn fill_for_block(&self, ast: &'a AST, path: &mut SymbolIndex, block: &Vec<Node>) -> Result<Vec<(Symbol, SymbolMetadata)>> {
        let mut symbols = Vec::<(Symbol, SymbolMetadata)>::new();

        for (idx, stmt) in block.iter().enumerate() {
            match stmt {
                Node::Decl(decl) => {
                    let mut new_path = path.new_index_with_push(idx);
                    symbols.append(&mut self.fill_for_decl(ast, &mut new_path, decl)?);
                }

                
                _ => {

                }
            }
        }

        Ok(symbols)
    }

    fn fill_for_decl(&self, ast: &'a AST, path: &mut SymbolIndex, decl: &Decl) -> Result<Vec<(Symbol, SymbolMetadata)>> {
        let mut symbols = Vec::<(Symbol, SymbolMetadata)>::new();
        let name: String = if let Node::Ident(name_idx) = *decl.name {
            ast.get_src_for_token(name_idx)?.to_string()
        } else {
            unreachable!();
        };
        
        match decl.expr.deref() {
            Node::FnDef(_, _, block) => {
                symbols.append(&mut self.fill_for_block(ast, path, block)?);
            }

            Node::Uint(_) => {
                symbols.push((name, SymbolMetadata {
                    index: path.clone(),
                    ty: SymbolType::Uint,
                }));
            }
            Node::Int(_) => {
                symbols.push((name, SymbolMetadata {
                    index: path.clone(),
                    ty: SymbolType::Int,
                }));
            }
 
            Node::Float(_) => {
                symbols.push((name, SymbolMetadata {
                    index: path.clone(),
                    ty: SymbolType::Float,
                }));
            }
            Node::True(_) => {
                symbols.push((name, SymbolMetadata {
                    index: path.clone(),
                    ty: SymbolType::Bool,
                }));
            }
            Node::False(_) => {
                symbols.push((name, SymbolMetadata {
                    index: path.clone(),
                    ty: SymbolType::Bool,
                }));
            }
            Node::Char(_) => {
                symbols.push((name, SymbolMetadata {
                    index: path.clone(),
                    ty: SymbolType::Char,
                }));
            }
            
            Node::Ident(ident) => {
                let md = self.lookup(ast.get_src_for_token(*ident)?).unwrap();
                // TODO this will crash on unknown ident
                symbols.push((name, SymbolMetadata {
                    index: path.clone(),
                    ty: md.ty.clone(),
                }));
            }

            Node::StringLiteral(_) => {
                symbols.push((name, SymbolMetadata {
                    index: path.clone(),
                    ty: SymbolType::String,
                }));
            }
            Node::TypeInit(op_ty, _) => {
                if op_ty.is_none() && decl.ty.is_none() {
                    println!("need either a type hint or type name in rhs of decl.");
                    unreachable!();
                }
                // if op_ty.is_some() && decl.ty.is_some() && *(op_ty.unwrap().clone()) != decl.ty.unwrap() {
                //     println!("type hint and rhs type are not similar.");
                //     unreachable!();
                // } // TODO
                let ty: Node = if op_ty.is_some() {
                    op_ty.clone().unwrap().deref().clone()
                } else {
                    decl.ty.clone().unwrap().clone()
                };

                match ty {
                    Node::Ident(ident) => {
                        let md = self.lookup(ast.get_src_for_token(ident)?).unwrap(); // TODO this will crash on unknown ident
                        symbols.push((name, SymbolMetadata {
                            index: path.clone(),
                            ty: SymbolType::Index(md.index.clone()),
                        }))
                    }

                    _ => {
                        println!("cannot get type info for: {:?}", ty);
                        unreachable!();
                    }
                }
            }
            Node::Enum(_, _) |
            Node::Struct(_) |
            Node::IntTy(_) |
            Node::Int8Ty(_) |
            Node::Int16Ty(_) |
            Node::Int32Ty(_) |
            Node::Int64Ty(_) |
            Node::Int128Ty(_) |
            Node::UintTy(_) |
            Node::Uint8Ty(_) |
            Node::Uint16Ty(_) |
            Node::Uint32Ty(_) |
            Node::Uint64Ty(_) |
            Node::Uint128Ty(_) |
            Node::FloatTy(_) |
            Node::BoolTy(_) |
            Node::StringTy(_) |
            Node::CharTy(_) |
            Node::VoidTy(_)
                => {
                symbols.push((name, SymbolMetadata {
                    index: path.clone(),
                    ty: SymbolType::Type,
                }));

                }

            _ => {
                println!("cannot infer type for {:?}", decl.expr);
                unreachable!();
            }


            
    // Sum(Box<Node>, Box<Node>),
    // Subtract(Box<Node>, Box<Node>),
    // Multiply(Box<Node>, Box<Node>),
    // Div(Box<Node>, Box<Node>),
    // Mod(Box<Node>, Box<Node>),
    // FieldAccess(Vec<Node>),
    // FnCall(Box<Node>, Vec<Node>),
    // If(Box<Node>, Box<Vec<Node>>, Option<Vec<Node>>),
    // TypeInit(Option<Box<Node>>, Vec<(Node, Node)>),
    // Cmp(Type, Box<Node>, Box<Node>), // op, lhs, rhs
    // Ref(Box<Node>),
    // Deref(Box<Node>),

    // Return(Box<Node>),
        }

        Ok(symbols)
    }

    pub fn new(asts: &'a Vec<AST>) -> Result<Self> {
        let mut symbols = Vec::<(String, SymbolMetadata)>::new();
        let mut st = Self {
            asts, symbols
        };
        for ast in st.asts.iter() {
            for (top_level_idx, node) in ast.top_level.iter().enumerate() {
                match node {
                    Node::Decl(decl) => {
                        let mut path = SymbolIndex::new(vec![top_level_idx]);
                        let mut syms = st.fill_for_decl(ast, &mut path, decl)?;
                        st.symbols.append(&mut syms);
                    }

                    _ => {}
                    
                }
            }

        }
        Ok(st)

    }
   
}


