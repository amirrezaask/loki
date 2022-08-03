use std::{collections::HashMap, ops::Deref};
use anyhow::Result;

use super::parser::{ Node, AST , Decl };

#[derive(Debug, Clone)]
pub struct SymbolTable<'a> {
    pub asts: &'a Vec<AST>,
    pub symbols: HashMap<String, Vec<SymbolMetadata>>,
}
#[derive(Clone, Debug, PartialEq, Eq)]
enum SymbolType {
    Name(String), // points to a symbol name that can be a type def. we should find closest one to our current scope
    Uint,
    Int,
    Float,
    Bool,
    Char,
    String,
    Type,
}

#[derive(Debug, Clone)]
pub struct SymbolMetadata {
    index: SymbolIndex,
    ty: SymbolType
}

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
    fn add_sym(&mut self, sym: String, md: SymbolMetadata) {
        if !self.symbols.contains_key(&sym) {
            self.symbols.insert(sym, vec![md]);
        }
        else {
            self.symbols.get_mut(&sym).unwrap().push(md);
        }
    }

    pub fn lookup(&self, sym: &str, idx: &SymbolIndex) -> Option<&SymbolMetadata> {
        let mds = self.symbols.get(sym)?;
        let scores = Vec::<usize>::new();

        for md in mds {
            let score: usize = 0;
            for i in 0..idx.v.len() {
                if i >= md.index.v.len() {
                    break;
                }
                if md.index.v[i] == idx.v[i] {
                    score+=1;
                }
            }
            scores.push(score);
        }
        let max_score_idx = 0;
        for (idx, score) in scores.iter().enumerate() {
            if *score >= scores[max_score_idx] {
                max_score_idx = idx;
            }
        }

        return Some(&mds[max_score_idx]);
    }

    fn fill_for_block(&self, ast: &'a AST, path: &mut SymbolIndex, block: &Vec<Node>) -> Result<()> {
        let mut symbols = Vec::<(String, SymbolMetadata)>::new();

        for (idx, stmt) in block.iter().enumerate() {
            match stmt {
                Node::Decl(decl) => {
                    let mut new_path = path.new_index_with_push(idx);
                    self.fill_for_decl(ast, &mut new_path, &mut decl)?;
                }

                
                _ => {
                    unreachable!();
                }
            }
        }

        Ok(())
    }

    fn fill_for_decl(&self, mut ast: &'a AST, path: &mut SymbolIndex, decl: &mut Decl) -> Result<()> {
        let name: String = if let Node::Ident(name_idx) = *decl.name {
            ast.get_src_for_token(name_idx)?.to_string()
        } else {
            unreachable!();
        };
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
        match decl.expr.deref() {
            Node::FnDef(_, _, block) => {
                //TODO: store return type in symbols table as the "type" of the function ?
                self.fill_for_block(ast, path, block)?;
            }

            Node::Uint(_) => {
                self.add_sym(name, SymbolMetadata {
                    index: path.clone(),
                    ty: SymbolType::Uint,
                });
                if decl.ty.is_none() {
                    decl.ty = Box::new(Some(Node::UintTy(0)));
                }
            }
            Node::Int(_) => {
                self.add_sym(name, SymbolMetadata {
                    index: path.clone(),
                    ty: SymbolType::Int,
                });
                if decl.ty.is_none() {
                    decl.ty = Box::new(Some(Node::IntTy(0)));
                }

            }
 
            Node::Float(_) => {
                self.add_sym(name, SymbolMetadata {
                    index: path.clone(),
                    ty: SymbolType::Float,
                });
                if decl.ty.is_none() {
                    decl.ty = Box::new(Some(Node::FloatTy(0)));
                }
            }
            Node::True(_) => {
                self.add_sym(name, SymbolMetadata {
                    index: path.clone(),
                    ty: SymbolType::Bool,
                });
                if decl.ty.is_none() {
                    decl.ty = Box::new(Some(Node::BoolTy(0)));
                }
            }
            Node::False(_) => {
                self.add_sym(name, SymbolMetadata {
                    index: path.clone(),
                    ty: SymbolType::Bool,
                });
                if decl.ty.is_none() {
                    decl.ty = Box::new(Some(Node::BoolTy(0)));
                }
            }
            Node::Char(_) => {
                self.add_sym(name, SymbolMetadata {
                    index: path.clone(),
                    ty: SymbolType::Char,
                });
                if decl.ty.is_none() {
                    decl.ty = Box::new(Some(Node::CharTy(0)));
                }
            }
            Node::StringLiteral(_) => {
                self.add_sym(name, SymbolMetadata {
                    index: path.clone(),
                    ty: SymbolType::String,
                });
                
                if decl.ty.is_none() {
                    decl.ty = Box::new(Some(Node::StringTy(0)));
                }
            }
            Node::Ident(ident) => { // user expression is an ident so we should get it's type
                let name = ast.get_src_for_token(*ident)?;
                let md = self.lookup(name, path).unwrap();
                // TODO this will crash on unknown ident
                self.add_sym(name.to_string(), SymbolMetadata {
                    index: path.clone(),
                    ty: md.ty.clone(),
                });
                if decl.ty.is_none() {
                    decl.ty = Box::new(Some(Node::Ident(*ident)));
                }
   
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
                        let name = ast.get_src_for_token(ident)?;
                        let md = self.lookup(name, path).unwrap(); // TODO this will crash on unknown ident
                        if md.ty == SymbolType::Type {
                            self.add_sym(name.to_string(), SymbolMetadata {
                                index: path.clone(),
                                ty: SymbolType::Name(name.to_string()),
                            })

                        }
                        if decl.ty.is_none() {
                            decl.ty = Box::new(Some(Node::Ident(ident)));
                        }
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
                self.add_sym(name, SymbolMetadata {
                    index: path.clone(),
                    ty: SymbolType::Type,
                });

                }

            _ => {
                println!("cannot infer type for {:?}", decl.expr);
                unreachable!();
            }


            
 
        }

        Ok(())
    }

    pub fn new(asts: &'a mut Vec<AST>) -> Result<Self> {
        let symbols = HashMap::<String, Vec<SymbolMetadata>>::new();
        let mut st = Self {
            asts, symbols
        };
        for ast in st.asts.iter() {
            for (top_level_idx, node) in ast.top_level.iter_mut().enumerate() {
                match node {
                    Node::Decl(decl) => {
                        let mut path = SymbolIndex::new(vec![top_level_idx]);
                        let mut syms = st.fill_for_decl(ast, &mut path, decl)?;
                    }

                    _ => {}
                    
                }
            }

        }
        Ok(st)

    }

    
}
