use anyhow::Result;
use std::{
    collections::HashMap,
    ops::Deref,
};

use crate::ast::NodeID;

use crate::ast::{ Node, NodeData, AST , Decl };

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub symbols_by_id: HashMap<NodeID, SymbolMetadata>,
    pub symbols_by_name: HashMap<String, Vec<SymbolMetadata>>,
    pub symbols_by_scope: HashMap<SymbolLocation, SymbolMetadata>,
        
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SymbolType {
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
    // filename: String,
    location: SymbolLocation,
    id: NodeID,
    name: String,
    pub ty: SymbolType
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct SymbolLocation {
    // filename: String,// TODO to handle multiple files correctly
    v: Vec<usize>,
}

impl SymbolLocation {
    pub fn new(v: Vec<usize>) -> Self {
        Self {v}
    }

    pub fn new_index_with_push(&self, idx: usize) -> Self {
        let mut v = self.clone();
        v.v.push(idx);
        return v;
    }
    
}

impl SymbolTable {
    fn add_sym_by_name(&mut self, md: &SymbolMetadata) {
        if !self.symbols_by_name.contains_key(&md.name) {
            self.symbols_by_name.insert(md.name.clone(), vec![md.clone()]);
        }
        else {
            self.symbols_by_name.get_mut(&md.name).unwrap().push(md.clone());
        }
        
    }
    fn add_sym_by_id(&mut self, md: &SymbolMetadata) {
        if !self.symbols_by_id.contains_key(&md.id) {
            self.symbols_by_id.insert(md.id, md.clone());
        } else {
            panic!("node already defined in symbol table: {:?}", md);
        }

    }
    fn add_sym_by_location(&mut self, md: &SymbolMetadata) {
        self.symbols_by_scope.insert(md.location.clone(), md.clone());
    }
    fn add_sym(&mut self, md: SymbolMetadata) {
        self.add_sym_by_location(&md);
        self.add_sym_by_id(&md);
        self.add_sym_by_name(&md);
    }
    

    pub fn lookup_by_name(&self, name: String) -> Option<&Vec<SymbolMetadata>> {
        self.symbols_by_name.get(&name)
    }

    pub fn lookup_by_id(&self, id: NodeID) -> Option<&SymbolMetadata> {
        self.symbols_by_id.get(&id)
    }

    pub fn lookup_closest_by_name(&self, name: String, location: SymbolLocation) -> Option<SymbolMetadata> {
        let mds = self.symbols_by_name.get(&name)?;
        let mut scores = Vec::<usize>::new();

        for md in mds {
            let mut score: usize = 0;
            for i in 0..location.v.len() {
                if i >= md.location.v.len() {
                    break;
                }
                if md.location.v[i] == location.v[i] {
                    score+=1;
                }
            }
            scores.push(score);
        }
        let mut max_score_idx = 0;
        for (idx, score) in scores.iter().enumerate() {
            if *score >= scores[max_score_idx] {
                max_score_idx = idx;
            }
        }

        return Some(mds[max_score_idx].clone());
    }

    fn fill_for_block(&mut self, ast: &AST, path: &mut SymbolLocation, block: &Vec<Node>) -> Result<()> {
        for (idx, stmt) in block.iter().enumerate() {
            match &stmt.data {
                NodeData::Decl(decl) => {
                    let mut new_path = path.new_index_with_push(idx);
                    self.fill_for_decl(ast, &mut new_path, &decl)?;
                }
                NodeData::If(_, then, _else) => {
                    let mut new_path = path.new_index_with_push(idx);
                    self.fill_for_block(ast, &mut new_path, then)?;
                    // if _else.is_some() { // TODO
                    //     let mut new_path = path.new_index_with_push(idx);
                    //     self.fill_for_block(ast, &mut new_path, &_else.unwrap())?;
                    // }
                }

                
                _ => {
                }
            }
        }

        Ok(())
    }

    fn fill_for_decl(&mut self, ast: &AST, path: &mut SymbolLocation, decl: &Decl) -> Result<()> {
        let name: String = ast.get_name_for_ident(*decl.name.clone())?.to_string();
        let id = decl.name.id;
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
        match &decl.expr.deref().data {
            NodeData::FnDef(_, _, block) => {
                //TODO: store return type in symbols table as the "type" of the function ?
                self.fill_for_block(ast, path, &block)?;
            }

            NodeData::Uint(_) => {
                self.add_sym(SymbolMetadata {
                    name,
                    id,
                    location: path.clone(),
                    ty: SymbolType::Uint,
                });
            }
            NodeData::Int(_) => {
                self.add_sym(SymbolMetadata {
                    name, id,
                    location: path.clone(),
                    ty: SymbolType::Int,
                });
            }
 
            NodeData::Float(_) => {
                self.add_sym(SymbolMetadata {
                    name, id,
                    location: path.clone(),
                    ty: SymbolType::Float,
                });
            }
            NodeData::True(_) | NodeData::False(_) => {
                self.add_sym(SymbolMetadata {
                    name, id,
                    location: path.clone(),
                    ty: SymbolType::Bool,
                });
            }
            
            NodeData::Char(_) => {
                self.add_sym(SymbolMetadata {
                    name, id,
                    location: path.clone(),
                    ty: SymbolType::Char,
                });
            }
            NodeData::StringLiteral(_) => {
                self.add_sym(SymbolMetadata {
                    name, id,
                    location: path.clone(),
                    ty: SymbolType::String,
                });
            }
            NodeData::Ident(ident) => { // user expression is an ident so we should get it's type
                let md = self.lookup_closest_by_name(ast.get_name_for_ident(*decl.expr.clone())?.to_string(), path.clone()).unwrap();
                // TODO this will crash on unknown ident
                self.add_sym(SymbolMetadata {
                    name,
                    id,
                    location: path.clone(),
                    ty: md.ty.clone(),
                });
            }

            NodeData::Initialize(op_ty, _) => {
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

                match ty.data {
                    NodeData::Ident(ident) => {
                        let name = ast.get_src_for_token(ident)?;
                        let md = self.lookup_closest_by_name(name.to_string(), path.clone()).unwrap(); // TODO this will crash on unknown ident
                        if md.ty == SymbolType::Type {
                            self.add_sym(SymbolMetadata {
                                name: name.to_string(), id,
                                location: path.clone(),
                                ty: SymbolType::Name(name.to_string()),
                            })

                        }
                    }

                    _ => {
                        println!("cannot get type info for: {:?}", ty);
                        unreachable!();
                    }
                }
            }
            NodeData::Enum(_, _) |
            NodeData::Struct(_) |
            NodeData::IntTy(_) |
            NodeData::Int8Ty(_) |
            NodeData::Int16Ty(_) |
            NodeData::Int32Ty(_) |
            NodeData::Int64Ty(_) |
            NodeData::Int128Ty(_) |
            NodeData::UintTy(_) |
            NodeData::Uint8Ty(_) |
            NodeData::Uint16Ty(_) |
            NodeData::Uint32Ty(_) |
            NodeData::Uint64Ty(_) |
            NodeData::Uint128Ty(_) |
            NodeData::FloatTy(_) |
            NodeData::BoolTy(_) |
            NodeData::StringTy(_) |
            NodeData::CharTy(_) |
            NodeData::VoidTy(_)
                => {
                    self.add_sym(SymbolMetadata {
                        name, id,
                        location: path.clone(),
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

    pub fn new(asts: &Vec<AST>) -> Result<Self> {
        let symbols_by_name = HashMap::<String, Vec<SymbolMetadata>>::new();
        let symbols_by_id = HashMap::<NodeID, SymbolMetadata>::new();
        let symbols_by_scope = HashMap::<SymbolLocation, SymbolMetadata>::new();

        let mut st = Self {
            symbols_by_id, symbols_by_name, symbols_by_scope
        };
        for ast in asts.iter() {
            for (top_level_idx, node) in ast.top_level.iter().enumerate() {
                match &node.data {
                    NodeData::Decl(decl) => {
                        let mut path = SymbolLocation::new(vec![top_level_idx]);
                        st.fill_for_decl(ast, &mut path, &decl)?;
                    }

                    _ => {}
                    
                }
            }

        }
        Ok(st)

    }

    
}
