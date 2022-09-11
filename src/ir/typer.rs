use std::collections::HashMap;
use std::ops::Deref;

use serde::Serialize;
use anyhow::{Result, anyhow};

use super::ir::BinaryOperation;
use super::ir::Expression;
use super::ir::IR;
use super::ir::Index;
use super::ir::AstTag;
use super::ir::Node;
use super::ir::NodeData;
use super::ir::Statement;
use super::ir;
use super::ir::UnaryOperation;
use super::lexer::Token;

type BitSize = i64;
#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum Type {
    NoType,

    SignedInt(BitSize),
    UnsignedInt(BitSize),
    Float(BitSize),

    Bool,

    IntPtr,
    UintPtr,

    Char,
    String,

    Array(Box<Type>),
    DynamicArray(Box<Type>),

    Initialize(Box<Type>),

    Struct {
        fields: Vec<(String, Type)>, // name: type
    },
    Enum {
        variants: Vec<String>,
    },

    TypeRef {
        name: String,
        actual_ty: Box<Type>,
    },

    Pointer(Box<Type>),

    FnType(Vec<Type>, Box<Type>),

    CVarArgs,
    CString,

    Void,
}

type File = String;

pub struct TypedIR<'a> {
    ir: IR,
    scoped_symbols: HashMap<Index, HashMap<String, Type>>,
    other_files_symbols: &'a HashMap<File, HashMap<String, Type>>,
}


impl<'a> TypedIR<'a> {
    fn add_type(&mut self, index: Index, ty: Type) {
        let node = self.ir.nodes.get_mut(&index).unwrap();
        node.type_information = Some(ty);
    }
    fn find_identifier_type(&self, mut scope: Index, identifier: String) -> Result<Type> {
        let mut scope_node = self.ir.nodes.get(&scope).unwrap().clone();
        loop {
            match self.scoped_symbols.get(&scope) {
                Some(scope_symbols) => {
                    match scope_symbols.get(&identifier) {
                        Some(ty) => {
                            return Ok(ty.clone());
                        }
                        None => {
                            match scope_node.parent_block {
                                Some(parent) => {
                                    scope = parent; 
                                    scope_node = self.ir.nodes.get(&scope).unwrap().clone();
                                }
                                None => break,
                            }
                        }
                    }
                }
                None => {
                    return Err(anyhow!("no containing scope found to lookup identitifer {}", identifier));
                }
            }
        }
    
        panic!("no type for identifier {} found, still have not looked into loaded files sorry", identifier);
        // check file loads
    }

    fn resolve_type_definition(&mut self, type_definition_index: Index) -> Result<Type> {

        unimplemented!()
    }

    fn resolve_namespace_access_type(&self, ns_type: Type, field: String) -> Result<Type> {
        match &ns_type {
            Type::Struct { ref fields } => {
                for sf in fields {
                    if sf.0 == field {
                        return Ok(sf.1.clone());
                    }
                }
                return Err(anyhow!("struct {:?} does not have field named {}", ns_type, field));
            },
            Type::Enum { ref variants } => {
                for variant in variants {
                    if variant == &field {
                        return Ok(Type::UnsignedInt(64));
                    }
                }
                return Err(anyhow!("enum {:?} does not have field named {}", ns_type, field));
            },
            Type::Pointer(ref actual_ty) => {
               return self.resolve_namespace_access_type(ns_type, field);
            },
            Type::TypeRef { name, actual_ty } => {
               return self.resolve_namespace_access_type(ns_type, field);
            },
            _ => {
                panic!("unexpted namespace {:?}", ns_type);
            }
        }
    }
    fn type_expression(&mut self, expression_index: Index) -> Result<Type> {
        let node = self.ir.nodes.get(&expression_index).unwrap().clone();
        match node.data {
            NodeData::Expression(ref expr) => {
                match expr {
                    // TODO: check the actual numbers and find smallest possible type for it.
                    Expression::Unsigned(_) => {
                        self.add_type(expression_index, Type::UnsignedInt(64));

                        return Ok(Type::UnsignedInt(64));
                    },
                    Expression::Signed(_) => {
                        self.add_type(expression_index, Type::SignedInt(64));
                        return Ok(Type::SignedInt(64));
                    },
                    Expression::StringLiteral(_) => {
                        self.add_type(expression_index, Type::String);
                        return Ok(Type::String);
                    },
                    Expression::Float(_) => {
                        self.add_type(expression_index, Type::Float(64));
                        return Ok(Type::Float(64));
                    },
                    Expression::Bool(_) => {
                        self.add_type(expression_index, Type::Bool);
                        return Ok(Type::Bool);
                    },
                    Expression::Char(_) => {
                        self.add_type(expression_index, Type::Char);
                        return Ok(Type::Char);
                    },
                    
                    Expression::Paren(ref inner_expr) => {
                        let inner_type = self.type_expression(*inner_expr)?;
                        self.add_type(*inner_expr, inner_type.clone());
                        return Ok(inner_type);
                    },
                    
                    Expression::Identifier(ref identifier) => {
                        let identifier_type = self.find_identifier_type(node.parent_block.unwrap(), identifier.clone())?;
                        self.add_type(expression_index, identifier_type.clone());
                        return Ok(identifier_type);
                    },
                    Expression::UnaryOperation { operator, expr } => {
                        let expr_type = self.type_expression(*expr)?;
                        match operator {
                            UnaryOperation::Not => {
                                self.add_type(expression_index, Type::Bool);
                                return Ok(Type::Bool);
                            },
                        }
                    },
                    Expression::ArrayIndex { arr, idx } => {
                        let arr_type = self.type_expression(*arr)?;
                        let idx_type = self.type_expression(*idx)?;
                        match arr_type {
                            Type::Array(ref element_type) => {
                                self.add_type(expression_index, *element_type.clone());
                                return Ok(*element_type.clone())
                            },
                            _ => unreachable!(),                            
                        }
                    },
                    Expression::BinaryOperation { operation, left, right } => {
                        let left_type = self.type_expression(*left)?;
                        let right_type = self.type_expression(*right)?;
                        match operation {
                            BinaryOperation::Sum | 
                            BinaryOperation::Subtract |
                            BinaryOperation::Divide |
                            BinaryOperation::Modulu |
                            BinaryOperation::Multiply 
                            => {
                                if left_type == right_type {
                                    self.add_type(expression_index, left_type.clone());
                                    return Ok(left_type);
                                } else {
                                    unimplemented!();
                                }
                            },
                            BinaryOperation::Greater |
                            BinaryOperation::GreaterEqual |
                            BinaryOperation::Less |
                            BinaryOperation::LessEqual |
                            BinaryOperation::Equal |
                            BinaryOperation::NotEqual |
                            BinaryOperation::BinaryAnd |
                            BinaryOperation::BinaryOr => {
                                if left_type == Type::Bool && right_type == Type::Bool {
                                    self.add_type(expression_index, Type::Bool);
                                    return Ok(left_type);
                                } else {
                                    unimplemented!();
                                }
                            }
                        }
                    },
                    Expression::NamespaceAccess { namespace, field: field_id } => {
                        let ns_type = self.type_expression(*namespace)?;
                        let field_node = self.ir.nodes.get(field_id).unwrap();

                        // get struct/fileload/enum out of namespace then get field type of it
                        if let NodeData::Expression(Expression::Identifier(ref field)) = field_node.data {
                            let field_type = self.resolve_namespace_access_type(ns_type, field.clone())?;
                            self.add_type(expression_index, field_type.clone());
                            self.add_type(*field_id, field_type.clone());
                            return Ok(field_type);
                        }
                        else {
                            unreachable!();
                        }
                    },
                    Expression::Initialize { ty, fields } => {
                        let initialize_type = self.resolve_type_definition(*ty)?;
                        self.add_type(expression_index, initialize_type.clone());
                        // check if fields are valid in context of it's type.
                        return Ok(initialize_type);
                    },
                    Expression::InitializeArray { ref elements } => {
                        let mut array_elem_type: Option<Type> = None;
                        for elem in elements {
                            let elem_type = self.type_expression(*elem)?;
                            if array_elem_type.is_none() {
                                array_elem_type = Some(elem_type);
                            } else {
                                if array_elem_type.clone().unwrap() != elem_type {
                                    return Err(anyhow!("all elements of array should be of same type, expected {:?} got {:?}", array_elem_type, elem_type));
                                }
                            }
                        }

                        return Ok(array_elem_type.unwrap());
                    },
                    Expression::Function { ref args, ret_ty: ref ret_ty_node, ref body } => {
                        for arg in args {
                            self.type_statement(*arg)?;
                        }

                        let arg_types: Vec<Type> = args.iter().map(|arg_idx| {
                            if let NodeData::Statement(Statement::Decl { name, ty }) = self.ir.nodes.get(arg_idx).unwrap().data {
                                self.ir.nodes.get(&ty).clone().unwrap().type_information.as_ref().unwrap();
                            }
                            unreachable!();
                        }).collect();

                        let ret_type = self.resolve_type_definition(*ret_ty_node)?;
                        self.type_scope(*body)?;
                        let fn_type = Type::FnType(arg_types, Box::new(ret_type));
                        self.add_type(expression_index, fn_type.clone());
                        return Ok(fn_type);
                    },
                    Expression::FunctionCall { ref fn_name, ref args } => {
                        let fn_type = self.type_expression(*fn_name)?;
                        if let Type::FnType(_, ret) = fn_type {
                            return Ok(ret.deref().clone());
                        } else {
                            unreachable!()
                        }

                    },
                    Expression::PointerOf(ref pointee) => {
                        let pointee_type = self.type_expression(*pointee)?;
                        self.add_type(expression_index, Type::Pointer(Box::new(pointee_type.clone())));
                        return Ok(Type::Pointer(Box::new(pointee_type.clone())));
                    },
                    Expression::Deref(ref pointer) => {
                        let pointer_type = self.type_expression(*pointer)?;
                        match pointer_type {
                            Type::Pointer(ref inner) => {
                                self.add_type(expression_index, inner.deref().clone());
                                return Ok(inner.deref().clone());
                            },
                            _ => {
                                unimplemented!()
                            }
                        }
                    },
                }
            },
            NodeData::Statement(_) => panic!("unexpected when typing an expression {:?}", node),
            NodeData::TypeDefinition(_) => panic!("unexpected when typing an expression {:?}", node)
            
        }        
    }

    
    fn type_statement(&mut self, stmt_index: Index) -> Result<()> {
        let node = self.ir.nodes.get(&stmt_index).unwrap().clone();
        match node.data {
            NodeData::Statement(ref stmt) => {
                match stmt {
                    Statement::Load(_) | Statement::Host(_) => Ok(()),

                    Statement::Def { mutable, ref name, ref expr } => {
                        let expr_ty = self.type_expression(*expr)?;
                        self.add_type(*name, expr_ty);

                        return Ok(());
                    }

                    Statement::Decl { ref name, ref ty } => {
                        let decl_type = self.resolve_type_definition(*ty)?;
                        self.add_type(*name, decl_type);

                        return Ok(());
                    },

                    Statement::Assign { ref lhs, ref rhs } => {
                        self.type_expression(*lhs)?;
                        self.type_expression(*rhs)?;

                        return Ok(());
                    },

                    Statement::Scope { ref owner, is_file_root, ref stmts } => {
                        self.type_scope(node.id) 
                    }

                    Statement::If { ref cases } => {
                        for case in cases {
                            let cond_type = self.type_expression(case.0)?;
                            self.type_scope(case.1)?;
                        }

                        return Ok(());
                    },

                    Statement::For { ref start, ref cond, ref cont, ref body } => {
                        self.type_statement(*start)?;
                        self.type_expression(*cond)?;
                        self.type_statement(*cont)?;
                        self.type_statement(*body)?;

                        return Ok(());
                    },
                    
                    Statement::ForIn { ref iterator, ref iterable, ref body } => {
                        self.type_expression(*iterator)?;
                        self.type_expression(*iterable)?;
                        self.type_statement(*body)?;

                        return Ok(());
                    },
                    
                    Statement::While { ref cond, ref body } => {
                        self.type_expression(*cond)?;
                        self.type_statement(*body)?;

                        return Ok(())
                    },
                    
                    Statement::Break => {
                        unimplemented!()
                    },
                    
                    Statement::Continue => {
                        unimplemented!()
                    },
                    
                    Statement::Goto(_) => {
                        unimplemented!()
                    },
                    
                    Statement::Return(ref expr) => {
                    
                        self.type_expression(*expr)?;
                        return Ok(());
                    },
                }
            },
            NodeData::TypeDefinition(_) => {
                return Err(anyhow!("unexpected as statement {:?}", node));
            },
            NodeData::Expression(_) => {
                return Err(anyhow!("unexpected as statement {:?}", node));
            },
        }

    }
    fn type_root(&mut self, root_index: Index) -> Result<()> {
        // TODO : resolve all root symbols before going anyfurther.
        self.type_scope(root_index)?;
        
        Ok(())
    }
    fn type_scope(&mut self, scope_index: Index) -> Result<()> {
        let scope = self.ir.nodes.get(&scope_index).unwrap().clone();
        if let NodeData::Statement(Statement::Scope { owner, is_file_root, ref stmts }) = scope.data {
            for stmt in stmts {
                self.type_statement(*stmt)?;
            }
        }
        let node = self.ir.nodes.get(&scope_index).unwrap();
        Ok(())
    }

    pub fn new(ir: IR, other_files_symbols: &HashMap<String, HashMap<String, Type>>) -> Result<Self> {
        let mut tir = TypedIR {
            ir,
            scoped_symbols: todo!(),
            other_files_symbols,
        };
        tir.type_root(ir.root)?;
        Ok(tir)
    }
}