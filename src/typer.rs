use std::collections::HashMap;
use std::hash::Hash;
use std::ops::Deref;

use serde::Serialize;
use crate::compliation::Dependency;
use crate::compliation::DependencyReason;
use crate::errors::*;
use crate::ir::TypeDefinition;
use crate::utils;

use super::errors::Error;
use super::errors::TypeCheckError;
use super::ir::BinaryOperation;
use super::ir::Expression;
use super::ir::IR;
use super::ir::NodeIndex;
use super::ir::AstTag;
use super::ir::Node;
use super::ir::NodeData;
use super::ir::Statement;
use super::ir;
use super::ir::UnaryOperation;
use crate::lexer::Token;

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

impl IR {
    fn add_type(&mut self, index: NodeIndex, ty: Type) {
        let node = self.nodes.get_mut(&index).unwrap();
        node.type_information = Some(ty);
    }
    fn find_identifier_type(&self, other_files_exports: &HashMap<String, HashMap<String, Type>>, mut scope: NodeIndex, identifier: String) -> Option<Type> {
        let mut this_scope = self.nodes.get(&scope).unwrap().clone();
        loop {
            let this_scope_symbols = self.scoped_symbols.get(&scope);
            if this_scope_symbols.is_none() {
                // this scope has no other symbols registered yet.
                // look for the parent
                
                if this_scope.parent_block.is_none() {
                    // scope has parent so it's file root, we should check the other files
                    break;
                }
                scope = this_scope.parent_block.unwrap();
                this_scope = self.nodes.get(&scope).unwrap().clone();
                continue;
            } 
            let this_scope_symbols = this_scope_symbols.unwrap();
            // this scope has some symbols so let's check them.
            match this_scope_symbols.get(&identifier) {
                Some(ty) => {
                    return Some(ty.clone());
                }
                None => {
                    // this scope has no symbol named identifier so check for parent.
                    if this_scope.parent_block.is_some() {
                        scope = this_scope.parent_block.unwrap();
                        this_scope = self.nodes.get(&scope).unwrap().clone();
                        continue;
                    }
                    // if this scope has no parents this means we just checked until root of the file and no luck
                    // so we should wait for other files to provide us this symbol.
                    break;
                }
            }
        }
        let loaded_files = self.get_loads();
        for file in loaded_files {
            let abs = utils::find_abs_path_to_file(&file).unwrap();
            match other_files_exports.get(&abs) {
                Some(exports) => {
                    let ty = exports.get(&identifier);
                    if ty.is_some() {
                        return Some(ty.unwrap().clone());
                    }
                },
                None => continue,
            }
        }

        return None;

    }

    fn resolve_type_definition(&mut self, type_definition_index: NodeIndex) -> Result<Option<Type>> {
        let type_definition = self.get_node(type_definition_index).unwrap();
        match type_definition.data {
            NodeData::TypeDefinition(ref td) => {
                match td {
                    TypeDefinition::CVarArgs => unimplemented!(),
                    TypeDefinition::CString => unimplemented!(),
                    TypeDefinition::IntPtr => unimplemented!(),
                    TypeDefinition::UintPtr => unimplemented!(),
                    TypeDefinition::Int(ref size) => {
                        self.add_type(type_definition_index, Type::SignedInt(*size));
                        return Ok(Some(Type::SignedInt(*size)));
                    },
                    TypeDefinition::Uint(size) => {
                        self.add_type(type_definition_index, Type::UnsignedInt(*size));
                        return Ok(Some(Type::UnsignedInt(*size)));
                    },
                    TypeDefinition::Float(size) => {
                        self.add_type(type_definition_index, Type::Float(*size));
                        return Ok(Some(Type::Float(*size)));
                    },
                    TypeDefinition::Bool => {
                        self.add_type(type_definition_index, Type::Bool);
                        return Ok(Some(Type::Bool));
                    },
                    TypeDefinition::String => {
                        self.add_type(type_definition_index, Type::String);
                        return Ok(Some(Type::String));
                    },
                    TypeDefinition::Char => {
                        self.add_type(type_definition_index, Type::Char);
                        return Ok(Some(Type::Char));
                    },
                    TypeDefinition::Void => {
                        self.add_type(type_definition_index, Type::Void);
                        return Ok(Some(Type::Void));
                    },
                    TypeDefinition::Pointer(_) => unimplemented!(),
                    TypeDefinition::Array { length, elem_ty } => unimplemented!(),
                    TypeDefinition::Function { args, ret } => unimplemented!(),
                    TypeDefinition::Struct(_) => unimplemented!(),
                    TypeDefinition::Enum(_) => unimplemented!(),
                }
            },
            _ => {
                unreachable!()
            }
        }
    }

    fn resolve_namespace_access_type(&self, ns_type: Type, field: String) -> Result<Type> {
        match &ns_type {
            Type::Struct { ref fields } => {
                for sf in fields {
                    if sf.0 == field {
                        return Ok(sf.1.clone());
                    }
                }
                return Err(Error {
                    filename: self.filename.clone(),
                    line: 0,
                    col: 0,
                    reason: Reason::TypeCheckError(TypeCheckError::StructDoesNotHaveField(ns_type, field)),
                })
            },
            Type::Enum { ref variants } => {
                for variant in variants {
                    if variant == &field {
                        return Ok(Type::UnsignedInt(64));
                    }
                }
                return Err(Error {
                    filename: self.filename.clone(),
                    line: 0,
                    col: 0,
                    reason: Reason::TypeCheckError(TypeCheckError::EnumDoesNotHaveVariant(ns_type, field)),
                })
            },
            Type::Pointer(ref actual_ty) => {
               return self.resolve_namespace_access_type(ns_type, field);
            },
            Type::TypeRef { name, actual_ty } => {
               return self.resolve_namespace_access_type(ns_type, field);
            },
            _ => {
                return Err(Error {
                    filename: self.filename.clone(),
                    line: 0,
                    col: 0,
                    reason: Reason::TypeCheckError(TypeCheckError::InvalidNamespace(ns_type)),
                })
            }
        }
    }
    fn type_expression(&mut self, other_files_exports: &HashMap<String, HashMap<String, Type>>,expression_index: NodeIndex) -> Result<Option<Type>> {
        let node = self.nodes.get(&expression_index).unwrap().clone();
        match node.data {
            NodeData::Expression(ref expr) => {
                match expr {
                    // TODO: check the actual numbers and find smallest possible type for it.
                    Expression::Unsigned(_) => {
                        self.add_type(expression_index, Type::UnsignedInt(64));

                        return Ok(Some(Type::UnsignedInt(64)));
                    },
                    Expression::Signed(_) => {
                        self.add_type(expression_index, Type::SignedInt(64));
                        return Ok(Some(Type::SignedInt(64)));
                    },
                    Expression::StringLiteral(_) => {
                        self.add_type(expression_index, Type::String);
                        return Ok(Some(Type::String));
                    },
                    Expression::Float(_) => {
                        self.add_type(expression_index, Type::Float(64));
                        return Ok(Some(Type::Float(64)));
                    },
                    Expression::Bool(_) => {
                        self.add_type(expression_index, Type::Bool);
                        return Ok(Some(Type::Bool));
                    },
                    Expression::Char(_) => {
                        self.add_type(expression_index, Type::Char);
                        return Ok(Some(Type::Char));
                    },
                    
                    Expression::Paren(ref inner_expr) => {
                        let inner_type = self.type_expression(other_files_exports, *inner_expr)?;
                        match inner_type {
                            Some(ty) => {
                                self.add_type(expression_index, ty.clone());
                                return Ok(Some(ty.clone()));
                            }
                            None => {
                                self.dependencies.push(Dependency { file: self.filename.clone(), node_index: expression_index, reason: DependencyReason::Node(*inner_expr) });
                                return Ok(None);
                            }
                        }
                    },
                    
                    Expression::Identifier(ref identifier) => {
                        let identifier_type = self.find_identifier_type(other_files_exports, node.parent_block.unwrap(), identifier.clone());
                        match identifier_type {
                            Some(ty) => {
                                self.add_type(expression_index, ty.clone());
                                return Ok(Some(ty));
                            }
                            None => {
                                self.dependencies.push(Dependency { file: self.filename.clone(), node_index: expression_index, reason: DependencyReason::Identifier(identifier.clone()) });
                                return Ok(None);
                            }
                        }
                    },
                    Expression::UnaryOperation { operator, expr } => {
                        let expr_type = self.type_expression(other_files_exports, *expr)?;
                        match operator {
                            UnaryOperation::Not => {
                                self.add_type(expression_index, Type::Bool);
                                return Ok(Some(Type::Bool));
                            },
                        }
                    },
                    Expression::ArrayIndex { arr, idx } => {
                        let arr_type = self.type_expression(other_files_exports, *arr)?;
                        let idx_type = self.type_expression(other_files_exports, *idx)?;
                        match arr_type {
                            Some(ty) => {
                                match ty {
                                    Type::Array(ref element_type) => {
                                        self.add_type(expression_index, element_type.deref().clone());
                                        match idx_type {
                                            Some(index_ty) => {
                                                match index_ty {
                                                    Type::SignedInt(_) | Type::UnsignedInt(_)=> {
                                                        return Ok(Some(*element_type.clone()))
                                                    },
                                                    _ => {
                                                        return Err(Error {
                                                            filename: self.filename.clone(),
                                                            line: 0,
                                                            col: 0,
                                                            reason: Reason::TypeCheckError(TypeCheckError::ArrayIndexShouldBeEitherUnsignedOrSignedInt(index_ty)),
                                                        });
                                                    }
                                                }
                                            }
                                            None => {
                                                return Ok(None);
                                            }
                                        }
                                    }
                                    _ => {
                                        return Err(Error {
                                            filename: self.filename.clone(),
                                            line: 0,
                                            col: 0,
                                            reason: Reason::TypeCheckError(TypeCheckError::OnlyArraysCanBeIndexed(ty)),
                                        });
                                    }
                                }
                            }
                            None => {
                                self.dependencies.push(Dependency { file: self.filename.clone(), node_index: expression_index, reason: DependencyReason::Node(*arr) });
                                return Ok(None)
                            }                          
                        }
                    },
                    Expression::BinaryOperation { operation, left, right } => {
                        let left_type = self.type_expression(other_files_exports, *left)?;
                        let right_type = self.type_expression(other_files_exports, *right)?;
                        if left_type.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: expression_index, reason: DependencyReason::Node(*left) });
                            return Ok(None);
                        }
                        if right_type.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: expression_index, reason: DependencyReason::Node(*right) });
                            return Ok(None);
                        }
                        match operation {
                            BinaryOperation::Sum | 
                            BinaryOperation::Subtract |
                            BinaryOperation::Divide |
                            BinaryOperation::Modulu |
                            BinaryOperation::Multiply 
                            => {
                                if left_type == right_type {
                                    self.add_type(expression_index, left_type.clone().unwrap());
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
                                if left_type == Some(Type::Bool) && right_type == Some(Type::Bool) {
                                    self.add_type(expression_index, Type::Bool);
                                    return Ok(left_type);
                                } else {
                                    unimplemented!();
                                }
                            }
                        }
                    },
                    Expression::NamespaceAccess { namespace, field: field_id } => {
                        let ns_type = self.type_expression(other_files_exports, *namespace)?;
                        if ns_type.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: expression_index, reason: DependencyReason::Node(*namespace) });
                            return Ok(None);
                        }
                        let field_node = self.nodes.get(field_id).unwrap();

                        // get struct/fileload/enum out of namespace then get field type of it
                        if let NodeData::Expression(Expression::Identifier(ref field)) = field_node.data {
                            let field_type = self.resolve_namespace_access_type(ns_type.unwrap(), field.clone())?;
                            self.add_type(expression_index, field_type.clone());
                            self.add_type(*field_id, field_type.clone());
                            return Ok(Some(field_type));
                        }
                        else {
                            unreachable!();
                        }
                    },
                    //TODO check if fields are valid in context of it's type.
                    Expression::Initialize { ty, fields } => {
                        let initialize_type = self.resolve_type_definition(*ty)?;
                        if initialize_type.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: expression_index, reason: DependencyReason::Node(*ty) });
                            return Ok(None);
                        }
                        let initialize_type = initialize_type.unwrap();
                        self.add_type(expression_index, initialize_type.clone());
                        return Ok(Some(initialize_type));
                    },
                    Expression::InitializeArray { ref elements } => {
                        let mut array_elem_type: Option<Type> = None;
                        for elem in elements {
                            let elem_type = self.type_expression(other_files_exports, *elem)?;
                            if elem_type.is_none() {
                                self.dependencies.push(Dependency { file: self.filename.clone(), node_index: expression_index, reason: DependencyReason::Node(*elem) });
                                return Ok(None);
                            }
                            if array_elem_type.is_none() {
                                array_elem_type = elem_type;
                            } else {
                                if array_elem_type.clone().unwrap() != elem_type.clone().unwrap() {
                                    return Err(Error {
                                        filename: self.filename.clone(),
                                        line: 0,
                                        col: 0,
                                        reason: Reason::TypeCheckError(TypeCheckError::ArrayElementsShouldBeOfSameType(array_elem_type.unwrap(), elem_type.unwrap())),
                                    });
                                }
                            }
                        }

                        return Ok(Some(Type::Array(Box::new(array_elem_type.unwrap()))));
                    },
                    Expression::Function { ref args, ret_ty: ref ret_ty_node, ref body } => {
                        for arg in args {
                            let arg_type = self.type_statement(other_files_exports, *arg)?;
                            if arg_type.is_none() {
                                self.dependencies.push(Dependency { file: self.filename.clone(), node_index: expression_index, reason: DependencyReason::Node(*arg) });
                                return Ok(None);
                            }
                        }

                        let arg_types: Vec<Type> = args.iter().map(|arg_idx| {
                            if let NodeData::Statement(Statement::Decl { name, ty }) = self.nodes.get(arg_idx).unwrap().data {
                                self.nodes.get(&ty).clone().unwrap().type_information.as_ref().unwrap();
                            }
                            unreachable!();
                        }).collect();

                        let ret_type = self.resolve_type_definition(*ret_ty_node)?;
                        if ret_type.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: expression_index, reason: DependencyReason::Node(*ret_ty_node) });
                            return Ok(None);
                        }
                        let body_type = self.type_scope(other_files_exports, *body)?;
                        if body_type.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: expression_index, reason: DependencyReason::Node(*body) });
                            return Ok(None);
                        }
                        let fn_type = Type::FnType(arg_types, Box::new(ret_type.unwrap()));
                        self.add_type(expression_index, fn_type.clone());
                        return Ok(Some(fn_type));
                    },
                    Expression::FunctionCall { ref fn_name, ref args } => {
                        let fn_type = self.type_expression(other_files_exports, *fn_name)?;
                        if fn_type.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: expression_index, reason: DependencyReason::Node(*fn_name) });
                            return Ok(None);
                        }
                        let fn_type = fn_type.unwrap();
                        if let Type::FnType(_, ret) = fn_type {
                            return Ok(Some(ret.deref().clone()));
                        } else {
                            unreachable!()
                        }

                    },
                    Expression::PointerOf(ref pointee) => {
                        let pointee_type = self.type_expression(other_files_exports, *pointee)?;
                        if pointee_type.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: expression_index, reason: DependencyReason::Node(*pointee) });
                            return Ok(None);
                        }
                        let pointee_type = pointee_type.unwrap();
                        self.add_type(expression_index, Type::Pointer(Box::new(pointee_type.clone())));
                        return Ok(Some(Type::Pointer(Box::new(pointee_type.clone()))));
                    },
                    Expression::Deref(ref pointer) => {
                        let pointer_type = self.type_expression(other_files_exports, *pointer)?;
                        if pointer_type.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: expression_index, reason: DependencyReason::Node(*pointer) });
                            return Ok(None);
                        }
                        let pointer_type = pointer_type.unwrap();
                        match pointer_type {
                            Type::Pointer(ref inner) => {
                                self.add_type(expression_index, inner.deref().clone());
                                return Ok(Some(inner.deref().clone()));
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

    
    fn type_statement(&mut self, other_files_exports: &HashMap<String, HashMap<String, Type>>, stmt_index: NodeIndex) -> Result<Option<Type>> {
        let node = self.nodes.get(&stmt_index).unwrap().clone();
        match node.data {
            NodeData::Statement(ref stmt) => {
                match stmt {
                    Statement::Load(_) | Statement::Host(_) => {
                        self.add_type(stmt_index, Type::NoType);
                        return Ok(Some(Type::NoType))
                    },

                    Statement::Def { mutable, ref name, ty: None, ref expr } => {
                        let expr_ty = self.type_expression(other_files_exports, *expr)?;
                        if expr_ty.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: *name, reason: DependencyReason::Node(*expr) });
                            return Ok(None);
                        }
                        let expr_ty = expr_ty.unwrap();
                        self.add_type(*name, expr_ty.clone());
                        self.add_type(stmt_index, Type::NoType);
                        if node.parent_block.is_some() {
                            self.add_symbol_to_scope(node.parent_block.unwrap(), *name, expr_ty.clone());
                            let block =  self.get_node(node.parent_block.unwrap())?;
                            if let NodeData::Statement(Statement::Scope { owner: _, is_file_root, stmts: _ }) = block.data {
                                if is_file_root {
                                    self.add_exported_symbol(*name, expr_ty.clone());
                                }
                            }
                            
                        }
                        return Ok(Some(Type::NoType));
                    }
                    Statement::Def { mutable, ref name, ty: Some(ref ty), ref expr } => {
                        let expr_ty = self.type_expression(other_files_exports, *expr)?;
                        if expr_ty.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: *name, reason: DependencyReason::Node(*expr) });
                            return Ok(None);
                        }
                        let type_annotation_type = self.resolve_type_definition(*ty)?;
                        if type_annotation_type.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: *name, reason: DependencyReason::Node(*ty) });
                            return Ok(None);
                        }
                        let expr_ty = expr_ty.unwrap();
                        let type_annotation_type = type_annotation_type.unwrap();
                        self.add_type(*name, type_annotation_type.clone());
                        self.add_type(stmt_index, Type::NoType);
                        if node.parent_block.is_some() {
                            self.add_symbol_to_scope(node.parent_block.unwrap(), *name, type_annotation_type.clone());
                            if node.parent_block.is_some() {
                                self.add_symbol_to_scope(node.parent_block.unwrap(), *name, expr_ty.clone());
                                let block =  self.get_node(node.parent_block.unwrap())?;
                                if let NodeData::Statement(Statement::Scope { owner: _, is_file_root, stmts: _ }) = block.data {
                                    if is_file_root {
                                        self.add_exported_symbol(*name, expr_ty.clone());
                                    }
                                }
                                
                            }
                        }
                        return Ok(Some(Type::NoType));
                    }
                    

                    Statement::Decl { ref name, ref ty } => {
                        let decl_type = self.resolve_type_definition(*ty)?;
                        if decl_type.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: stmt_index, reason: DependencyReason::Node(*ty) });
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: *name, reason: DependencyReason::Node(*ty) });
                            return Ok(None);
                        }
                        self.add_type(*name, decl_type.clone().unwrap());
                        if node.parent_block.is_some() {
                            self.add_symbol_to_scope(node.parent_block.unwrap(), *name, decl_type.clone().unwrap());
                            if node.parent_block.is_some() {
                                self.add_symbol_to_scope(node.parent_block.unwrap(), *name, decl_type.clone().unwrap());
                                let block =  self.get_node(node.parent_block.unwrap())?;
                                if let NodeData::Statement(Statement::Scope { owner: _, is_file_root, stmts: _ }) = block.data {
                                    if is_file_root {
                                        self.add_exported_symbol(*name, decl_type.clone().unwrap());
                                    }
                                }
                                
                            }
                        }

                        return Ok(Some(decl_type.clone().unwrap()));
                    },

                    Statement::Assign { ref lhs, ref rhs } => {
                        let lhs_type = self.type_expression(other_files_exports, *lhs)?;
                        let rhs_type = self.type_expression(other_files_exports, *rhs)?;
                        if lhs_type.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: stmt_index, reason: DependencyReason::Node(*lhs) });
                            return Ok(None);
                        }
                        if rhs_type.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: stmt_index, reason: DependencyReason::Node(*rhs) });
                            return Ok(None);
                        }

                        return Ok(Some(Type::NoType));
                    },

                    Statement::Scope { ref owner, is_file_root, ref stmts } => {
                        self.type_scope(other_files_exports, node.id) 
                    }

                    Statement::If { ref cases } => {
                        for case in cases {
                            let cond_type = self.type_expression(other_files_exports, case.0)?;
                            if cond_type.is_none() {
                                self.dependencies.push(Dependency { file: self.filename.clone(), node_index: stmt_index, reason: DependencyReason::Node(case.0) });
                                return Ok(None);
                            }
                            let cond_type = cond_type.unwrap();
                            let scope_type = self.type_scope(other_files_exports, case.1)?;
                            if scope_type.is_none() {
                                self.dependencies.push(Dependency { file: self.filename.clone(), node_index: stmt_index, reason: DependencyReason::Node(case.1) });
                                return Ok(None);
                            }
                        }

                        return Ok(Some(Type::NoType));
                    },

                    Statement::For { ref start, ref cond, ref cont, ref body } => {
                        let start_type = self.type_statement(other_files_exports, *start)?;
                        if start_type.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: stmt_index, reason: DependencyReason::Node(*start) });
                            return Ok(None);
                        }
                        let cond_type = self.type_expression(other_files_exports, *cond)?;
                        if cond_type.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: stmt_index, reason: DependencyReason::Node(*cond) });
                            return Ok(None);
                        }
                        let cont_type = self.type_statement(other_files_exports, *cont)?;
                        if cont_type.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: stmt_index, reason: DependencyReason::Node(*cont) });
                            return Ok(None);
                        }
                        let body_type = self.type_statement(other_files_exports, *body)?;
                        if body_type.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: stmt_index, reason: DependencyReason::Node(*body) });
                            return Ok(None);
                        }

                        return Ok(Some(Type::NoType));
                    },
                    
                    Statement::ForIn { ref iterator, ref iterable, ref body } => {
                        let iterator_type = self.type_expression(other_files_exports, *iterator)?;
                        if iterator_type.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: stmt_index, reason: DependencyReason::Node(*iterator) });
                            return Ok(None);
                        }

                        let iterable_type = self.type_expression(other_files_exports,*iterable)?;
                        if iterable_type.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: stmt_index, reason: DependencyReason::Node(*iterable) });
                            return Ok(None);
                        }
                        let body_type = self.type_statement(other_files_exports,*body)?;
                        if body_type.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: stmt_index, reason: DependencyReason::Node(*body) });
                            return Ok(None);
                        }
                        return Ok(Some(Type::NoType));
                    },
                    
                    Statement::While { ref cond, ref body } => {
                        let cond_type = self.type_expression(other_files_exports,*cond)?;
                        if cond_type.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: stmt_index, reason: DependencyReason::Node(*cond) });
                            return Ok(None);
                        }
                        let body_type = self.type_statement(other_files_exports,*body)?;
                        if body_type.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: stmt_index, reason: DependencyReason::Node(*body) });
                            return Ok(None);
                        }
                        return Ok(Some(Type::NoType));
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
                        let expr_type = self.type_expression(other_files_exports,*expr)?;
                        if expr_type.is_none() {
                            self.dependencies.push(Dependency { file: self.filename.clone(), node_index: stmt_index, reason: DependencyReason::Node(*expr) });
                            return Ok(None);
                        }
                        return Ok(Some(Type::NoType));
                    },
                }
            },
            NodeData::TypeDefinition(_) => {
                unreachable!()
            },
            NodeData::Expression(_) => {
                unreachable!()
            },
        }

    }
    pub fn type_root(&mut self, other_files_exports: &HashMap<String, HashMap<String, Type>>) -> Result<()> {
        self.dependencies = vec![];
        self.type_scope(other_files_exports, self.root)?;
        Ok(())
    }
    fn type_scope(&mut self, other_files_exports: &HashMap<String, HashMap<String, Type>>, scope_index: NodeIndex) -> Result<Option<Type>> {
        let scope = self.nodes.get(&scope_index).unwrap().clone();
        if let NodeData::Statement(Statement::Scope { owner, is_file_root, ref stmts }) = scope.data {
            for stmt in stmts {
                let ty = self.type_statement(other_files_exports, *stmt)?;
                if ty.is_none() {
                    self.dependencies.push(Dependency { file: self.filename.clone(), node_index: scope_index, reason: DependencyReason::Node(*stmt) });
                    return Ok(None);
                }
            }
            self.add_type(scope_index, Type::NoType);
        }
        Ok(Some(Type::NoType))
    }
}