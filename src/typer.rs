use std::collections::HashMap;
use std::hash::Hash;
use std::ops::Deref;

use crate::compliation::Dependency;
use crate::errors::*;
use crate::ir::TypeDefinition;
use crate::utils;
use serde::Serialize;

use super::errors::CompilerError;
use super::errors::TypeCheckError;
use super::ir;
use super::ir::AstTag;
use super::ir::BinaryOperation;
use super::ir::Expression;
use super::ir::Node;
use super::ir::NodeData;
use super::ir::NodeIndex;
use super::ir::Statement;
use super::ir::UnaryOperation;
use super::ir::IR;
use crate::lexer::Token;

type BitSize = i64;
#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum Type {
    NoType,

    Type(Box<Type>),
    SignedInt(BitSize),
    UnsignedInt(BitSize),
    Float(BitSize),

    Bool,

    CIntPtr,
    CUintPtr,
    CVarArgs,
    CString,

    Char,
    String,

    Array(u64, Box<Type>),

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

    FnType(Vec<(String, Type)>, Box<Type>),
    Void,

    SizeOfCall,
    CastCall,
    Label,
}

impl Type {
    pub fn is_pointer(&self) -> bool {
        match self {
            Type::Pointer(_) => true,
            _ => false,
        }
    }
    pub fn is_function(&self) -> bool {
        match self {
            Type::FnType(_, _) => true,
            _ => false,
        }
    }
}

impl Type {
    pub fn get_kind(&self) -> TypeKind {
        match self {
            Type::Type(_) => TypeKind::Type,
            Type::SignedInt(_) => TypeKind::Int,
            Type::UnsignedInt(_) => TypeKind::Int,
            Type::Float(_) => TypeKind::Float,
            Type::Bool => TypeKind::Bool,
            Type::CIntPtr => TypeKind::Int,
            Type::CUintPtr => TypeKind::Int,
            Type::CString => TypeKind::String,
            Type::Char => TypeKind::Char,
            Type::String => TypeKind::String,
            Type::Array(_, _) => TypeKind::Array,
            Type::Struct { fields } => TypeKind::Type,
            Type::Enum { variants } => TypeKind::Type,
            Type::TypeRef { name, actual_ty } => TypeKind::Type,
            Type::Pointer(_) => TypeKind::Pointer,
            Type::FnType(_, _) => TypeKind::Function,
            Type::Void => TypeKind::Void,
            _ => TypeKind::Void,
        }
    }
    pub fn get_array_elem_ty(&self) -> Type {
        match self {
            Type::Type(ty) => return ty.get_array_elem_ty(),
            Type::Array(_, elem_ty) => return *elem_ty.clone(),
            Type::String => Type::Char,
            _ => unreachable!(),
        }
    }
    pub fn is_type_definition(&self) -> bool {
        match self {
            Type::Type(ref t) => {
                if let Type::Struct { fields } = t.deref() {
                    return true;
                } else if let Type::Enum { variants: _ } = t.deref() {
                    return true;
                } else {
                    return t.is_type_definition();
                }
            }
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Serialize)]
enum TypeKind {
    Int,
    Void,
    Type,
    Function,
    Array,
    Float,
    String,
    Char,
    Bool,
    Pointer,
}

impl IR {
    pub fn two_sides_can_do_binary_operation(&self, left: NodeIndex, right: NodeIndex) -> bool {
        let left_node = self.get_node(left).unwrap();
        let right_node = self.get_node(right).unwrap();
        if left_node.type_information.clone().unwrap()
            == right_node.type_information.clone().unwrap()
        {
            return true;
        }
        if (left_node.is_literal() || right_node.is_literal())
            && (left_node.type_information.clone().unwrap().get_kind()
                == right_node.type_information.clone().unwrap().get_kind())
        {
            return true;
        }

        return false;
    }
    fn add_type(&mut self, index: NodeIndex, ty: Type) {
        let node = self.nodes.get_mut(&index).unwrap();
        node.type_information = Some(ty);
    }
    fn get_type(&self, ty: Type) -> Type {
        match ty {
            Type::Type(actual) => return actual.deref().clone(),
            _ => return ty,
        }
    }
    fn find_identifier_type(
        &mut self,
        other_files_exports: &HashMap<String, HashMap<String, Type>>,
        mut scope: NodeIndex,
        identifier_node: NodeIndex,
    ) -> Result<Option<Type>> {
        let identifier = self.nodes.get(&identifier_node).unwrap();
        let identifier = identifier.get_identifier()?;
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
                    if ty.is_type_definition() {
                        let actual = self.get_type(ty.clone());
                        return Ok(Some(Type::TypeRef {
                            name: identifier.clone(),
                            actual_ty: Box::new(actual),
                        }));
                    }
                    return Ok(Some(self.get_type(ty.clone())));
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
                        if ty.clone().unwrap().is_type_definition() {
                            let actual = self.get_type(ty.unwrap().clone());
                            return Ok(Some(Type::TypeRef {
                                name: identifier.clone(),
                                actual_ty: Box::new(actual),
                            }));
                        }
                        return Ok(Some(self.get_type(ty.unwrap().clone())));
                    }
                }
                None => continue,
            }
        }

        self.dependencies.push(Dependency {
            file: self.filename.clone(),
            node_index: identifier_node,
            needs: identifier,
        });
        return Ok(None);
    }

    fn resolve_namespace_access_type(
        &self,
        node_index: NodeIndex,
        ns_type: Type,
        field: String,
    ) -> Result<Type> {
        let node = self.get_node(node_index).unwrap();
        match &ns_type {
            Type::Struct { ref fields } => {
                for sf in fields {
                    if sf.0 == field {
                        return Ok(sf.1.clone());
                    }
                }
                return Err(CompilerError {
                    file_source: self.file_source.clone(),
                    filename: self.filename.clone(),
                    line: node.line,
                    col: node.col,
                    reason: Reason::TypeCheckError(TypeCheckError::StructDoesNotHaveField(
                        ns_type, field,
                    )),
                });
            }
            Type::Enum { ref variants } => {
                for variant in variants {
                    if variant == &field {
                        return Ok(Type::UnsignedInt(64));
                    }
                }
                return Err(CompilerError {
                    file_source: self.file_source.clone(),
                    filename: self.filename.clone(),
                    line: node.line,
                    col: node.col,
                    reason: Reason::TypeCheckError(TypeCheckError::EnumDoesNotHaveVariant(
                        ns_type, field,
                    )),
                });
            }
            Type::Pointer(ref actual_ty) => {
                return self.resolve_namespace_access_type(
                    node_index,
                    actual_ty.deref().clone(),
                    field,
                );
            }
            Type::TypeRef { name, actual_ty } => {
                return self.resolve_namespace_access_type(
                    node_index,
                    actual_ty.deref().clone(),
                    field,
                );
            }
            _ => {
                return Err(CompilerError {
                    file_source: self.file_source.clone(),
                    filename: self.filename.clone(),
                    line: node.line,
                    col: node.col,
                    reason: Reason::TypeCheckError(TypeCheckError::InvalidNamespace(ns_type)),
                })
            }
        }
    }
    fn type_expression(
        &mut self,
        other_files_exports: &HashMap<String, HashMap<String, Type>>,
        expression_index: NodeIndex,
    ) -> Result<Option<Type>> {
        let node = self.nodes.get(&expression_index).unwrap().clone();
        match node.data {
            NodeData::Expression(ref expr) => {
                match expr {
                    // TODO: check the actual numbers and find smallest possible type for it.
                    Expression::Unsigned(_) => {
                        self.add_type(expression_index, Type::UnsignedInt(64));

                        return Ok(Some(Type::UnsignedInt(64)));
                    }

                    Expression::Signed(_) => {
                        self.add_type(expression_index, Type::SignedInt(64));
                        return Ok(Some(Type::SignedInt(64)));
                    }
                    Expression::StringLiteral(ref s) => {
                        self.add_type(expression_index, Type::String);
                        return Ok(Some(Type::String));
                    }
                    Expression::Float(_) => {
                        self.add_type(expression_index, Type::Float(64));
                        return Ok(Some(Type::Float(64)));
                    }
                    Expression::Bool(_) => {
                        self.add_type(expression_index, Type::Bool);
                        return Ok(Some(Type::Bool));
                    }
                    Expression::Char(_) => {
                        self.add_type(expression_index, Type::Char);
                        return Ok(Some(Type::Char));
                    }

                    Expression::Paren(ref inner_expr) => {
                        let inner_type = self.type_expression(other_files_exports, *inner_expr)?;
                        match inner_type {
                            Some(ty) => {
                                self.add_type(expression_index, ty.clone());
                                return Ok(Some(ty.clone()));
                            }
                            None => {
                                return Ok(None);
                            }
                        }
                    }

                    Expression::Identifier(ref identifier) => {
                        let identifier_type = self.find_identifier_type(
                            other_files_exports,
                            node.parent_block.unwrap(),
                            expression_index,
                        )?;
                        match identifier_type {
                            Some(ty) => {
                                self.add_type(expression_index, ty.clone());
                                return Ok(Some(ty));
                            }
                            None => {
                                return Ok(None);
                            }
                        }
                    }
                    Expression::UnaryOperation { operator, expr } => {
                        let expr_type = self.type_expression(other_files_exports, *expr)?;
                        match operator {
                            UnaryOperation::Not => {
                                self.add_type(expression_index, Type::Bool);
                                return Ok(Some(Type::Bool));
                            }
                        }
                    }
                    Expression::ArrayIndex { arr, idx } => {
                        let arr_type = self.type_expression(other_files_exports, *arr)?;
                        let idx_type = self.type_expression(other_files_exports, *idx)?;
                        match arr_type {
                            Some(ty) => match ty {
                                Type::Array(_, ref element_type) => {
                                    self.add_type(expression_index, element_type.deref().clone());
                                    match idx_type {
                                        Some(index_ty) => match index_ty {
                                            Type::SignedInt(_) | Type::UnsignedInt(_) => {
                                                return Ok(Some(*element_type.clone()))
                                            }
                                            _ => {
                                                let index_node = self.nodes.get(idx).unwrap();
                                                return Err(CompilerError {
                                                            file_source: self.file_source.clone(),
                                                            filename: self.filename.clone(),
                                                            line: index_node.line,
                                                            col: index_node.col,
                                                            reason: Reason::TypeCheckError(TypeCheckError::ArrayIndexShouldBeEitherUnsignedOrSignedInt(index_ty)),
                                                        });
                                            }
                                        },
                                        None => {
                                            return Ok(None);
                                        }
                                    }
                                }
                                Type::String => {
                                    self.add_type(expression_index, Type::Char);
                                    return Ok(Some(Type::Char));
                                }
                                _ => {
                                    return Err(CompilerError {
                                        file_source: self.file_source.clone(),
                                        filename: self.filename.clone(),
                                        line: node.line,
                                        col: node.col,
                                        reason: Reason::TypeCheckError(
                                            TypeCheckError::OnlyArraysCanBeIndexed(ty),
                                        ),
                                    });
                                }
                            },
                            None => return Ok(None),
                        }
                    }
                    Expression::BinaryOperation {
                        operation,
                        left,
                        right,
                    } => {
                        let left_type = self.type_expression(other_files_exports, *left)?;
                        let right_type = self.type_expression(other_files_exports, *right)?;
                        if left_type.is_none() {
                            return Ok(None);
                        }
                        if right_type.is_none() {
                            return Ok(None);
                        }
                        match operation {
                            BinaryOperation::Sum
                            | BinaryOperation::Subtract
                            | BinaryOperation::Divide
                            | BinaryOperation::Modulu
                            | BinaryOperation::Multiply => {
                                if self.two_sides_can_do_binary_operation(*left, *right) {
                                    self.add_type(expression_index, left_type.clone().unwrap());
                                    return Ok(left_type);
                                } else {
                                    return Err(CompilerError {
                                        file_source: self.file_source.clone(),
                                        filename: self.filename.clone(),
                                        line: node.line,
                                        col: node.col,
                                        reason: Reason::TypeCheckError(TypeCheckError::TwoSidesOfABinaryOperatorShouldBeSameType(left_type.unwrap(), right_type.unwrap())),
                                    });
                                }
                            }
                            BinaryOperation::Greater
                            | BinaryOperation::GreaterEqual
                            | BinaryOperation::Less
                            | BinaryOperation::LessEqual
                            | BinaryOperation::Equal
                            | BinaryOperation::NotEqual => {
                                if self.two_sides_can_do_binary_operation(*left, *right) {
                                    self.add_type(expression_index, Type::Bool);
                                    return Ok(Some(Type::Bool));
                                } else {
                                    return Err(CompilerError {
                                        file_source: self.file_source.clone(),
                                        filename: self.filename.clone(),
                                        line: node.line,
                                        col: node.col,
                                        reason: Reason::TypeCheckError(TypeCheckError::TwoSidesOfABinaryOperatorShouldBeSameType(left_type.unwrap(), right_type.unwrap())),
                                    });
                                }
                            }
                            BinaryOperation::BinaryAnd | BinaryOperation::BinaryOr => {
                                if (left_type == Some(Type::Bool) && right_type == Some(Type::Bool))
                                    || (left_type == Some(Type::Type(Box::new(Type::Bool)))
                                        && right_type == Some(Type::Type(Box::new(Type::Bool))))
                                {
                                    self.add_type(expression_index, Type::Bool);
                                    return Ok(Some(Type::Bool));
                                } else {
                                    panic!("two sides of && || should be boolean. left {:?} right {:?}", left_type, right_type);
                                }
                            }
                            BinaryOperation::BitwiseOr => todo!(),
                            BinaryOperation::BitwiseAnd => todo!(),
                            BinaryOperation::BitwiseXor => todo!(),
                        }
                    }
                    Expression::NamespaceAccess {
                        namespace,
                        field: field_id,
                    } => {
                        let ns_type = self.type_expression(other_files_exports, *namespace)?;
                        if ns_type.is_none() {
                            return Ok(None);
                        }
                        let field_node = self.nodes.get(field_id).unwrap();

                        // get struct/fileload/enum out of namespace then get field type of it
                        if let NodeData::Expression(Expression::Identifier(ref field)) =
                            field_node.data
                        {
                            let field_type = self.resolve_namespace_access_type(
                                expression_index,
                                ns_type.unwrap(),
                                field.clone(),
                            )?;
                            self.add_type(expression_index, field_type.clone());
                            self.add_type(*field_id, field_type.clone());
                            return Ok(Some(field_type));
                        } else {
                            unreachable!();
                        }
                    }
                    Expression::Initialize { ty, ref fields } => {
                        let initialize_type = self.type_expression(other_files_exports, *ty)?;
                        if initialize_type.is_none() {
                            return Ok(None);
                        }
                        let type_node = self.get_node(*ty).unwrap();

                        let initialize_type = initialize_type.unwrap();
                        //TODO check if fields are valid in context of it's type.
                        for (name, value) in fields {
                            let value_type = self.type_expression(other_files_exports, *value)?;
                            if value_type.is_none() {
                                return Ok(None);
                            }
                            self.add_type(*name, value_type.unwrap());
                        }

                        self.add_type(expression_index, initialize_type.clone());
                        return Ok(Some(initialize_type.clone()));
                    }
                    Expression::InitializeArray { ty, ref elements } => {
                        let array_type = self.get_node(*ty)?;
                        if let NodeData::TypeDefinition(TypeDefinition::Array { length, elem_ty }) =
                            array_type.data
                        {
                            let len_type = self.type_expression(other_files_exports, length)?;
                            if len_type.is_none() {
                                return Ok(None);
                            }
                            let elem_type = self.type_expression(other_files_exports, elem_ty)?;
                            if elem_type.is_none() {
                                return Ok(None);
                            }
                            let mut array_elem_type: Option<Type> = None;
                            for elem in elements {
                                let this_elem_type =
                                    self.type_expression(other_files_exports, *elem)?;
                                if this_elem_type.is_none() {
                                    return Ok(None);
                                }
                                if array_elem_type.is_none() {
                                    array_elem_type = this_elem_type;
                                } else {
                                    if array_elem_type.clone().unwrap()
                                        != this_elem_type.clone().unwrap()
                                    {
                                        let this_elem_node = self.nodes.get(elem).unwrap();
                                        return Err(CompilerError {
                                            file_source: self.file_source.clone(),
                                            filename: self.filename.clone(),
                                            line: this_elem_node.line,
                                            col: this_elem_node.col,
                                            reason: Reason::TypeCheckError(
                                                TypeCheckError::ArrayElementsShouldBeOfSameType(
                                                    array_elem_type.unwrap(),
                                                    this_elem_type.unwrap(),
                                                ),
                                            ),
                                        });
                                    }
                                }
                            }
                            self.add_type(
                                *ty,
                                Type::Array(
                                    elements.len() as u64,
                                    Box::new(array_elem_type.clone().unwrap()),
                                ),
                            );
                            self.add_type(
                                expression_index,
                                Type::Array(
                                    elements.len() as u64,
                                    Box::new(array_elem_type.clone().unwrap()),
                                ),
                            );
                            return Ok(Some(Type::Array(
                                elements.len() as u64,
                                Box::new(array_elem_type.unwrap()),
                            )));
                        } else {
                            unreachable!()
                        }
                    }
                    Expression::Function {
                        ref args,
                        ret_ty: ref ret_ty_node,
                        ref body,
                    } => {
                        for arg in args {
                            let arg_type = self.type_statement(other_files_exports, *arg)?;
                            if arg_type.is_none() {
                                return Ok(None);
                            }
                        }

                        let arg_types: Vec<(String, Type)> = args
                            .iter()
                            .map(|arg_idx| {
                                if let NodeData::Statement(Statement::Decl { name, ty }) =
                                    self.nodes.get(arg_idx).unwrap().data
                                {
                                    return (
                                        self.nodes.get(&name).unwrap().get_identifier().unwrap(),
                                        self.nodes
                                            .get(&ty)
                                            .clone()
                                            .unwrap()
                                            .type_information
                                            .as_ref()
                                            .unwrap()
                                            .to_owned(),
                                    );
                                }
                                unreachable!()
                            })
                            .collect();

                        let ret_type = self.type_type(other_files_exports, *ret_ty_node)?;
                        if ret_type.is_none() {
                            return Ok(None);
                        }
                        let body_type = self.type_scope(other_files_exports, *body)?;
                        if body_type.is_none() {
                            return Ok(None);
                        }
                        let fn_type = Type::FnType(arg_types, Box::new(ret_type.unwrap()));
                        self.add_type(expression_index, fn_type.clone());
                        return Ok(Some(fn_type));
                    }
                    Expression::FunctionCall {
                        ref fn_name,
                        ref args,
                    } => {
                        let fn_name_node = self.get_node(fn_name.clone()).unwrap();

                        let fn_type = self.type_expression(other_files_exports, *fn_name)?;
                        if fn_type.is_none() {
                            return Ok(None);
                        }
                        for arg in args {
                            let arg_type = self.type_expression(other_files_exports, *arg)?;
                            if arg_type.is_none() {
                                return Ok(None);
                            }
                        }
                        let fn_type = fn_type.unwrap();
                        if let Type::FnType(_, ret) = fn_type {
                            self.add_type(expression_index, ret.deref().clone());
                            return Ok(Some(ret.deref().clone()));
                        } else {
                            unreachable!()
                        }
                    }
                    Expression::PointerOf(ref pointee) => {
                        let pointee_type = self.type_expression(other_files_exports, *pointee)?;
                        if pointee_type.is_none() {
                            return Ok(None);
                        }
                        let pointee_type = pointee_type.unwrap();
                        self.add_type(
                            expression_index,
                            Type::Pointer(Box::new(pointee_type.clone())),
                        );
                        return Ok(Some(Type::Pointer(Box::new(pointee_type.clone()))));
                    }
                    Expression::Deref(ref pointer) => {
                        let pointer_type = self.type_expression(other_files_exports, *pointer)?;
                        if pointer_type.is_none() {
                            return Ok(None);
                        }
                        let pointer_type = pointer_type.unwrap();
                        match pointer_type {
                            Type::Pointer(ref inner) => {
                                self.add_type(expression_index, inner.deref().clone());
                                return Ok(Some(inner.deref().clone()));
                            }
                            _ => {
                                panic!("deref type should be a pointer: {:?}", pointer_type);
                            }
                        }
                    }
                    Expression::Cast(cast_expr, cast_ty) => {
                        let cast_expr_type =
                            self.type_expression(other_files_exports, *cast_expr)?;
                        if cast_expr_type.is_none() {
                            return Ok(None);
                        }
                        let cast_type_type = self.type_expression(other_files_exports, *cast_ty)?;
                        if cast_type_type.is_none() {
                            return Ok(None);
                        }
                        match cast_type_type.clone().unwrap() {
                            Type::Type(actual) => {
                                self.add_type(expression_index, *actual.clone());
                                return Ok(Some(*actual.clone()));
                            }
                            _ => {
                                panic!("cast second argument should be a type {:?}", cast_type_type)
                            }
                        }
                    }
                    Expression::SizeOf(sizeof_expr) => {
                        let sizof_expr_type =
                            self.type_expression(other_files_exports, *sizeof_expr)?;
                        if sizof_expr_type.is_none() {
                            return Ok(None);
                        }
                        self.add_type(expression_index, Type::SignedInt(64));
                        return Ok(Some(Type::SignedInt(64)));
                    }
                }
            }
            NodeData::Statement(_) => panic!("unexpected when typing an expression {:?}", node),
            NodeData::TypeDefinition(ref td) => {
                match td {
                    TypeDefinition::Int(ref size) => {
                        let ty = Type::Type(Box::new(Type::SignedInt(*size)));
                        self.add_type(expression_index, ty.clone());
                        Ok(Some(ty))
                    }
                    TypeDefinition::Uint(size) => {
                        self.add_type(
                            expression_index,
                            Type::Type(Box::new(Type::UnsignedInt(*size))),
                        );
                        Ok(Some(Type::Type(Box::new(Type::UnsignedInt(*size)))))
                    }
                    TypeDefinition::Float(size) => {
                        self.add_type(expression_index, Type::Type(Box::new(Type::Float(*size))));
                        Ok(Some(Type::Type(Box::new(Type::Float(*size)))))
                    }
                    TypeDefinition::Bool => {
                        self.add_type(expression_index, Type::Type(Box::new(Type::Bool)));
                        Ok(Some(Type::Type(Box::new(Type::Bool))))
                    }
                    TypeDefinition::String => {
                        self.add_type(expression_index, Type::Type(Box::new(Type::String)));
                        Ok(Some(Type::Type(Box::new(Type::String))))
                    }
                    TypeDefinition::Char => {
                        self.add_type(expression_index, Type::Type(Box::new(Type::Char)));
                        Ok(Some(Type::Type(Box::new(Type::Char))))
                    }
                    TypeDefinition::Void => {
                        self.add_type(expression_index, Type::Type(Box::new(Type::Void)));
                        Ok(Some(Type::Type(Box::new(Type::Void))))
                    }
                    TypeDefinition::CVarArgs => {
                        self.add_type(expression_index, Type::Type(Box::new(Type::CVarArgs)));
                        Ok(Some(Type::Type(Box::new(Type::CVarArgs))))
                    }
                    TypeDefinition::CString => {
                        self.add_type(expression_index, Type::Type(Box::new(Type::CString)));
                        Ok(Some(Type::Type(Box::new(Type::CString))))
                    }
                    TypeDefinition::IntPtr => {
                        self.add_type(expression_index, Type::Type(Box::new(Type::CIntPtr)));
                        Ok(Some(Type::Type(Box::new(Type::CIntPtr))))
                    }
                    TypeDefinition::UintPtr => {
                        self.add_type(expression_index, Type::Type(Box::new(Type::CUintPtr)));
                        Ok(Some(Type::Type(Box::new(Type::CUintPtr))))
                    }
                    TypeDefinition::Pointer(ref pointee) => {
                        let pointee_node = self.get_node(*pointee).unwrap();
                        let pointee_type = self.type_expression(other_files_exports, *pointee)?;
                        if pointee_type.is_none() {
                            return Ok(None);
                        }
                        self.add_type(
                            expression_index,
                            Type::Pointer(Box::new(pointee_type.clone().unwrap())),
                        );
                        Ok(Some(Type::Type(Box::new(Type::Pointer(Box::new(
                            pointee_type.clone().unwrap(),
                        ))))))
                    }
                    TypeDefinition::Array { length, elem_ty } => {
                        // TODO: if we can constantly know the lenght we need to store it in type for maybe later usage.
                        let length_type = self.type_expression(other_files_exports, *length)?;
                        if length_type.is_none() {
                            return Ok(None);
                        }
                        let elem_type = self.type_expression(other_files_exports, *elem_ty)?;
                        if length_type.is_none() {
                            return Ok(None);
                        }
                        self.add_type(
                            expression_index,
                            Type::Array(10, Box::new(elem_type.clone().unwrap())),
                        );
                        Ok(Some(Type::Type(Box::new(Type::Array(
                            10,
                            Box::new(elem_type.clone().unwrap()),
                        )))))
                    }
                    TypeDefinition::Function { args, ret } => {
                        let mut arg_types: Vec<(String, Type)> = vec![];
                        for arg in args {
                            let decl = self.get_node(*arg).unwrap();
                            let decl_type = self.type_statement(other_files_exports, *arg)?;
                            if decl_type.is_none() {
                                return Ok(None);
                            }
                            if let NodeData::Statement(Statement::Decl { name, ty }) = decl.data {
                                arg_types.push((
                                    self.nodes.get(&name).unwrap().get_identifier().unwrap(),
                                    decl_type.unwrap(),
                                ))
                            }
                        }
                        let ret_type = self.type_expression(other_files_exports, *ret)?;
                        if ret_type.is_none() {
                            return Ok(None);
                        }
                        self.add_type(
                            expression_index,
                            Type::FnType(arg_types.clone(), Box::new(ret_type.clone().unwrap())),
                        );
                        Ok(Some(Type::Type(Box::new(Type::FnType(
                            arg_types.clone(),
                            Box::new(ret_type.clone().unwrap()),
                        )))))
                    }
                    TypeDefinition::Struct(ref decls) => {
                        let mut fields: Vec<(String, Type)> = vec![];
                        for decl in decls {
                            let decl_type = self.type_statement(other_files_exports, *decl)?;
                            if decl_type.is_none() {
                                return Ok(None);
                            }
                            if let (NodeData::Statement(Statement::Decl { name, ty: _ })) =
                                self.get_node(*decl).unwrap().data
                            {
                                let name_node = self.get_node(name)?;
                                if let NodeData::Expression(Expression::Identifier(ident)) =
                                    name_node.data
                                {
                                    fields.push((ident, decl_type.unwrap()))
                                } else {
                                    unreachable!();
                                }
                            } else {
                                unreachable!();
                            }
                        }
                        self.add_type(
                            expression_index,
                            Type::Type(Box::new(Type::Struct {
                                fields: fields.clone(),
                            })),
                        );
                        Ok(Some(Type::Type(Box::new(Type::Struct { fields }))))
                    }
                    TypeDefinition::Enum(ref decls) => {
                        let mut variants: Vec<String> = vec![];
                        for decl in decls {
                            let decl_type = self.type_statement(other_files_exports, *decl)?;
                            if decl_type.is_none() {
                                return Ok(None);
                            }
                            if let (NodeData::Statement(Statement::Decl { name, ty: _ })) =
                                self.get_node(*decl).unwrap().data
                            {
                                let name_node = self.get_node(name)?;
                                if let NodeData::Expression(Expression::Identifier(ident)) =
                                    name_node.data
                                {
                                    variants.push((ident));
                                } else {
                                    unreachable!();
                                }
                            } else {
                                unreachable!();
                            }
                        }
                        self.add_type(
                            expression_index,
                            Type::Type(Box::new(Type::Enum {
                                variants: variants.clone(),
                            })),
                        );
                        Ok(Some(Type::Type(Box::new(Type::Enum { variants }))))
                    }
                }
            }
        }
    }

    fn type_type(
        &mut self,
        other_files_exports: &HashMap<String, HashMap<String, Type>>,
        type_index: NodeIndex,
    ) -> Result<Option<Type>> {
        let type_node = self.get_node(type_index).unwrap();
        match type_node.data {
            NodeData::Expression(Expression::Identifier(type_name)) => {
                let actual_type = self.find_identifier_type(
                    other_files_exports,
                    type_node.parent_block.unwrap(),
                    type_node.id,
                )?;
                if actual_type.is_none() {
                    return Ok(None);
                }
                self.add_type(type_index, actual_type.clone().unwrap());
                Ok(Some(actual_type.unwrap()))
            }
            NodeData::TypeDefinition(td) => {
                match td {
                    TypeDefinition::Int(ref size) => {
                        let ty = Type::SignedInt(*size);
                        self.add_type(type_index, Type::SignedInt(*size));
                        Ok(Some(ty))
                    }
                    TypeDefinition::Uint(ref size) => {
                        self.add_type(type_index, Type::UnsignedInt(*size));
                        Ok(Some(Type::UnsignedInt(*size)))
                    }
                    TypeDefinition::Float(ref size) => {
                        self.add_type(type_index, Type::Float(*size));
                        Ok(Some(Type::Float(*size)))
                    }
                    TypeDefinition::Bool => {
                        self.add_type(type_index, Type::Bool);
                        Ok(Some(Type::Bool))
                    }
                    TypeDefinition::String => {
                        self.add_type(type_index, Type::String);
                        Ok(Some(Type::String))
                    }
                    TypeDefinition::Char => {
                        self.add_type(type_index, Type::Char);
                        Ok(Some(Type::Char))
                    }
                    TypeDefinition::Void => {
                        self.add_type(type_index, Type::Void);
                        Ok(Some(Type::Void))
                    }
                    TypeDefinition::CVarArgs => {
                        self.add_type(type_index, Type::CVarArgs);
                        Ok(Some(Type::CVarArgs))
                    }
                    TypeDefinition::CString => {
                        self.add_type(type_index, Type::CString);
                        Ok(Some(Type::CString))
                    }
                    TypeDefinition::IntPtr => {
                        self.add_type(type_index, Type::CIntPtr);
                        Ok(Some(Type::CIntPtr))
                    }
                    TypeDefinition::UintPtr => {
                        self.add_type(type_index, Type::CUintPtr);
                        Ok(Some(Type::CUintPtr))
                    }
                    TypeDefinition::Pointer(ref pointee) => {
                        let pointee_node = self.get_node(*pointee).unwrap();
                        let pointee_type = self.type_type(other_files_exports, *pointee)?;
                        if pointee_type.is_none() {
                            return Ok(None);
                        }
                        self.add_type(
                            type_index,
                            Type::Pointer(Box::new(pointee_type.clone().unwrap())),
                        );
                        Ok(Some(Type::Pointer(Box::new(pointee_type.clone().unwrap()))))
                    }
                    TypeDefinition::Array {
                        ref length,
                        ref elem_ty,
                    } => {
                        // TODO: if we can constantly know the lenght we need to store it in type for maybe later usage.
                        let length_type = self.type_expression(other_files_exports, *length)?;
                        if length_type.is_none() {
                            return Ok(None);
                        }
                        let elem_type = self.type_expression(other_files_exports, *elem_ty)?;
                        if length_type.is_none() {
                            return Ok(None);
                        }
                        self.add_type(
                            type_index,
                            Type::Array(10, Box::new(elem_type.clone().unwrap())),
                        );
                        Ok(Some(Type::Array(10, Box::new(elem_type.clone().unwrap()))))
                    }
                    TypeDefinition::Function { ref args, ref ret } => {
                        let mut arg_types: Vec<(String, Type)> = vec![];
                        for arg in args {
                            let decl = self.get_node(*arg).unwrap();
                            let decl_type = self.type_statement(other_files_exports, *arg)?;
                            if decl_type.is_none() {
                                return Ok(None);
                            }
                            if let NodeData::Statement(Statement::Decl { name, ty }) = decl.data {
                                arg_types.push((
                                    self.nodes.get(&name).unwrap().get_identifier().unwrap(),
                                    decl_type.unwrap(),
                                ))
                            }
                        }
                        let ret_type = self.type_type(other_files_exports, *ret)?;
                        if ret_type.is_none() {
                            return Ok(None);
                        }
                        self.add_type(
                            type_index,
                            Type::FnType(arg_types.clone(), Box::new(ret_type.clone().unwrap())),
                        );
                        Ok(Some(Type::FnType(
                            arg_types.clone(),
                            Box::new(ret_type.clone().unwrap()),
                        )))
                    }
                    TypeDefinition::Struct(ref decls) => {
                        let mut fields: Vec<(String, Type)> = vec![];
                        for decl in decls {
                            let decl_type = self.type_statement(other_files_exports, *decl)?;
                            if decl_type.is_none() {
                                return Ok(None);
                            }
                            if let (NodeData::Statement(Statement::Decl { name, ty: _ })) =
                                self.get_node(*decl).unwrap().data
                            {
                                let name_node = self.get_node(name)?;
                                if let NodeData::Expression(Expression::Identifier(ident)) =
                                    name_node.data
                                {
                                    fields.push((ident, decl_type.unwrap()))
                                } else {
                                    unreachable!();
                                }
                            } else {
                                unreachable!();
                            }
                        }
                        self.add_type(
                            type_index,
                            Type::Struct {
                                fields: fields.clone(),
                            },
                        );
                        Ok(Some(Type::Struct { fields }))
                    }
                    TypeDefinition::Enum(ref decls) => {
                        let mut variants: Vec<String> = vec![];
                        for decl in decls {
                            let decl_type = self.type_statement(other_files_exports, *decl)?;
                            if decl_type.is_none() {
                                return Ok(None);
                            }
                            if let (NodeData::Statement(Statement::Decl { name, ty: _ })) =
                                self.get_node(*decl).unwrap().data
                            {
                                let name_node = self.get_node(name)?;
                                if let NodeData::Expression(Expression::Identifier(ident)) =
                                    name_node.data
                                {
                                    variants.push((ident));
                                } else {
                                    unreachable!();
                                }
                            } else {
                                unreachable!();
                            }
                        }
                        self.add_type(
                            type_index,
                            Type::Enum {
                                variants: variants.clone(),
                            },
                        );
                        Ok(Some(Type::Enum { variants }))
                    }
                }
            }

            _ => panic!("expected type node got {:?}", type_node),
        }
    }

    fn type_statement(
        &mut self,
        other_files_exports: &HashMap<String, HashMap<String, Type>>,
        stmt_index: NodeIndex,
    ) -> Result<Option<Type>> {
        let node = self.nodes.get(&stmt_index).unwrap().clone();
        match node.data {
            NodeData::Statement(ref stmt) => match stmt {
                Statement::Load(_) | Statement::Host(_) => {
                    self.add_type(stmt_index, Type::NoType);
                    return Ok(Some(Type::NoType));
                }

                Statement::Def {
                    mutable,
                    ref name,
                    ty: None,
                    ref expr,
                } => {
                    let expr_ty = self.type_expression(other_files_exports, *expr)?;
                    if expr_ty.is_none() {
                        return Ok(None);
                    }
                    let expr_ty = expr_ty.unwrap();
                    self.add_type(*name, expr_ty.clone());
                    self.add_type(stmt_index, Type::NoType);
                    if node.parent_block.is_some() {
                        self.add_symbol_to_scope(
                            node.parent_block.unwrap(),
                            *name,
                            expr_ty.clone(),
                        );
                        let block = self.get_node(node.parent_block.unwrap())?;
                        if let NodeData::Statement(Statement::Scope {
                            owner: _,
                            is_file_root,
                            stmts: _,
                        }) = block.data
                        {
                            if is_file_root {
                                self.add_exported_symbol(*name, expr_ty.clone());
                            }
                        }
                    }
                    return Ok(Some(Type::NoType));
                }
                Statement::Def {
                    mutable,
                    ref name,
                    ty: Some(ref ty),
                    ref expr,
                } => {
                    let expr_ty = self.type_expression(other_files_exports, *expr)?;
                    if expr_ty.is_none() {
                        return Ok(None);
                    }
                    let type_annotation_type = self.type_type(other_files_exports, *ty)?;
                    if type_annotation_type.is_none() {
                        return Ok(None);
                    }

                    let expr_ty = expr_ty.unwrap();
                    let expr_node = self.nodes.get(expr).unwrap();
                    let type_annotation_type = type_annotation_type.unwrap();
                    if expr_ty.get_kind() != type_annotation_type.get_kind() {
                        return Err(CompilerError {
                            file_source: self.file_source.clone(),
                            filename: self.filename.clone(),
                            line: expr_node.line,
                            col: expr_node.col,
                            reason: Reason::TypeCheckError(
                                TypeCheckError::TwoSidesOfDefinitionHaveDifferentTypes(
                                    type_annotation_type,
                                    expr_ty,
                                ),
                            ),
                        });
                    }
                    self.add_type(*expr, type_annotation_type.clone());
                    self.add_type(*name, type_annotation_type.clone());
                    self.add_type(stmt_index, Type::NoType);
                    if node.parent_block.is_some() {
                        self.add_symbol_to_scope(
                            node.parent_block.unwrap(),
                            *name,
                            type_annotation_type.clone(),
                        );
                        if node.parent_block.is_some() {
                            self.add_symbol_to_scope(
                                node.parent_block.unwrap(),
                                *name,
                                expr_ty.clone(),
                            );
                            let block = self.get_node(node.parent_block.unwrap())?;
                            if let NodeData::Statement(Statement::Scope {
                                owner: _,
                                is_file_root,
                                stmts: _,
                            }) = block.data
                            {
                                if is_file_root {
                                    self.add_exported_symbol(*name, expr_ty.clone());
                                }
                            }
                        }
                    }
                    return Ok(Some(Type::NoType));
                }

                Statement::Decl { ref name, ref ty } => {
                    let decl_type = self.type_type(other_files_exports, *ty)?;
                    if decl_type.is_none() {
                        return Ok(None);
                    }
                    self.add_type(*name, decl_type.clone().unwrap());
                    if node.parent_block.is_some() {
                        self.add_symbol_to_scope(
                            node.parent_block.unwrap(),
                            *name,
                            decl_type.clone().unwrap(),
                        );
                        if node.parent_block.is_some() {
                            self.add_symbol_to_scope(
                                node.parent_block.unwrap(),
                                *name,
                                decl_type.clone().unwrap(),
                            );
                            let block = self.get_node(node.parent_block.unwrap())?;
                            if let NodeData::Statement(Statement::Scope {
                                owner: _,
                                is_file_root,
                                stmts: _,
                            }) = block.data
                            {
                                if is_file_root {
                                    self.add_exported_symbol(*name, decl_type.clone().unwrap());
                                }
                            }
                        }
                    }
                    self.add_type(stmt_index, decl_type.clone().unwrap());
                    return Ok(Some(decl_type.clone().unwrap()));
                }

                Statement::Assign { ref lhs, ref rhs } => {
                    let lhs_type = self.type_expression(other_files_exports, *lhs)?;
                    let rhs_type = self.type_expression(other_files_exports, *rhs)?;
                    if lhs_type.is_none() {
                        return Ok(None);
                    }
                    if rhs_type.is_none() {
                        return Ok(None);
                    }
                    self.add_type(stmt_index, Type::NoType);

                    return Ok(Some(Type::NoType));
                }

                Statement::Scope {
                    ref owner,
                    is_file_root,
                    ref stmts,
                } => self.type_scope(other_files_exports, node.id),

                Statement::If { ref cases } => {
                    for case in cases {
                        let cond_type = self.type_expression(other_files_exports, case.0)?;
                        if cond_type.is_none() {
                            return Ok(None);
                        }
                        let cond_type = cond_type.unwrap();
                        if let Type::Bool = cond_type {
                        } else {
                            panic!("if condition needs to be a boolean.")
                        }
                        let scope_type = self.type_scope(other_files_exports, case.1)?;
                        if scope_type.is_none() {
                            return Ok(None);
                        }
                    }
                    self.add_type(stmt_index, Type::NoType);

                    return Ok(Some(Type::NoType));
                }

                Statement::For {
                    ref start,
                    ref cond,
                    ref cont,
                    ref body,
                } => {
                    let start_type = self.type_statement(other_files_exports, *start)?;
                    if start_type.is_none() {
                        return Ok(None);
                    }
                    let cond_type = self.type_expression(other_files_exports, *cond)?;
                    if cond_type.is_none() {
                        return Ok(None);
                    }
                    let cont_type = self.type_statement(other_files_exports, *cont)?;
                    if cont_type.is_none() {
                        return Ok(None);
                    }
                    let body_type = self.type_statement(other_files_exports, *body)?;

                    if body_type.is_none() {
                        return Ok(None);
                    }
                    self.add_type(stmt_index, Type::NoType);

                    return Ok(Some(Type::NoType));
                }

                Statement::ForIn {
                    ref iterator,
                    ref iterable,
                    ref body,
                } => {
                    let iterable_type = self.type_expression(other_files_exports, *iterable)?;
                    if iterable_type.is_none() {
                        return Ok(None);
                    }
                    let iterable_type = iterable_type.unwrap();
                    if let Type::Array(_, ref elem_type) = iterable_type {
                        self.add_type(*iterator, elem_type.deref().clone());
                        self.add_type(*iterable, iterable_type.clone());
                        self.add_symbol_to_scope(*body, *iterator, elem_type.deref().clone());
                        let body_type = self.type_statement(other_files_exports, *body)?;
                        if body_type.is_none() {
                            return Ok(None);
                        }
                        self.add_type(stmt_index, Type::NoType);

                        return Ok(Some(Type::NoType));
                    } else if let Type::String = iterable_type {
                        self.add_type(*iterator, Type::Char);
                        self.add_type(*iterable, iterable_type.clone());
                        self.add_symbol_to_scope(*body, *iterator, Type::Char);
                        let body_type = self.type_statement(other_files_exports, *body)?;
                        if body_type.is_none() {
                            return Ok(None);
                        }
                        self.add_type(stmt_index, Type::NoType);
                        return Ok(Some(Type::NoType));
                    } else {
                        panic!("iterable type should be array {:?}", iterable_type);
                    }
                }

                Statement::While { ref cond, ref body } => {
                    let cond_type = self.type_expression(other_files_exports, *cond)?;
                    if cond_type.is_none() {
                        return Ok(None);
                    }
                    let body_type = self.type_statement(other_files_exports, *body)?;
                    if body_type.is_none() {
                        return Ok(None);
                    }
                    self.add_type(stmt_index, Type::NoType);

                    return Ok(Some(Type::NoType));
                }

                Statement::Break => {
                    self.add_type(stmt_index, Type::NoType);
                    return Ok(Some(Type::NoType));
                }

                Statement::Continue => {
                    self.add_type(stmt_index, Type::NoType);
                    return Ok(Some(Type::NoType));
                }

                Statement::Goto(_) => {
                    self.add_type(stmt_index, Type::NoType);
                    return Ok(Some(Type::NoType));
                }

                Statement::Return(ref expr) => {
                    let expr_type = self.type_expression(other_files_exports, *expr)?;
                    if expr_type.is_none() {
                        return Ok(None);
                    }
                    self.add_type(stmt_index, Type::NoType);

                    return Ok(Some(Type::NoType));
                }
            },
            NodeData::Expression(Expression::FunctionCall { fn_name, ref args }) => {
                let fn_name_node = self.get_node(fn_name).unwrap();
                if let NodeData::Expression(Expression::Identifier(name)) = fn_name_node.data {
                    if name == "sizeof" {
                        return Err(CompilerError {
                            filename: self.filename.clone(),
                            file_source: self.file_source.clone(),
                            line: fn_name_node.line,
                            col: fn_name_node.col,
                            reason: Reason::TypeCheckError(
                                TypeCheckError::SizeOfFunctionShouldBeUsedInAnExpression,
                            ),
                        });
                    }
                }
                let fn_ret_type = self.type_expression(other_files_exports, stmt_index)?;
                if fn_ret_type.is_none() {
                    return Ok(None);
                }
                self.add_type(stmt_index, Type::NoType);
                return Ok(Some(Type::NoType));
            }
            _ => {
                unreachable!();
            }
        }
    }
    pub fn type_root(
        &mut self,
        other_files_exports: &HashMap<String, HashMap<String, Type>>,
    ) -> Result<()> {
        // scanning file two times let us get rid of all dependencies that are later defined in the same file.
        self.dependencies = vec![];
        self.type_scope(other_files_exports, self.root)?;

        self.dependencies = vec![];
        self.type_scope(other_files_exports, self.root)?;
        Ok(())
    }
    fn type_scope(
        &mut self,
        other_files_exports: &HashMap<String, HashMap<String, Type>>,
        scope_index: NodeIndex,
    ) -> Result<Option<Type>> {
        let scope = self.nodes.get(&scope_index).unwrap().clone();
        if let NodeData::Statement(Statement::Scope {
            owner,
            is_file_root,
            ref stmts,
        }) = scope.data
        {
            for stmt in stmts {
                let ty = self.type_statement(other_files_exports, *stmt)?;
                if ty.is_none() {
                } else {
                    self.add_type(scope_index, Type::NoType);
                }
            }
            if stmts.len() == 0 || self.dependencies.len() == 0 {
                self.add_type(scope_index, Type::NoType);
                return Ok(Some(Type::NoType));
            }

            return Ok(None);
        } else {
            unreachable!();
        }
    }
}
