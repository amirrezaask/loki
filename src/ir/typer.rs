use std::collections::HashMap;

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

    Array(u64, Box<Type>),
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

    NamespaceAccess {
        ns: Box<Type>,
        field: Box<Type>
    },

    CVarArgs,
    CString,

    Void,
}

type ScopeSymbolIndex = (Index, String);

pub struct TypedIR {
    ir: IR,
    scoped_symbols: HashMap<ScopeSymbolIndex, Type>,
}


impl TypedIR {
    fn add_type(&mut self, index: Index, ty: Type) {
        let node = self.ir.nodes.get_mut(&index).unwrap();
        node.type_information = Some(ty);
    }
    fn find_identifier_type(&self, identifier: String) -> Result<Type> {
        unimplemented!()
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
                        let identifier_type = self.find_identifier_type(identifier.clone())?;
                        self.add_type(expression_index, identifier_type.clone());
                        return Ok(identifier_type);
                    },
                    Expression::UnaryOperation { operator, expr } => {
                        let expr_type = self.type_expression(*expr)?;
                        match operator {
                            UnaryOperation::Not => {
                                return Ok(Type::Bool);
                            },
                        }
                    },
                    Expression::ArrayIndex { arr, idx } => {
                        let arr_type = self.type_expression(*arr)?;
                        let idx_type = self.type_expression(*idx)?;

                        // get element type from array
                        unimplemented!();
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
                                    return Ok(left_type);
                                } else {
                                    unimplemented!();
                                }
                            }
                        }
                    },
                    Expression::NamespaceAccess { namespace, field } => {
                        let ns_type = self.type_expression(*namespace)?;
                        // get struct/file load/enum out of namespace then get field type of it
                        unimplemented!();
                    },
                    Expression::Initialize { ty, fields } => {
                        let initialize_type = self.resolve_type_definition(*ty)?;
                        // check if fields are valid in context of it's type.
                        return Ok(initialize_type);
                    },
                    Expression::InitializeArray { elements } => {
                        // do we need the type ?
                        unimplemented!()
                    },
                    Expression::Function { args, ret_ty, body } => todo!(),
                    Expression::FunctionCall { fn_name, args } => todo!(),
                    Expression::PointerOf(_) => todo!(),
                    Expression::Deref(_) => todo!(),
                }
            },
            NodeData::Statement(_) => panic!("unexpected when typing an expression {:?}", node),
            NodeData::TypeDefinition(_) => panic!("unexpected when typing an expression {:?}", node)
            
        }        
    }
    fn resolve_type_definition(&mut self, type_definition_index: Index) -> Result<Type> {

        unimplemented!()
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

    pub fn new(ir: IR) -> Result<Self> {
        let mut tir = TypedIR {
            ir,
            scoped_symbols: todo!(),
        };
        tir.type_root(ir.root)?;
        Ok(tir)
    }
}