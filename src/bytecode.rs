use std::{collections::HashMap, ops::Deref, vec};

use serde::Serialize;

use crate::{
    ir::{self, AstTag, BinaryOperation, NodeData, NodeIndex, Statement, UnaryOperation, IR},
    stack::Stack,
    typer::Type,
};

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Instruction {
    pub source_line: usize,
    pub source_column: usize,
    pub payload: InstructionPayload,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum InstructionPayload {
    Host(String),
    Definition {
        name: String,
        ty: Type,
        mutable: bool,
        value: Value,
    },
    Declaration {
        name: String,
        ty: Type,
    },
    Set {
        lhs: Value,
        rhs: Value,
    },
    Block {
        instructions: Vec<Instruction>,
    },
    Call {
        function: Value,
        args: Vec<Value>,
    },
    Label(String),
    JumpTrue {
        cond: Value,
        label: String,
    },
    JumpFalse {
        cond: Value,
        label: String,
    },
    Jump(String),
    Return(Value),
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Value {
    pub ty: Type,
    pub payload: ValuePayload,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum ValuePayload {
    Type(Type),
    Expression(Expression),
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

    Paren(Box<Value>),

    UnaryOperation {
        operator: UnaryOperation,
        expr: Box<Value>,
    },

    BinaryOperation {
        operation: BinaryOperation,
        left: Box<Value>,
        right: Box<Value>,
    },

    ArrayIndex {
        arr: Box<Value>,
        idx: Box<Value>,
    },

    NamespaceAccess {
        namespace: Box<Value>,
        field: Box<Value>,
    },

    Call {
        fn_name: Box<Value>,
        args: Vec<Value>,
    },

    PointerOf(Box<Value>),

    Deref(Box<Value>),

    Label(String),

    Function {
        args: Vec<(String, Type)>,
        ret: Type,
        body: Vec<Instruction>,
    },

    Cast {
        expr: Box<Value>,
        ty: Type,
    },

    SizeOf(Box<Value>),
}
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Module {
    pub instructions: Vec<Instruction>,
}

impl IR {
    fn compile_expression(
        &self,
        continue_jstack: &mut Stack<NodeIndex>,
        expression_index: NodeIndex,
    ) -> (Vec<Instruction>, Value) {
        let expr_node = self.nodes.get(&expression_index).unwrap();
        match expr_node.data {
            NodeData::Expression(ref expr) => {
                match expr {
                    crate::ir::Expression::Cast(cast_expr, cast_type) => {
                        let (_, cast_value) = self.compile_expression(continue_jstack, *cast_expr);
                        let (_, cast_type) = self.compile_expression(continue_jstack, *cast_type);
                        return (
                            vec![],
                            Value {
                                ty: cast_type.ty.clone(),
                                payload: ValuePayload::Expression(Expression::Cast {
                                    expr: Box::new(cast_value),
                                    ty: cast_type.ty,
                                }),
                            },
                        );
                    }
                    crate::ir::Expression::SizeOf(sizeof_expr) => {
                        let (insts, sizeof_value) =
                            self.compile_expression(continue_jstack, *sizeof_expr);
                        return (
                            insts,
                            Value {
                                ty: Type::SignedInt(64),
                                payload: ValuePayload::Expression(Expression::SizeOf(Box::new(
                                    sizeof_value,
                                ))),
                            },
                        );
                    }
                    crate::ir::Expression::Unsigned(number) => {
                        return (
                            vec![],
                            Value {
                                ty: expr_node.type_information.clone().unwrap(),
                                payload: ValuePayload::Expression(Expression::Unsigned(*number)),
                            },
                        );
                    }
                    crate::ir::Expression::Signed(number) => {
                        return (
                            vec![],
                            Value {
                                ty: expr_node.type_information.clone().unwrap(),
                                payload: ValuePayload::Expression(Expression::Signed(*number)),
                            },
                        );
                    }
                    crate::ir::Expression::Float(number) => {
                        return (
                            vec![],
                            Value {
                                ty: expr_node.type_information.clone().unwrap(),
                                payload: ValuePayload::Expression(Expression::Float(*number)),
                            },
                        );
                    }

                    crate::ir::Expression::StringLiteral(s) => {
                        return (
                            vec![],
                            Value {
                                ty: expr_node.type_information.clone().unwrap(),
                                payload: ValuePayload::Expression(Expression::StringLiteral(
                                    s.clone(),
                                )),
                            },
                        );
                    }
                    crate::ir::Expression::Bool(b) => {
                        return (
                            vec![],
                            Value {
                                ty: expr_node.type_information.clone().unwrap(),
                                payload: ValuePayload::Expression(Expression::Bool(b.clone())),
                            },
                        );
                    }
                    crate::ir::Expression::Char(c) => {
                        return (
                            vec![],
                            Value {
                                ty: expr_node.type_information.clone().unwrap(),
                                payload: ValuePayload::Expression(Expression::Char(c.clone())),
                            },
                        );
                    }
                    crate::ir::Expression::Identifier(ident) => {
                        return (
                            vec![],
                            Value {
                                ty: expr_node.type_information.clone().unwrap(),
                                payload: ValuePayload::Expression(Expression::Identifier(
                                    ident.clone(),
                                )),
                            },
                        );
                    }
                    crate::ir::Expression::Paren(inner) => {
                        let (insts, expr) = self.compile_expression(continue_jstack, *inner);
                        return (
                            insts,
                            Value {
                                ty: expr_node.type_information.clone().unwrap(),
                                payload: ValuePayload::Expression(Expression::Paren(Box::new(
                                    expr,
                                ))),
                            },
                        );
                    }
                    crate::ir::Expression::UnaryOperation { operator, expr } => {
                        let (insts, expr) = self.compile_expression(continue_jstack, expr.clone());
                        return (
                            insts,
                            Value {
                                ty: expr_node.type_information.clone().unwrap(),
                                payload: ValuePayload::Expression(Expression::UnaryOperation {
                                    operator: operator.clone(),
                                    expr: Box::new(expr),
                                }),
                            },
                        );
                    }
                    crate::ir::Expression::ArrayIndex { arr, idx } => {
                        let (mut arr_insts, arr) =
                            self.compile_expression(continue_jstack, arr.clone());
                        let (mut idx_insts, idx) =
                            self.compile_expression(continue_jstack, idx.clone());
                        arr_insts.append(&mut idx_insts);
                        return (
                            arr_insts,
                            Value {
                                ty: expr_node.type_information.clone().unwrap(),
                                payload: ValuePayload::Expression(Expression::ArrayIndex {
                                    arr: Box::new(arr),
                                    idx: Box::new(idx),
                                }),
                            },
                        );
                    }
                    crate::ir::Expression::BinaryOperation {
                        operation,
                        left,
                        right,
                    } => {
                        let (mut left_insts, left) =
                            self.compile_expression(continue_jstack, left.clone());
                        let (mut right_insts, right) =
                            self.compile_expression(continue_jstack, right.clone());
                        left_insts.append(&mut right_insts);
                        return (
                            left_insts,
                            Value {
                                ty: expr_node.type_information.clone().unwrap(),
                                payload: ValuePayload::Expression(Expression::BinaryOperation {
                                    operation: operation.clone(),
                                    left: Box::new(left),
                                    right: Box::new(right),
                                }),
                            },
                        );
                    }
                    crate::ir::Expression::NamespaceAccess { namespace, field } => {
                        let namespace_node = self.get_node(namespace.clone()).unwrap();
                        if let Type::TypeRef {
                            name: enum_name,
                            actual_ty: actual,
                        } = namespace_node.type_information.clone().unwrap()
                        {
                            match *actual {
                                Type::Enum { variants } => {
                                    let field_node = self.get_node(field.clone()).unwrap();
                                    return (
                                        vec![],
                                        Value {
                                            ty: Type::UnsignedInt(64),
                                            payload: ValuePayload::Expression(
                                                Expression::Identifier(format!(
                                                    "___LOKI_GENERATED__Enum__{}_{}",
                                                    namespace_node.get_identifier().unwrap(),
                                                    field_node.get_identifier().unwrap()
                                                )),
                                            ),
                                        },
                                    );
                                }
                                _ => {}
                            }
                        }
                        let (mut namespace_insts, namespace) =
                            self.compile_expression(continue_jstack, namespace.clone());
                        let (mut field_insts, field) =
                            self.compile_expression(continue_jstack, field.clone());
                        namespace_insts.append(&mut field_insts);
                        return (
                            namespace_insts,
                            Value {
                                ty: expr_node.type_information.clone().unwrap(),
                                payload: ValuePayload::Expression(Expression::NamespaceAccess {
                                    namespace: Box::new(namespace),
                                    field: Box::new(field),
                                }),
                            },
                        );
                    }
                    crate::ir::Expression::Initialize { ty, fields } => {
                        let name = format!("___LOKI_GENERATED__{}", expression_index);
                        let mut insts = vec![Instruction {
                            source_line: expr_node.line,
                            source_column: expr_node.col,
                            payload: InstructionPayload::Declaration {
                                name: name.clone(),
                                ty: expr_node.type_information.clone().unwrap(),
                            },
                        }];
                        // for each field add a assign instruction
                        for (field, value) in fields {
                            let value_expr = self.get_node(*value).unwrap();
                            let (mut more_insts, value_compiled) =
                                self.compile_expression(continue_jstack, *value);
                            insts.append(&mut more_insts);
                            insts.push(Instruction {
                                source_line: expr_node.line,
                                source_column: expr_node.col,
                                payload: InstructionPayload::Set {
                                    lhs: Value {
                                        // is the namespace access to that field of the struct.
                                        ty: value_expr.type_information.clone().unwrap(),
                                        payload: ValuePayload::Expression(
                                            Expression::NamespaceAccess {
                                                namespace: Box::new(Value {
                                                    ty: expr_node.type_information.clone().unwrap(),
                                                    payload: ValuePayload::Expression(
                                                        Expression::Identifier(name.clone()),
                                                    ),
                                                }),
                                                field: Box::new(Value {
                                                    ty: expr_node.type_information.clone().unwrap(),
                                                    payload: ValuePayload::Expression(
                                                        Expression::Identifier(
                                                            self.get_identifier_as_string(
                                                                field.clone(),
                                                            ),
                                                        ),
                                                    ),
                                                }),
                                            },
                                        ),
                                    },
                                    rhs: value_compiled,
                                },
                            });
                        }
                        return (
                            insts,
                            Value {
                                ty: expr_node.type_information.clone().unwrap(),
                                payload: ValuePayload::Expression(Expression::Identifier(name)),
                            },
                        );
                    }
                    crate::ir::Expression::InitializeArray { ty, elements } => {
                        let name = format!("___LOKI_GENERATED__{}", expression_index);
                        let mut insts = vec![];

                        insts.push(Instruction {
                            source_line: expr_node.line,
                            source_column: expr_node.col,
                            payload: InstructionPayload::Declaration {
                                name: name.clone(),
                                ty: expr_node.type_information.clone().unwrap(),
                            },
                        });

                        // for each field add a assign instruction
                        for (array_index, element) in elements.iter().enumerate() {
                            let element_expr = self.get_node(*element).unwrap();
                            let (mut element_insts, element_compiled) =
                                self.compile_expression(continue_jstack, *element);
                            insts.append(&mut element_insts);
                            insts.push(Instruction {
                                source_line: expr_node.line,
                                source_column: expr_node.col,
                                payload: InstructionPayload::Set {
                                    lhs: Value {
                                        // is the namespace access to that field of the struct.
                                        ty: element_expr.type_information.clone().unwrap(),
                                        payload: ValuePayload::Expression(Expression::ArrayIndex {
                                            arr: Box::new(Value {
                                                ty: expr_node.type_information.clone().unwrap(),
                                                payload: ValuePayload::Expression(
                                                    Expression::Identifier(name.clone()),
                                                ),
                                            }),
                                            idx: Box::new(Value {
                                                ty: expr_node.type_information.clone().unwrap(),
                                                payload: ValuePayload::Expression(
                                                    Expression::Unsigned(array_index as u64),
                                                ),
                                            }),
                                        }),
                                    },
                                    rhs: element_compiled,
                                },
                            });
                        }
                        return (
                            insts,
                            Value {
                                ty: expr_node.type_information.clone().unwrap(),
                                payload: ValuePayload::Expression(Expression::Identifier(format!(
                                    "___LOKI_GENERATED__{}",
                                    expression_index
                                ))),
                            },
                        );
                    }
                    crate::ir::Expression::Function { args, ret_ty, body } => {
                        let mut compiled_args = vec![];
                        for arg in args {
                            let arg_node = self.get_node(*arg).unwrap();
                            if let NodeData::Statement(Statement::Decl { name, ty }) = arg_node.data
                            {
                                compiled_args.push((
                                    self.get_identifier_as_string(name),
                                    arg_node.type_information.unwrap(),
                                ));
                            }
                        }
                        if let Type::FnType(_, ret) = expr_node.type_information.clone().unwrap() {
                            return (
                                vec![],
                                Value {
                                    ty: expr_node.type_information.clone().unwrap(),
                                    payload: ValuePayload::Expression(Expression::Function {
                                        args: compiled_args,
                                        ret: *ret.clone(),
                                        body: self.compile_scope(continue_jstack, *body),
                                    }),
                                },
                            );
                        } else {
                            unreachable!()
                        }
                    }
                    crate::ir::Expression::FunctionCall { fn_name, args } => {
                        let mut compiled_args = vec![];
                        let mut insts = vec![];
                        for arg in args {
                            let (mut arg_insts, arg) =
                                self.compile_expression(continue_jstack, *arg);
                            insts.append(&mut arg_insts);
                            compiled_args.push(arg);
                        }
                        let (mut fn_name_insts, fn_name) =
                            self.compile_expression(continue_jstack, fn_name.clone());
                        insts.append(&mut fn_name_insts);
                        return (
                            insts,
                            Value {
                                ty: expr_node.type_information.clone().unwrap(),
                                payload: ValuePayload::Expression(Expression::Call {
                                    fn_name: Box::new(fn_name),
                                    args: compiled_args,
                                }),
                            },
                        );
                    }
                    crate::ir::Expression::PointerOf(pointee) => {
                        let (pointer_insts, pointer) =
                            self.compile_expression(continue_jstack, pointee.clone());
                        return (
                            pointer_insts,
                            Value {
                                ty: expr_node.type_information.clone().unwrap(),
                                payload: ValuePayload::Expression(Expression::PointerOf(Box::new(
                                    pointer,
                                ))),
                            },
                        );
                    }
                    crate::ir::Expression::Deref(pointer) => {
                        let (deref_insts, deref) =
                            self.compile_expression(continue_jstack, pointer.clone());
                        return (
                            deref_insts,
                            Value {
                                ty: expr_node.type_information.clone().unwrap(),
                                payload: ValuePayload::Expression(Expression::Deref(Box::new(
                                    deref,
                                ))),
                            },
                        );
                    }
                }
            }
            NodeData::TypeDefinition(ref td) => {
                return (
                    vec![],
                    Value {
                        ty: expr_node.type_information.clone().unwrap(),
                        payload: ValuePayload::Type(expr_node.type_information.clone().unwrap()),
                    },
                )
            }

            NodeData::Statement(_) => unreachable!(),
        }
    }
    fn get_identifier_as_string(&self, ident_index: NodeIndex) -> String {
        return self
            .nodes
            .get(&ident_index)
            .unwrap()
            .get_identifier()
            .unwrap();
    }
    fn compile_statement(
        &self,
        loop_stack: &mut Stack<NodeIndex>,
        index: NodeIndex,
    ) -> Vec<Instruction> {
        let node = self.get_node(index).unwrap();
        match node.data {
            NodeData::Statement(ref statement) => {
                match statement {
                    Statement::Load(path) => return vec![],
                    Statement::Host(path) => {
                        return vec![Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Host(path.clone()),
                        }];
                    }

                    /*
                        for initialize/initialize array lowering
                        - if in a definition node -> we do exception in our system and use seperate function to generate a series of instructions for that def.
                        - if in expression we go normal way and add the def and assigments before returning the expression instruction which will be just a reference to that identifier we used.
                    */
                    Statement::Def {
                        ref mutable,
                        ref name,
                        ref ty,
                        ref expr,
                    } => {
                        let expr_node = self.nodes.get(expr).unwrap();
                        let mut insts = vec![];
                        if let NodeData::Expression(crate::ir::Expression::Initialize {
                            ty,
                            ref fields,
                        }) = expr_node.data
                        {
                            insts.push(Instruction {
                                source_line: node.line,
                                source_column: node.col,
                                payload: InstructionPayload::Declaration {
                                    name: self.get_identifier_as_string(name.clone()),
                                    ty: expr_node.type_information.clone().unwrap(),
                                },
                            });
                            for (field, value) in fields {
                                let value_expr = self.get_node(*value).unwrap();
                                let (mut value_insts, value_compiled) =
                                    self.compile_expression(loop_stack, *value);
                                insts.append(&mut value_insts);
                                insts.push(Instruction {
                                    source_line: node.line,
                                    source_column: node.col,
                                    payload: InstructionPayload::Set {
                                        lhs: Value {
                                            // is the namespace access to that field of the struct.
                                            ty: value_expr.type_information.clone().unwrap(),
                                            payload: ValuePayload::Expression(
                                                Expression::NamespaceAccess {
                                                    namespace: Box::new(Value {
                                                        ty: expr_node
                                                            .type_information
                                                            .clone()
                                                            .unwrap(),
                                                        payload: ValuePayload::Expression(
                                                            Expression::Identifier(
                                                                self.get_identifier_as_string(
                                                                    name.clone(),
                                                                ),
                                                            ),
                                                        ),
                                                    }),
                                                    field: Box::new(Value {
                                                        ty: expr_node
                                                            .type_information
                                                            .clone()
                                                            .unwrap(),
                                                        payload: ValuePayload::Expression(
                                                            Expression::Identifier(
                                                                self.get_identifier_as_string(
                                                                    field.clone(),
                                                                ),
                                                            ),
                                                        ),
                                                    }),
                                                },
                                            ),
                                        },
                                        rhs: value_compiled,
                                    },
                                });
                            }
                            return insts;
                            // for each field add assign instruction.
                        } else if let NodeData::Expression(
                            crate::ir::Expression::InitializeArray { ty, ref elements },
                        ) = expr_node.data
                        {
                            let mut insts = vec![];
                            insts.push(Instruction {
                                source_line: node.line,
                                source_column: node.col,
                                payload: InstructionPayload::Declaration {
                                    name: self.get_identifier_as_string(name.clone()),
                                    ty: expr_node.type_information.clone().unwrap(),
                                },
                            });
                            for (array_index, element) in elements.iter().enumerate() {
                                let element_expr = self.get_node(*element).unwrap();
                                let (mut element_insts, element_compiled) =
                                    self.compile_expression(loop_stack, *element);
                                insts.append(&mut element_insts);
                                insts.push(Instruction {
                                    source_line: node.line,
                                    source_column: node.col,
                                    payload: InstructionPayload::Set {
                                        lhs: Value {
                                            // is the namespace access to that field of the struct.
                                            ty: element_expr.type_information.clone().unwrap(),
                                            payload: ValuePayload::Expression(
                                                Expression::ArrayIndex {
                                                    arr: Box::new(Value {
                                                        ty: expr_node
                                                            .type_information
                                                            .clone()
                                                            .unwrap(),
                                                        payload: ValuePayload::Expression(
                                                            Expression::Identifier(
                                                                self.get_identifier_as_string(
                                                                    name.clone(),
                                                                ),
                                                            ),
                                                        ),
                                                    }),
                                                    idx: Box::new(Value {
                                                        ty: expr_node
                                                            .type_information
                                                            .clone()
                                                            .unwrap(),
                                                        payload: ValuePayload::Expression(
                                                            Expression::Unsigned(
                                                                array_index as u64,
                                                            ),
                                                        ),
                                                    }),
                                                },
                                            ),
                                        },
                                        rhs: element_compiled,
                                    },
                                });
                            }
                            return insts;
                        } else if let NodeData::TypeDefinition(crate::ir::TypeDefinition::Enum(
                            ref variants,
                        )) = &expr_node.data
                        {
                            let mut insts = vec![];
                            for (index, variant) in variants.iter().enumerate() {
                                let decl_node = self.nodes.get(&variant).unwrap();
                                if let NodeData::Statement(Statement::Decl {
                                    name: ref variant_name,
                                    ref ty,
                                }) = decl_node.data
                                {
                                    insts.push(Instruction {
                                        source_line: node.line,
                                        source_column: node.col,
                                        payload: InstructionPayload::Definition {
                                            mutable: *mutable,
                                            name: format!(
                                                "___LOKI_GENERATED__Enum__{}_{}",
                                                self.get_identifier_as_string(name.clone()),
                                                self.get_identifier_as_string(variant_name.clone())
                                            ),
                                            value: Value {
                                                ty: Type::UnsignedInt(64),
                                                payload: ValuePayload::Expression(
                                                    Expression::Unsigned(index as u64),
                                                ),
                                            },
                                            ty: Type::UnsignedInt(64),
                                        },
                                    });
                                }
                            }
                            return insts;
                        } else {
                            let mut insts = vec![];
                            let (mut value_insts, value) =
                                self.compile_expression(loop_stack, expr.clone());
                            insts.append(&mut value_insts);
                            insts.push(Instruction {
                                source_line: node.line,
                                source_column: node.col,
                                payload: InstructionPayload::Definition {
                                    mutable: *mutable,
                                    name: self.get_identifier_as_string(name.clone()),
                                    value,
                                    ty: expr_node.type_information.clone().unwrap(),
                                },
                            });
                            return insts;
                        }
                    }
                    Statement::Decl { name, ty } => {
                        let mut insts = vec![];
                        if node.tags.contains(&AstTag::Foreign) {
                            return vec![];
                        }
                        insts.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Declaration {
                                name: self.get_identifier_as_string(name.clone()),
                                ty: node.type_information.clone().unwrap(),
                            },
                        });
                        return insts;
                    }
                    Statement::Assign { lhs, rhs } => {
                        let mut insts = vec![];
                        let (mut lhs_insts, lhs) = self.compile_expression(loop_stack, lhs.clone());
                        let (mut rhs_insts, rhs) = self.compile_expression(loop_stack, rhs.clone());
                        insts.append(&mut lhs_insts);
                        insts.append(&mut rhs_insts);
                        insts.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,

                            payload: InstructionPayload::Set { lhs, rhs },
                        });
                        return insts;
                    }
                    Statement::Scope {
                        owner,
                        is_file_root,
                        ref stmts,
                    } => {
                        let mut insts = vec![];
                        let scope = self.compile_scope(loop_stack, index);
                        insts.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Block {
                                instructions: scope,
                            },
                        });
                        return insts;
                    }
                    Statement::If { ref cases } => {
                        /*
                            a := 2;
                            if (a < 1) {
                            } else if (a > 1) {
                            } else {
                            }
                            JumpTrue(a<1, Case_1)
                            JumpTrue(a>1, Case_2)
                            Jump(Else)
                        */
                        let mut insts = vec![];
                        for (cond, body) in cases {
                            let cond_node = self.get_node(*cond).unwrap();
                            let (mut cond_insts, cond_value) =
                                self.compile_expression(loop_stack, *cond);
                            insts.append(&mut cond_insts);
                            insts.push(Instruction {
                                source_line: cond_node.line,
                                source_column: cond_node.col,
                                payload: InstructionPayload::JumpTrue {
                                    cond: cond_value,
                                    label: format!("Case{}", cond),
                                },
                            })
                        }

                        for (cond, body) in cases {
                            let cond_node = self.get_node(*cond).unwrap();
                            insts.push(Instruction {
                                source_line: cond_node.line,
                                source_column: cond_node.col,
                                payload: InstructionPayload::Label(format!("Case{}", cond)),
                            });
                            let mut body_insts = self.compile_scope(loop_stack, *body);
                            insts.append(&mut body_insts);
                            insts.push(Instruction {
                                source_line: cond_node.line,
                                source_column: cond_node.col,
                                payload: InstructionPayload::Jump(format!("IfEnd{}", node.id)),
                            });
                        }

                        insts.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Label(format!("IfEnd{}", node.id)),
                        });

                        return insts;
                    }
                    Statement::For {
                        start,
                        cond,
                        cont,
                        body,
                    } => {
                        let mut insts = vec![];
                        let mut scope_insts = vec![];
                        let mut loop_insts = vec![];

                        let mut start_insts = self.compile_statement(loop_stack, *start);
                        scope_insts.append(&mut start_insts);

                        let (cond_insts, cond) = self.compile_expression(loop_stack, *cond);

                        scope_insts.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Label(format!("LOOP{}", node.id)),
                        });

                        loop_stack.push(node.id);

                        loop_insts.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::JumpFalse {
                                cond,
                                label: format!("LOOPEND{}", node.id),
                            },
                        });

                        let mut body_insts = self.compile_scope(loop_stack, *body);
                        loop_insts.append(&mut body_insts);
                        loop_insts.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Jump(format!("LOOPCONT{}", node.id)),
                        });

                        scope_insts.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Block {
                                instructions: loop_insts,
                            },
                        });

                        let mut cont_insts = self.compile_statement(loop_stack, *cont);
                        scope_insts.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Label(format!("LOOPCONT{}", node.id)),
                        });
                        scope_insts.append(&mut cont_insts);
                        scope_insts.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Jump(format!("LOOP{}", node.id)),
                        });

                        insts.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Block {
                                instructions: scope_insts,
                            },
                        });
                        insts.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Label(format!("LOOPEND{}", node.id)),
                        });
                        loop_stack.pop();
                        return insts;
                    }
                    Statement::ForIn {
                        iterator,
                        iterable,
                        body,
                    } => {
                        vec![]
                    }
                    Statement::While { cond, body } => {
                        let mut insts = vec![];
                        let mut scope_insts = vec![];
                        let mut loop_insts = vec![];

                        let (cond_insts, cond) = self.compile_expression(loop_stack, *cond);

                        scope_insts.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Label(format!("LOOP{}", node.id)),
                        });

                        loop_stack.push(node.id);

                        loop_insts.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::JumpFalse {
                                cond,
                                label: format!("LOOPEND{}", node.id),
                            },
                        });

                        let mut body_insts = self.compile_scope(loop_stack, *body);
                        loop_insts.append(&mut body_insts);
                        loop_insts.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Jump(format!("LOOPCONT{}", node.id)),
                        });

                        scope_insts.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Block {
                                instructions: loop_insts,
                            },
                        });

                        scope_insts.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Label(format!("LOOPCONT{}", node.id)),
                        });
                        scope_insts.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Jump(format!("LOOP{}", node.id)),
                        });

                        insts.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Block {
                                instructions: scope_insts,
                            },
                        });
                        insts.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Label(format!("LOOPEND{}", node.id)),
                        });
                        loop_stack.pop();
                        return insts;
                    }
                    Statement::Break => {
                        vec![Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Jump(format!(
                                "LOOPEND{}",
                                loop_stack.top().unwrap()
                            )),
                        }]
                    }
                    Statement::Continue => {
                        let mut insts = vec![];
                        insts.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Jump(format!(
                                "LOOPCONT{}",
                                loop_stack.top().unwrap()
                            )),
                        });
                        return insts;
                    }
                    Statement::Goto(expr) => {
                        vec![]
                    }
                    Statement::Return(expr) => {
                        let mut insts = vec![];
                        let (mut ret_insts, ret) =
                            self.compile_expression(loop_stack, expr.clone());
                        insts.append(&mut ret_insts);
                        insts.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Return(ret),
                        });
                        return insts;
                    }
                }
            }
            NodeData::TypeDefinition(_) => unreachable!(),
            NodeData::Expression(crate::ir::Expression::FunctionCall {
                ref fn_name,
                ref args,
            }) => {
                let mut insts = vec![];
                let mut compiled_args = vec![];
                for arg in args {
                    let (mut arg_insts, arg) = self.compile_expression(loop_stack, *arg);
                    insts.append(&mut arg_insts);
                    compiled_args.push(arg);
                }
                let (mut function_insts, function) =
                    self.compile_expression(loop_stack, fn_name.clone());
                insts.append(&mut function_insts);
                insts.push(Instruction {
                    source_line: node.line,
                    source_column: node.col,
                    payload: InstructionPayload::Call {
                        function,
                        args: compiled_args,
                    },
                });
                return insts;
            }
            NodeData::Expression(_) => panic!("unexpected {:?}", node),
        }
    }
    fn compile_scope(
        &self,
        continue_jstack: &mut Stack<NodeIndex>,
        index: NodeIndex,
    ) -> Vec<Instruction> {
        let root_node = self.nodes.get(&index).unwrap();
        let mut instructions = vec![];
        if let NodeData::Statement(Statement::Scope {
            owner,
            is_file_root,
            ref stmts,
        }) = root_node.data
        {
            for stmt in stmts {
                let mut insts = self.compile_statement(continue_jstack, *stmt);
                instructions.append(&mut insts);
            }
        }

        return instructions;
    }
}

pub fn make_module(irs: &mut HashMap<String, IR>) -> Module {
    let mut module = Module {
        instructions: vec![],
    };
    let mut root_instructions = vec![];
    for (file, ir) in irs {
        let mut stmts = ir.compile_scope(&mut Stack::new(), ir.root);
        root_instructions.append(&mut stmts);
    }

    for inst in &root_instructions {
        module.instructions.push(inst.clone());
    }

    module
}
