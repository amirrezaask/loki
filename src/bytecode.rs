use std::{vec, collections::HashMap};

use serde::Serialize;

use crate::{typer::Type, ir::{UnaryOperation, BinaryOperation, IR, NodeData, Statement, NodeIndex, AstTag, self}};

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Instruction {
    pub source_line: usize,
    pub source_column: usize,
    pub payload: InstructionPayload,
}
#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum InstructionPayload {
    Load(String),
    Host(String),
    Definition {
        mutable: bool,
        name: String,
        ty: Type,
        value: Value,
    },
    Declaration {
        name: String,
        ty: Type,
    },
    Assign {
        lhs: Value,
        rhs: Value,
    },

    Scope(Scope),

    Branch {
        cases: Vec<(Value, Vec<Instruction>)>,
    },

    While {
        cond: Value,
        body: Vec<Instruction>,
    },

    Break,
    Continue,

    Call { function: Value, args: Vec<Value> },

    Goto(Value),
    Return(Value),
}
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Value {
    pub ty: Type,
    pub payload: ValuePayload,
}

impl Type {
    pub fn is_struct_definition(&self) -> bool {
        match self {
            Type::Type(t) => return t.is_struct_definition(),
            Type::Struct { fields } => true,
            _ => false,
        }
    }
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
}
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Scope {
    pub instructions: Vec<Instruction>
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Module {
    pub instructions: Vec<Instruction>,
}
impl IR {
    fn compile_expression(&self, current_scope_instructions: &mut Vec<Instruction>, expression_index: NodeIndex) -> Value {
        let expr_node = self.nodes.get(&expression_index).unwrap();
        match expr_node.data {
            NodeData::Expression(ref expr) => {
                match expr {
                    crate::ir::Expression::Unsigned(number) => {
                        return Value {
                            ty: expr_node.type_information.clone().unwrap(),
                            payload: ValuePayload::Expression(Expression::Unsigned(*number)),
                        };
                    },
                    crate::ir::Expression::Signed(number) => {
                        return Value {
                            ty: expr_node.type_information.clone().unwrap(),
                            payload: ValuePayload::Expression(Expression::Signed(*number)),
                        };
                    },
                    crate::ir::Expression::Float(number) => {
                        return Value {
                            ty: expr_node.type_information.clone().unwrap(),
                            payload: ValuePayload::Expression(Expression::Float(*number)),
                        };
                    },

                    crate::ir::Expression::StringLiteral(s) => {
                        return Value {
                            ty: expr_node.type_information.clone().unwrap(),
                            payload: ValuePayload::Expression(Expression::StringLiteral(s.clone())),
                        };

                    },
                    crate::ir::Expression::Bool(b) => {
                        return Value {
                            ty: expr_node.type_information.clone().unwrap(),
                            payload: ValuePayload::Expression(Expression::Bool(b.clone())),
                        };
                    },
                    crate::ir::Expression::Char(c) => {
                        return Value {
                            ty: expr_node.type_information.clone().unwrap(),
                            payload: ValuePayload::Expression(Expression::Char(c.clone())),
                        };
                    },
                    crate::ir::Expression::Identifier(ident) => {
                        return Value {
                            ty: expr_node.type_information.clone().unwrap(),
                            payload: ValuePayload::Expression(Expression::Identifier(ident.clone())),
                        };
                    },
                    crate::ir::Expression::Paren(inner) => {
                        return Value {
                            ty: expr_node.type_information.clone().unwrap(),
                            payload: ValuePayload::Expression(Expression::Paren(Box::new(self.compile_expression(current_scope_instructions,*inner)))),
                        };
                    },
                    crate::ir::Expression::UnaryOperation { operator, expr } => {
                        return Value {
                            ty: expr_node.type_information.clone().unwrap(),
                            payload: ValuePayload::Expression(Expression::UnaryOperation{ operator: operator.clone(), expr: Box::new(self.compile_expression(current_scope_instructions,expr.clone())) }),
                        };
                    },
                    crate::ir::Expression::ArrayIndex { arr, idx } => {
                        return Value {
                            ty: expr_node.type_information.clone().unwrap(),
                            payload: ValuePayload::Expression(Expression::ArrayIndex { arr: Box::new(self.compile_expression(current_scope_instructions,arr.clone())), idx: Box::new(self.compile_expression(current_scope_instructions,idx.clone())) }),
                        };
                    },
                    crate::ir::Expression::BinaryOperation { operation, left, right } => {
                        return Value {
                            ty: expr_node.type_information.clone().unwrap(),
                            payload: ValuePayload::Expression(Expression::BinaryOperation { 
                                operation: operation.clone(), 
                                left: Box::new(self.compile_expression(current_scope_instructions,left.clone())),
                                right: Box::new(self.compile_expression(current_scope_instructions,right.clone())) }),
                        };
                    },
                    crate::ir::Expression::NamespaceAccess { namespace, field } => {
                        let namespace_node = self.get_node(namespace.clone()).unwrap();
                        if let Type::Enum { variants } = namespace_node.type_information.clone().unwrap() {
                            let field_node = self.get_node(field.clone()).unwrap();
                            return Value {
                                ty: Type::UnsignedInt(64),
                                payload: ValuePayload::Expression(Expression::Identifier(format!("___LOKI_GENERATED__Enum__{}_{}", namespace_node.get_identifier().unwrap(), field_node.get_identifier().unwrap()))),
                            }
                        }
                        return Value {
                            ty: expr_node.type_information.clone().unwrap(),
                            payload: ValuePayload::Expression(Expression::NamespaceAccess { namespace: Box::new(self.compile_expression(current_scope_instructions,namespace.clone())), field: Box::new(self.compile_expression(current_scope_instructions,field.clone())) })
                        };
                    },
                    crate::ir::Expression::Initialize { ty, fields } => {
                        let name = format!("___LOKI_GENERATED__{}", expression_index);
                       
                        current_scope_instructions.push(Instruction { 
                            source_line: expr_node.line,
                            source_column: expr_node.col,
                            payload: InstructionPayload::Declaration { name: name.clone(), ty: expr_node.type_information.clone().unwrap() } 
                        });
                        // for each field add a assign instruction
                        for (field, value) in fields {
                            let value_expr = self.get_node(*value).unwrap();
                            let value_compiled = self.compile_expression(current_scope_instructions, *value);
                            current_scope_instructions.push(Instruction { 
                                source_line: expr_node.line,
                                source_column: expr_node.col,
                                payload: InstructionPayload::Assign { 
                                    lhs: Value { // is the namespace access to that field of the struct.
                                        ty: value_expr.type_information.clone().unwrap(),
                                        payload: ValuePayload::Expression(Expression::NamespaceAccess { 
                                            namespace: Box::new(Value {
                                                ty: expr_node.type_information.clone().unwrap(),
                                                payload: ValuePayload::Expression(Expression::Identifier(name.clone())),
                                            }),
                                            field: Box::new(Value {
                                                ty: expr_node.type_information.clone().unwrap(),
                                                payload: ValuePayload::Expression(Expression::Identifier(self.get_identifier_as_string(field.clone()))),
                                            })
                                        }),
                                    },
                                    rhs: value_compiled,
                                }
                            });
                        }
                        return Value {
                            ty: expr_node.type_information.clone().unwrap(),
                            payload: ValuePayload::Expression(Expression::Identifier(name)),
                        };

                    },
                    crate::ir::Expression::InitializeArray { ty, elements } => {
                        let name = format!("___LOKI_GENERATED__{}", expression_index);
                        current_scope_instructions.push(Instruction { 
                            source_line: expr_node.line,
                            source_column: expr_node.col,
                            payload: InstructionPayload::Declaration { name: name.clone(), ty: expr_node.type_information.clone().unwrap() } 
                        });

                        // for each field add a assign instruction
                        for (array_index, element) in elements.iter().enumerate() {
                            let element_expr = self.get_node(*element).unwrap();
                            let element_compiled = self.compile_expression(current_scope_instructions, *element);
                            current_scope_instructions.push(Instruction { 
                                source_line: expr_node.line,
                                source_column: expr_node.col,
                                payload: InstructionPayload::Assign { 
                                    lhs: Value { // is the namespace access to that field of the struct.
                                        ty: element_expr.type_information.clone().unwrap(),
                                        payload: ValuePayload::Expression(Expression::ArrayIndex { 
                                            arr: Box::new(Value {
                                                ty: expr_node.type_information.clone().unwrap(),
                                                payload: ValuePayload::Expression(Expression::Identifier(name.clone())),
                                            }),
                                            idx: Box::new(Value {
                                                ty: expr_node.type_information.clone().unwrap(),
                                                payload: ValuePayload::Expression(Expression::Unsigned(array_index as u64)),
                                            })
                                        })
                                    },
                                    rhs: element_compiled,
                                }
                            });
                        }
                        return Value {
                            ty: expr_node.type_information.clone().unwrap(),
                            payload: ValuePayload::Expression(Expression::Identifier(format!("___LOKI_GENERATED__{}", expression_index))),
                        };
                    },
                    crate::ir::Expression::Function { args, ret_ty, body } => {
                        let mut compiled_args = vec![];
                        for arg in args {
                            let arg_node = self.get_node(*arg).unwrap();
                            if let NodeData::Statement(Statement::Decl { name, ty }) = arg_node.data {
                                compiled_args.push((self.get_identifier_as_string(name), arg_node.type_information.unwrap()));
                            }
                        }
                        if let Type::FnType(_, ret) = expr_node.type_information.clone().unwrap() {
                            return Value {
                                ty: expr_node.type_information.clone().unwrap(),
                                payload: ValuePayload::Expression(Expression::Function { args: compiled_args, ret: *ret.clone(), body: self.compile_scope(*body) })
                            };
                        } else {
                            unreachable!()
                        }
                        
                    },
                    crate::ir::Expression::FunctionCall { fn_name, args } => {
                        let mut compiled_args = vec![];
                        for arg in args {
                            compiled_args.push(self.compile_expression(current_scope_instructions,*arg));
                        }
                        return Value {
                            ty: expr_node.type_information.clone().unwrap(),
                            payload: ValuePayload::Expression(Expression::Call { fn_name: Box::new(self.compile_expression(current_scope_instructions,fn_name.clone())), args: compiled_args })
                        };
                    },
                    crate::ir::Expression::PointerOf(pointee) => {
                        return Value {
                            ty: expr_node.type_information.clone().unwrap(),
                            payload: ValuePayload::Expression(Expression::PointerOf(Box::new(self.compile_expression(current_scope_instructions,pointee.clone())))),
                        };
                    },
                    crate::ir::Expression::Deref(pointer) => {
                        return Value {
                            ty: expr_node.type_information.clone().unwrap(),
                            payload: ValuePayload::Expression(Expression::Deref(Box::new(self.compile_expression(current_scope_instructions,pointer.clone())))),
                        };
                    },
                }
            },
            NodeData::TypeDefinition(ref td) => {
                return Value {
                    ty: expr_node.type_information.clone().unwrap(),
                    payload: ValuePayload::Type(expr_node.type_information.clone().unwrap()),
                }
            },

            NodeData::Statement(_) => unreachable!(),
        }
    }
    fn get_identifier_as_string(&self, ident_index: NodeIndex) -> String {
        return self.nodes.get(&ident_index).unwrap().get_identifier().unwrap();
    }
    fn compile_statement(&self, mut current_scope_instructions: &mut Vec<Instruction>, index: NodeIndex) {
        let node = self.get_node(index).unwrap();
        match node.data {
            NodeData::Statement(ref statement) => {
                match statement {
                    Statement::Load(path) => {
                        current_scope_instructions.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Load(path.clone()) 
                        });
                    },
                    Statement::Host(path) => {
                        current_scope_instructions.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Host(path.clone()) 
                        });
                    },
                    
                    /*
                        for initialize/initialize array lowering
                        - if in a definition node -> we do exception in our system and use seperate function to generate a series of instructions for that def.
                        - if in expression we go normal way and add the def and assigments before returning the expression instruction which will be just a reference to that identifier we used.
                    */
                    Statement::Def { ref mutable, ref name, ref ty, ref expr } => {
                        let expr_node = self.nodes.get(expr).unwrap();
                        if let NodeData::Expression(crate::ir::Expression::Initialize { ty, ref fields }) = expr_node.data {
                            current_scope_instructions.push(Instruction { 
                                source_line: node.line,
                                source_column: node.col,
                                payload: InstructionPayload::Declaration { name: self.get_identifier_as_string(name.clone()), ty: expr_node.type_information.clone().unwrap() } 
                            });
                            for (field, value) in fields {
                                let value_expr = self.get_node(*value).unwrap();
                                let value_compiled = self.compile_expression(current_scope_instructions, *value);
                                current_scope_instructions.push(Instruction { 
                                    source_line: node.line,
                                    source_column: node.col,
                                    payload: InstructionPayload::Assign { 
                                        lhs: Value { // is the namespace access to that field of the struct.
                                            ty: value_expr.type_information.clone().unwrap(),
                                            payload: ValuePayload::Expression(Expression::NamespaceAccess { 
                                                namespace: Box::new(Value {
                                                    ty: expr_node.type_information.clone().unwrap(),
                                                    payload: ValuePayload::Expression(Expression::Identifier(self.get_identifier_as_string(name.clone()))),
                                                }),
                                                field: Box::new(Value {
                                                    ty: expr_node.type_information.clone().unwrap(),
                                                    payload: ValuePayload::Expression(Expression::Identifier(self.get_identifier_as_string(field.clone()))),
                                                })
                                            }),
                                        },
                                        rhs: value_compiled,
                                    }
                                });
                            }
                            // for each field add assign instruction.
                        } else if let NodeData::Expression(crate::ir::Expression::InitializeArray { ty, ref elements }) = expr_node.data {
                            current_scope_instructions.push(Instruction { 
                                source_line: node.line,
                                source_column: node.col,
                                payload: InstructionPayload::Declaration { name: self.get_identifier_as_string(name.clone()), ty: expr_node.type_information.clone().unwrap() } 
                            });
                            for (array_index, element) in elements.iter().enumerate() {
                                let element_expr = self.get_node(*element).unwrap();
                                let element_compiled = self.compile_expression(current_scope_instructions, *element);
                                current_scope_instructions.push(Instruction { 
                                    source_line: node.line,
                                    source_column: node.col,
                                    payload: InstructionPayload::Assign { 
                                        lhs: Value { // is the namespace access to that field of the struct.
                                            ty: element_expr.type_information.clone().unwrap(),
                                            payload: ValuePayload::Expression(Expression::ArrayIndex { 
                                                arr: Box::new(Value {
                                                    ty: expr_node.type_information.clone().unwrap(),
                                                    payload: ValuePayload::Expression(Expression::Identifier(self.get_identifier_as_string(name.clone()))),
                                                }),
                                                idx: Box::new(Value {
                                                    ty: expr_node.type_information.clone().unwrap(),
                                                    payload: ValuePayload::Expression(Expression::Unsigned(array_index as u64)),
                                                })
                                            })
                                        },
                                        rhs: element_compiled,
                                    }
                                });
                            }
                        } else if let NodeData::TypeDefinition(crate::ir::TypeDefinition::Enum(ref variants)) = &expr_node.data  {
                            for (index, variant) in variants.iter().enumerate() {
                                let decl_node = self.nodes.get(&variant).unwrap();
                                if let NodeData::Statement(Statement::Decl { name: ref variant_name, ref ty }) = decl_node.data {
                                    current_scope_instructions.push(Instruction {
                                        source_line: node.line,
                                        source_column: node.col,
                                        payload: InstructionPayload::Definition { 
                                            mutable: false, 
                                            name: format!("___LOKI_GENERATED__Enum__{}_{}", self.get_identifier_as_string(name.clone()), self.get_identifier_as_string(variant_name.clone())) ,
                                            value: Value { 
                                                ty: Type::UnsignedInt(64),
                                                payload: ValuePayload::Expression(Expression::Unsigned(index as u64))
                                            },
                                            ty: Type::UnsignedInt(64),
                                        }
                                    });
                                }
                            }
                        } else {
                            let value = self.compile_expression(current_scope_instructions,expr.clone());
                            current_scope_instructions.push(Instruction {
                                source_line: node.line,
                                source_column: node.col,
                                payload: InstructionPayload::Definition { 
                                    mutable: *mutable, 
                                    name: self.get_identifier_as_string(name.clone()),
                                    value,
                                    ty: expr_node.type_information.clone().unwrap(),
                                }
                            });
                        }
                        
                    },
                    Statement::Decl { name, ty } => {
                        if node.tags.contains(&AstTag::Foreign) {
                            return;
                        }
                        current_scope_instructions.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Declaration { 
                                name: self.get_identifier_as_string(name.clone()),
                                ty: node.type_information.clone().unwrap(),
                            }
                        });
                    },
                    Statement::Assign { lhs, rhs } => {
                        let lhs = self.compile_expression(current_scope_instructions,lhs.clone());
                        let rhs = self.compile_expression(current_scope_instructions,rhs.clone());
                        current_scope_instructions.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,

                            payload: InstructionPayload::Assign {
                               lhs, rhs
                            }
                        });
                    },
                    Statement::Scope { owner, is_file_root, ref stmts } => {
                        current_scope_instructions.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Scope(Scope { instructions: self.compile_scope(index) })
                        });     
                    },
                    Statement::If { ref cases } => {
                        let mut compiled_cases = vec![];
                        for case in cases {
                            compiled_cases.push((self.compile_expression(current_scope_instructions,case.0), self.compile_scope(case.1)));
                        }

                        current_scope_instructions.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Branch { cases: compiled_cases }
                        });
                    },
                    Statement::For { start, cond, cont, body } => {
                        let cond = self.compile_expression(current_scope_instructions,cond.clone());
                        self.compile_statement(current_scope_instructions, *start);
                        if let NodeData::Statement(Statement::Scope { owner, is_file_root, ref stmts } ) = self.get_node(*body).unwrap().data {
                            let mut instructions = vec![];
                            self.compile_statement(&mut instructions, *cont);
                            for stmt in stmts {
                                self.compile_statement(&mut instructions, *stmt);
                            }
                            current_scope_instructions.push(Instruction {
                                source_line: node.line,
                                source_column: node.col,
                                payload: InstructionPayload::While { cond: cond, body: instructions }
                            });
                        }
                    },
                    Statement::ForIn { iterator, iterable, body } => {
                        panic!("for in transformation still not supported in bytecode.")
                    },
                    Statement::While { cond, body } => {
                        let cond = self.compile_expression(current_scope_instructions,cond.clone());
                        current_scope_instructions.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::While { cond: cond, body: self.compile_scope(body.clone()) }
                        });
                    },
                    Statement::Break => {
                        current_scope_instructions.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Continue
                        });
                    },
                    Statement::Continue => {
                        current_scope_instructions.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Continue
                        });
                    },
                    Statement::Goto(expr) => {
                        let goto = self.compile_expression(current_scope_instructions, expr.clone());
                        current_scope_instructions.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Goto(goto)
                        });
                    }
                    Statement::Return(expr) => {
                        let ret = self.compile_expression(current_scope_instructions, expr.clone());
                        current_scope_instructions.push(Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Return(ret)
                        });
                    },
                }
            },
            NodeData::TypeDefinition(_) => unreachable!(),
            NodeData::Expression(crate::ir::Expression::FunctionCall { ref fn_name, ref args }) => {
                let mut compiled_args = vec![];
                for arg in args {
                    compiled_args.push(self.compile_expression(current_scope_instructions,*arg));
                }
                let function = self.compile_expression(current_scope_instructions,fn_name.clone());
                current_scope_instructions.push(Instruction { 
                    source_line: node.line,
                    source_column: node.col,
                    payload: InstructionPayload::Call { 
                        function,
                        args: compiled_args,
                    } 
                });
            },
            NodeData::Expression(_) => panic!("unexpected {:?}", node),
        }
    }
    fn compile_scope(&self, index: NodeIndex) -> Vec<Instruction> {
        let root_node = self.nodes.get(&index).unwrap();
        let mut instructions = vec![];
        if let NodeData::Statement(Statement::Scope { owner, is_file_root, ref stmts } ) = root_node.data {
            for stmt in stmts {
                self.compile_statement(&mut instructions, *stmt);
            }
        }

        return instructions;
    }
}

fn analyze_instruction_dependencies(inst: &Instruction) -> Vec<String> {

    unreachable!()
}
fn analyze_type_dependencies(ty: &Type) -> Vec<String> {
    match ty {
        Type::Type(inner) => return analyze_type_dependencies(inner),
        Type::Array(_, inner) => return analyze_type_dependencies(inner),
        Type::Struct { ref fields } => {
            let mut deps = vec![];
            for field in fields {
                deps.append(&mut analyze_type_dependencies(&field.1));
            }

            deps
        },
        Type::TypeRef { name, actual_ty } => {
            vec![name.clone()]
        },
        Type::Pointer(inner) => return analyze_type_dependencies(inner),
        _ => vec![],
    }
}

// returns a list of identifiers that a function needs.
fn analyze_function_dependencies(function: &Expression) -> Vec<String> {
    let mut deps: Vec<String> = vec![];
    match function {
        Expression::Function { ref args, ref ret, ref body } => {
            for (_, ty) in args {
                deps.append(&mut analyze_type_dependencies(ty));
            }
            deps.append(&mut analyze_type_dependencies(ret));
            for inst in body {
                deps.append(&mut analyze_instruction_dependencies(inst));
            }
        },
        _ => {}
    }

    return deps;
}


pub fn make_module(irs: &mut HashMap<String, IR>) -> Module {
    let mut module = Module {
        instructions: vec![],
    };
    let mut root_instructions = vec![];
    for (file, ir) in irs {
        let mut stmts = ir.compile_scope(ir.root);
        root_instructions.append(&mut stmts);
    }

    for inst in &root_instructions {
        module.instructions.push(inst.clone());
    }

    module

}
