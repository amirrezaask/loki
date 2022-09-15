use serde::Serialize;

use crate::{typer::Type, ir::{UnaryOperation, BinaryOperation, IR, NodeData, Statement, NodeIndex}};

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Instruction {
    pub source_line: usize,
    pub source_column: usize,
    pub payload: InstructionPayload
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

    Goto(Value),
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

    InitializeArray (Vec<Value>),

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
    instructions: Vec<Instruction>
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Module {
    pub source_filename: String,
    pub root: Scope,
}

impl IR {
    fn compile_expression(&self, expression_index: NodeIndex) -> Value {
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
                            payload: ValuePayload::Expression(Expression::Paren(Box::new(self.compile_expression(*inner)))),
                        };
                    },
                    crate::ir::Expression::UnaryOperation { operator, expr } => {
                        return Value {
                            ty: expr_node.type_information.clone().unwrap(),
                            payload: ValuePayload::Expression(Expression::UnaryOperation{ operator: operator.clone(), expr: Box::new(self.compile_expression(expr.clone())) }),
                        };
                    },
                    crate::ir::Expression::ArrayIndex { arr, idx } => {
                        return Value {
                            ty: expr_node.type_information.clone().unwrap(),
                            payload: ValuePayload::Expression(Expression::ArrayIndex { arr: Box::new(self.compile_expression(arr.clone())), idx: Box::new(self.compile_expression(idx.clone())) }),
                        };
                    },
                    crate::ir::Expression::BinaryOperation { operation, left, right } => {
                        return Value {
                            ty: expr_node.type_information.clone().unwrap(),
                            payload: ValuePayload::Expression(Expression::BinaryOperation { 
                                operation: operation.clone(), 
                                left: Box::new(self.compile_expression(left.clone())),
                                right: Box::new(self.compile_expression(right.clone())) }),
                        };
                    },
                    crate::ir::Expression::NamespaceAccess { namespace, field } => {
                        return Value {
                            ty: expr_node.type_information.clone().unwrap(),
                            payload: ValuePayload::Expression(Expression::NamespaceAccess { namespace: Box::new(self.compile_expression(namespace.clone())), field: Box::new(self.compile_expression(field.clone())) })
                        };
                    },
                    crate::ir::Expression::Initialize { ty, fields } => {
                        panic!("initialize still not supported");
                    },
                    crate::ir::Expression::InitializeArray { ty, elements } => {
                        panic!("initialize still not supported");
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
                            compiled_args.push(self.compile_expression(*arg));
                        }
                        return Value {
                            ty: expr_node.type_information.clone().unwrap(),
                            payload: ValuePayload::Expression(Expression::Call { fn_name: Box::new(self.compile_expression(fn_name.clone())), args: compiled_args })
                        };
                    },
                    crate::ir::Expression::PointerOf(pointee) => {
                        return Value {
                            ty: expr_node.type_information.clone().unwrap(),
                            payload: ValuePayload::Expression(Expression::PointerOf(Box::new(self.compile_expression(pointee.clone())))),
                        };
                    },
                    crate::ir::Expression::Deref(pointer) => {
                        return Value {
                            ty: expr_node.type_information.clone().unwrap(),
                            payload: ValuePayload::Expression(Expression::Deref(Box::new(self.compile_expression(pointer.clone())))),
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
    fn compile_statement(&self, index: NodeIndex) -> Instruction {
        let node = self.get_node(index).unwrap();
        match node.data {
            NodeData::Statement(ref statement) => {
                match statement {
                    Statement::Load(path) => {
                        return Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Load(path.clone()) 
                        };
                    },
                    Statement::Host(path) => {
                        return Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Host(path.clone()) 
                        };
                    },
                    Statement::Def { ref mutable, ref name, ref ty, ref expr } => {
                        let expr_node = self.nodes.get(&expr).unwrap();
                        println!("expr_node {:?}", expr_node);
                        return Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Definition { 
                                mutable: *mutable, 
                                name: self.get_identifier_as_string(name.clone()),
                                value: self.compile_expression(expr.clone()),
                                ty: expr_node.type_information.clone().unwrap(),
                            }
                        };
                    },
                    Statement::Decl { name, ty } => {
                        println!("decl node {:?}", node);
                        return Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Declaration { 
                                name: self.get_identifier_as_string(name.clone()),
                                ty: node.type_information.clone().unwrap(),
                            }
                        };
                    },
                    Statement::Assign { lhs, rhs } => {
                        return Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Assign {
                                lhs: self.compile_expression(lhs.clone()),
                                rhs: self.compile_expression(rhs.clone()),
                            }
                        };
                    },
                    Statement::Scope { owner, is_file_root, ref stmts } => {
                        return Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Scope(Scope { instructions: self.compile_scope(index) })
                        };     
                    },
                    Statement::If { ref cases } => {
                        let mut compiled_cases = vec![];
                        for case in cases {
                            compiled_cases.push((self.compile_expression(case.0), self.compile_scope(case.1)));
                        }

                        return Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Branch { cases: compiled_cases }
                        };
                    },
                    Statement::For { start, cond, cont, body } => {
                        panic!("for transformation still not supported in bytecode.")
                    },
                    Statement::ForIn { iterator, iterable, body } => {
                        panic!("for in transformation still not supported in bytecode.")
                    },
                    Statement::While { cond, body } => {
                        return Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::While { cond: self.compile_expression(cond.clone()), body: self.compile_scope(body.clone()) }
                        };
                    },
                    Statement::Break => {
                        return Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Continue
                        };
                    },
                    Statement::Continue => {
                        return Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Continue
                        };
                    },
                    Statement::Goto(expr) => {
                        return Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Goto(self.compile_expression(expr.clone()))
                        };
                    }
                    Statement::Return(expr) => {
                        return Instruction {
                            source_line: node.line,
                            source_column: node.col,
                            payload: InstructionPayload::Return(self.compile_expression(expr.clone()))
                        };
                    },
                }
            },
            NodeData::TypeDefinition(_) => unreachable!(),
            NodeData::Expression(_) => unreachable!(),
        }
    }
    fn compile_scope(&self, index: NodeIndex) -> Vec<Instruction> {
        let root_node = self.nodes.get(&index).unwrap();
        let mut instructions = vec![];
        if let NodeData::Statement(Statement::Scope { owner, is_file_root, ref stmts } ) = root_node.data {
            for stmt in stmts {
                let inst = self.compile_statement(*stmt);
                instructions.push(inst);
            }
        }

        return instructions;
    }
    pub fn into_module(&self) -> Module {
        let mut module = Module {
            source_filename: self.filename.clone(),
            root: Scope { instructions: vec![] },
        };
        let root_instructions = self.compile_scope(self.root);
        module.root.instructions = root_instructions;
        return module;
    }

}