use std::ops::Deref;

use super::{Node, NodeData, Repr, Ast};
use crate::ast::{AstType, SymbolTable};
use crate::lexer::{Tokenizer, Type};
use crate::parser::Parser;
use anyhow::Result;

pub struct CPP<'a> {
    ast: &'a Ast,
}

impl<'a> CPP<'a> {

    fn repr_ast_ty(&self, ty: AstType) -> Result<String> {
        match ty {
            AstType::Initialize(name) => {
                Ok(format!("{}", self.repr_ast_ty(*name)?))
            }
            AstType::Unknown => {
                Ok("".to_string())
            }
            AstType::SignedInt(_) => {
                Ok("int".to_string())
            }
            AstType::UnsignedInt(_) => {
                Ok("unsigned int".to_string())
            }
            AstType::Float(_) => {
                Ok("float".to_string())
            }
            AstType::Bool => {
                Ok("bool".to_string())
            }
            AstType::Char => {
                Ok("char".to_string())
            }
            AstType::String => {
                Ok("std::string".to_string())
            }
            AstType::Array(_, _) => {
                unreachable!()
            }
            AstType::DynamicArray(_) => {
                unreachable!()
            }
            AstType::TypeName(name) => {
                Ok(name)
            }
            AstType::TypeDefStruct => {
                Ok("".to_string())
            }
            AstType::TypeDefEnum => {
                Ok("".to_string())
            }
            AstType::TypeDefUnion => {
                Ok("".to_string())
            }
            AstType::Ref(name) => {
                Ok(format!("*{}", self.repr_ast_ty(name.deref().clone())?))
            }
            AstType::Deref(name) => {
                Ok(format!("&{}", self.repr_ast_ty(name.deref().clone())?))

            }
            AstType::Void => {
                Ok("void".to_string())
            }
        }
    }
    fn get_def_typ(&self, node: &Node) -> Result<AstType> {
        match &node.data {
            NodeData::Def(def) => {
                
                match def.expr.type_annotation {
                    AstType::Unknown => {
                        println!("type inference shit the bed. {:?}", def);
                        unreachable!();
                    }
                    _ => Ok(def.expr.type_annotation.clone()),
                }

            },
            _ => {
                println!("wrong node passed to ty inference : {:?}", node);
                unreachable!();
            }
        }
    }
    fn repr_block(&self, nodes: &Vec<Node>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in nodes.iter() {
            output.push(format!("\t{};", self.repr(node)?));
        }

        Ok(output.join("\n"))
    }
    fn repr_fn_def_args(&self, node_tys: &Vec<Node>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in node_tys {
            match &node.data {
                NodeData::Decl(name, ty) => {
                    output.push(format!("{} {}", self.repr(&ty)?, self.repr(&name)?));
                }

                _ => {
                    unreachable!()
                }
            }

        }
        Ok(output.join(", "))
    }
    fn repr_struct_fields(&self, node_tys: &Vec<Node>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in node_tys {
            match &node.data {
                NodeData::Decl(name, ty) => {
                    output.push(format!("\t{} {};", self.repr(&ty)?, self.repr(&name)?));
                }
                _ => {
                    unreachable!();
                }
            }

        }
        Ok(output.join("\n"))
    }
    fn repr_struct_init_fields(&self, fields: &Vec<(Node, Node)>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in fields {
            output.push(format!("\t.{}={}", self.repr(&node.0)?, self.repr(&node.1)?));
        }
        Ok(output.join(",\n"))
    }
    fn repr_vec_node(&self, nodes: &Vec<Node>, sep: &str) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in nodes.iter() {
            output.push(self.repr(node)?);
        }

        Ok(output.join(sep))
    }

    fn repr_enum_variants(&self, variants: &Vec<(Node, Option<Node>)>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in variants.iter() {
            output.push(self.repr(&node.0)?);
        }

        Ok(output.join(",\n"))
    }
    fn repr_union_variants(&self, variants: &Vec<(Node, Option<Node>)>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in variants.iter() {
            match &node.1 {
                Some(ty) => {
                    output.push(format!("\t{} {};", self.repr(&ty)?, self.repr(&node.0)?));
                }
                None => {
                    output.push(format!("\t{} {};", "void*", self.repr(&node.0)?));
                }
            }
        }

        Ok(output.join("\n"))
    }
    fn repr_field_access_path(&self, path: &Vec<Node>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in path.iter() {
            output.push(self.repr(&node)?);
        }

        Ok(output.join("."))
    }

    fn repr_operator(&self, op: &Type) -> Result<String> {
        match op {
            Type::LeftAngle => Ok("<".to_string()),
            Type::RightAngle => Ok(">".to_string()),
            Type::LessEqual => Ok("<=".to_string()),
            Type::GreaterEqual => Ok(">=".to_string()),
            Type::DoubleEqual => Ok("==".to_string()),
            Type::NotEqual => Ok("!=".to_string()),
            _ => {
                panic!("unsupported operator: {:?}", op);
           }
        }
    }

    fn repr_array_elems(&self, elems: &Vec<Node>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in elems.iter() {
            output.push(format!("{}", self.repr(&node)?));
        }

        Ok(output.join(","))
    }

    fn repr(&self, node: &Node) -> Result<String> {
        match &node.data {
            NodeData::Host(import) => {
                Ok(format!(
                    "#include \"{}\"",
                    self.ast.get_src_for_token(import.clone())?
                ))
            }
            NodeData::Load(_) => Ok("".to_string()),
            NodeData::Assign(name, val) => {
                Ok(format!("{} = {}", self.repr(name)?, self.repr(val)?))
            }
            NodeData::Decl(name, ty) => {
                Ok(format!("{} {}", self.repr(ty)?, self.repr(name)?))
            }

            NodeData::ForIn(op_name, list, body) => {
                let mut op_name = op_name.clone();
                if op_name.is_none() {
                    op_name = Some(Box::new(Node {
                        id: format!("_{}", -1),
                        data: NodeData::TEXT("it".to_string()),
                        type_annotation: AstType::Unknown,
                    }));
                }
                Ok(format!("for (auto {}: {}) {{\n{}\n}}", self.repr(&op_name.unwrap())?, self.repr(list)?, self.repr_block(body)?))
            }

            NodeData::For(start, cond, cont, body) => {
                Ok(format!("for ({};{};{}) {{\n{}\n}}", self.repr(start)?, self.repr(cond)?, self.repr(cont)?, self.repr_block(body)?))
            }

            NodeData::While(cond, body) => {
                Ok(format!("while ({}) {{\n{}\n}}",  self.repr(cond)?, self.repr_block(body)?))
            }

            NodeData::Def(decl) => match &decl.expr.deref().data {
                NodeData::FnDef(proto, block) => 
                    if let NodeData::FnPrototype(args, ret) = &proto.data {
                        Ok(format!("{} {}({}) {{\n{}\n}}",
                        self.repr(&*ret)?,
                        self.repr(&*decl.name)?,
                        self.repr_fn_def_args(&args)?,
                        self.repr_block(&block)?,))
                    } else {
                        unreachable!()
                    }

                NodeData::InitializeArray(ty, elems) => {
                    if let NodeData::ArrayTy(size, elem_ty) = ty.clone().unwrap().data {
                        match decl.mutable {
                            true => {
                                Ok(format!("const {} {}[{}] = {{{}}}",
                                           self.repr(&elem_ty)?,
                                           self.repr(&decl.name)?,
                                           self.repr(&size)?,
                                           self.repr_array_elems(elems)?
                                ))
                            }
                            false => {
                                Ok(format!("{} {}[{}] = {{{}}}",
                                           self.repr(&elem_ty)?,
                                           self.repr(&decl.name)?,
                                           self.repr(&size)?,
                                           self.repr_array_elems(elems)?
                                ))
                            }
                        }

                    } else {
                        unreachable!();
                    }

                }

                NodeData::Struct(fields) => Ok(format!(
                    "struct {} {{\n{}\n}};",
                    self.repr(&*decl.name)?,
                    self.repr_struct_fields(&fields)?
                )),

                NodeData::Enum(is_union, variants) => {
                    if !is_union {
                        Ok(format!(
                            "enum {} {{\n{}\n}};",
                            self.repr(&*decl.name)?,
                            self.repr_enum_variants(&variants)?
                        ))
                    } else {
                        Ok(format!(
                            "union {} {{\n{}\n}};",
                            self.repr(&*decl.name)?,
                            self.repr_union_variants(&variants)?
                        ))
                    }
                }

                NodeData::Initialize(_, fields) => {
                    let ty = self.get_def_typ(node)?;
                    let fields = self.repr_struct_init_fields(&fields)?;
                    match decl.mutable {
                        false => Ok(format!(
                            "const {} {} = {{\n{}}}",
                            self.repr_ast_ty(ty)?,
                            self.repr(decl.name.deref())?,
                            fields
                        )),
                        true => Ok(format!(
                            "{} {} = {{\n{}}}",
                            self.repr_ast_ty(ty)?,
                            self.repr(decl.name.deref())?,
                            fields,
                        )),
                    }
                }

                NodeData::ContainerField(cf) => {
                    let container = &cf.container;
                    let field = &cf.field;
                    //TODO make type inference work for this when I got hosele
                    let container = self.repr(&container)?;
                    let field = self.repr(&field)?;
                    let mut sep = ".";
                    if cf.container_is_enum {
                        sep = "::";
                    }
                    match decl.mutable {
                        false => Ok(format!(
                            "const auto {} = {}{}{}",
                            self.repr(decl.name.deref())?,
                            container,
                            sep,
                            field
                        )),
                        true => Ok(format!(
                            "auto {} = {}{}{}",
                            self.repr(decl.name.deref())?,
                            container, sep, field,
                        )),
                    }
                }

                _ => {
                    let ty = self.get_def_typ(node)?;
                    match decl.mutable {
                        false => Ok(format!(
                            "const {} {} = {}",
                            self.repr_ast_ty(ty)?,
                            self.repr(decl.name.deref())?,
                            self.repr(decl.expr.deref())?
                        )),
                        true => Ok(format!(
                            "{} {} = {}",
                            self.repr_ast_ty(ty)?,
                            self.repr(decl.name.deref())?,
                            self.repr(decl.expr.deref())?
                        )),
                    }
                }
            },
            NodeData::Initialize(ty, fields) => {
                let ty: String = if ty.is_some() {
                    self.repr(&ty.clone().unwrap())?
                } else {
                    ".".to_string()
                };
                let fields = self.repr_struct_init_fields(&fields)?;
                return Ok(format!("({}){{\n{}\n}}", ty, fields));
            }
            NodeData::InitializeArray(ty, elems) => {
                    if let NodeData::ArrayTy(size, elem_ty) = ty.clone().unwrap().data {
                        Ok(format!("{}[{}]{{{}}}",
                                   self.repr(&elem_ty)?,
                                   self.repr(&size)?,
                                   self.repr_array_elems(elems)?
                        ))
                    } else {
                        unreachable!();
                    }

                }
            
            // primitive types
            NodeData::Uint(tok_idx) => Ok(format!("{}", self.ast.get_src_for_token(*tok_idx)?)),
            NodeData::Int(tok_idx) => Ok(format!("{}", self.ast.get_src_for_token(*tok_idx)?)),
            NodeData::StringLiteral(tok_idx) => {
                Ok(format!("\"{}\"", self.ast.get_src_for_token(*tok_idx)?))
            }
            NodeData::Float(tok_idx) => Ok(format!("{}", self.ast.get_src_for_token(*tok_idx)?)),
            NodeData::True(tok_idx) => Ok(format!("{}", self.ast.get_src_for_token(*tok_idx)?)),
            NodeData::False(tok_idx) => Ok(format!("{}", self.ast.get_src_for_token(*tok_idx)?)),
            NodeData::Char(tok_idx) => Ok(format!("{}", self.ast.get_src_for_token(*tok_idx)?)),
            NodeData::Ident(s) => Ok(s.clone()),
            NodeData::TEXT(s) => Ok(format!("{}", s)),

            // keywords
            NodeData::IntTy(_) => Ok(format!("int")),
            NodeData::Int8Ty(_) => Ok(format!("int8_t")),
            NodeData::Int16Ty(_) => Ok(format!("int16_t")),
            NodeData::Int32Ty(_) => Ok(format!("int32_t")),
            NodeData::Int64Ty(_) => Ok(format!("int64_t")),
            NodeData::Int128Ty(_) => {
                unimplemented!()
            }

            NodeData::UintTy(_) => Ok(format!("unsigned int")),
            NodeData::Uint8Ty(_) => Ok(format!("uint8_t")),
            NodeData::Uint16Ty(_) => Ok(format!("uint16_t")),
            NodeData::Uint32Ty(_) => Ok(format!("uint32_t")),
            NodeData::Uint64Ty(_) => Ok(format!("uint64_t")),
            NodeData::Uint128Ty(_) => {
                unimplemented!();
            }

            NodeData::FloatTy(_) => Ok(format!("long")),
            NodeData::BoolTy(_) => Ok(format!("bool")),
            NodeData::StringTy(_) => Ok(format!("std::string")),
            NodeData::CharTy(_) => Ok(format!("char")),
            NodeData::VoidTy(_) => Ok(format!("void")),

            //Expressions
            NodeData::Sum(lhs, rhs) => Ok(format!("({} + {})", self.repr(&lhs)?, self.repr(&rhs)?)),
            NodeData::Subtract(lhs, rhs) => Ok(format!("({} - {})", self.repr(&lhs)?, self.repr(&rhs)?)),
            NodeData::Multiply(lhs, rhs) => Ok(format!("({} * {})", self.repr(&lhs)?, self.repr(&rhs)?)),
            NodeData::Div(lhs, rhs) => Ok(format!("({} / {})", self.repr(&lhs)?, self.repr(&rhs)?)),
            NodeData::Mod(lhs, rhs) => Ok(format!("({} % {})", self.repr(&lhs)?, self.repr(&rhs)?)),
            NodeData::ContainerField(ref cf) => {
                if cf.container_is_enum {
                    return Ok(format!("{}::{}", self.repr(&cf.container)?, self.repr(&cf.field)?));

                }
                return Ok(format!("{}.{}", self.repr(&cf.container)?, self.repr(&cf.field)?));
            },
            NodeData::Cmp(op, lhs, rhs) => Ok(format!("{} {} {}", self.repr(lhs)?, self.repr_operator(op)?, self.repr(rhs)?)),
            NodeData::FnDef(_, _) => {
                unreachable!();
            }

            NodeData::FnCall(name, args) => Ok(format!(
                "{}({})",
                self.repr(&name)?,
                self.repr_vec_node(&args, ",")?
            )),
            NodeData::If(cond, then, _else) => {
                let mut base = format!(
                    "if ({}) {{\n{}\n\t}}",
                    self.repr(&cond)?,
                    self.repr_block(&then)?
                );

                if _else.is_some() {
                    base += &format!(
                        " else {{\n{}\n\t}}",
                        self.repr_block(&_else.clone().unwrap())?
                    );
                }

                Ok(base)
            }
            NodeData::Return(expr) => Ok(format!("return {}", self.repr(&expr)?)),
            NodeData::CCompilerFlag(_) => { Ok ("".to_string()) },
            _ => {
                println!("unhandled in cpp codegen {:?}", node);
                unreachable!()
            }
        }
    }
    pub fn new(ast: &'a Ast) -> Self {
        Self { ast }
    }

    pub fn generate(&mut self) -> Result<String> {
        let mut out: Vec<String> = vec![];
        for node in self.ast.top_level.iter() {
            out.push(format!("{};", self.repr(&node)?));
        }
        
        Ok(out.join("\n"))
    }
}
