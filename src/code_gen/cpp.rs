use std::ops::Deref;

use super::{AstNode, AstNodeData, Repr, Ast};
use crate::ast::{AstNodeType, SymbolTable, AstOperation};
use crate::lexer::{Tokenizer, TokenType};
use crate::parser::Parser;
use anyhow::Result;

pub struct CPP<'a> {
    ast: &'a Ast,
}

impl<'a> CPP<'a> {
    fn repr_ast_op(&self, op: &AstOperation) -> Result<String> {
        match op {
            AstOperation::Sum => Ok("+".to_string()),
            AstOperation::Subtract => Ok("-".to_string()),
            AstOperation::Divide => Ok("/".to_string()),
            AstOperation::Modulu => Ok("%".to_string()),
            AstOperation::Multiply => Ok("*".to_string()),
            AstOperation::MemberAcces => Ok(".".to_string()),
            AstOperation::Greater =>  Ok(">".to_string()),
            AstOperation::GreaterEqual =>  Ok(">=".to_string()),
            AstOperation::Less =>  Ok("<".to_string()),
            AstOperation::LessEqual =>  Ok("<=".to_string()),
            AstOperation::Equal =>  Ok("==".to_string()),
            AstOperation::NotEqual =>  Ok("!=".to_string()),
        }
    }
    fn repr_ast_ty(&self, ty: AstNodeType) -> Result<String> {
        match ty {
            AstNodeType::NoType => {
                unreachable!()
            },
            AstNodeType::Initialize(name) => {
                Ok(format!("{}", self.repr_ast_ty(*name)?))
            }
            AstNodeType::Unknown => {
                Ok("UNKNOWN".to_string())
            }
            AstNodeType::SignedInt(_) => {
                Ok("int".to_string())
            }
            AstNodeType::UnsignedInt(_) => {
                Ok("unsigned int".to_string())
            }
            AstNodeType::Float(_) => {
                Ok("float".to_string())
            }
            AstNodeType::Bool => {
                Ok("bool".to_string())
            }
            AstNodeType::Char => {
                Ok("char".to_string())
            }
            AstNodeType::String => {
                Ok("std::string".to_string())
            }
            AstNodeType::Array(_, _) => {
                unreachable!()
            }
            AstNodeType::DynamicArray(_) => {
                unreachable!()
            }
            AstNodeType::TypeName(name) => {
                Ok(name)
            }
            AstNodeType::TypeDefStruct => {
                Ok("".to_string())
            }
            AstNodeType::TypeDefEnum => {
                Ok("".to_string())
            }
            AstNodeType::TypeDefUnion => {
                Ok("".to_string())
            }
            AstNodeType::Ref(name) => {
                Ok(format!("*{}", self.repr_ast_ty(name.deref().clone())?))
            }
            AstNodeType::Deref(name) => {
                Ok(format!("&{}", self.repr_ast_ty(name.deref().clone())?))

            }
            AstNodeType::Void => {
                Ok("void".to_string())
            }

            AstNodeType::FnType(_, _) => {unreachable!()},
        }
    }
    fn get_def_typ(&self, node: &AstNode) -> Result<AstNodeType> {
        match &node.data {
            AstNodeData::Def(def) => {
                
                match def.expr.infered_type {
                    AstNodeType::Unknown => {
                        println!("type inference shit the bed. {:?}", def);
                        unreachable!();
                    }
                    _ => Ok(def.expr.infered_type.clone()),
                }

            },
            _ => {
                println!("wrong node passed to ty inference : {:?}", node);
                unreachable!();
            }
        }
    }
    fn repr_block(&self, nodes: &Vec<AstNode>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in nodes.iter() {
            output.push(format!("\t{};", self.repr_ast_node(node)?));
        }

        Ok(output.join("\n"))
    }
    fn repr_fn_def_args(&self, node_tys: &Vec<AstNode>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in node_tys {
            match &node.data {
                AstNodeData::Ident(name) => {
                    output.push(format!("{} {}", self.repr_ast_ty(node.infered_type.clone())?, name));
                }

                _ => {
                    unreachable!()
                }
            }

        }
        Ok(output.join(", "))
    }
    fn repr_struct_fields(&self, node_tys: &Vec<AstNode>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in node_tys {
            match &node.data {
                AstNodeData::Decl(name) => {
                    output.push(format!("\t{} {};", self.repr_ast_ty(node.infered_type.clone())?, name));
                }
                _ => {
                    unreachable!();
                }
            }

        }
        Ok(output.join("\n"))
    }
    fn repr_struct_init_fields(&self, fields: &Vec<(AstNode, AstNode)>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in fields {
            output.push(format!("\t.{}={}", self.repr_ast_node(&node.0)?, self.repr_ast_node(&node.1)?));
        }
        Ok(output.join(",\n"))
    }
    fn repr_vec_node(&self, nodes: &Vec<AstNode>, sep: &str) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in nodes.iter() {
            output.push(self.repr_ast_node(node)?);
        }

        Ok(output.join(sep))
    }

    fn repr_enum_variants(&self, variants: &Vec<(AstNode, Option<AstNode>)>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in variants.iter() {
            output.push(self.repr_ast_node(&node.0)?);
        }

        Ok(output.join(",\n"))
    }
    fn repr_union_variants(&self, variants: &Vec<(AstNode, Option<AstNode>)>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in variants.iter() {
            match &node.1 {
                Some(ty) => {
                    output.push(format!("\t{} {};", self.repr_ast_node(&ty)?, self.repr_ast_node(&node.0)?));
                }
                None => {
                    output.push(format!("\t{} {};", "void*", self.repr_ast_node(&node.0)?));
                }
            }
        }

        Ok(output.join("\n"))
    }
    fn repr_field_access_path(&self, path: &Vec<AstNode>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in path.iter() {
            output.push(self.repr_ast_node(&node)?);
        }

        Ok(output.join("."))
    }

    fn repr_operator(&self, op: &TokenType) -> Result<String> {
        match op {
            TokenType::LeftAngle => Ok("<".to_string()),
            TokenType::RightAngle => Ok(">".to_string()),
            TokenType::LessEqual => Ok("<=".to_string()),
            TokenType::GreaterEqual => Ok(">=".to_string()),
            TokenType::DoubleEqual => Ok("==".to_string()),
            TokenType::NotEqual => Ok("!=".to_string()),
            _ => {
                panic!("unsupported operator: {:?}", op);
           }
        }
    }

    fn repr_array_elems(&self, elems: &Vec<AstNode>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in elems.iter() {
            output.push(format!("{}", self.repr_ast_node(&node)?));
        }

        Ok(output.join(","))
    }

    fn repr_ast_node(&self, node: &AstNode) -> Result<String> {
        match &node.data {
            AstNodeData::Host(import) => {
                Ok(format!(
                    "#include \"{}\"",
                    import
                ))
            }
            AstNodeData::Load(_) => Ok("".to_string()),
            AstNodeData::Assign(name, val) => {
                Ok(format!("{} = {}", self.repr_ast_node(name)?, self.repr_ast_node(val)?))
            }
            AstNodeData::Decl(name) => {
                Ok(format!("{} {}", self.repr_ast_ty(node.infered_type.clone())?, name))
            }

            AstNodeData::ForIn(op_name, list, body) => {
                let mut op_name = op_name.clone();
                if op_name.is_none() {
                    op_name = Some(Box::new(AstNode {
                        id: format!("_{}", -1),
                        data: AstNodeData::Ident("it".to_string()),
                        infered_type: AstNodeType::Unknown,
                    }));
                }
                Ok(format!("for (auto {}: {}) {{\n{}\n}}", self.repr_ast_node(&op_name.unwrap())?, self.repr_ast_node(list)?, self.repr_block(body)?))
            }

            AstNodeData::For(start, cond, cont, body) => {
                Ok(format!("for ({};{};{}) {{\n{}\n}}", self.repr_ast_node(start)?, self.repr_ast_node(cond)?, self.repr_ast_node(cont)?, self.repr_block(body)?))
            }

            AstNodeData::While(cond, body) => {
                Ok(format!("while ({}) {{\n{}\n}}",  self.repr_ast_node(cond)?, self.repr_block(body)?))
            }

            AstNodeData::Def(def) => match &def.expr.deref().data {
                AstNodeData::FnDef(ref fn_def) => 
                    Ok(format!("{} {}({}) {{\n{}\n}}",
                        self.repr_ast_node(&*fn_def.sign.ret)?,
                        def.name,
                        self.repr_fn_def_args(&fn_def.sign.args)?,
                        self.repr_block(&fn_def.body)?
                    )),
                

                AstNodeData::InitializeArray(ty, elems) => {
                    if let AstNodeData::ArrayTy(size, elem_ty) = ty.clone().unwrap().data {
                        match def.mutable {
                            true => {
                                Ok(format!("const {} {}[{}] = {{{}}}",
                                           self.repr_ast_ty(elem_ty)?,
                                           def.name,
                                           size,
                                           self.repr_array_elems(elems)?
                                ))
                            }
                            false => {
                                Ok(format!("{} {}[{}] = {{{}}}",
                                           self.repr_ast_ty(elem_ty)?,
                                           def.name,
                                           size,
                                           self.repr_array_elems(elems)?
                                ))
                            }
                        }

                    } else {
                        unreachable!();
                    }

                }

                AstNodeData::Struct(fields) => Ok(format!(
                    "struct {} {{\n{}\n}};",
                    def.name,
                    self.repr_struct_fields(&fields)?
                )),

                AstNodeData::Enum(is_union, variants) => {
                    if !is_union {
                        Ok(format!(
                            "enum class {} {{\n{}\n}};",
                            def.name,
                            self.repr_enum_variants(&variants)?
                        ))
                    } else {
                        Ok(format!(
                            "union {} {{\n{}\n}};",
                            def.name,
                            self.repr_union_variants(&variants)?
                        ))
                    }
                }

                AstNodeData::Initialize(_, fields) => {
                    let ty = self.get_def_typ(node)?;
                    let fields = self.repr_struct_init_fields(&fields)?;
                    match def.mutable {
                        false => Ok(format!(
                            "const {} {} = {{\n{}}}",
                            self.repr_ast_ty(ty)?,
                            def.name,
                            fields
                        )),
                        true => Ok(format!(
                            "{} {} = {{\n{}}}",
                            self.repr_ast_ty(ty)?,
                            def.name,
                            fields,
                        )),
                    }
                }

                AstNodeData::ContainerField(cf) => {
                    let container = &cf.container;
                    let field = &cf.field;
                    //TODO make type inference work for this when I got hosele
                    let container = self.repr_ast_node(&container)?;
                    let field = self.repr_ast_node(&field)?;
                    let mut sep = ".";
                    if cf.container_is_enum {
                        sep = "::";
                    }
                    match def.mutable {
                        false => Ok(format!(
                            "const auto {} = {}{}{}",
                            def.name,
                            container,
                            sep,
                            field
                        )),
                        true => Ok(format!(
                            "auto {} = {}{}{}",
                            def.name,
                            container, sep, field,
                        )),
                    }
                }

                _ => {
                    let ty = self.get_def_typ(node)?;
                    match def.mutable {
                        false => Ok(format!(
                            "const {} {} = {}",
                            self.repr_ast_ty(ty)?,
                            def.name,
                            self.repr_ast_node(def.expr.deref())?
                        )),
                        true => Ok(format!(
                            "{} {} = {}",
                            self.repr_ast_ty(ty)?,
                            def.name,
                            self.repr_ast_node(def.expr.deref())?
                        )),
                    }
                }
            },
            AstNodeData::Initialize(ty, fields) => {
                let fields = self.repr_struct_init_fields(&fields)?;
                return Ok(format!("({}){{\n{}\n}}", self.repr_ast_node(ty)?, fields));
            }
            AstNodeData::InitializeArray(ty, elems) => {
                    if let AstNodeData::ArrayTy(size, elem_ty) = ty.clone().unwrap().data {
                        Ok(format!("{}[{}]{{{}}}",
                                   self.repr_ast_ty(elem_ty)?,
                                   size,
                                   self.repr_array_elems(elems)?
                        ))
                    } else {
                        unreachable!();
                    }

                }
            
            // primitive types
            AstNodeData::Uint(number) => Ok(format!("{}", number)),
            AstNodeData::Int(number) => Ok(format!("{}", number)),
            AstNodeData::StringLiteral(s) => {
                Ok(format!("\"{}\"", s))
            }
            AstNodeData::Float(f) => Ok(format!("{}",f)),
            AstNodeData::Bool(b) => Ok(format!("{}", b)),
            AstNodeData::Char(c) => Ok(format!("{}", c)),
            AstNodeData::Ident(s) => Ok(s.clone()),

            // keywords
            AstNodeData::IntTy => Ok(format!("int")),
            AstNodeData::Int8Ty => Ok(format!("int8_t")),
            AstNodeData::Int16Ty => Ok(format!("int16_t")),
            AstNodeData::Int32Ty => Ok(format!("int32_t")),
            AstNodeData::Int64Ty => Ok(format!("int64_t")),
            AstNodeData::Int128Ty => {
                unimplemented!()
            }

            AstNodeData::UintTy => Ok(format!("unsigned int")),
            AstNodeData::Uint8Ty => Ok(format!("uint8_t")),
            AstNodeData::Uint16Ty => Ok(format!("uint16_t")),
            AstNodeData::Uint32Ty => Ok(format!("uint32_t")),
            AstNodeData::Uint64Ty => Ok(format!("uint64_t")),
            AstNodeData::Uint128Ty => {
                unimplemented!();
            }

            AstNodeData::FloatTy => Ok(format!("long")),
            AstNodeData::BoolTy => Ok(format!("bool")),
            AstNodeData::StringTy => Ok(format!("std::string")),
            AstNodeData::CharTy => Ok(format!("char")),
            AstNodeData::VoidTy => Ok(format!("void")),

            //Expressions
            AstNodeData::BinaryOperation(ref binary_operation) => {
                Ok(format!("{} {} {}", self.repr_ast_node(&*binary_operation.left)?, self.repr_ast_op(&binary_operation.operation)?, self.repr_ast_node(&*binary_operation.right)?))
            }
            AstNodeData::ContainerField(ref cf) => {
                if cf.container_is_enum {
                    return Ok(format!("{}::{}", self.repr_ast_node(&cf.container)?, self.repr_ast_node(&cf.field)?));

                }
                return Ok(format!("{}.{}", self.repr_ast_node(&cf.container)?, self.repr_ast_node(&cf.field)?));
            },
            AstNodeData::FnDef(_) => {
                unreachable!();
            }

            AstNodeData::FnCall(name, args) => Ok(format!(
                "{}({})",
                self.repr_ast_node(&name)?,
                self.repr_vec_node(&args, ",")?
            )),
            AstNodeData::If(ref ast_if) => {
                let mut cases = Vec::<String>::new();
                cases.push(format!("if ({} == true) {{\n{}\n}}", self.repr_ast_node(&ast_if.cases[0].0)?, self.repr_block(&ast_if.cases[0].1)?));
                for case in &ast_if.cases[1..] {
                    cases.push(format!("else if ({} == true) {{\n{}\n}}", self.repr_ast_node(&case.0)?, self.repr_block(&case.1)?))
                }
                Ok(cases.join("\n"))
            }
            AstNodeData::Return(expr) => Ok(format!("return {}", self.repr_ast_node(&expr)?)),
            AstNodeData::Break => Ok(format!("break")),
            AstNodeData::CompilerFlags(_) => { Ok ("".to_string()) },
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
            out.push(format!("{};", self.repr_ast_node(&node)?));
        }
        
        Ok(out.join("\n"))
    }
}
