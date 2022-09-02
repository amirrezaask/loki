use std::collections::HashMap;
use std::ops::Deref;

use super::{AstNode, AstNodeData, Repr, Ast};
use crate::ast::{Type, AstOperation, NodeID, AstTag};
use crate::lexer::{Tokenizer, TokenType};
use crate::parser::Parser;
use anyhow::anyhow;
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
            AstOperation::Greater =>  Ok(">".to_string()),
            AstOperation::GreaterEqual =>  Ok(">=".to_string()),
            AstOperation::Less =>  Ok("<".to_string()),
            AstOperation::LessEqual =>  Ok("<=".to_string()),
            AstOperation::Equal =>  Ok("==".to_string()),
            AstOperation::NotEqual =>  Ok("!=".to_string()),
            AstOperation::BinaryOr =>  Ok("||".to_string()),
            AstOperation::BinaryAnd =>  Ok("&&".to_string()),
        }
    }
    fn repr_ast_ty(&self, ty: Type) -> Result<String> {
        match ty {
            Type::LoadedFile => {
                Ok("".to_string())
            }
            Type::TypeRef { ref name, actual_ty } => {
                Ok(name.to_string())
            }
            Type::NoType => {
                unreachable!()
            },
            Type::Initialize(name) => {
                Ok(format!("{}", self.repr_ast_ty(*name)?))
            }
            Type::Unknown => {
                Ok("UNKNOWN".to_string())
            }
            Type::SignedInt(_) => {
                Ok("int".to_string())
            }
            Type::CVarArgs => {
                Ok("...".to_string())
            }
            Type::CString => {
                Ok("char*".to_string())
            }
            Type::UnsignedInt(_) => {
                Ok("unsigned int".to_string())
            }
            Type::Float(_) => {
                Ok("float".to_string())
            }
            Type::Bool => {
                Ok("bool".to_string())
            }
            Type::Char => {
                Ok("char".to_string())
            }
            Type::String => {
                Ok("std::string".to_string())
            }
            Type::Array(_, _) => {
                unreachable!()
            }
            Type::DynamicArray(_) => {
                unreachable!()
            }
            Type::Struct{fields} => {
                Ok(format!("struct {{\n{}\n}}", self.repr_struct_fields_for_type(fields)?))
            }
            Type::Enum{variants: _} => {
                Ok("".to_string())
            }
            Type::Union => {
                Ok("".to_string())
            }
            Type::Pointer(name) => {
                Ok(format!("{}*", self.repr_ast_ty(name.deref().clone())?))
            }
            Type::NamespaceAccess(nsa) => {
                Ok(self.repr_ast_ty(nsa.field.deref().clone())?)
            }
            Type::Void => {
                Ok("void".to_string())
            }

            Type::FnType(_, _) => {unreachable!()},
        }
    }
    fn get_def_typ(&self, node: &AstNode) -> Result<Type> {
        match &node.data {
            AstNodeData::Def{mutable, name, expr} => {
                let def_expr = self.ast.get_node(expr.clone())?;
                match def_expr.type_information {
                    Type::Unknown => {
                        let expr = self.ast.get_node(expr.clone())?;
                        println!("type inference bug, type of {:?} is not declared", expr);
                        unreachable!();
                    }
                    _ => Ok(def_expr.type_information.clone()),
                }

            },
            _ => {
                println!("wrong node passed to ty inference : {:?}", node);
                unreachable!();
            }
        }
    }
    fn repr_block(&self, id: &NodeID) -> Result<String> {
        let mut output = Vec::<String>::new();
        let nodes = self.ast.get_node(id.to_string())?.get_block()?;
        for node in nodes.iter() {
            output.push(format!("\t{};", self.repr_ast_node(node.clone())?));
        }

        Ok(output.join("\n"))
    }
    fn repr_fn_def_args(&self, node_tys: &Vec<NodeID>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node_id in node_tys {
            let node = self.ast.get_node(node_id.clone())?;
            let ident_node_id = node.get_decl()?;
            let ident_node = self.ast.get_node(ident_node_id)?;
            let name = ident_node.get_ident()?;
            output.push(format!("{} {}", self.repr_ast_ty(node.type_information.clone())?, name));
        }
        Ok(output.join(", "))
    }
    fn repr_struct_fields_for_type(&self, fields: Vec<(String, Type)>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for (name, ty) in fields {
            output.push(format!("\t{} {};", self.repr_ast_ty(ty)?, name));
        }


        Ok(output.join(";\n"))

    }
    fn repr_struct_fields(&self, node_tys: &Vec<NodeID>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node_id in node_tys {
            let node = self.ast.get_node(node_id.clone())?;
            match &node.data {
                AstNodeData::Decl(name_id) => {
                    output.push(format!("\t{} {};", self.repr_ast_ty(node.type_information.clone())?, self.repr_ast_node(name_id.clone())?));
                }
                _ => {
                    unreachable!();
                }
            }

        }
        Ok(output.join("\n"))
    }
    fn repr_struct_init_fields(&self, mut ty: Type, fields: &Vec<(NodeID, NodeID)>) -> Result<String> {
        let mut output = vec!["".to_string(); fields.len()];
        if ty.is_type_ref() {
            ty = ty.get_actual_ty_type_ref()?;
        }
        let ordered_struct_fields = ty.get_struct_fields()?;
        for node in fields {
            let name = self.ast.get_node(node.0.clone())?.get_ident()?;
            let idx = ordered_struct_fields.iter().position(|f| f.0 == name).unwrap();
            output[idx] =  format!("\t{}", self.repr_ast_node(node.1.clone())?);
        }
        Ok(output.join(",\n"))
    }
    fn repr_vec_node(&self, nodes: &Vec<NodeID>, sep: &str) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in nodes.iter() {
            output.push(self.repr_ast_node(node.clone())?);
        }

        Ok(output.join(sep))
    }

    fn repr_enum_variants(&self, variants: &Vec<NodeID>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for decl_id in variants.iter() {
            let decl_node = self.ast.get_node(decl_id.clone())?;
            let ident_id = decl_node.get_name_for_defs_and_decls().unwrap();
            output.push(self.repr_ast_node(ident_id.clone())?);
        }

        Ok(output.join(",\n"))
    }
    fn repr_union_variants(&self, variants: &Vec<(NodeID, Option<NodeID>)>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in variants.iter() {
            match &node.1 {
                Some(ty) => {
                    output.push(format!("\t{} {};", self.repr_ast_node(ty.clone())?, self.repr_ast_node(node.0.clone())?));
                }
                None => {
                    output.push(format!("\t{} {};", "void*", self.repr_ast_node(node.0.clone())?));
                }
            }
        }

        Ok(output.join("\n"))
    }
    fn repr_field_access_path(&self, path: &Vec<NodeID>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in path.iter() {
            output.push(self.repr_ast_node(node.clone())?);
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
                Err(anyhow!("unsupported operator: {:?}", op))
           }
        }
    }

    fn repr_namespace_access(&self, namespace: &str, field: &str) -> Result<String> {
        let ns_ty = self.ast.get_node(namespace.to_string())?;
        let _ = self.ast.get_node(field.to_string())?;
        if ns_ty.type_information.is_enum_def() {
            return Ok(format!("{}::{}", self.repr_ast_node(namespace.to_string())?, self.repr_ast_node(field.to_string())?));
        }

        if ns_ty.type_information.is_pointer() {
            return Ok(format!("{}->{}", self.repr_ast_node(namespace.to_string())?, self.repr_ast_node(field.to_string())?));
        }

        return Ok(format!("{}.{}", self.repr_ast_node(namespace.to_string())?, self.repr_ast_node(field.to_string())?));
    }

    fn repr_array_elems(&self, elems: &Vec<NodeID>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in elems.iter() {
            output.push(format!("{}", self.repr_ast_node(node.clone())?));
        }

        Ok(output.join(","))
    }

    fn repr_ast_node(&self, node_id: NodeID) -> Result<String> {
        let node = self.ast.get_node(node_id)?;
        match &node.data {
            AstNodeData::Host(import) => {
                Ok(format!(
                    "#include \"{}\"",
                    import
                ))
            }
            AstNodeData::Load(_) => Ok("".to_string()),
            AstNodeData::Assign{lhs: name, rhs: val} => {
                Ok(format!("{} = {}", self.repr_ast_node(name.clone())?, self.repr_ast_node(val.clone())?))
            }
            AstNodeData::Decl(name) => {
                if node.tags.contains(&AstTag::Foreign) || node.tags.contains(&AstTag::NoCodeGen) {
                    return Ok("".to_string());
                }
                Ok(format!("{} {}", self.repr_ast_ty(node.type_information.clone())?, self.repr_ast_node(name.clone())?))
            }

            AstNodeData::ForIn{iterator, iterable, body} => {
                let ty = self.ast.get_node(iterator.clone())?.type_information;
                Ok(format!("for ({} {}: {}) {{\n{}\n}}", self.repr_ast_ty(ty)?, self.repr_ast_node(iterator.clone())?, self.repr_ast_node(iterable.clone())?, self.repr_block(body)?))
            }

            AstNodeData::For{start, cond, cont, body} => {
                Ok(format!("for ({};{};{}) {{\n{}\n}}", self.repr_ast_node(start.clone())?, self.repr_ast_node(cond.clone())?, self.repr_ast_node(cont.clone())?, self.repr_block(body)?))
            }

            AstNodeData::While{cond, body} => {
                Ok(format!("while ({}) {{\n{}\n}}",  self.repr_ast_node(cond.clone())?, self.repr_block(body)?))
            }

            AstNodeData::Def{mutable, name, expr} => {
                let def_expr = self.ast.get_node(expr.clone())?;
                match &def_expr.data {
                    AstNodeData::InitializeArray { elements } => {
                        let expr = self.ast.get_node(expr.clone())?;
                        let array_elems = expr.get_array_elems()?;
                        Ok(format!("{} {}[{}] = {{{}}}", 
                        self.repr_ast_ty(expr.type_information.get_array_ty_elem_ty()?)?,
                        self.repr_ast_node(name.clone())?,
                        expr.type_information.get_array_ty_size()?,
                        self.repr_array_elems(&array_elems)?
                    ))
                    }
                    AstNodeData::FnDef{ sign: sign_id, ref body} => { 
                        let sign = self.ast.get_node(sign_id.to_string())?;
                        let (args, ret) = sign.get_fn_signature()?;
                        Ok(format!("{} {}({}) {{\n{}\n}}",
                            self.repr_ast_node(ret.clone())?,
                            self.repr_ast_node(name.clone())?,
                            self.repr_fn_def_args(&args)?,
                            self.repr_block(body)?
                        ))
                    },
                    

                    AstNodeData::StructTy(fields) => Ok(format!(
                        "struct {} {{\n{}\n}};",
                        self.repr_ast_node(name.clone())?,
                        self.repr_struct_fields(fields)?
                    )),

                    AstNodeData::EnumTy(variants) => {
                        Err(anyhow!("compiler error: enums should be lowerd into constants by this point but we got: {:?}", node))
                    }

                    AstNodeData::Initialize{ty: _, fields} => {
                        let ty = self.get_def_typ(&node)?;
                        let fields = self.repr_struct_init_fields(ty.clone(), &fields)?;
                        match mutable {
                            false => Ok(format!(
                                "const {} {} = {{\n{}}}",
                                self.repr_ast_ty(ty)?,
                                self.repr_ast_node(name.clone())?,
                                fields
                            )),
                            true => Ok(format!(
                                "{} {} = {{\n{}}}",
                                self.repr_ast_ty(ty)?,
                                self.repr_ast_node(name.clone())?,
                                fields,
                            )),
                        }
                    }

                    // AstNodeData::NamespaceAccess(nsa) => {
                    //     Ok(self.repr_namespace_access(nsa)?)
                    // }

                    _ => {
                        let ty = self.get_def_typ(&node)?;
                        match mutable {
                            false => Ok(format!(
                                "const {} {} = {}",
                                self.repr_ast_ty(ty)?,
                                self.repr_ast_node(name.clone())?,
                                self.repr_ast_node(expr.clone())?
                            )),
                            true => Ok(format!(
                                "{} {} = {}",
                                self.repr_ast_ty(ty)?,
                                self.repr_ast_node(name.clone())?,
                                self.repr_ast_node(expr.clone())?
                            )),
                        }
                    }
                }
            },
            AstNodeData::Initialize{ty, fields} => {
                let fields = self.repr_struct_init_fields(node.type_information, &fields)?;
                return Ok(format!("({}){{\n{}\n}}", self.repr_ast_node(ty.clone())?, fields));
            }
            
            // primitive types
            AstNodeData::Unsigned(number) => Ok(format!("{}", number)),
            AstNodeData::Signed(number) => Ok(format!("{}", number)),
            AstNodeData::StringLiteral(s) => {
                Ok(format!("\"{}\"", s))
            }
            AstNodeData::Float(f) => Ok(format!("{}",f)),
            AstNodeData::Bool(b) => Ok(format!("{}", b)),
            AstNodeData::Char(c) => Ok(format!("'{}'", c)),
            AstNodeData::Ident(s) => Ok(s.clone()),

            AstNodeData::Deref(ptr) => {
                Ok(format!("(*{})", self.repr_ast_node(ptr.clone())?))
            }

            AstNodeData::PointerTo(obj) => {
                Ok(format!("&{}", self.repr_ast_node(obj.clone())?))
            }

            AstNodeData::IntTy(size) => Ok(format!("int")),

            AstNodeData::UintTy(size) => Ok(format!("unsigned int")),
            
            AstNodeData::FloatTy(size) => Ok(format!("long")),

            AstNodeData::BoolTy => Ok(format!("bool")),
            AstNodeData::StringTy => Ok(format!("std::string")),
            AstNodeData::CharTy => Ok(format!("char")),
            AstNodeData::VoidTy => Ok(format!("void")),

            //Expressions
            AstNodeData::BinaryOperation {operation, left, right } => {
                Ok(format!("{} {} {}", self.repr_ast_node(left.clone())?, self.repr_ast_op(&operation)?, self.repr_ast_node(right.clone())?))
            }
            AstNodeData::NamespaceAccess{ ref namespace, ref field} => {
                Ok(self.repr_namespace_access(namespace, field)?)
            },

            AstNodeData::ArrayIndex { ref arr, ref idx } => {
                Ok(format!("{}[{}]", self.repr_ast_node(arr.to_string())?, self.repr_ast_node(idx.to_string())?))
            }
            AstNodeData::FnDef{sign: _, body: _} => {
                unreachable!();
            }

            AstNodeData::FnCall{fn_name: name, args} => Ok(format!(
                "{}({})",
                self.repr_ast_node(name.clone())?,
                self.repr_vec_node(&args, ",")?
            )),
            AstNodeData::If { ref cases } => {
                let mut _cases = Vec::<String>::new();
                _cases.push(format!("if ({} == true) {{\n{}\n}}", self.repr_ast_node(cases[0].0.clone())?, self.repr_block(&cases[0].1)?));
                for case in &cases[1..] {
                    _cases.push(format!("else if ({} == true) {{\n{}\n}}", self.repr_ast_node(case.0.clone())?, self.repr_block(&case.1)?))
                }
                Ok(_cases.join("\n"))
            }
            AstNodeData::Return(expr) => Ok(format!("return {}", self.repr_ast_node(expr.clone())?)),
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
        let top_leve_nodes = self.ast.get_node(self.ast.top_level.clone())?.get_block()?;
        for node in top_leve_nodes.iter() {
            let repr = self.repr_ast_node(node.clone())?;
            if repr == "" {
                continue;
            }
            out.push(format!("{};", repr));
        }
        
        Ok(out.join("\n"))
    }
}
