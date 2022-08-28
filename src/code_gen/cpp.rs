use std::ops::Deref;

use super::{AstNode, AstNodeData, Repr, Ast};
use crate::compiler::Compiler;
use crate::ast::{AstNodeType, AstOperation, NodeID, AstTag};
use crate::lexer::{Tokenizer, TokenType};
use crate::parser::Parser;
use anyhow::anyhow;
use anyhow::Result;

pub struct CPP<'a> {
    ast: &'a Ast,
    node_manager: &'a Compiler,
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
    fn repr_ast_ty(&self, ty: AstNodeType) -> Result<String> {
        match ty {
            AstNodeType::LoadedFile => {
                Ok("".to_string())
            }
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
            AstNodeType::CVarArgs => {
                Ok("...".to_string())
            }
            AstNodeType::CString => {
                Ok("char*".to_string())
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
            AstNodeType::Struct => {
                Ok("".to_string())
            }
            AstNodeType::Enum => {
                Ok("".to_string())
            }
            AstNodeType::Union => {
                Ok("".to_string())
            }
            AstNodeType::Pointer(name) => {
                Ok(format!("{}*", self.repr_ast_ty(name.deref().clone())?))
            }
            AstNodeType::NamespaceAccess(nsa) => {
                Ok(self.repr_ast_ty(nsa.field.deref().clone())?)
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
                let def_expr = self.node_manager.get_node(def.expr.clone())?;
                match def_expr.infered_type {
                    AstNodeType::Unknown => {
                        println!("type inference shit the bed. {:?}", def);
                        unreachable!();
                    }
                    _ => Ok(def_expr.infered_type.clone()),
                }

            },
            _ => {
                println!("wrong node passed to ty inference : {:?}", node);
                unreachable!();
            }
        }
    }
    fn repr_block(&self, nodes: &Vec<NodeID>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in nodes.iter() {
            output.push(format!("\t{};", self.repr_ast_node(node.clone())?));
        }

        Ok(output.join("\n"))
    }
    fn repr_fn_def_args(&self, node_tys: &Vec<NodeID>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node_id in node_tys {
            let node = self.node_manager.get_node(node_id.clone())?;
            let ident_node_id = node.get_decl()?;
            let ident_node = self.node_manager.get_node(ident_node_id)?;
            let name = ident_node.get_ident()?;
            output.push(format!("{} {}", self.repr_ast_ty(node.infered_type.clone())?, name));
        }
        Ok(output.join(", "))
    }
    fn repr_struct_fields(&self, node_tys: &Vec<NodeID>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node_id in node_tys {
            let node = self.node_manager.get_node(node_id.clone())?;
            match &node.data {
                AstNodeData::Decl(name_id) => {
                    output.push(format!("\t{} {};", self.repr_ast_ty(node.infered_type.clone())?, self.repr_ast_node(name_id.clone())?));
                }
                _ => {
                    unreachable!();
                }
            }

        }
        Ok(output.join("\n"))
    }
    fn repr_struct_init_fields(&self, fields: &Vec<(NodeID, NodeID)>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in fields {
            output.push(format!("\t{}:{}", self.repr_ast_node(node.0.clone())?, self.repr_ast_node(node.1.clone())?));
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

    fn repr_enum_variants(&self, variants: &Vec<(NodeID, Option<NodeID>)>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for (decl_id, _) in variants.iter() {
            let decl_node = self.node_manager.get_node(decl_id.clone())?;
            let ident_id = decl_node.get_name_for_defs_and_decls(&self.node_manager).unwrap();
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
        let ns_ty = self.node_manager.get_node(namespace.to_string())?;
        let _ = self.node_manager.get_node(field.to_string())?;
        if ns_ty.infered_type.is_enum() {
            return Ok(format!("{}::{}", self.repr_ast_node(namespace.to_string())?, self.repr_ast_node(field.to_string())?));
        }

        if ns_ty.infered_type.is_pointer() {
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
        let node = self.node_manager.get_node(node_id)?;
        match &node.data {
            AstNodeData::Host(import) => {
                Ok(format!(
                    "#include \"{}\"",
                    import
                ))
            }
            AstNodeData::Load(_) => Ok("".to_string()),
            AstNodeData::Assign(name, val) => {
                Ok(format!("{} = {}", self.repr_ast_node(name.clone())?, self.repr_ast_node(val.clone())?))
            }
            AstNodeData::Decl(name) => {
                if node.tags.contains(&AstTag::Foreign) {
                    return Ok("".to_string());
                }
                Ok(format!("{} {}", self.repr_ast_ty(node.infered_type.clone())?, self.repr_ast_node(name.clone())?))
            }

            AstNodeData::ForIn(op_name, list, body) => {
                Ok(format!("for (auto {}: {}) {{\n{}\n}}", self.repr_ast_node(op_name.clone())?, self.repr_ast_node(list.clone())?, self.repr_block(body)?))
            }

            AstNodeData::For(start, cond, cont, body) => {
                Ok(format!("for ({};{};{}) {{\n{}\n}}", self.repr_ast_node(start.clone())?, self.repr_ast_node(cond.clone())?, self.repr_ast_node(cont.clone())?, self.repr_block(body)?))
            }

            AstNodeData::While(cond, body) => {
                Ok(format!("while ({}) {{\n{}\n}}",  self.repr_ast_node(cond.clone())?, self.repr_block(body)?))
            }

            AstNodeData::Def(def) => {
                let def_expr = self.node_manager.get_node(def.expr.clone())?;
                match &def_expr.data {
                
                AstNodeData::FnDef{ sign: sign_id, body} => { 
                    let sign = self.node_manager.get_node(sign_id.to_string())?;
                    let (args, ret) = sign.get_fn_signature()?;
                    Ok(format!("{} {}({}) {{\n{}\n}}",
                        self.repr_ast_node(ret.clone())?,
                        self.repr_ast_node(def.name.clone())?,
                        self.repr_fn_def_args(&args)?,
                        self.repr_block(&body)?
                    ))
                },
                

                AstNodeData::Struct(fields) => Ok(format!(
                    "struct {} {{\n{}\n}};",
                    self.repr_ast_node(def.name.clone())?,
                    self.repr_struct_fields(fields)?
                )),

                AstNodeData::Enum(is_union, variants) => {
                    if !is_union {
                        Ok(format!(
                            "enum {} {{\n{}\n}};",
                            self.repr_ast_node(def.name.clone())?,
                            self.repr_enum_variants(&variants)?
                        ))
                    } else {
                        Ok(format!(
                            "union {} {{\n{}\n}};",
                            self.repr_ast_node(def.name.clone())?,
                            self.repr_union_variants(&variants)?
                        ))
                    }
                }

                AstNodeData::Initialize{ty: _, fields} => {
                    let ty = self.get_def_typ(&node)?;
                    let fields = self.repr_struct_init_fields(&fields)?;
                    match def.mutable {
                        false => Ok(format!(
                            "const {} {} = {{\n{}}}",
                            self.repr_ast_ty(ty)?,
                            self.repr_ast_node(def.name.clone())?,
                            fields
                        )),
                        true => Ok(format!(
                            "{} {} = {{\n{}}}",
                            self.repr_ast_ty(ty)?,
                            self.repr_ast_node(def.name.clone())?,
                            fields,
                        )),
                    }
                }

                // AstNodeData::NamespaceAccess(nsa) => {
                //     Ok(self.repr_namespace_access(nsa)?)
                // }

                _ => {
                    let ty = self.get_def_typ(&node)?;
                    match def.mutable {
                        false => Ok(format!(
                            "const {} {} = {}",
                            self.repr_ast_ty(ty)?,
                            self.repr_ast_node(def.name.clone())?,
                            self.repr_ast_node(def.expr.clone())?
                        )),
                        true => Ok(format!(
                            "{} {} = {}",
                            self.repr_ast_ty(ty)?,
                            self.repr_ast_node(def.name.clone())?,
                            self.repr_ast_node(def.expr.clone())?
                        )),
                    }
                }
            }
            },
            AstNodeData::Initialize{ty, fields} => {
                let fields = self.repr_struct_init_fields(&fields)?;
                return Ok(format!("({}){{\n{}\n}}", self.repr_ast_node(ty.clone())?, fields));
            }
            
            // primitive types
            AstNodeData::Uint(number) => Ok(format!("{}", number)),
            AstNodeData::Int(number) => Ok(format!("{}", number)),
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
    pub fn new(ast: &'a Ast, node_manager: &'a Compiler) -> Self {
        Self { ast, node_manager }
    }

    pub fn generate(&mut self) -> Result<String> {
        let mut out: Vec<String> = vec![];
        for node in self.ast.top_level.iter() {
            let repr = self.repr_ast_node(node.clone())?;
            if repr == "" {
                continue;
            }
            out.push(format!("{};", repr));
        }
        
        Ok(out.join("\n"))
    }
}
