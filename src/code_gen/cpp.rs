use std::ops::Deref;

use super::{AstNode, AstNodeData, Repr, Ast};
use crate::node_manager::AstNodeManager;
use crate::ast::{AstNodeType, AstOperation, NodeID, AstTag, NamespaceAccess};
use crate::lexer::{Tokenizer, TokenType};
use crate::parser::Parser;
use anyhow::Result;

pub struct CPP<'a> {
    ast: &'a Ast,
    node_manager: &'a AstNodeManager,
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
            AstNodeType::NamespaceAccess(_, field) => {
                Ok(self.repr_ast_ty(field.deref().clone())?)
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
                let def_expr = self.node_manager.get_node(def.expr.clone());
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
            let node = self.node_manager.get_node(node_id.clone());
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
    fn repr_struct_fields(&self, node_tys: &Vec<NodeID>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node_id in node_tys {
            let node = self.node_manager.get_node(node_id.clone());
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
            output.push(format!("\t.{}={}", self.repr_ast_node(node.0.clone())?, self.repr_ast_node(node.1.clone())?));
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
        for node in variants.iter() {
            output.push(self.repr_ast_node(node.0.clone())?);
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
                panic!("unsupported operator: {:?}", op);
           }
        }
    }

    fn repr_namespace_access(&self, nsa: &NamespaceAccess) -> Result<String> {
        let ns_ty = self.node_manager.get_node(nsa.namespace.clone());
        let field_ty = self.node_manager.get_node(nsa.field.clone());

        if ns_ty.is_enum() {
            return Ok(format!("{}::{}", self.repr_ast_node(nsa.namespace.clone())?, self.repr_ast_node(nsa.field.clone())?));
        }

        if ns_ty.is_pointer() {
            return Ok(format!("{}->{}", self.repr_ast_node(nsa.namespace.clone())?, self.repr_ast_node(nsa.field.clone())?));
        }

        return Ok(format!("{}.{}", self.repr_ast_node(nsa.namespace.clone())?, self.repr_ast_node(nsa.field.clone())?));
    }

    fn repr_array_elems(&self, elems: &Vec<NodeID>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in elems.iter() {
            output.push(format!("{}", self.repr_ast_node(node.clone())?));
        }

        Ok(output.join(","))
    }

    fn repr_ast_node(&self, node_id: NodeID) -> Result<String> {
        let node = self.node_manager.get_node(node_id);
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
                Ok(format!("{} {}", self.repr_ast_ty(node.infered_type.clone())?, name))
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
                let def_expr = self.node_manager.get_node(def.expr.clone());
                match &def_expr.data {
                
                AstNodeData::FnDef(ref fn_def) => 
                    Ok(format!("{} {}({}) {{\n{}\n}}",
                        self.repr_ast_node(fn_def.sign.ret.clone())?,
                        self.repr_ast_node(def.name.clone())?,
                        self.repr_fn_def_args(&fn_def.sign.args)?,
                        self.repr_block(&fn_def.body)?
                    )),
                

                AstNodeData::Struct(fields) => Ok(format!(
                    "struct {} {{\n{}\n}};",
                    self.repr_ast_node(def.name.clone())?,
                    self.repr_struct_fields(fields)?
                )),

                AstNodeData::Enum(is_union, variants) => {
                    if !is_union {
                        Ok(format!(
                            "enum class {} {{\n{}\n}};",
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

                AstNodeData::Initialize(_, fields) => {
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
            AstNodeData::Initialize(ty, fields) => {
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
            AstNodeData::Char(c) => Ok(format!("{}", c)),
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
            AstNodeData::BinaryOperation(ref binary_operation) => {
                Ok(format!("{} {} {}", self.repr_ast_node(binary_operation.left.clone())?, self.repr_ast_op(&binary_operation.operation)?, self.repr_ast_node(binary_operation.right.clone())?))
            }
            AstNodeData::NamespaceAccess(ref nsa) => {
                Ok(self.repr_namespace_access(nsa)?)
            },
            AstNodeData::FnDef(_) => {
                unreachable!();
            }

            AstNodeData::FnCall(name, args) => Ok(format!(
                "{}({})",
                self.repr_ast_node(name.clone())?,
                self.repr_vec_node(&args, ",")?
            )),
            AstNodeData::If(ref ast_if) => {
                let mut cases = Vec::<String>::new();
                cases.push(format!("if ({} == true) {{\n{}\n}}", self.repr_ast_node(ast_if.cases[0].0.clone())?, self.repr_block(&ast_if.cases[0].1)?));
                for case in &ast_if.cases[1..] {
                    cases.push(format!("else if ({} == true) {{\n{}\n}}", self.repr_ast_node(case.0.clone())?, self.repr_block(&case.1)?))
                }
                Ok(cases.join("\n"))
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
    pub fn new(ast: &'a Ast, node_manager: &'a AstNodeManager) -> Self {
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
