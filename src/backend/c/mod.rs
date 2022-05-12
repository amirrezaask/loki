use std::ops::Deref;
use std::process::Command;

use super::{Repr, Compiler};
use crate::backend::CodeGen;
use crate::parser::{self};
use crate::parser::Node;
use anyhow::Result;

pub struct C {
    pub arch: String,
    pub os: String,
    pub ast: Node,
}

impl CodeGen for C {
    fn generate(&self) -> Result<String> {
        let codes = vec![self.ast.repr()?];
        return Ok(codes.join("\n"));
    }
}

impl Compiler for C {
    fn compile(name: &str, output: &str) {
        // "-Wno-everything"
        Command::new("cc").args(vec![name, "-o", output]).output().expect("compile error");
    }
}

impl Repr<C> for parser::IdentAndTy {
    fn repr(&self) -> Result<String> {
        Ok(format!("{} {}", self.ty.repr()?, self.ident.repr()?))
    }
}

fn type_def(kind: &str, name: &str, body: &str) -> String {
    format!("typedef {} {{\n\t{}\n\t}} {}", kind, body, name)
}


impl Repr<C> for Option<Node> {
    fn repr(&self) -> Result<String> {
        if self.is_some() {
            self.as_ref().unwrap().repr()
        } else {
            Ok("".to_string())
        }
    }
}

impl Repr<C> for parser::Operator {
    fn repr(&self) -> Result<String> {
        Ok(match self {
            parser::Operator::Plus => "+".to_string(), 
            parser::Operator::Minus => "-".to_string(), 
            parser::Operator::Multiply => "*".to_string(), 
            parser::Operator::Div => "/".to_string(), 
            parser::Operator::Mod => "%".to_string(), 
            parser::Operator::Equality => "==".to_string(), 
            parser::Operator::Lesser => "<".to_string(), 
            parser::Operator::Greater => ">".to_string(), 
            parser::Operator::LesserEq => "<=".to_string(), 
            parser::Operator::GreaterEq => ">=".to_string(), 
        })
    }
}

impl Repr<C> for Node {
    fn repr(&self) -> Result<String> {
        match self {
            // primitives
            Node::Ident(i) => Ok(i.clone()),
            Node::Int(i) => Ok(format!("{}", i)),
            Node::Uint(i) => Ok(format!("{}", i)),
            Node::Float(i) => Ok(format!("{}", i)),
            Node::Str(i) => Ok(format!("\"{}\"", i)),
            Node::Bool(i) => Ok(format!("{}", i)),
            // primitive types
            Node::IntTy => Ok("int".to_string()),
            Node::UintTy => Ok("unsigned int".to_string()),
            Node::VoidTy => Ok("void".to_string()),
            Node::FloatTy => Ok("double".to_string()),
            Node::StringTy => Ok("char *".to_string()),
            Node::BooleanTy => Ok("bool".to_string()),
            Node::CharTy => Ok("char".to_string()),
            // expr
            Node::StructTy(fields) => {
                let field_ty: Result<Vec<String>> = fields.iter().map(|it| it.repr()).collect(); 
                let field_ty = field_ty?;
                let field_ty = field_ty.join("\n;");

                Ok(format!("struct {{\n\t{}\n}}", field_ty))
            },
            Node::Return(e) => {
                Ok(format!("return {}", e.repr()?))
            },
            Node::Operation(op) => {
                let lhs = op.lhs.repr()?;
                let operator = op.op.repr()?;
                let rhs = op.rhs.repr()?;
                Ok(format!("{} {} {}", lhs, operator, rhs))
            }
            Node::FnDef(def) => {
                let args_tys: Result<Vec<String>> = def.ty.args.iter().map(|it| it.repr()).collect();
                let args_tys = args_tys?; 
                let args_tys = args_tys.join(", ");

                Ok(format!("{} {{name}} ({}) {{\n\t{}\n}}", def.ty.return_ty.repr()?, args_tys, def.block.repr()?))
            }
            Node::Inc(i) => {
                Ok(format!("{}++", i.repr()?))
            },
            Node::Dec(i) => {
                Ok(format!("{}--", i.repr()?))
            },
            Node::While(w) => {
                let cond = w.cond.repr()?;
                let body = w.block.repr()?;
                Ok(format!("while ({}) {{\n\t{}\n}}", cond, body))
            },
            Node::For(f) => {
                let init = f.init.repr()?;
                let cond = f.cond.repr()?;
                let cont = f.cont.repr()?;
                let body = f.body.repr()?;

                Ok(format!("for ({};{};{}) {{\n\t{}\n}}", init,cond, cont, body))
            },
            Node::Decl(name, o_ty, expr) => {
                if let Node::FnDef(def) = expr.deref() {
                    let args_tys: Result<Vec<String>> = def.ty.args.iter().map(|it| it.repr()).collect();
                    let args_tys = args_tys?;
                    let args_tys = args_tys.join(", ");

                    Ok(format!("{} {}({}) {{\n\t{}\n\t}}", def.ty.return_ty.repr()?, name.repr()?, args_tys, def.block.repr()?))
                } else if let Node::EnumTy(variants) = expr.deref() {
                    let variants: Result<Vec<String>> = variants.iter().map(|v| v.repr()).collect();
                    let variants = variants?;
                    Ok(type_def("enum", name.repr()?.as_ref(), variants.join(",\n").as_ref()))
                } else if let Node::UnionTy(fields) = expr.deref() {
                    let field_ty: Result<Vec<String>> = fields.iter().map(|it| it.repr()).collect(); 
                    let mut field_ty = field_ty?;
                    for f in field_ty.iter_mut() {
                        f.push_str(";");
                    }
                    Ok(type_def("union", name.repr()?.as_ref(), field_ty.join("\n").as_ref()))
                } else if let Node::StructTy(fields) = expr.deref() {
                    let field_ty: Result<Vec<String>> = fields.iter().map(|it| it.repr()).collect(); 
                    let mut field_ty = field_ty?;
                    for f in field_ty.iter_mut() {
                        f.push_str(";");
                    }
                    Ok(type_def("struct", name.repr()?.as_ref(), field_ty.join("\n").as_ref()))
                } 
                else {
                    let n_str = name.repr()?;
                    let ty_str = o_ty
                        .clone()
                        .expect("C backend needs all types defined")
                        .repr();
                    if let Node::StringTy = o_ty.clone().unwrap() {
                        let expr_str = expr.repr()?;
                        Ok(format!("char {}[] = {}", n_str, expr_str))
                    } else {
                        let expr_str = expr.repr()?;
                        Ok(format!("{} {} = {}", ty_str?, n_str, expr_str))
                    }
                }
            }
            Node::List(stmts) => {
                let stmts: Result<Vec<String>> = stmts.iter().map(|n| n.repr()).collect();
                let stmts = stmts?;
                Ok(stmts.join("\n"))
            },
            Node::Application(call) => {
                let args: Result<Vec<String>> = call.args.iter().map(|n| n.repr()).collect();
                let args = args?;
                Ok(format!("{}({})", call.name.repr()?, args.join(", ")))
            },

            Node::Import(i) => {
                Ok(format!("#include {}", i.path.repr()?))
            },
            Node::Stmt(n) => n.repr(),
            Node::Block(nodes) => {
                let all: Result<Vec<String>> = nodes.iter().map(|n| n.repr()).collect();
                let mut all = all?;
                all.last_mut().unwrap().push(';');
                Ok(all.join(";\n"))
            },
            _ => panic!("unsupported: {:?}", self),
        }
    }
}

#[test]
fn test_return() {
    let code = Node::Return(Box::new(Node::Uint(1))).repr();
    assert!(code.is_ok());
    assert_eq!(code.unwrap(), "return 1".to_string());
}