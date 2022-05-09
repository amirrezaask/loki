use std::collections::HashMap;
use std::ops::Deref;

use super::Repr;
use crate::backend::CodeGen;
use crate::parser::{self};
use crate::parser::Node;
use anyhow::Result;

mod reprs;
pub struct C {
    pub arch: String,
    pub os: String,
    pub ast: Node,
}

impl CodeGen for C {
    fn generate(&self) -> Result<String> {
        let codes = vec!["#include<stdio.h>".to_string(), self.ast.repr()];
        return Ok(codes.join("\n"));
    }
}


impl Repr<C> for parser::IdentAndTy {
    fn repr(&self) -> String {
        format!("{} {}", self.ty.repr(), self.ident.repr())
    }
}


impl Repr<C> for Option<Node> {
    fn repr(&self) -> String {
        if self.is_some() {
            self.as_ref().unwrap().repr()
        } else {
            "".to_string()
        }
    }
}

impl Repr<C> for Node {
    fn repr(&self) -> String {
        match self {
            // primitives
            Node::Ident(i) => i.clone(),
            Node::Int(i) => format!("{}", i),
            Node::Uint(i) => format!("{}", i),
            Node::Float(i) => format!("{}", i),
            Node::Str(i) => format!("\"{}\"", i),
            Node::Bool(i) => format!("{}", i),
            // expr
            Node::StructTy(fields) => {
                let field_ty: Vec<String> = fields.iter().map(|it| it.repr()).collect(); 
                let field_ty = field_ty.join("\n;");

                format!("struct {{\n{}\n}}", field_ty)
            },
            Node::FnDef(def) => {
                let args_tys: Vec<String> = def.ty.args.iter().map(|it| it.repr()).collect();
                let args_tys = args_tys.join(", ");

                format!("{} {{name}} ({}) {{\n\t{}\n\t}}", def.ty.return_ty.repr(), args_tys, def.block.repr())
            }
            Node::Decl(name, o_ty, expr) => {
                if let Node::FnDef(def) = expr.deref() {
                    let args_tys: Vec<String> = def.ty.args.iter().map(|it| it.repr()).collect();
                    let args_tys = args_tys.join(", ");

                    format!("{} {}({}) {{\n\t{}\n\t}}", def.ty.return_ty.repr(), name.repr(), args_tys, def.block.repr())
                } else {
                    let n_str = name.repr();
                    let ty_str = o_ty
                        .clone()
                        .expect("C backend needs all types defined")
                        .repr();
                    let ty_str = o_ty.repr();
                    let expr_str = expr.repr();
                    format!("{} {} = {};", ty_str, n_str, expr_str)
                }
            }
            Node::List(stmts) => {
                let stmts: Vec<String> = stmts.iter().map(|n| n.repr()).collect();
                stmts.join("\n")
            },
            Node::FnCall(call) => {
                let args: Vec<String> = call.args.iter().map(|n| n.repr()).collect();
                format!("{}({});", call.name.repr(), args.join(", "))
            }

            Node::Stmt(n) => n.repr(),
            Node::Block(nodes) => {
                let all: Vec<String> = nodes.iter().map(|n| n.repr()).collect();
                all.join("\n")
            },
            _ => panic!("unsupported: {:?}", self),
        }
    }
}
#[test]
fn please() {
    let code = "
main = fn() void {
    printf(\"Hello World\");
};";

    let ast = parser::module(code.to_string());
    assert!(ast.is_ok());

    let (_, ast) = ast.unwrap();
    assert_eq!(ast.repr(), "void main() {\n\tprintf(\"Hello World\");\n\t}");
}