use std::ops::Deref;

use super::{Node, Repr, AST};
use crate::parser::Parser;
use crate::tokenizer::Tokenizer;
use anyhow::Result;

pub struct C {
    ast: AST,
}

impl C {
    fn get_decl_ty(&self, node: &Node) -> Result<Node> {
        match node {
            Node::Decl(decl) => match decl.ty.deref() {
                Some(ty) => Ok(ty.clone()),
                None => match decl.expr.deref() {
                    Node::Uint(_) => Ok(Node::Uint(0)),
                    Node::True(_) | Node::False(_) => Ok(Node::BoolTy(0)),
                    Node::Int(_) => Ok(Node::IntTy(0)),
                    Node::StringLiteral(_) => Ok(Node::StringTy(0)),
                    Node::Float(_) => Ok(Node::FloatTy(0)),
                    Node::Char(_) => Ok(Node::Char(0)),
                    Node::TypeInit(name, _) => match name {
                        Some(n) => Ok(n.deref().clone()),
                        None => {
                            unreachable!();
                        }
                    },
                    _ => {
                        println!("cannot infer type for: {:?}", decl.expr);
                        unreachable!();
                    }
                },
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
            output.push(self.repr(node)? + ";");
        }

        Ok(output.join("\n\t"))
    }
    fn repr_fn_def_args(&self, node_tys: &Vec<(Node, Node)>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in node_tys {
            output.push(format!("{} {}", self.repr(&node.1)?, self.repr(&node.0)?));
        }
        Ok(output.join(", "))
    }
    fn repr_struct_fields(&self, node_tys: &Vec<(Node, Node)>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in node_tys {
            output.push(format!("{} {};", self.repr(&node.1)?, self.repr(&node.0)?));
        }
        Ok(output.join("\n"))
    }
    fn repr_struct_init_fields(&self, fields: &Vec<(Node, Node)>) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in fields {
            output.push(format!(".{}={},", self.repr(&node.0)?, self.repr(&node.1)?));
        }
        Ok(output.join("\n"))
    }
    fn repr_vec_node(&self, nodes: &Vec<Node>, sep: &str) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in nodes.iter() {
            output.push(self.repr(node)?);
        }

        Ok(output.join(sep))
    }
    fn repr(&self, node: &Node) -> Result<String> {
        match node {
            Node::Import(import) => Ok(format!(
                "#include \"{}\"",
                self.ast.get_src_for_token(import.path)?
            )),
            Node::Decl(decl) => match decl.expr.deref() {
                Node::FnDef(args, ret, block) => Ok(format!(
                    "{} {}({}) {{\n\t{}\n}}",
                    self.repr(&*ret)?,
                    self.repr(&*decl.name)?,
                    self.repr_fn_def_args(&args)?,
                    self.repr_block(&block)?,
                )),

                Node::Struct(fields) => Ok(format!(
                    "struct {} {{\n\t{}\n}};",
                    self.repr(&*decl.name)?,
                    self.repr_struct_fields(&fields)?
                )),

                Node::TypeInit(_, fields) => {
                    let ty = self.get_decl_ty(node)?;
                    let fields = self.repr_struct_init_fields(fields)?;
                    match decl.mutable {
                        false => Ok(format!(
                            "const {} {} = {{\n{}}}",
                            self.repr(&ty)?,
                            self.repr(decl.name.deref())?,
                            fields
                        )),
                        true => Ok(format!(
                            "{} {} = {{\n{}}}",
                            self.repr(&ty)?,
                            self.repr(decl.name.deref())?,
                            fields,
                        )),
                    }
                }

                _ => {
                    let ty = self.get_decl_ty(node)?;
                    match decl.mutable {
                        false => Ok(format!(
                            "const {} {} = {}",
                            self.repr(&ty)?,
                            self.repr(decl.name.deref())?,
                            self.repr(decl.expr.deref())?
                        )),
                        true => Ok(format!(
                            "{} {} = {}",
                            self.repr(&ty)?,
                            self.repr(decl.name.deref())?,
                            self.repr(decl.expr.deref())?
                        )),
                    }
                }
            },
            // primitive types
            Node::Uint(tok_idx) => Ok(format!("{}", self.ast.get_src_for_token(*tok_idx)?)),
            Node::Int(tok_idx) => Ok(format!("{}", self.ast.get_src_for_token(*tok_idx)?)),
            Node::StringLiteral(tok_idx) => {
                Ok(format!("\"{}\"", self.ast.get_src_for_token(*tok_idx)?))
            }
            Node::Float(tok_idx) => Ok(format!("{}", self.ast.get_src_for_token(*tok_idx)?)),
            Node::True(tok_idx) => Ok(format!("{}", self.ast.get_src_for_token(*tok_idx)?)),
            Node::False(tok_idx) => Ok(format!("{}", self.ast.get_src_for_token(*tok_idx)?)),
            Node::Char(tok_idx) => Ok(format!("{}", self.ast.get_src_for_token(*tok_idx)?)),
            Node::Ident(tok_idx) => Ok(format!("{}", self.ast.get_src_for_token(*tok_idx)?)),

            // keywords
            Node::IntTy(_) => Ok(format!("int")),
            Node::Int8Ty(_) => Ok(format!("int8_t")),
            Node::Int16Ty(_) => Ok(format!("int16_t")),
            Node::Int32Ty(_) => Ok(format!("int32_t")),
            Node::Int64Ty(_) => Ok(format!("int64_t")),
            Node::Int128Ty(_) => {
                unimplemented!()
            }

            Node::UintTy(_) => Ok(format!("unsigned int")),
            Node::Uint8Ty(_) => Ok(format!("uint8_t")),
            Node::Uint16Ty(_) => Ok(format!("uint16_t")),
            Node::Uint32Ty(_) => Ok(format!("uint32_t")),
            Node::Uint64Ty(_) => Ok(format!("uint64_t")),
            Node::Uint128Ty(_) => {
                unimplemented!();
            }

            Node::FloatTy(_) => Ok(format!("long")),
            Node::BoolTy(_) => Ok(format!("bool")),
            Node::StringTy(_) => Ok(format!("std::string")),
            Node::CharTy(_) => Ok(format!("char")),
            Node::VoidTy(_) => Ok(format!("void")),

            //Expressions
            Node::Sum(lhs, rhs) => Ok(format!("({} + {})", self.repr(lhs)?, self.repr(rhs)?)),
            Node::Subtract(lhs, rhs) => Ok(format!("({} - {})", self.repr(lhs)?, self.repr(rhs)?)),
            Node::Multiply(lhs, rhs) => Ok(format!("({} * {})", self.repr(lhs)?, self.repr(rhs)?)),
            Node::Div(lhs, rhs) => Ok(format!("({} / {})", self.repr(lhs)?, self.repr(rhs)?)),
            Node::Mod(lhs, rhs) => Ok(format!("({} % {})", self.repr(lhs)?, self.repr(rhs)?)),
            Node::FieldAccess(lhs, rhs) => Ok(format!("{}.{}", self.repr(lhs)?, self.repr(rhs)?)),
            Node::FnDef(args, ret_ty, block) => {
                unreachable!();
            }

            Node::FnCall(name, args) => Ok(format!(
                "{}({})",
                self.repr(name)?,
                self.repr_vec_node(args, ",")?
            )),
            Node::If(cond, then, _else) => {
                let mut base = format!(
                    "if ({}) {{\n\t{}\n\t}}",
                    self.repr(cond)?,
                    self.repr_block(then)?
                );

                if _else.is_some() {
                    base += &format!(
                        " else {{\n\t{}\n}}",
                        self.repr_block(&_else.clone().unwrap())?
                    );
                }

                Ok(base)
            }
            Node::Return(expr) => Ok(format!("return {}", self.repr(expr)?)),
            _ => {
                println!("{:?}", node);
                unreachable!()
            }
        }
    }
    pub fn new(ast: AST) -> Self {
        Self { ast }
    }

    pub fn generate(&mut self) -> Result<String> {
        let mut out: Vec<String> = vec![
            "#include <string>".to_string(),
            "#include <cstdio>".to_string(),
        ];

        for node in self.ast.top_level.iter() {
            out.push(self.repr(&node)?);
        }

        Ok(out.join("\n"))
    }
}

#[test]
fn hello_world() -> Result<()> {
    let program = "main :: fn() int {\n\tprintf(\"Hello world\");};";
    let mut tokenizer = Tokenizer::new(program);
    let tokens = tokenizer.all()?;
    let mut parser = Parser::new_with_tokens(program.to_string(), tokens)?;
    let ast = parser.get_ast()?;
    let mut code_gen = C::new(ast);
    let code = code_gen.generate()?;

    assert_eq!("int main() {\n\tprintf(\"Hello world\");\n}", code);

    Ok(())
}

#[test]
fn hello_world_with_if() -> Result<()> {
    let program = "main :: fn() int {
x :: true;
if (x) {
\t\tprintf(\"true\");
} else {
\t\tprintf(\"false\");
}
};";
    let mut tokenizer = Tokenizer::new(program);
    let tokens = tokenizer.all()?;
    let mut parser = Parser::new_with_tokens(program.to_string(), tokens)?;
    let ast = parser.get_ast()?;
    let mut code_gen = C::new(ast);
    let code = code_gen.generate()?;

    assert_eq!(
        "int main() {
\tconst bool x = true;
\tif (x) {
\tprintf(\"true\");
\t} else {
\tprintf(\"false\");
};
}",
        code
    );

    Ok(())
}
