use super::{Node, Repr, AST};
use anyhow::Result;

pub struct C {
    ast: AST,
}

impl C {
    fn repr_vec_node(&self, nodes: &Vec<Node>, sep: &str) -> Result<String> {
        let mut output = Vec::<String>::new();
        for node in nodes.iter() {
            output.push(self.repr(node)?);
        }

        Ok(output.join(sep))
    }
    fn repr(&self, node: &Node) -> Result<String> {
        match node {
            Node::Import(import) => Ok(format!("include \"{}\"", import.path)),
            Node::Decl(decl) => {
                unimplemented!();
            }
            // primitive types
            Node::Uint(tok_idx) => Ok(format!("{}", self.ast.get_src_for_token(*tok_idx)?)),
            Node::Int(tok_idx) => Ok(format!("{}", self.ast.get_src_for_token(*tok_idx)?)),
            Node::StringLiteral(tok_idx) => {
                Ok(format!("{}", self.ast.get_src_for_token(*tok_idx)?))
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
            Node::StringTy(_) => Ok(format!("char*")),
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
                Ok(format!(
                    "if ({}) {{\n\t{}\n\t}}",
                    self.repr(cond)?,
                    self.repr_vec_node(then, ";")?
                ))
                //TODO: else
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
        let mut out = Vec::<String>::new();

        for node in self.ast.top_level.iter() {
            out.push(self.repr(&node)?);
        }

        Ok(out.join("\n"))
    }
}
