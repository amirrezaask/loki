use std::ops::Deref;

use super::{Node, NodeData, Repr, AST};
use crate::symbol_table::SymbolTable;
use crate::tokenizer::Tokenizer;
use crate::parser::Parser;
use anyhow::Result;

pub struct CPP<'a> {
    ast: &'a AST,
    st: &'a SymbolTable,
}

impl<'a> CPP<'a> {
    fn get_decl_ty(&self, node: &Node) -> Result<Node> {
        match &node.data {
            NodeData::Decl(decl) => match decl.ty.deref() {
                Some(ty) => Ok(ty.clone()),
                None => {
                    println!("type inference shit the bed. {:?}", decl);
                    unreachable!();
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
            output.push(format!("\t{} {};", self.repr(&node.1)?, self.repr(&node.0)?));
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

    fn repr(&self, node: &Node) -> Result<String> {
        match &node.data {
            NodeData::Host(import) => {
                Ok(format!(
                    "#include \"{}\"",
                    self.ast.get_src_for_token(import.clone())?
                ))
            }
            NodeData::Load(_) => Ok("".to_string()),
            NodeData::Decl(decl) => match &decl.expr.deref().data {
                NodeData::FnDef(args, ret, block) => Ok(format!(
                    "{} {}({}) {{\n{}\n}}",
                    self.repr(&*ret)?,
                    self.repr(&*decl.name)?,
                    self.repr_fn_def_args(&args)?,
                    self.repr_block(&block)?,
                )),

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
                    let ty = self.get_decl_ty(node)?;
                    let fields = self.repr_struct_init_fields(&fields)?;
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
            NodeData::Uint(tok_idx) => Ok(format!("{}", self.ast.get_src_for_token(*tok_idx)?)),
            NodeData::Int(tok_idx) => Ok(format!("{}", self.ast.get_src_for_token(*tok_idx)?)),
            NodeData::StringLiteral(tok_idx) => {
                Ok(format!("\"{}\"", self.ast.get_src_for_token(*tok_idx)?))
            }
            NodeData::Float(tok_idx) => Ok(format!("{}", self.ast.get_src_for_token(*tok_idx)?)),
            NodeData::True(tok_idx) => Ok(format!("{}", self.ast.get_src_for_token(*tok_idx)?)),
            NodeData::False(tok_idx) => Ok(format!("{}", self.ast.get_src_for_token(*tok_idx)?)),
            NodeData::Char(tok_idx) => Ok(format!("{}", self.ast.get_src_for_token(*tok_idx)?)),
            NodeData::Ident(tok_idx) => Ok(format!("{}", self.ast.get_src_for_token(*tok_idx)?)),
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
            NodeData::FieldAccess(path) => Ok(format!("{}", self.repr_field_access_path(&path)?)),
            NodeData::FnDef(_, _, _) => {
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
            _ => {
                println!("unhandled in cpp codegen {:?}", node);
                unreachable!()
            }
        }
    }
    pub fn new(st: &'a SymbolTable, ast: &'a AST) -> Self {
        Self { st , ast }
    }

    pub fn generate(&mut self) -> Result<String> {
        let mut out: Vec<String> = vec![];
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
    let asts = vec![parser.get_ast()?];
    let st = SymbolTable::new(&asts)?;
    let mut code_gen = CPP::new(&st, &asts[0]);
    let code = code_gen.generate()?;

    assert_eq!(
        "int main() {\n\tprintf(\"Hello world\");\n}",
        code
    );

    Ok(())
}

#[test]
fn struct_def() -> Result<()> {
    let program = "S :: struct {
a: int,
b: string
};";
    let mut tokenizer = Tokenizer::new(program);
    let tokens = tokenizer.all()?;
    let parser = Parser::new_with_tokens(program.to_string(), tokens)?;
    let asts = vec![parser.get_ast()?];
    let st = SymbolTable::new(&asts)?;
    let mut code_gen = CPP::new(&st, &asts[0]);
    let code = code_gen.generate()?;
    assert_eq!(
        "struct S {
\tint a;
std::string b;
};",
        code
    );

    Ok(())
}

// #[test]
// fn struct_init() -> Result<()> {
//     let program = "d :Human: { name = \"amirreza\" };";
//     let mut tokenizer = Tokenizer::new(program);
//     let tokens = tokenizer.all()?;
//     let parser = Parser::new_with_tokens(program.to_string(), tokens)?;
//     let asts = vec![parser.get_ast()?];
//     let st = SymbolTable::new(&asts)?;
//     let mut code_gen = CPP::new(&st, &asts[0]);
//     let code = code_gen.generate()?;

//     assert_eq!(
//         "const Human d = {\n.name=\"amirreza\"}",
//         code
//     );

//     Ok(())
// }

#[test]
fn enum_def() -> Result<()> {
    let program = "e :: enum {
a,
b
};";
    let mut tokenizer = Tokenizer::new(program);
    let tokens = tokenizer.all()?;
    let parser = Parser::new_with_tokens(program.to_string(), tokens)?;
    let asts = vec![parser.get_ast()?];
    let st = SymbolTable::new(&asts)?;
    let mut code_gen = CPP::new(&st, &asts[0]);
    let code = code_gen.generate()?;

    assert_eq!(
        "enum e {
\ta,
b
};",
        code
    );

    Ok(())
}

#[test]
fn union_def() -> Result<()> {
    let program = "e :: enum {
a(int),
b
};";
    let mut tokenizer = Tokenizer::new(program);
    let tokens = tokenizer.all()?;
    let parser = Parser::new_with_tokens(program.to_string(), tokens)?;
    let asts = vec![parser.get_ast()?];
    let st = SymbolTable::new(&asts)?;
    let mut code_gen = CPP::new(&st, &asts[0]);
    let code = code_gen.generate()?;

    assert_eq!(
        "union e {
\tint a;
void* b;
};",
        code
    );

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
    let parser = Parser::new_with_tokens(program.to_string(), tokens)?;
    let mut asts = vec![parser.get_ast()?];
    let st = SymbolTable::new(&asts)?;
    println!("symbol table: {:?}", st.symbols_by_name);
    for ast in asts.iter_mut() {
        ast.infer_types(&st)?;
    }

    println!("{:?}", asts[0].top_level);
    let mut code_gen = CPP::new(&st, &asts[0]);
    let code = code_gen.generate()?;

    assert_eq!(
        "int main() {
\tconst bool x = true;
\tif (x) {
\tprintf(\"true\");
\t} else {
\tprintf(\"false\");
\t};
}",
        code
    );

    Ok(())
}

// #[test]
// fn field_access() -> Result<()> {
//     let program = "x :int: a.b.c;";
//     let mut tokenizer = Tokenizer::new(program);
//     let tokens = tokenizer.all()?;
//     let parser = Parser::new_with_tokens(program.to_string(), tokens)?;
//     let asts = vec![parser.get_ast()?];
//     let st = SymbolTable::new(&asts)?;
//     let mut code_gen = CPP::new(&st, &asts[0]);
//     let code = code_gen.generate()?;

//     assert_eq!(
//         "const int x = a.b.c",
//         code
//     );

//     Ok(())
// }
