use crate::code_gen::Backend;

// compiler that glue all parts together
use super::parser::Node;
use super::parser::Parser;
use super::parser::AST;
use anyhow::Result;
use std::io::Write;
use std::process::Command;

pub struct Compiler {}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn parse_file(&self, path: &str) -> Result<AST> {
        let program = std::fs::read_to_string(path)?;
        let mut tokenizer = crate::tokenizer::Tokenizer::new(program.as_str());
        let tokens = tokenizer.all()?;
        let parser = Parser::new_with_tokens(program.to_string(), tokens)?;
        let ast = parser.get_ast()?;
        Ok(ast)
    }

    pub fn do_file(&self, path: &str) -> Result<String> {
        println!("compiling: {}", path);
        let main_ast = self.parse_file(path)?;
        let mut loads = Vec::<String>::new();
        let mut outputs = Vec::<String>::new();

        for node in main_ast.top_level.iter() {
            match node {
                Node::Load(path_idx) => {
                    loads.push(main_ast.get_src_for_token(*path_idx)?.to_string());
                }
                _ => {
                    continue;
                }
            }
        }
        let mut code_gen = crate::code_gen::cpp::CPP::new(main_ast);
        let code = code_gen.generate()?;
        outputs.push(code);
        for file in loads {
            let file_code = self.do_file(&file)?;
            outputs.insert(0, file_code);
        }
        Ok(outputs.join("\n"))
    }

    pub fn compile_file(&self, path: &str, backend: Backend) -> Result<()> {
        match backend {
            Backend::CPP => {
                return self.compile_file_cpp(path);
            }
        }
    }

    pub fn compile_file_cpp(&self, path: &str) -> Result<()> {
        let code = self.do_file(path)?;
        let out_file_name = format!("{}.cpp", path);
        let mut out_file = std::fs::File::create(&out_file_name)?;
        out_file.write_all(code.as_bytes())?;
        let cpp_output = Command::new("clang++").arg(&out_file_name).output()?;
        std::fs::remove_file(out_file_name)?;
        if !cpp_output.status.success() {
            println!(
                "C++ compiler error:\n{}",
                String::from_utf8_lossy(&cpp_output.stderr)
            );
        }
        Ok(())
    }
}
