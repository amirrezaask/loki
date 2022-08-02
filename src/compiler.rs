use crate::code_gen::Backend;
use crate::code_gen::cpp::CPP;
use crate::semantic::SymbolTable;

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
        println!("parsing {}", path);
        let program = std::fs::read_to_string(path)?;
        let mut tokenizer = crate::tokenizer::Tokenizer::new(program.as_str());
        let tokens = tokenizer.all()?;
        let parser = Parser::new_with_tokens(program.to_string(), tokens)?;
        let ast = parser.get_ast()?;
        Ok(ast)
    }

    pub fn get_ast_for(&self, path: &str) -> Result<Vec<AST>> {
        println!("generating ast for {}", path);
        let main_ast = self.parse_file(path)?;
        let mut loads = Vec::<String>::new();
        let mut asts = Vec::<AST>::new();
        
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
        asts.push(main_ast);
        for file in loads {
            let mut file_ast = self.get_ast_for(&file)?;
            asts.append(&mut file_ast)
        }

        Ok(asts)

    }

    pub fn compile_file(&self, path: &str, backend: Backend) -> Result<()> {
        match backend {
            Backend::CPP => {
                return self.compile_file_cpp(path);
            }
        }
    }

    pub fn compile_file_cpp(&self, path: &str) -> Result<()> {
        let asts = self.get_ast_for(path)?;
        let st = SymbolTable::new(&asts)?;
        // println!("symbol table: {:?}", st.symbols);
        let mut codes = Vec::<String>::new();

        for ast in asts.iter() {
            let mut codegen = CPP::new(&st, &ast);
            let code = codegen.generate()?;
            codes.push(code);
        }

        
        let final_code = codes.join("\n");
        
        let out_file_name = format!("{}.cpp", path);
        let mut out_file = std::fs::File::create(&out_file_name)?;
        out_file.write_all(final_code.as_bytes())?;
        let cpp_output = Command::new("clang++").arg(&out_file_name).output()?;
        // std::fs::remove_file(out_file_name)?;
        if !cpp_output.status.success() {
            println!(
                "C++ compiler error:\n{}",
                String::from_utf8_lossy(&cpp_output.stderr)
            );
        }
        Ok(())
    }
}
