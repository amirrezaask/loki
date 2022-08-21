use crate::code_gen::Backend;
use crate::code_gen::cpp::CPP;
use std::ffi::OsStr;
use std::path::Path;
use std::time::Instant;

// compiler that glue all parts together
use super::parser::Parser;
use crate::node_manager::AstNodeManager;
use crate::ast::{Ast};
use crate::ast::AstNodeData;
use anyhow::Result;
use serde_json::json;
use std::io::Write;
use std::process::Command;


pub struct Compiler {
    total_lines: u64,
    total_tokens: u64,
    node_manager: AstNodeManager    
}


impl Compiler {
    pub fn new() -> Self {
        Self {total_lines: 0 , total_tokens: 0, node_manager: AstNodeManager::new() }
    }

    pub fn parse_file(&mut self, path: &str) -> Result<Ast> {
        // let abs_path = std::path::PathBuf::from_str(path)?.canonicalize()?;
        // println!("{:?}", abs_path);
        let program = std::fs::read_to_string(path)?;
        let mut tokenizer = crate::lexer::Tokenizer::new(program.as_str());
        let tokens = tokenizer.all()?;
        let parser = Parser::new_with_tokens(path.to_string(), program, tokens, &mut self.node_manager)?;
        let ast = parser.get_ast()?;
        Ok(ast)
    }

    pub fn dump_ast(name: &str, ast: &Ast) -> Result<()> {
        let mut out_file = std::fs::File::create(format!("{}_ast_dump.json", name))?;
        out_file.write_all(serde_json::to_string(&ast.top_level).unwrap().as_bytes())?;

        Ok(())
    }

    pub fn dump_node_manager_before(&self) -> Result<()> {
        let mut out_file = std::fs::File::create("node_manager_before_infer_dump.json")?;
        out_file.write_all(serde_json::to_string(&self.node_manager).unwrap().as_bytes())?;

        Ok(())
    }

    pub fn dump_node_manager_after(&self) -> Result<()> {
        let mut out_file = std::fs::File::create("node_manager_after_infer_dump.json")?;
        out_file.write_all(serde_json::to_string(&self.node_manager).unwrap().as_bytes())?;

        Ok(())
    }
    pub fn get_ast_for(&mut self, path: &str) -> Result<Vec<Ast>> {
        let main_ast = self.parse_file(path)?;
        // Self::dump_ast(path, &main_ast)?;
        self.total_lines += main_ast.src.lines().count() as u64;
        self.total_tokens += main_ast.tokens.len() as u64;
        let mut loads = Vec::<String>::new();
        let mut asts = Vec::<Ast>::new();
        
        for node_id in main_ast.top_level.iter() {
            let node = self.node_manager.get_node(node_id.clone());
            match node.data {
                AstNodeData::Load(ref path) => {
                    loads.push(path.clone());
                }
                _ => {
                    continue;
                }
            }
        }

        for file in loads {
            let mut file_ast = self.get_ast_for(&file)?;
            asts.append(&mut file_ast)
        }
        asts.push(main_ast);
        Ok(asts)

    }

    pub fn compile_file(&mut self, path: &str, backend: Backend) -> Result<()> {
        match backend {
            Backend::CPP => {
                return self.compile_file_cpp(path);
            }
        }
    }

    pub fn compile_file_cpp(&mut self, path: &str) -> Result<()> {
        let bin_name: String = {
            let this = Path::new(path).file_stem();
            let default = OsStr::new("main");
            match this {
                Some(x) => x,
                None => default,
            }
        }.to_string_lossy().to_string();
        let frontend_time_start = Instant::now();

        let mut asts = self.get_ast_for(path)?;
        let mut codes = Vec::<String>::new();

        self.dump_node_manager_before()?;

        self.node_manager.infer_types()?;

        self.dump_node_manager_after()?;
        let frontend_elapsed = frontend_time_start.elapsed();

        let ty_infer_time_start = Instant::now();

        

        let ty_infer_elapsed = ty_infer_time_start.elapsed();

        let backend_code_gen_time_start = Instant::now();
        for ast in asts.iter() {
            let mut codegen = CPP::new(&ast, &self.node_manager);
            let code = codegen.generate()?;
            codes.push(code);
        }
        // println!("st: {:?}", self.st);
        // let mut compiler_flags_by_user = Vec::<String>::new();
        // for ast in asts.iter() {
        //     compiler_flags_by_user.append(&mut ast.get_compiler_flags());
        // }
        let backend_codegen_elapsed = backend_code_gen_time_start.elapsed();

        
        let final_code = codes.join("\n");
        
        let out_file_name = "main.cpp".to_string();
        let writing_output_time_start = Instant::now();

        let mut out_file = std::fs::File::create(&out_file_name)?;
        out_file.write_all(final_code.as_bytes())?;
        let writing_output_elapsed = writing_output_time_start.elapsed();
        let calling_c_compiler_time_start = Instant::now();

        let mut cpp_command = Command::new("c++");

        cpp_command.arg("-o").arg(bin_name).arg(&out_file_name);

        // for flag in compiler_flags_by_user {
        //     cpp_command.arg(flag);
        // }
        let cpp_output = cpp_command.output()?;

        // std::fs::remove_file(out_file_name)?;
        if !cpp_output.status.success() {
            println!(
                "C++ compiler error:\n{}",
                String::from_utf8_lossy(&cpp_output.stderr)
            );
        }
        let calling_c_compiler_time_elapsed = calling_c_compiler_time_start.elapsed();

        println!("Totol lines processed: {}", self.total_lines);
        println!("Totol tokens processed: {}", self.total_tokens);

        println!("compiler front end took: {}ns", frontend_elapsed.as_nanos());
        println!("compiler type inference took: {}ns", ty_infer_elapsed.as_nanos());
        println!("compiler code generation took: {}ns", backend_codegen_elapsed.as_nanos());
        println!("writing output file: {}ns", writing_output_elapsed.as_nanos());
        println!("calling C compiler: {}milis", calling_c_compiler_time_elapsed.as_millis());

        Ok(())
    }
}
