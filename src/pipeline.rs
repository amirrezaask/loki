use crate::code_gen::Backend;
use crate::code_gen::cpp::CPP;
use std::ffi::OsStr;
use std::path::Path;
use std::str::FromStr;
use std::time::Instant;

use super::parser::Parser;
use crate::ast::{Ast};
use crate::ast::AstNodeData;
use crate::lexer::Token;
use anyhow::Result;
use anyhow::anyhow;
use serde_json::json;
use std::io::Write;
use std::process::Command;


pub struct Pipeline {
    total_lines: u64,
    total_tokens: u64,
}


impl Pipeline {
    pub fn new() -> Self {
        Self {total_lines: 0 , total_tokens: 0 }
    }

    fn find_abs_path_to_file(&self, name: &str) -> Result<String> {
        if Path::new(name).is_file() {
            return Ok(name.to_string());
        }
        let s = std::env::var("LOKI_MODULE_PATH")?;
        let mut paths: Vec<&str> = s.split(';').collect();
        paths.insert(0, ".");
        for path in paths {
            let files = std::fs::read_dir(path);
            if files.is_err() {
                continue;
            }
            let files = files.unwrap();
            for file in files {
                if file.is_err() {
                    continue;
                }
                let file = file.unwrap();
                if file.file_name() == name {
                    return Ok(file.path().to_str().unwrap().to_string());
                } 
            }
        }

        return Err(anyhow!("file {} not found in LOKI_MODULE_PATH", name));
    }

    pub fn parse_file(&mut self, path: &str) -> Result<Ast> {
        let program = std::fs::read_to_string(self.find_abs_path_to_file(path)?)?;
        let mut tokenizer = crate::lexer::Tokenizer::new(program.as_str());
        let tokens = tokenizer.all()?;
        self.dump_tokens(path, &tokens)?;
        let parser = Parser::new(path.to_string(), program, tokens)?;
        let ast = parser.get_ast()?;
        Ok(ast)
    }

    pub fn dump_tokens(&self, name: &str, tokens: &Vec<Token>) -> Result<()> {
        let mut out_file = std::fs::File::create(format!("{}_token_dump.json", name))?;
        out_file.write_all(serde_json::to_string_pretty(tokens).unwrap().as_bytes())?;

        Ok(())
    }

    pub fn dump_ast_before(&self, ast: &Ast) -> Result<()> {
        let mut out_file = std::fs::File::create(format!("ast_{}_before_analyze_dump.json", ast.filename.replace("/", "-").replace("\\", "-")))?;
        out_file.write_all(serde_json::to_string_pretty(ast).unwrap().as_bytes())?;

        Ok(())
    }

    pub fn dump_ast_after(&self, ast: &Ast) -> Result<()> {
        let mut out_file = std::fs::File::create(format!("ast_{}_after_analyze_dump.json", ast.filename.replace("/", "-").replace("\\", "-")))?;
        out_file.write_all(serde_json::to_string_pretty(ast).unwrap().as_bytes())?;

        Ok(())
    }
    pub fn get_ast_for(&mut self, path: &str) -> Result<Vec<Ast>> {
        let mut main_ast = self.parse_file(path)?;
        self.total_lines += main_ast.src.lines().count() as u64;
        self.total_tokens += main_ast.tokens.len() as u64;
        main_ast.src = "".to_string();
        let mut loads = Vec::<String>::new();
        let mut asts = Vec::<Ast>::new();
        let top_leve_nodes = main_ast.get_node(main_ast.top_level.clone())?.get_block()?;
        for node_id in top_leve_nodes.iter() {
            let node = main_ast.get_node(node_id.clone())?;
            match node.data {
                AstNodeData::Load(ref path) => {
                    loads.push(path.clone());
                }
                _ => {
                    continue;
                }
            }
        }
        for file in &loads {
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

       
        let frontend_elapsed = frontend_time_start.elapsed();
        let ty_infer_time_start = Instant::now();
        let mut asts_clone = asts.clone();
        for (idx, ast) in asts.iter_mut().enumerate() {
            self.dump_ast_before(ast)?;
            ast.infer_types(&asts_clone)?;
            self.dump_ast_after(ast)?;
            asts_clone.insert(idx, ast.clone());
        }
        for (idx, ast) in asts.iter_mut().enumerate() {
            ast.lower_features()?;
        }

        let ty_infer_elapsed = ty_infer_time_start.elapsed();

        let backend_code_gen_time_start = Instant::now();
        for ast in asts.iter() {
            let mut codegen = CPP::new(&ast);
            let code = codegen.generate()?;
            codes.push(code);
        }
        
        let backend_codegen_elapsed = backend_code_gen_time_start.elapsed();

        
        let final_code = codes.join("\n");
        
        let out_file_name = "LOKI_OUT.cpp".to_string();
        let writing_output_time_start = Instant::now();

        let mut out_file = std::fs::File::create(&out_file_name)?;
        out_file.write_all(final_code.as_bytes())?;
        let writing_output_elapsed = writing_output_time_start.elapsed();
        let calling_c_compiler_time_start = Instant::now();

        let mut cpp_command = Command::new("c++");

        cpp_command.arg("-o").arg(bin_name).arg(&out_file_name);

        
        let cpp_output = cpp_command.output()?;

        // std::fs::remove_file(out_file_name)?;
        if !cpp_output.status.success() {
            return Err(anyhow::format_err!("C++ compiler error:\n{}",
                String::from_utf8_lossy(&cpp_output.stderr)));
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
