use crate::code_gen::Backend;
use crate::code_gen::cpp::CPP;
use std::ffi::OsStr;
use std::path::Path;
use std::time::Instant;

// compiler that glue all parts together
use super::parser::Parser;
use crate::ast::{Ast, SymbolTable};
use crate::ast::Node;
use crate::ast::NodeData;
use anyhow::Result;
use std::io::Write;
use std::process::Command;

pub struct Compiler {
    total_lines: u64,
    total_tokens: u64,
    st: SymbolTable,
}

impl Compiler {
    pub fn new() -> Self {
        Self {total_lines: 0 , total_tokens: 0, st: SymbolTable::new()}
    }

    pub fn parse_file(&mut self, path: &str) -> Result<Ast> {
        let program = std::fs::read_to_string(path)?;
        let mut tokenizer = crate::tokenizer::Tokenizer::new(program.as_str());
        let tokens = tokenizer.all()?;
        let parser = Parser::new_with_tokens(path.to_string(), program.to_string(), tokens)?;
        let ast = parser.get_ast(&mut self.st)?;
        Ok(ast)
    }

    pub fn get_ast_for(&mut self, path: &str) -> Result<Vec<Ast>> {
        println!("{}", path);
        let main_ast = self.parse_file(path)?;

        self.total_lines += main_ast.src.lines().count() as u64;
        self.total_tokens += main_ast.tokens.iter().count() as u64;
        let mut loads = Vec::<String>::new();
        let mut asts = Vec::<Ast>::new();
        
        for node in main_ast.top_level.iter() {
            match node.data {
                NodeData::Load(path_idx) => {
                    loads.push(main_ast.get_src_for_token(path_idx)?.to_string());
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
        let bin_name: String = Path::new(path).file_stem().unwrap_or(OsStr::new("main")).to_string_lossy().to_string();
        let frontend_time_start = Instant::now();

        let mut asts = self.get_ast_for(path)?;
        let mut codes = Vec::<String>::new();
        println!("{:?}", self.st.file_scope_defs);

        let frontend_elapsed = frontend_time_start.elapsed();

        let ty_infer_time_start = Instant::now();

        for ast in asts.iter_mut() {
            ast.add(&mut self.st, &ast.filename.clone(), &ast.src.clone(), &ast.tokens.clone())?;
        }
        println!("symbol table should be complete: {:?}", self.st);        
        let ty_infer_elapsed = ty_infer_time_start.elapsed();

        let backend_code_gen_time_start = Instant::now();
        for ast in asts.iter() {
            let mut codegen = CPP::new(&ast);
            let code = codegen.generate()?;
            codes.push(code);
        }
        let backend_codegen_elapsed = backend_code_gen_time_start.elapsed();

        
        let final_code = codes.join("\n");
        
        let out_file_name = format!("main.cpp");
        let writing_output_time_start = Instant::now();

        let mut out_file = std::fs::File::create(&out_file_name)?;
        out_file.write_all(final_code.as_bytes())?;
        let writing_output_elapsed = writing_output_time_start.elapsed();
        let calling_c_compiler_time_start = Instant::now();

        let cpp_output = Command::new("clang++").arg("-o").arg(bin_name).arg(&out_file_name).output()?;
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
