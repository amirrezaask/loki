use serde::Serialize;
use std::ffi::OsStr;
use std::fmt::Debug;
use std::io::Write;
use std::path::Path;
use std::process::Command;
use std::time::Instant;
use std::{collections::HashMap, hash::Hash};

use super::ir::IR;
use super::typer::Type;
use crate::bytecode::{make_module, Module};
use crate::c_backend;
use crate::ir::{Node, Statement};
use crate::{errors::*, ir::NodeIndex, parser::Parser, utils};

pub type FileIndex = usize;
type File = String;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Dependency {
    pub file: File,
    pub node_index: NodeIndex,
    pub needs: String,
}
pub struct Compilation {
    total_lines: u64,
    irs: HashMap<File, IR>,
    exported_symbols: HashMap<String, HashMap<String, Type>>,
    dependencies: Vec<Dependency>,
    modules: HashMap<String, Module>,
    verbose: bool,
    debug: bool,
}

impl Compilation {
    fn parse_file(&mut self, path: &str) -> Result<File> {
        let abs_path = utils::find_abs_path_to_file(path).unwrap();
        let program = std::fs::read_to_string(abs_path.clone()).unwrap();
        let mut tokenizer = crate::lexer::Tokenizer::new(abs_path.clone(), program.as_str());
        let tokens = tokenizer.all()?;
        let parser = Parser::new(abs_path.to_string(), program.clone(), tokens)?;
        let ast = parser.get_ast()?;
        self.total_lines += program.lines().count() as u64;
        self.irs.insert(abs_path.clone(), ast);
        Ok(abs_path.clone())
    }

    fn resolve_loads(&mut self, file: File) -> Result<()> {
        let file_ast = self.irs.get(&file).unwrap();
        let loads = file_ast.get_loads();
        for load in &loads {
            if self.irs.contains_key(load) {
                continue;
            }
            let loaded_file_ir = self.parse_file(load)?;
            self.resolve_loads(loaded_file_ir)?;
        }

        Ok(())
    }
    fn pretty_print_unknown_nodes(nodes: &HashMap<NodeIndex, Node>) {
        for (k, v) in nodes.iter() {
            if v.type_information.is_none() {
                println!("{}: {:?}", k, v)
            }
        }
    }
    fn pretty_print<T: Debug>(list: Vec<T>) {
        for elem in &list {
            println!("{:?}", elem);
        }
    }
    pub fn new(main_file: &str, verbose: bool, debug: bool) -> Result<()> {
        let mut compilation = Compilation {
            total_lines: 0,
            irs: HashMap::new(),
            dependencies: vec![],
            exported_symbols: HashMap::new(),
            modules: HashMap::new(),
            verbose,
            debug,
        };
        let whole_thing = Instant::now();
        let frontend_time_start = Instant::now();
        // parse main file
        let main_file_abs_path = compilation.parse_file(main_file)?;
        // parse all loaded files recursively
        compilation.resolve_loads(main_file_abs_path.clone());

        let frontend_elapsed = frontend_time_start.elapsed();

        let type_check_time_start = Instant::now();
        let main_ir = compilation.irs.get_mut(&main_file_abs_path).unwrap();
        // now we have all of our files used in our program in compilation.IRs
        let keys: Vec<File> = compilation.irs.keys().map(|f| f.clone()).collect();
        let mut keys_index: usize = 0;
        let mut finished_type_checking = 0;
        loop {
            if finished_type_checking == keys.len() {
                break;
            }

            let file = &keys[keys_index];
            let mut still_hope = false;
            for (name, ir) in &compilation.irs {
                if name != file && !ir.type_checked {
                    still_hope = true;
                }
            }
            let ir = compilation.irs.get_mut(file).unwrap();
            if ir.type_checked {
                keys_index += 1;
                if keys_index >= keys.len() {
                    keys_index = 0;
                }
                continue;
            }
            // check all dependencies to see if they are ready.
            // if not check if other files are fully typed.
            // if yes and still unresolved dependencies it's an undeclared error.
            // if no skip this file for now.

            // try to do a pass on the file and type as much as possible
            ir.type_root(&compilation.exported_symbols)?;
            // Self::pretty_print_unknown_nodes(&ir.nodes);

            // get file exported symbols that are fully type checked and add them to compilation so other files know about these.
            let mut file_exports = compilation.exported_symbols.get_mut(file);
            if file_exports.is_none() {
                compilation
                    .exported_symbols
                    .insert(file.clone(), HashMap::new());
                file_exports = compilation.exported_symbols.get_mut(file);
            }
            let file_exports = file_exports.unwrap();
            for (k, v) in ir.exported_symbols.iter() {
                file_exports.insert(k.clone(), v.clone());
            }

            if !still_hope && (ir.dependencies.len() > 0 || ir.any_unknowns()) {
                Self::pretty_print_unknown_nodes(&ir.nodes);
                let node = ir.get_node(ir.dependencies[0].node_index).unwrap();
                return Err(CompilerError {
                    filename: ir.filename.clone(),
                    file_source: ir.file_source.clone(),
                    line: node.line,
                    col: node.col,
                    reason: Reason::TypeCheckError(TypeCheckError::UndeclaredIdentifier(
                        ir.dependencies[0].needs.clone(),
                    )),
                });
                break;
            }

            if ir.dependencies.len() == 0 && !ir.any_unknowns() {
                finished_type_checking += 1;
                ir.type_checked = true;
            }
            keys_index += 1;
            if keys_index >= keys.len() {
                keys_index = 0;
            }
        }
        let type_check_elapsed = type_check_time_start.elapsed();

        let byte_code_generation_start = Instant::now();
        let module = make_module(&mut compilation.irs);
        if compilation.debug {
            println!("=== DEBUG MODE ===");
            for inst in &module.instructions {
                println!("{:#?}", inst)
            }
            println!("=== DEBUG MODE ===");
        }
        let byte_code_generation_elapsed = byte_code_generation_start.elapsed();

        let backend_code_generation_start = Instant::now();
        let backend_code = c_backend::emit_for_module(module);
        let backend_code_generation_elapsed = backend_code_generation_start.elapsed();

        let bin_name: String = {
            let this = Path::new(main_file).file_stem();
            let default = OsStr::new("main");
            match this {
                Some(x) => x,
                None => default,
            }
        }
        .to_string_lossy()
        .to_string();

        let out_file_name = &format!("{}.c", main_file);
        let writing_generated_code_into_disk = Instant::now();
        let mut out_file = std::fs::File::create(out_file_name).unwrap();
        out_file.write_all(&backend_code.as_bytes()).unwrap();
        let writing_generated_code_into_disk = writing_generated_code_into_disk.elapsed();

        let calling_backend_compiler = Instant::now();
        let mut cpp_command = Command::new("c++");

        cpp_command.arg("-o").arg(bin_name).arg(out_file_name);

        let cpp_output = cpp_command.output().unwrap();
        let calling_backend_compiler = calling_backend_compiler.elapsed();

        if !cpp_output.status.success() {
            panic!(
                "C++ compiler error:\n{}",
                String::from_utf8_lossy(&cpp_output.stderr)
            );
        }
        if !compilation.debug {
            std::fs::remove_file(out_file_name).unwrap();
        }
        let whole_thing = whole_thing.elapsed();
        if compilation.verbose {
            println!("Compiler frontend took {}ns", frontend_elapsed.as_nanos());
            println!("Type checker took {}ns", type_check_elapsed.as_nanos());
            println!(
                "Bytecode generation took {}ns",
                byte_code_generation_elapsed.as_nanos()
            );
            println!(
                "Backend code generation took {}ns",
                backend_code_generation_elapsed.as_nanos()
            );
            println!("Our side took {}millis", (frontend_elapsed + type_check_elapsed + byte_code_generation_elapsed + backend_code_generation_elapsed).as_millis());
            println!(
                "Writing generated code to disk {}ns",
                writing_generated_code_into_disk.as_nanos()
            );
            println!(
                "Calling backend compiler took {}ns",
                calling_backend_compiler.as_nanos()
            );
            println!("Whole compilation took {}milis", whole_thing.as_millis());
            println!("Total lines processed {}", compilation.total_lines);
        }

        Ok(())
    }
}
