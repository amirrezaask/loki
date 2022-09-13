
use std::fmt::Debug;
use std::{collections::HashMap, hash::Hash};
use serde::Serialize;

use super::{ir::IR};
use super::typer::Type;
use crate::ir::{Node, Statement};
use crate::{errors::*, utils, parser::Parser, ir::NodeIndex};

pub type FileIndex = usize;
type File = String;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum DependencyReason {
    File(String),
    Node(NodeIndex),
    Identifier(String),
}


#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Dependency {
    pub file: File,
    pub node_index: NodeIndex,
    pub reason: DependencyReason,
}

pub struct Compilation {
    total_lines: u64,
    IRs: HashMap<File, IR>,
    exported_symbols: HashMap<String, HashMap<String, Type>>,
    dependencies: Vec<Dependency>,
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
        self.IRs.insert(abs_path.clone(), ast);
        Ok(abs_path.clone())
    }
    
    fn resolve_loads(&mut self, file: File) -> Result<()> {
        let file_ast = self.IRs.get(&file).unwrap();
        let loads = file_ast.get_loads();
        for load in &loads {
            if self.IRs.contains_key(load) {
                continue;
            }
            let loaded_file_ir = self.parse_file(load)?;
            self.resolve_loads(loaded_file_ir)?;
        }

        Ok(())
    }
    fn pretty_print_unknown_nodes(nodes: &HashMap<NodeIndex, Node>) {
        for (k,v) in nodes.iter() {
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

    pub fn new(main_file: &str) -> Result<()> {
        let mut compilation = Compilation { 
            total_lines: 0,
            IRs: HashMap::new(), 
            dependencies: vec![],
            exported_symbols: HashMap::new()
        };
        println!("[+] parsing main file: {}", main_file);
        // parse main file
        let main_file_abs_path = compilation.parse_file(main_file)?;
        // parse all loaded files recursively
        compilation.resolve_loads(main_file_abs_path.clone());
        println!("[+] resolved main file loads...");
        let main_ir = compilation.IRs.get_mut(&main_file_abs_path).unwrap();
        // now we have all of our files used in our program in compilation.IRs
        let keys: Vec<File> = compilation.IRs.keys().map(|f| f.clone()).collect();
        let mut keys_index: usize = 0;
        let mut finished_type_checking = 0;
        loop {
            if finished_type_checking == keys.len() {
                break;
            }
            let file = &keys[keys_index];
            let ir = compilation.IRs.get_mut(file).unwrap();
            if ir.type_checked {
                keys_index +=1;
                if keys_index >= keys.len() {
                    keys_index = 0;
                }
                continue;    
            }
            println!("[+] trying to type check file {}", file);

            // try to do a pass on the file and type as much as possible
            ir.type_root(&compilation.exported_symbols)?;
            Self::pretty_print(ir.dependencies.clone());
            Self::pretty_print_unknown_nodes(&ir.nodes);
            // get file exported symbols that are fully type checked and add them to compilation struct so other files know about these.
            let mut file_exports = compilation.exported_symbols.get_mut(file);
            if file_exports.is_none() {
                compilation.exported_symbols.insert(file.clone(), HashMap::new());
                file_exports = compilation.exported_symbols.get_mut(file);
            }
            let file_exports = file_exports.unwrap();
            for (k,v) in ir.exported_symbols.iter() {
                file_exports.insert(k.clone(), v.clone());
            }
            if ir.dependencies.len() == 0 && !ir.any_unknowns() {
                println!("[+] {} passed type check", file);
                finished_type_checking += 1;
                ir.type_checked = true;
            }
            keys_index +=1;
            if keys_index >= keys.len() {
                keys_index = 0;
            }
        }
        Ok(())
    }
}