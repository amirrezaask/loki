mod code_gen;
mod parser;
mod tokenizer;
use anyhow::Result;
use std::process::Command;
use std::{env, io::Write};
const about: &'static str = "loki compiler 0.2";
const usage: &'static str =
    "Usage: loki [command] [args]\n\temit-cpp: emits CPP code using C code backend.\n";

pub fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        println!("{}\n{}", about, usage);
        return Ok(());
    }
    let args = args[1..].to_vec();
    //    println!("{:?}", args);
    let command = &args[0];
    match command.as_str() {
        "emit-c" | "emit-cpp" => {
            if args.len() < 2 {
                panic!("emit-c needs a filename");
            }
            let file_name = &args[1];
            let program = std::fs::read_to_string(file_name)?;
            let mut tokenizer = crate::tokenizer::Tokenizer::new(program.as_str());
            let tokens = tokenizer.all()?;
            let mut parser = crate::parser::Parser::new_with_tokens(program.to_string(), tokens)?;
            let ast = parser.get_ast()?;
            let mut code_gen = crate::code_gen::c::C::new(ast);
            let code = code_gen.generate()?;

            println!("{}", code);
        }

        "compile" => {
            if args.len() < 2 {
                panic!("emit-c needs a filename");
            }
            let file_name = &args[1];
            let program = std::fs::read_to_string(file_name)?;
            let mut tokenizer = crate::tokenizer::Tokenizer::new(program.as_str());
            let tokens = tokenizer.all()?;
            let mut parser = crate::parser::Parser::new_with_tokens(program.to_string(), tokens)?;
            let ast = parser.get_ast()?;
            let mut code_gen = crate::code_gen::c::C::new(ast);
            let code = code_gen.generate()?;
            let out_file_name = format!("{}.cpp", file_name);
            let mut out_file = std::fs::File::create(&out_file_name)?;
            out_file.write_all(code.as_bytes())?;
            let cpp_output = Command::new("clang++").arg(&out_file_name).output()?;
            std::fs::remove_file(out_file_name)?;
            if !cpp_output.status.success() {
                println!(
                    "C++ compiler error: {}",
                    String::from_utf8_lossy(&cpp_output.stderr)
                );
            }
        }

        _ => {
            panic!("Unknown command: {}", command)
        }
    }
    Ok(())
}
