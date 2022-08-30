use anyhow::Result;
use crate::pipeline;
use crate::code_gen;
use std::fs;
use std::path::Path;

#[test]

fn test_suite() -> Result<()> {
    let files = fs::read_dir("./examples").unwrap();
    let mut results: Vec<Result<()>> = vec![];
    let mut has_error = false;
    let mut counter = 0;
    let mut success = 0;
    let mut failed = 0;
    for file in files {
        println!("============================================================");
        let file = file.unwrap();
        if file.file_type().unwrap().is_file() && Path::new(file.path().to_str().unwrap()).extension().unwrap() == "loki" {
            counter += 1;
            let mut p = pipeline::Pipeline::new();
            match p.compile_file(file.path().to_str().unwrap(), code_gen::Backend::CPP) {
                Ok(_) => {
                    success += 1;
                },
                Err(e) => {
                    has_error = true;
                    failed+=1;
                    println!("Error: test file {:?} error:\n {:?}", file.path().to_str().unwrap(), e);
                }
            }
        }
    }

    println!("=========================Results=============================");


    if has_error {
        return Err(anyhow::format_err!("FAILED"));
    }
    println!("{} test files\n{} success\n{} failed.", counter, success, failed);
    Ok(())
}
