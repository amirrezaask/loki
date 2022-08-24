use anyhow::Result;
use crate::pipeline;
use crate::code_gen;
use std::fs;

#[test]

fn test_suite() -> Result<()> {
    let files = fs::read_dir("./examples").unwrap();
    let mut results: Vec<Result<()>> = vec![];
    let mut has_error = false;
    for file in files {
        println!("============================================================");
        let file = file.unwrap();
        if file.file_type().unwrap().is_file()  {
            let mut p = pipeline::Pipeline::new();
            match p.compile_file(file.path().to_str().unwrap(), code_gen::Backend::CPP) {
                Ok(_) => {},
                Err(e) => {
                    has_error = true;
                    println!("Error: test file {:?} error:\n {:?}", file.path().to_str().unwrap(), e);
                }
            }
        }
    }

    if has_error {
        return Err(anyhow::format_err!("FAILED"));
    }

    Ok(())
}
