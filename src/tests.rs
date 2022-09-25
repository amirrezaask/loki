use anyhow::Result;
use std::fs;
use std::path::Path;

use crate::compliation;

#[test]
fn suite() -> Result<()> {
    let files = vec![
        "examples/variables_constants.loki",
        "examples/assignments.loki",
        "examples/arrays.loki",
        "examples/comparisons.loki",
        "examples/conditionals.loki",
        "examples/dynamic_array.loki",
        "examples/enums.loki",
        "examples/structs.loki",
        "examples/functions.loki",
        "examples/loops.loki",
        "examples/module_load.loki",
        "examples/pointers.loki",
        "examples/sizeandcast.loki",
        "examples/euler/01.loki",
        "examples/euler/02.loki"
    ];
    let mut results: Vec<Result<()>> = vec![];
    let mut has_error = false;
    let mut counter = 0;
    let mut success = 0;
    let mut failed = 0;
    for file in files {
        counter += 1;
        match compliation::Compilation::new(file, false, false) {
            Ok(_) => {
                success += 1;
                let os = std::env::consts::OS;
                if os == "windows" {
                    std::fs::remove_file(format!("{}.exe", file));
                } else {
                    std::fs::remove_file(file);
                }
            },
            Err(e) => {
                has_error = true;
                failed+=1;
                println!("Error: test file {:?} error:\n {:?}", file, e);
            }
        }
    }

    println!("=========================Results=============================");
    println!("{} test files\n{} success\n{} failed.", counter, success, failed);


    if has_error {
        return Err(anyhow::format_err!("FAILED"));
    }
    Ok(())
}
