use anyhow::Result;
use std::fs;
use std::path::Path;

// #[test]
// fn test_suite() -> Result<()> {
//     let files = fs::read_dir("./examples").unwrap();
//     let mut results: Vec<Result<()>> = vec![];
//     let mut has_error = false;
//     let mut counter = 0;
//     let mut success = 0;
//     let mut failed = 0;
//     for file in files {
//         println!("============================================================");
//         let file = file.unwrap();
//         if file.file_type().unwrap().is_file() && Path::new(file.path().to_str().unwrap()).extension().unwrap() == "loki" {
//             counter += 1;
//             let mut p = pipeline::Pipeline::new();
//             match p.compile_file(file.path().to_str().unwrap(), code_gen::Backend::CPP) {
//                 Ok(_) => {
//                     success += 1;
//                     let os = std::env::consts::OS;
//                     if os == "windows" {
//                         std::fs::remove_file(format!("{}.exe", file.path().file_stem().unwrap().to_str().unwrap())).unwrap();
//                     } else {
//                         std::fs::remove_file(file.path().file_stem().unwrap()).unwrap();
//                     }
//                 },
//                 Err(e) => {
//                     has_error = true;
//                     failed+=1;
//                     println!("Error: test file {:?} error:\n {:?}", file.path().to_str().unwrap(), e);
//                 }
//             }
//         }
//     }

//     println!("=========================Results=============================");
//     println!("{} test files\n{} success\n{} failed.", counter, success, failed);


//     if has_error {
//         return Err(anyhow::format_err!("FAILED"));
//     }
//     Ok(())
// }
