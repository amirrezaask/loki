use std::path::Path;

use anyhow::{anyhow, Result};
use rand::Rng;

pub fn generate_node_id() -> String {
    const CHARSET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZ\
                            abcdefghijklmnopqrstuvwxyz\
                            ";
    const id_len: usize = 15;
    let mut rng = rand::thread_rng();

    let id: String = (0..id_len)
        .map(|_| {
            let idx = rng.gen_range(0..CHARSET.len());
            CHARSET[idx] as char
        })
        .collect();

    return id;
}

pub fn find_abs_path_to_file(name: &str) -> Result<String> {
    if std::env::current_dir()
        .unwrap()
        .join(Path::new(name))
        .is_file()
    {
        return Ok(Path::new(name)
            .to_str()
            .unwrap()
            .to_string()
            .replace("\\\\", "\\")
            .replace("\\", "/"));
    }
    let s = std::env::var("LOKI_MODULE_PATH").unwrap_or_else(|_| return ".".to_string());
    let mut paths: Vec<&str> = s.split(';').collect();
    for path in &paths {
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
            if file.file_name() == name
                || file.file_name() == format!("{}.{}", name, "loki").as_str()
            {
                return Ok(file.path().to_str().unwrap().to_string());
            }
        }
    }

    return Err(anyhow!(
        "file {} not found in LOKI_MODULE_PATH: {:?}",
        name,
        paths
    ));
}
