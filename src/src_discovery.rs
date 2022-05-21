use std::path::{Path, PathBuf};

use anyhow::Result;

pub struct SourceDiscovery {
    paths: Vec<String>,
}

impl SourceDiscovery {
    pub fn new(paths: Vec<String>) -> SourceDiscovery {
        Self {
            paths: paths.into_iter().map(|p| p.to_string()).collect(),
        }
    }

    pub fn read_file_to_string(&self, path: &str) -> Result<String> {
        for base_path in self.paths.iter() {
            let path: PathBuf = Path::new(base_path).join(path).iter().collect();
            if std::fs::metadata(path.clone())?.is_file() {
                return Ok(std::fs::read_to_string(path)?)
            }
        }

        Err(crate::errors::Error::unknown("".to_string()).into())
    }
}
