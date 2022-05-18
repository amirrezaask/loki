use clap::ArgMatches;

use super::compile;


use anyhow::Result;
use super::core::*;

pub fn run(arg_matches: &ArgMatches) -> Result<()> {
    compile(arg_matches)?;
    let binary_name = get_output_binary_name(arg_matches);
    let res = std::process::Command::new(format!("./{}", binary_name)).output()?;
    println!("{}", String::from_utf8(res.stdout)?);

    std::fs::remove_file(binary_name)?;
    Ok(())
}