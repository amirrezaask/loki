use anyhow::Result;
use crate::compiler;
use crate::code_gen;

#[test]
fn test_00_consts() -> Result<()> {
    let mut c = compiler::Compiler::new();
    c.compile_file("tests/00_consts.loki", code_gen::Backend::CPP)?;

    Ok(())
}

#[test]
fn test_01_variables() -> Result<()> {
    let mut c = compiler::Compiler::new();
    c.compile_file("tests/01_variables.loki", code_gen::Backend::CPP)?;

    Ok(())
}


#[test]
fn test_02_assignments() -> Result<()> {
    let mut c = compiler::Compiler::new();
    c.compile_file("tests/02_assignments.loki", code_gen::Backend::CPP)?;

    Ok(())
}


#[test]
fn test_03_comparisons() -> Result<()> {
    let mut c = compiler::Compiler::new();
    c.compile_file("tests/03_comparisons.loki", code_gen::Backend::CPP)?;

    Ok(())
}

#[test]
fn test_04_structs() -> Result<()> {
    let mut c = compiler::Compiler::new();
    c.compile_file("tests/04_structs.loki", code_gen::Backend::CPP)?;

    Ok(())
}


#[test]
fn test_05_enums() -> Result<()> {
    let mut c = compiler::Compiler::new();
    c.compile_file("tests/05_enums.loki", code_gen::Backend::CPP)?;

    Ok(())
}

#[test]
fn test_06_namespace_access() -> Result<()> {
    let mut c = compiler::Compiler::new();
    c.compile_file("tests/06_namespace_access.loki", code_gen::Backend::CPP)?;

    Ok(())
}

#[test]
fn test_07_conditionals() -> Result<()> {
    let mut c = compiler::Compiler::new();
    c.compile_file("tests/07_conditionals.loki", code_gen::Backend::CPP)?;

    Ok(())
}

#[test]
fn test_08_while() -> Result<()> {
    let mut c = compiler::Compiler::new();
    c.compile_file("tests/08_while.loki", code_gen::Backend::CPP)?;

    Ok(())
}


#[test]
fn test_09_for() -> Result<()> {
    let mut c = compiler::Compiler::new();
    c.compile_file("tests/09_for.loki", code_gen::Backend::CPP)?;

    Ok(())
}
#[test]
fn test_10_pointers() -> Result<()> {
    let mut c = compiler::Compiler::new();
    c.compile_file("tests/10_pointers.loki", code_gen::Backend::CPP)?;

    Ok(())
}
