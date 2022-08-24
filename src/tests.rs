use anyhow::Result;
use crate::compiler;
use crate::code_gen;

#[test]
fn test_00_variables_constants() -> Result<()> {
    let mut c = compiler::Compiler::new();
    c.compile_file("examples/00_variables_constants.loki", code_gen::Backend::CPP)?;

    Ok(())
}

#[test]
fn test_01_functions() -> Result<()> {
    let mut c = compiler::Compiler::new();
    c.compile_file("examples/01_functions.loki", code_gen::Backend::CPP)?;

    Ok(())
}


#[test]
fn test_02_assignments() -> Result<()> {
    let mut c = compiler::Compiler::new();
    c.compile_file("examples/02_assignments.loki", code_gen::Backend::CPP)?;

    Ok(())
}


#[test]
fn test_03_comparisons() -> Result<()> {
    let mut c = compiler::Compiler::new();
    c.compile_file("examples/03_comparisons.loki", code_gen::Backend::CPP)?;

    Ok(())
}

#[test]
fn test_04_structs() -> Result<()> {
    let mut c = compiler::Compiler::new();
    c.compile_file("examples/04_structs.loki", code_gen::Backend::CPP)?;

    Ok(())
}


#[test]
fn test_05_enums() -> Result<()> {
    let mut c = compiler::Compiler::new();
    c.compile_file("examples/05_enums.loki", code_gen::Backend::CPP)?;

    Ok(())
}

#[test]
fn test_06_namespace_access() -> Result<()> {
    let mut c = compiler::Compiler::new();
    c.compile_file("examples/06_namespace_access.loki", code_gen::Backend::CPP)?;

    Ok(())
}

#[test]
fn test_07_conditionals() -> Result<()> {
    let mut c = compiler::Compiler::new();
    c.compile_file("examples/07_conditionals.loki", code_gen::Backend::CPP)?;

    Ok(())
}

#[test]
fn test_08_while() -> Result<()> {
    let mut c = compiler::Compiler::new();
    c.compile_file("examples/08_while.loki", code_gen::Backend::CPP)?;

    Ok(())
}


#[test]
fn test_09_for() -> Result<()> {
    let mut c = compiler::Compiler::new();
    c.compile_file("examples/09_for.loki", code_gen::Backend::CPP)?;

    Ok(())
}
#[test]
fn test_10_pointers() -> Result<()> {
    let mut c = compiler::Compiler::new();
    c.compile_file("examples/10_pointers.loki", code_gen::Backend::CPP)?;

    Ok(())
}
