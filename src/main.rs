mod parser;

fn main() -> Result<(), parser::ParseErr> {

    println!("{:?}", parser::_struct("struct {\n\tname: string,\n\tage: int\n}".to_string())?);
    Ok(())
}



