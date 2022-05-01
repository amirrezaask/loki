mod tokenizer;
mod parser;
mod parser_combinator;

#[cfg(test)]
mod tests;

fn main() {
    let tokens = tokenizer::tokenize("fn_name(fn_name2(fn_name3(12)), 12, 34)").unwrap();
    println!("{:?}", parser::Parser::new(tokens).parse_next_expr().unwrap());
}



