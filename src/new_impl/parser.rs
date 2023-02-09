use super::{lexer::Tokenizer, ast::{Node, Statement}};

pub struct Parser {
    tokenizer: Tokenizer,
}


impl Parser {
    pub fn new(tokenizer: Tokenizer) -> Self {
        Self { tokenizer }
    }

    pub fn parse() -> Node {
        Node::Statement(Statement::Block(vec![]))
    }
}